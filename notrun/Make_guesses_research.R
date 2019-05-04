# Researching models to use for making guesses (i.e., model the data and forecast next day)

# Once decide on models to use, make a compact 'Make_guesses.R' script to models the bike count data and produces a guess for the next day
# Guesses will be stored and compared with actual counts, and a running score will be kept

# Setup ----
source('Bike_counter_get.R')
library(tidyverse)
library(MASS) # for negative binomial regression. Masks select from dplyr
library(pscl) # for zero-inflated models
library(randomForest)
library(doParallel)
library(forecast)

# create 'tomorrow' data to guess on
today <- Sys.Date()
tomorrow <- today + 1

# Load historical weather, then forecast weather
hist_wx_file_name = file.path('Data', paste0('Hist_WX_', socrata_ID, '.RData'))

if(file.exists(hist_wx_file_name)){
  load(hist_wx_file_name) 
} else {
  hist_wx <- get_historical_wx()
  # Only keep unique values (should only have unique rows)
  hist_wx <- hist_wx[!duplicated(hist_wx %>% select(time, datetime)),]
  save("hist_wx", file = hist_wx_file_name)
}

# Get forecasted weather, append and save
curr_wx <- get_curr_forecast_wx()

# Possible that some rows of curr_wx are present in hist_wx if get_curr_forecast_wx was run already for this day; only append new rows to hist_wx
curr_wx_add <- curr_wx[!curr_wx$time %in% hist_wx$time,] 
hist_wx <- suppressMessages(full_join(hist_wx, curr_wx_add))
save("hist_wx", file = hist_wx_file_name) 

# create 'today' and 'tomorrow' data to guess on
today <- Sys.Date()
tomorrow <- today + 1

today_dat <- data.frame(year = format(today, '%Y'),
                        month = format(today, '%m'),
                        day = format(today, '%A'),  # Full weekday name
                        hour = seq(0, 23, by = 1),
                        date = today,
                        stringsAsFactors = F)

tomorrow_dat <- data.frame(year = format(tomorrow, '%Y'),
                           month = format(tomorrow, '%m'),
                           day = format(tomorrow, '%A'),  # Full weekday name
                           hour = seq(0, 23, by = 1),
                           date = tomorrow,
                           stringsAsFactors = F)

curr_dat <- full_join(today_dat, tomorrow_dat,
                      by = c('year', 'date', 'month', 'day', 'hour'))

# Join with weather. 
hourly_day_wx <- left_join(hourly_day,
                           hist_wx,
                           by = c('year', 'date', 'month', 'day', 'hour'))

curr_dat_wx <- left_join(curr_dat,
                         curr_wx,
                         by = c('year', 'date', 'month', 'day', 'hour'))

# RF requires factors, not character vectors
hourly_day <- hourly_day %>%
  ungroup() %>%
  mutate(day = as.factor(day),
         fhour = as.factor(hour),
         fyear = as.factor(year),
         fmonth = as.factor(month))

curr_dat <- curr_dat %>%
  ungroup() %>%
  mutate(day = as.factor(day),
         fhour = as.factor(hour),
         fyear = as.factor(year),
         fmonth = as.factor(month))

hourly_day_wx <- hourly_day_wx %>%
  ungroup() %>%
  mutate(day = as.factor(day),
         fhour = as.factor(hour),
         fyear = as.factor(year),
         fmonth = as.factor(month),
         precipType = as.factor(precipType))

curr_dat_wx <- curr_dat_wx %>%
  ungroup %>%
  mutate(day = as.factor(day),
         fhour = as.factor(hour),
         fyear = as.factor(year),
         fmonth = as.factor(month),
         precipType = as.factor(precipType))

# Now creating a 'rainy' variable for precipProbability over 0.15. Otherwise, regression models having some matrix invertability problems -- not enough variation at some factor levels. Should write a tryCatch to first try probability, if errors then move to rainy factor.
hourly_day_wx$rainy <- hourly_day_wx$precipProbability >= 0.15
curr_dat_wx$rainy <- curr_dat_wx$precipProbability >= 0.15

# Standard regression approaches ----

# regression (no auto-regression or time series)
# normal
# poisson
# negative binomial
# zero-inflated negative binomial

hourly_mod0 <- glm(total ~ day + hour + year, data = hourly_day)
hourly_mod1 <- glm(total ~ day + hour + year, data = hourly_day,
                   family = 'poisson')

AIC(hourly_mod0, hourly_mod1) # Poisson distinctly worse in AIC, but correct for this distribution
hist(hourly_day$total) # are we overdispersed?
mean(hourly_day$total); var(hourly_day$total) # very much so

hourly_mod2 <- glm.nb(total ~ day + hour + year, data = hourly_day)

AIC(hourly_mod0, 
    hourly_mod1,
    hourly_mod2) # Much better with negative binomial!

# Proving the NB is better with a likelihood ratio test:
pchisq(2 * (logLik(hourly_mod2) - logLik(hourly_mod1)), df = 1, lower.tail = FALSE)

# NB with factor hour and year
hourly_mod3 <- glm.nb(total ~ day + fhour + fyear, data = hourly_day)

# NB with factor hour and year, and adding month as well
hourly_mod4 <- glm.nb(total ~ day + fhour + fyear + fmonth, data = hourly_day)

AIC(hourly_mod2, 
    hourly_mod3, # Better
    hourly_mod4) # Even better

pchisq(2 * (logLik(hourly_mod4) - logLik(hourly_mod3)), df = 1, lower.tail = FALSE) # 4 better significantly

# Zero-inflated Poisson and NB ----
# These are fit numerically by optim under the hood, so are more time consuming than OLS regressions above
hourly_mod5 <- zeroinfl(total ~ day + fhour + fyear + fmonth, data = hourly_day,
                        dist = 'pois')

hourly_mod6 <- zeroinfl(total ~ day + fhour + fyear + fmonth, 
                        data = hourly_day,
                        dist = 'negbin')

AIC(hourly_mod1, # Poisson 
    hourly_mod4, 
    hourly_mod5, # Worse -- Poisson still bad, but a lot better after accouting for excess zeros
    hourly_mod6) # New best model

# Coefficients of best model. These are incidence rate ratios, since we are dealing with count data. 
exp(coef(hourly_mod6))

# Guess tomorrow using best model
tomorrow_dat$total <- predict(hourly_mod6, tomorrow_dat, type = "response")

regression_guess <- sum(tomorrow_dat$total)  

# How does this guess compare to the mean for tomorrow's day of week and month of year? Just want to see ballpark. Looks good. For April Sundays, can see clear effect of temperature (was super cold in April 2018), which is not yet included.
hourly_day %>%
  filter(fmonth == as.character(tomorrow_dat$fmonth[1]),
         day == as.character(tomorrow_dat$day[1])) %>%
  group_by(year) %>%
  summarize(n_days = n()/24,
            ave_total = sum(total)/n_days)

# Time series approaches ----

# Time series with seasonality
# Transform to a time-series object; does not use any covariates
dayts <- daily$total
start_day = min(daily$date)
end_day = max(daily$date)
dayts <- ts(dayts, 
            start = c(as.numeric(format(start_day, "%Y")),
                      as.numeric(format(start_day, "%j"))),
            end = c(as.numeric(format(end_day, "%Y")),
                    as.numeric(format(end_day, "%j"))),
            frequency = 365)

# Decompose into trends, seasonality, and random

count_components <- decompose(dayts)
plot(count_components) # strongly non stationary, strong seasonal trend

# Holt-Winter exponential smoothing
# No covariates used

dayts_forecasts <- HoltWinters(dayts, beta=FALSE, gamma=FALSE)
plot(dayts_forecasts) # excellent 
dayts_forecasts2 <- forecast(dayts_forecasts, h = 365)
plot(dayts_forecasts2, include = 365*3)  # simple exponential smoothing fails at forecasting

dayts_forecasts <- HoltWinters(dayts, beta=NULL, gamma=NULL)
plot(dayts_forecasts) # pretty good
dayts_forecasts2 <- forecast(dayts_forecasts, h = 365)
plot(dayts_forecasts2, include = 365*3)  # Much better forecasts, maybe overestimating trend

# ARIMA (Autoregressive integrated moving averages)
# Can use with covariates as ARIMAX model
acf(dayts, lag.max = 365) # extremely strong autocorrelation
pacf(dayts, lag.max = 60) # strong weekly partial autocorrelation at 7, 14, 21.

# ARIMA models take three parameters: p, d, q. These are the AR order, degree of differencing, and MA order
suggest_arima <- auto.arima(dayts,
                            max.p = 30) # suggests ARIMA(5,1,3): 5th order AR, 3 MA components
# if increase max.p to 30 (i.e., this day's count is dependent on what happend on this day last month), suggested model is ARIMA(13, 1, 0)

# !!!! Very slow! 
suggest_arima_full <- auto.arima(dayts, 
                                 max.p = 30,
                                 D = 1, # Force it to look for seasonal pattern
                                 stepwise = F,
                                 seasonal = T) # Suggests ARIMA(5, 1, 0)

dayts_arima1 <- arima(dayts, order=c(5, 1, 0))
arima_forecast1 <- forecast(dayts_arima1, h = 365)
plot(arima_forecast1, include = 365) # quite bad

dayts_arima2 <- arima(dayts, order=c(13, 1, 3)) # Best model by AIC, but not great
arima_forecast2 <- forecast(dayts_arima2, h = 30)
plot(arima_forecast2, include = 120) # so-so for next month, but wrong trend

dayts_arima3 <- arima(dayts, order=c(13, 1, 0))
arima_forecast3 <- forecast(dayts_arima3, h = 30)
plot(arima_forecast3, include = 250) # same as above, but smoother

# Try specifying seasonality

dayts_arima4 <- arima(dayts, order=c(13, 1, 0),
                      seasonal = )
arima_forecast3 <- forecast(dayts_arima3, h = 30)
plot(arima_forecast3, include = 250) # same as above, but smoother


AIC(dayts_arima1, dayts_arima2, dayts_arima3)

# ARIMA guess

# TBATS is another option and can do a better job with multiple seasonalities... 
dayts_tbats1 <- tbats(dayts)
tbats_forecast1 <- forecast(dayts_tbats1, h = 120)
plot(tbats_forecast1, include = 30) # pretty bad, just gets general weekly pattern

# Try defining as multi-seasonal time series
daymsts <- daily$total
start_day = min(daily$date)
end_day = max(daily$date)
daymsts <- msts(daymsts,
                start = c(as.numeric(format(start_day, "%Y")),
                          as.numeric(format(start_day, "%j"))),
                end = c(as.numeric(format(end_day, "%Y")),
                        as.numeric(format(end_day, "%j"))),
            seasonal.periods = c(7, 365))
daymsts_tbats1 <- tbats(daymsts)
tbats_forecast2 <- forecast(daymsts_tbats1, h = 30)
plot(tbats_forecast2, include = 120) # still pretty bad except for near term forecasts


# ML approaches ----

# Random forest
# Code adapted from another project, can clean up later
train.dat = hourly_day
response.var = 'Total'
class(train.dat) = 'data.frame' # drop grouped_df, tbl

avail.cores = parallel::detectCores()
if(avail.cores > 8) avail.cores = 10 # Limit usage below max if on r4.4xlarge AWS instance (probably won't ever go that big)
rf.inputs = list(ntree.use = avail.cores * 50, 
                 avail.cores = avail.cores, 
                 mtry = 10,
                 maxnodes = 1000,
                 nodesize = 100)
test.split = .30

fitvars <- c('day', 'fhour', 'fyear', 'fmonth')

# Remove any rows with NA in predictors
cc <- complete.cases(train.dat[,fitvars])
train.dat <- train.dat[cc,]

# Provide mtry if null
if(is.null(rf.inputs$mtry)){
  mtry.use = if (!is.factor(response.var)) max(floor(length(fitvars)/3), 1) else floor(sqrt(length(fitvars)))
} else {mtry.use = rf.inputs$mtry}

trainrows <- sort(sample(1:nrow(train.dat), size = nrow(train.dat)*(1-test.split), replace = F))
testrows <- (1:nrow(train.dat))[!1:nrow(train.dat) %in% trainrows]
rundat = train.dat[trainrows,]
test.dat.use = train.dat[testrows,]

# Start RF in parallel
starttime = Sys.time()

# make a cluster of all available cores
cl <- makeCluster(rf.inputs$avail.cores, useXDR = F) 
registerDoParallel(cl)

rf.out <- foreach(ntree = rep(rf.inputs$ntree.use/rf.inputs$avail.cores, rf.inputs$avail.cores),
                  .combine = randomForest::combine, .multicombine=T, .packages = 'randomForest') %dopar% 
  randomForest(x = rundat[,fitvars], y = rundat[,response.var], 
               ntree = ntree, mtry = mtry.use, 
               maxnodes = rf.inputs$maxnodes, nodesize = rf.inputs$nodesize,
               keep.forest = T)

stopCluster(cl); rm(cl); gc(verbose = F) # Stop the cluster immediately after finished the RF

timediff = Sys.time() - starttime
cat(round(timediff,2), attr(timediff, "unit"), "to fit RF model \n")

# Some diagnostics
rf.pred <- predict(rf.out, test.dat.use[fitvars])

( mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
             as.numeric(rf.pred))^2 )
( rmse = sqrt( 
  mean(
    c( as.numeric(as.character(test.dat.use[,response.var])) - as.numeric(rf.pred) ) ^2 ) ) )

# Make a guess with the RF model. Tomorrow_dat factors have to have the same levels as in the rundat, so need to add the empty levels

levadd <- function(factor_var){
  tlev <- levels(curr_dat[,factor_var])
  addlev <- levels(rundat[,factor_var])[!levels(rundat[,factor_var]) %in% tlev]
  levels(curr_dat[,factor_var]) = c(levels(curr_dat[,factor_var]), addlev)
  curr_dat[,factor_var]
}

for(i in fitvars) { 
  curr_dat[,i] = levadd(factor_var = i) 
  }

curr_dat$Total_RF <- predict(rf.out, curr_dat[,fitvars])

ml_guess <- sum(curr_dat$Total_RF )

# next: store these guesses, display on dashboard

# CNN...


# EDA stuff ----

# Slight detour: Do we have outliers?
# look at the hours with the highest counts. Here select 500 highest values
max_total = hourly_day[order(hourly_day$total, decreasing =T),][1:500,]
table(max_total$hour) # Seems possible: max times are all at peak commuting hours

max_entries = hourly_day[order(hourly_day$entries, decreasing =T),][1:500,]
table(max_entries$hour) # Entries are W-bound coming from Bosotn, max values all at 5 or 6pm

max_exits = hourly_day[order(hourly_day$exits, decreasing =T),][1:500,]
table(max_exits$hour) # Exits are E-bound going to Boston, max values predominantly 8am.
