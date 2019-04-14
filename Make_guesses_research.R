# Researching models to use for making guesses (i.e., model the data and forecast next day)

# Once decide on models to use, make a compact 'Make_guesses.R' script to models the bike count data and produces a guess for the next day
# Guesses will be stored and compared with actual counts, and a running score will be kept

# Setup ----
source('Bike_counter_get.R')
library(tidyverse)
library(MASS) # for negative binomial regression
library(pscl) # for zero-inflated models
library(randomForest)
library(doParallel)

# create 'tomorrow' data to guess on
today <- Sys.Date()
tomorrow <- today + 1

# Create factors for hour, year, and month, instead of numeric
hourly_day$day = as.factor(hourly_day$day)
hourly_day$fhour = as.factor(hourly_day$hour)
hourly_day$fyear = as.factor(hourly_day$year)
hourly_day$fmonth = as.factor(hourly_day$month)

tomorrow_dat <- data.frame(year = format(tomorrow, '%Y'),
                           month = format(tomorrow, '%m'),
                           day = format(tomorrow, '%A'),  # Full weekday name
                           hour = seq(0, 23, by = 1),
                           date = tomorrow)
tomorrow_dat$fhour = as.factor(tomorrow_dat$hour)
tomorrow_dat$fyear = as.factor(tomorrow_dat$year)
tomorrow_dat$fmonth = as.factor(tomorrow_dat$month)

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

# time series with seasonality

# ARIMA (Autoregressive integrated moving averages)

# ML approaches ----

# Random forest
# Code adapted from another project, can clean up later
train.dat = hourly_day
response.var = 'total'
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
rf.prob <- predict(rf.out, test.dat.use[fitvars])

( mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
             as.numeric(rf.prob))^2 )

# Make a guess with the RF model. Tomorrow_dat factors have to have the same levels as in the rundat, so need to add the empty levels

levadd <- function(factor_var){
  tlev <- levels(tomorrow_dat[,factor_var])
  addlev <- levels(rundat[,factor_var])[!levels(rundat[,factor_var]) %in% tlev]
  levels(tomorrow_dat[,factor_var]) = c(levels(tomorrow_dat[,factor_var]), addlev)
  tomorrow_dat[,factor_var]
}

for(i in fitvars) { 
  tomorrow_dat[,i] = levadd(factor_var = i) 
  }

tomorrow_dat$total_RF <- predict(rf.out, tomorrow_dat[,fitvars])

ml_guess <- sum(tomorrow_dat$total_RF)

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
