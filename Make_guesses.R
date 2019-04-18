# Models to use for making guesses (i.e., model the data and forecast next day)

# Guesses will be stored and compared with actual counts, and a running score will be kept
# Data required: hourly_day data frame for ZINB and RF, daily for time series forecast

# Setup ----
library(pscl) # for zero-inflated models
library(randomForest)
library(forecast)

REFIT = F # Set to T to re-fit the regression and ML models

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

# Load fitted models
load('Regression_models.RData')
load('Random_forest_models.RData')

# Standard regression approaches ----
# zero-inflated negative binomial
if(REFIT){
  hourly_mod6 <- zeroinfl(total ~ day + fhour + fyear + fmonth, 
                          data = hourly_day,
                          dist = 'negbin')
  hourly_mod6_entries <- zeroinfl(entries ~ day + fhour + fyear + fmonth, 
                          data = hourly_day,
                          dist = 'negbin')
  hourly_mod6_exits <- zeroinfl(exits ~ day + fhour + fyear + fmonth, 
                          data = hourly_day,
                          dist = 'negbin')
  
  save(list = c('hourly_mod6', 'hourly_mod6_entries', 'hourly_mod6_exits'),
       file = 'Regression_models.RData')
}

# Guess tomorrow
tomorrow_dat$total <- predict(hourly_mod6, tomorrow_dat, type = "response")
tomorrow_dat$exits <- predict(hourly_mod6_exits, tomorrow_dat, type = "response")
tomorrow_dat$entries <- predict(hourly_mod6_entries, tomorrow_dat, type = "response")

regression_guess_total <- sum(tomorrow_dat$total)  
regression_guess_entries <- sum(tomorrow_dat$entries)  
regression_guess_exits <- sum(tomorrow_dat$exits)  

# Time series approaches ----
dayts <- daily$total
start_day = min(daily$date)
end_day = max(daily$date)
dayts <- ts(dayts, 
            start = c(as.numeric(format(start_day, "%Y")),
                      as.numeric(format(start_day, "%j"))),
            end = c(as.numeric(format(end_day, "%Y")),
                    as.numeric(format(end_day, "%j"))),
            frequency = 365)

# Holt-Winter exponential smoothing
dayts_forecasts <- HoltWinters(dayts, beta=NULL, gamma=NULL)
ts_guess_total <- forecast(dayts_forecasts, h = 1)
ts_guess_total <- as.numeric(ts_guess_total$mean)

# Repeat, for entries
dayts <- daily$entries
start_day = min(daily$date)
end_day = max(daily$date)
dayts <- ts(dayts, 
            start = c(as.numeric(format(start_day, "%Y")),
                      as.numeric(format(start_day, "%j"))),
            end = c(as.numeric(format(end_day, "%Y")),
                    as.numeric(format(end_day, "%j"))),
            frequency = 365)

dayts_forecasts <- HoltWinters(dayts, beta=NULL, gamma=NULL)
ts_guess_entries <- forecast(dayts_forecasts, h = 1)
ts_guess_entries <- as.numeric(ts_guess_entries$mean)

# Repeat, for exits
dayts <- daily$exits
start_day = min(daily$date)
end_day = max(daily$date)
dayts <- ts(dayts, 
            start = c(as.numeric(format(start_day, "%Y")),
                      as.numeric(format(start_day, "%j"))),
            end = c(as.numeric(format(end_day, "%Y")),
                    as.numeric(format(end_day, "%j"))),
            frequency = 365)

dayts_forecasts <- HoltWinters(dayts, beta=NULL, gamma=NULL)
ts_guess_exits <- forecast(dayts_forecasts, h = 1)
ts_guess_exits <- as.numeric(ts_guess_exits$mean)

# ML approaches ----

# Random forest
if(REFIT){
  library(doParallel)
  
  train.dat = hourly_day
  response.var = c('total', 'entries', 'exits') 
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
  
  trainrows <- sort(sample(1:nrow(train.dat), size = nrow(train.dat)*(1-test.split), replace = F))
  testrows <- (1:nrow(train.dat))[!1:nrow(train.dat) %in% trainrows]
  rundat = train.dat[trainrows,]
  test.dat.use = train.dat[testrows,]
  
  # Start RF in parallel
  starttime = Sys.time()
  
  # make a cluster of all available cores
  cl <- makeCluster(rf.inputs$avail.cores, useXDR = F) 
  registerDoParallel(cl)
  
  # Loop over each response variable
  for(i in 1:length(response.var)){
  rf.out <- foreach(ntree = rep(rf.inputs$ntree.use/rf.inputs$avail.cores, rf.inputs$avail.cores),
                    .combine = randomForest::combine, .multicombine=T, .packages = 'randomForest') %dopar% 
    randomForest(x = rundat[,fitvars], y = rundat[,response.var[i]], 
                 ntree = ntree, mtry = mtry.use, 
                 maxnodes = rf.inputs$maxnodes, nodesize = rf.inputs$nodesize,
                 keep.forest = T)
  
  assign(paste0('rf_mod_', response.var[i]), rf.out)
  }
  stopCluster(cl); rm(cl); gc(verbose = F) # Stop the cluster immediately after finished the RF
  
  timediff = Sys.time() - starttime
  cat(round(timediff,2), attr(timediff, "unit"), "to fit RF models \n")
  
  # # Some diagnostics
  # rf.prob <- predict(rf.out, test.dat.use[fitvars])
  # 
  # ( mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
  #              as.numeric(rf.prob))^2 )
  
  rfmods = ls()[grep('rf_mod_', ls())]
  
  save(list = c('fitvars', 'rundat', rfmods),
       file = 'Random_forest_models.RData')
} 
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

tomorrow_dat$total_RF <- predict(rf_mod_total, tomorrow_dat[,fitvars])
tomorrow_dat$entries_RF <- predict(rf_mod_entries, tomorrow_dat[,fitvars])
tomorrow_dat$exits_RF <- predict(rf_mod_exits, tomorrow_dat[,fitvars])

rf_guess_total <- sum(tomorrow_dat$total_RF)
rf_guess_entries <- sum(tomorrow_dat$entries_RF)
rf_guess_exits <- sum(tomorrow_dat$exits_RF)

# next: store these guesses, display on dashboard
