# Models to use for making guesses (i.e., model the data and forecast next day)

# Guesses will be stored and compared with actual counts, and a running score will be kept
# Data required: hourly_day data frame for ZINB and RF, daily for time series forecast

# Setup ----
library(pscl) # for zero-inflated models
library(randomForest)
library(forecast)

REFIT = F # Set to T to re-fit the regression and ML models

# Load historical weather, then forecast weather
hist_wx_file_name = paste0('Hist_WX_', socrata_ID, '.RData')

if(file.exists(hist_wx_file_name)){
  load(hist_wx_file_name) 
  } else {
  hist_wx1 <- get_historical_wx(limit_1000 = T, chunk = NA)
  save("hist_wx1", file = hist_wx_file_name)
  }

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
  hourly_mod6 <- zeroinfl(Total ~ day + fhour + fyear + fmonth, 
                          data = hourly_day,
                          dist = 'negbin')
  hourly_mod6_Westbound <- zeroinfl(Westbound ~ day + fhour + fyear + fmonth, 
                          data = hourly_day,
                          dist = 'negbin')
  hourly_mod6_Eastbound <- zeroinfl(Eastbound ~ day + fhour + fyear + fmonth, 
                          data = hourly_day,
                          dist = 'negbin')
  
  save(list = c('hourly_mod6', 'hourly_mod6_Westbound', 'hourly_mod6_Eastbound'),
       file = 'Regression_models.RData')
}

# Guess tomorrow
tomorrow_dat$Total     <- predict(hourly_mod6, tomorrow_dat, type = "response")
tomorrow_dat$Eastbound <- predict(hourly_mod6_Eastbound, tomorrow_dat, type = "response")
tomorrow_dat$Westbound <- predict(hourly_mod6_Westbound, tomorrow_dat, type = "response")

regression_guess_Total     <- sum(tomorrow_dat$Total)  
regression_guess_Westbound <- sum(tomorrow_dat$Westbound)  
regression_guess_Eastbound <- sum(tomorrow_dat$Eastbound)  

# Time series approaches ----
dayts <- daily$Total
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
ts_guess_Total <- forecast(dayts_forecasts, h = 1) # need to change to 2 for today and tomorrow
ts_guess_Total <- as.numeric(ts_guess_Total$mean)

# Repeat, for Westbound
dayts <- daily$Westbound
start_day = min(daily$date)
end_day = max(daily$date)
dayts <- ts(dayts, 
            start = c(as.numeric(format(start_day, "%Y")),
                      as.numeric(format(start_day, "%j"))),
            end = c(as.numeric(format(end_day, "%Y")),
                    as.numeric(format(end_day, "%j"))),
            frequency = 365)

dayts_forecasts    <- HoltWinters(dayts, beta=NULL, gamma=NULL)
ts_guess_Westbound <- forecast(dayts_forecasts, h = 1)
ts_guess_Westbound <- as.numeric(ts_guess_Westbound$mean)

# Repeat, for Eastbound
dayts <- daily$Eastbound
start_day = min(daily$date)
end_day = max(daily$date)
dayts <- ts(dayts, 
            start = c(as.numeric(format(start_day, "%Y")),
                      as.numeric(format(start_day, "%j"))),
            end = c(as.numeric(format(end_day, "%Y")),
                    as.numeric(format(end_day, "%j"))),
            frequency = 365)

dayts_forecasts    <- HoltWinters(dayts, beta=NULL, gamma=NULL)
ts_guess_Eastbound <- forecast(dayts_forecasts, h = 1)
ts_guess_Eastbound <- as.numeric(ts_guess_Eastbound$mean)

# ML approaches ----

# Random forest
if(REFIT){
  library(doParallel)
  
  train.dat = hourly_day
  response.var = c('Total', 'Westbound', 'Eastbound') 
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
                 ntree = ntree, mtry = rf.inputs$mtry, 
                 maxnodes = rf.inputs$maxnodes, nodesize = rf.inputs$nodesize,
                 keep.forest = T)
  
  # Some diagnostics
  rf.pred <- predict(rf.out, test.dat.use[fitvars], type = 'response')
  
  ( rmse = sqrt( 
              mean(
                c( as.numeric(as.character(test.dat.use[,response.var[i]])) - as.numeric(rf.pred) ) ^2 ) ) )
  
  assign(paste0('rf_mod_', response.var[i]), rf.out)
  assign(paste0('rf_pred_', response.var[i]), rf.pred)
  assign(paste0('rf_rmse_', response.var[i]), rmse)
  }
  stopCluster(cl); rm(cl); gc(verbose = F) # Stop the cluster immediately after finished the RF
  
  timediff = Sys.time() - starttime
  cat(round(timediff,2), attr(timediff, "unit"), "to fit RF models \n")
  
  rfmods = ls()[grep('rf_mod_', ls())]
  rfpreds = ls()[grep('rf_pred_', ls())]
  rfrmses= ls()[grep('rf_rmse_', ls())]
  
  save(list = c('fitvars', 'rundat', rfmods, rfpreds, rfrmses),
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

tomorrow_dat$Total_RF     <- predict(rf_mod_Total, tomorrow_dat[,fitvars])
tomorrow_dat$Westbound_RF <- predict(rf_mod_Westbound, tomorrow_dat[,fitvars])
tomorrow_dat$Eastbound_RF <- predict(rf_mod_Eastbound, tomorrow_dat[,fitvars])

rf_guess_Total     <- sum(tomorrow_dat$Total_RF)
rf_guess_Westbound <- sum(tomorrow_dat$Westbound_RF)
rf_guess_Eastbound <- sum(tomorrow_dat$Eastbound_RF)

# next: store these guesses, display on dashboard
