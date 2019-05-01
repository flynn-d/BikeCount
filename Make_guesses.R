# Models to use for making guesses (i.e., model the data and forecast next day)

# Guesses will be stored and compared with actual counts, and a running score will be kept
# Data required: hourly_day data frame for ZINB and RF, daily for time series forecast

# Setup ----
library(pscl) # for zero-inflated models
library(randomForest)
library(forecast)

REFIT = F # Set to T to re-fit the regression and ML models

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

load(file.path('Models', paste0('Regression_models_', socrata_ID,'.RData')))
load(file.path('Models', paste0('Random_forest_models_', socrata_ID,'.RData')))

# Standard regression approaches ----
# zero-inflated negative binomial
if(REFIT){
  hourly_mod_Total <- zeroinfl(Total ~ day + fhour + fmonth, 
                          data = hourly_day,
                          dist = 'negbin')
  
  cat('hourly_mod_Total complete \n')
  hourly_mod_wx_Total <- zeroinfl(Total ~ day + fhour + fmonth +
                                  precipProbability + temperature, 
                          data = hourly_day_wx,
                          dist = 'negbin')
  
  cat('hourly_mod_wx_Total complete \n')
  hourly_mod_Westbound <- zeroinfl(Westbound ~ day + fhour + fmonth, 
                          data = hourly_day,
                          dist = 'negbin')
  
  cat('hourly_mod_Westbound complete')
  hourly_mod_wx_Westbound <- zeroinfl(Westbound ~ day + fhour + fmonth +
                                      rainy + temperature, 
                                      data = hourly_day_wx,
                                      dist = 'negbin')
  
  cat('hourly_mod_wx_Westbound complete \n')
  hourly_mod_Eastbound <- zeroinfl(Eastbound ~ day + fhour + fmonth, 
                          data = hourly_day,
                          dist = 'negbin')
  
  cat('hourly_mod_Eastbound complete \n')
  
  hourly_mod_wx_Eastbound <- zeroinfl(Eastbound ~ day + fhour + fmonth +
                                      rainy + temperature, 
                                      data = hourly_day_wx,
                                      dist = 'negbin')
  
  cat('hourly_mod_wx_Eastbound complete \n')
  
  save(list = c('hourly_mod_Total', 'hourly_mod_Westbound', 'hourly_mod_Eastbound',
                'hourly_mod_wx_Total', 'hourly_mod_wx_Westbound', 'hourly_mod_wx_Eastbound'),
       file = file.path('Models', paste0('Regression_models_', socrata_ID,'.RData')))
}

# Guess today and tomorrow, with and without wx. 
# TODO: Clean this up with some functions to make it more flexible
curr_dat$Total     <- predict(hourly_mod_Total, curr_dat, type = "response")
curr_dat$Eastbound <- predict(hourly_mod_Eastbound, curr_dat, type = "response")
curr_dat$Westbound <- predict(hourly_mod_Westbound, curr_dat, type = "response")

regression_guess_Total     <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Total)))
regression_guess_Westbound <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Westbound)))
regression_guess_Eastbound <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Eastbound)))

regression_guess_Total_today     <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Total)))
regression_guess_Westbound_today <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Westbound)))
regression_guess_Eastbound_today <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Eastbound)))

curr_dat$Total_wx     <- predict(hourly_mod_wx_Total, curr_dat_wx, type = "response")
curr_dat$Eastbound_wx <- predict(hourly_mod_wx_Eastbound, curr_dat_wx, type = "response")
curr_dat$Westbound_wx <- predict(hourly_mod_wx_Westbound, curr_dat_wx, type = "response")

regression_guess_wx_Total     <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Total_wx)))
regression_guess_wx_Westbound <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Westbound_wx)))
regression_guess_wx_Eastbound <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Eastbound_wx)))

regression_guess_wx_Total_today     <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Total_wx)))
regression_guess_wx_Westbound_today <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Westbound_wx)))
regression_guess_wx_Eastbound_today <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Eastbound_wx)))

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
ts_guess_Total <- forecast(dayts_forecasts, h = 2) # need to change to 2 for today and tomorrow
ts_guess_Total_today <- as.numeric(ts_guess_Total$mean[1])
ts_guess_Total_tomorrow <- as.numeric(ts_guess_Total$mean[2])


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
ts_guess_Westbound <- forecast(dayts_forecasts, h = 2)
ts_guess_Westbound_today <- as.numeric(ts_guess_Westbound$mean[1])
ts_guess_Westbound_tomorrow <- as.numeric(ts_guess_Westbound$mean[2])

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
ts_guess_Eastbound <- forecast(dayts_forecasts, h = 2)
ts_guess_Eastbound_today <- as.numeric(ts_guess_Eastbound$mean[1])
ts_guess_Eastbound_tomorrow <- as.numeric(ts_guess_Eastbound$mean[2])

# ML approaches ----

# Random forest
if(REFIT){
  library(doParallel)
  
  avail.cores = parallel::detectCores()
  if(avail.cores > 8) avail.cores = 10 # Limit usage below max if on r4.4xlarge AWS instance (probably won't ever go that big)
  rf.inputs = list(ntree.use = avail.cores * 50, 
                   avail.cores = avail.cores, 
                   mtry = 10,
                   maxnodes = 1000,
                   nodesize = 100)
  test.split = .30
  
  train.dat = hourly_day
  response.var = c('Total', 'Westbound', 'Eastbound') 
  class(train.dat) = 'data.frame' # drop grouped_df, tbl
  
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
  cl <- makeCluster(rf.inputs$avail.cores) 
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
  cat(round(timediff,2), attr(timediff, "unit"), "to fit RF models without wx \n")
  
  rfmods  = ls()[grep('rf_mod_', ls())]
  rfpreds = ls()[grep('rf_pred_', ls())]
  rfrmses = ls()[grep('rf_rmse_', ls())]
  
  # Add weather to RF ----
  train.dat = hourly_day_wx
  response.var = c('Total', 'Westbound', 'Eastbound') 
  class(train.dat) = 'data.frame' # drop grouped_df, tbl
  
  fitvars_wx <- c('day', 'fhour', 'fyear', 'fmonth', 'precipIntensity', 'precipProbability', 'temperature', 'windSpeed', 'visibility', 'precipType')
  
  # Remove any rows with NA in predictors
  cc <- complete.cases(train.dat[, fitvars_wx])
  train.dat <- train.dat[cc, c(response.var, fitvars_wx)]
  
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
      randomForest(x = rundat[,fitvars_wx], y = rundat[,response.var[i]], 
                   ntree = ntree, mtry = rf.inputs$mtry, 
                   maxnodes = rf.inputs$maxnodes, nodesize = rf.inputs$nodesize,
                   keep.forest = T)
    
    # Some diagnostics
    rf.pred <- predict(rf.out, test.dat.use[fitvars_wx], type = 'response')
    
    ( rmse = sqrt( 
      mean(
        c( as.numeric(as.character(test.dat.use[,response.var[i]])) - as.numeric(rf.pred) ) ^2 ) ) )
    
    assign(paste0('rf_mod_wx_', response.var[i]), rf.out)
    assign(paste0('rf_pred_wx_', response.var[i]), rf.pred)
    assign(paste0('rf_rmse_wx_', response.var[i]), rmse)
  }
  stopCluster(cl); rm(cl); gc(verbose = F) # Stop the cluster immediately after finished the RF
  
  timediff = Sys.time() - starttime
  cat(round(timediff,2), attr(timediff, "unit"), "to fit RF models with wx \n")
  
  rfmods  = ls()[grep('rf_mod_', ls())]
  rfpreds = ls()[grep('rf_pred_wx_', ls())]
  rfrmses = ls()[grep('rf_rmse_wx_', ls())]
  
  save(list = c('fitvars', 'fitvars_wx', 'rundat', rfmods, rfpreds, rfrmses),
       file = file.path('Models', paste0('Random_forest_models_', socrata_ID,'.RData')))
} 
# Make a guess with the RF model. Tomorrow_dat factors have to have the same levels as in the rundat, so need to add the empty levels

levadd <- function(factor_var, curr = curr_dat, run = rundat){
  tlev <- levels(curr[,factor_var])
  addlev <- levels(run[,factor_var])[!levels(run[,factor_var]) %in% tlev]
  levels(curr[,factor_var]) = c(levels(curr[,factor_var]), addlev)
  curr[,factor_var]
}

for(i in fitvars) { 
  curr_dat[,i] = levadd(factor_var = i, curr_dat, rundat) 
}

for(i in fitvars_wx) { 
  curr_dat_wx[,i] = levadd(factor_var = i, curr_dat_wx, rundat) 
}

curr_dat$Total_RF     <- predict(rf_mod_Total, curr_dat[,fitvars])
curr_dat$Westbound_RF <- predict(rf_mod_Westbound, curr_dat[,fitvars])
curr_dat$Eastbound_RF <- predict(rf_mod_Eastbound, curr_dat[,fitvars])

rf_guess_Total     <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Total_RF)))
rf_guess_Westbound <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Westbound_RF)))
rf_guess_Eastbound <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Eastbound_RF)))

rf_guess_Total_today     <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Total_RF)))
rf_guess_Westbound_today <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Westbound_RF)))
rf_guess_Eastbound_today <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Eastbound_RF)))

# with wx
curr_dat$Total_wx_RF     <- predict(rf_mod_wx_Total, curr_dat_wx[,fitvars_wx])
curr_dat$Westbound_wx_RF <- predict(rf_mod_wx_Westbound, curr_dat_wx[,fitvars_wx])
curr_dat$Eastbound_wx_RF <- predict(rf_mod_wx_Eastbound, curr_dat_wx[,fitvars_wx])

rf_guess_wx_Total     <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Total_wx_RF)))
rf_guess_wx_Westbound <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Westbound_wx_RF)))
rf_guess_wx_Eastbound <- as.numeric(curr_dat %>% filter(date == tomorrow) %>% summarize(sum(Eastbound_wx_RF)))

rf_guess_wx_Total_today     <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Total_wx_RF)))
rf_guess_wx_Westbound_today <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Westbound_wx_RF)))
rf_guess_wx_Eastbound_today <- as.numeric(curr_dat %>% filter(date == today) %>% summarize(sum(Eastbound_wx_RF)))

# next: store these guesses, display on dashboard
