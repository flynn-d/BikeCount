# Bike counter API on Broadway, Cambridge MA

# https://data.cambridgema.gov/resource/gxzm-dpwp.json
fetchtime <- Sys.time()
# source('get_dependencies.R') # Run this once on a new instance, may be time-consuming 
library(RSocrata)
library(dplyr)

# Grab data from API ----

# Load historical count once. Takes 7s initially to download, vs. 0.1s to load from working directory. 
# We will increment the historical data daily
if(length(grep("BikeCountHist.RData", dir()))==0){
  hist_count <- read.socrata("https://data.cambridgema.gov/resource/gxzm-dpwp.csv")
  save("hist_count", file = "BikeCountHist.RData")
} else { load("BikeCountHist.RData") }

rows_hist <- nrow(hist_count)
last_day <- max(hist_count$date)

# Try to get fresher data. Defaults to hist_count if nothing fresher found.
if(Sys.Date() > as.Date(last_day)){
  # Combine order and offset. Default is to order new to old, while our RSocrata query returns results old to new.
  response <- httr::GET(paste0("https://data.cambridgema.gov/resource/gxzm-dpwp.csv?$order=date&$offset=", rows_hist))
  
  # read_csv parses datetime, so need to provide correct time zone as well
  r_df <- readr::read_csv(httr::content(response, 
                                        as = "text", 
                                        type = "text/csv", 
                                        encoding = "utf-8"),
                 locale = readr::locale(tz = 'America/New_York')
                 )
  
  # Check to see if there is actually new data or if just delayed compared to Sys.Date()
  if(nrow(r_df) > 0){
    new_count <- as.data.frame(r_df)
    hist_count <- rbind(hist_count, new_count)
    count <- hist_count
  } else { 
      count <- hist_count }
}
save(list=c('hist_count'), file = 'BikeCountHist.RData')
fetchtimediff <- Sys.time() - fetchtime

# Get some metrics ----
metrictime <- Sys.time()

# read.socrata reads 'date' as date-time, all at midnight. Need to reformat as actualy date only, without time.
count <- as_tibble(count)

count <- count %>% 
  mutate(date = as.Date(date),
         year = format(datetime, '%Y'))

# Count by year, month, day of week, hour of day, for each day. This is is the input for the ZINB and RF models.
hourly_day = count %>%
  mutate(hour = as.numeric(format(datetime, '%H')),
         month = format(datetime, '%m')) %>%
  group_by(year, month, day, hour, date) %>%
  dplyr::summarise(total = sum(total),
                   entries = sum(entries),
                   exits = sum(exits))

# Count by hour of day and month.  Used in the hourly view. 
hourly_hour_month1 <- count %>%
  mutate(hour = as.numeric(format(datetime, '%H')),
         month = format(datetime, '%m')) %>%
  group_by(hour, month) %>%
  summarize(total = mean(total),
            entries = mean(entries),
            exits = mean(exits))

# Daily summary, no hourly breakdown. This is used in the default daily view, the day of week view, and the time series model.
daily = count %>%
  group_by(year, date, day_of_week = as.factor(day)) %>%
  dplyr::summarise(total = sum(total),
                   entries = sum(entries),
                   exits = sum(exits))

max_day = daily %>% dplyr::filter(total == max(total))

latest_day = daily %>% ungroup(daily) %>% dplyr::filter(date == max(date))

# Order day of week factor better
# levels(daily$day_of_week)
levels(daily$day_of_week) <- paste(c(6, 2, 7, 1, 5, 3, 4),
                                   levels(daily$day_of_week))
daily$day_of_week <- as.factor(as.character(daily$day_of_week))
levels(daily$day_of_week) <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                               'Thursday', 'Friday', 'Saturday')

metrictimediff <- Sys.time() - metrictime
# cat('Fetch took:', fetchtimediff, attr(fetchtimediff, 'units'), '\n') # Use these for profiling run time
# cat('Metrics took:', metrictimediff, attr(metrictimediff, 'units'), '\n')
