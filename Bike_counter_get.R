# Bike counter API on Broadway, Cambridge MA

# https://data.cambridgema.gov/resource/gxzm-dpwp.json

# install.packages('RSocrata', dep=T)

# devtools::install_github('Chicago/RSocrata', ref='dev')

library(RSocrata)
library(tidyverse)
library(lubridate) 

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("~/Projects/BikeCount")

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
  r_df <- read_csv(httr::content(response, 
                                        as = "text", 
                                        type = "text/csv", 
                                        encoding = "utf-8")#,
                 #  locale = locale(tz = 'America/New_York')
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

# Get some metrics ----

# Day with the most total rides: max_day
# Week with most todal rides: max_week
# Today compared to same day last year: latest_day and last_year_compare
# Last week compared to same week last year: latest_complete_week and last_year_compare_week
# Year to date total trips and last year's comparison value: latest_ytd and ytd_compare

# read.socrata reads 'date' as date-time, all at midnight. Need to reformat as actualy date only, without time.
count <- as_tibble(count)

count <- count %>% 
  mutate(date = as.Date(date),
         year = year(datetime))

hourly_day = count %>%
  mutate(hour = as.numeric(format(datetime, '%H')),
         month = format(datetime, '%m')) %>%
  group_by(year, month, day, hour, date) %>%
  dplyr::summarise(total = sum(total),
                   entries = sum(entries),
                   exits = sum(exits))

hourly = count %>%
  mutate(hour = as.numeric(format(datetime, '%H')),
         month = format(datetime, '%m')) %>%
  group_by(year, month, day, hour) %>%
  dplyr::summarise(total = mean(total),
                   entries = mean(entries),
                   exits = mean(exits))

hourly_hour_month <- hourly %>%
  group_by(hour, month) %>%
  summarize(total = mean(total),
            entries = mean(entries),
            exits = mean(exits))

daily = count %>%
  group_by(year, date, day_of_week = as.factor(day)) %>%
  dplyr::summarise(total = sum(total),
                   entries = sum(entries),
                   exits = sum(exits))

max_day = daily %>% filter(total == max(total))

latest_day = daily %>% ungroup(daily) %>% filter(date == max(date))
last_year_compare = daily %>% filter(date == paste(year(latest_day$date)-1, month(latest_day$date), day(latest_day$date), sep="-"))

# Order day of week factor better
# levels(daily$day_of_week)
levels(daily$day_of_week) <- paste(c(6, 2, 7, 1, 5, 3, 4),
                                   levels(daily$day_of_week))
daily$day_of_week <- as.factor(as.character(daily$day_of_week))
levels(daily$day_of_week) <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                               'Thursday', 'Friday', 'Saturday')

# Year to date
latest_ytd = daily %>% 
  ungroup(daily) %>% 
  filter(year == max(year)) %>%
  dplyr::summarize(nrecord = n(), 
            ytd = sum(total))

ytd_compare = daily %>% 
  ungroup(daily) %>%
  filter(year == max(year)-1) %>%
  filter(date <= paste(year(latest_day$date)-1, month(latest_day$date), day(latest_day$date), sep="-")) %>%
  dplyr::summarize(nrecord = n(),
            ytd = sum(total))

# Weekly counts and comparison

weekly <- daily %>%
  mutate(weekofyear = week(date)) %>%
  group_by(year, weekofyear) %>%
  dplyr::summarize(complete_week = length(total) == 7,
                   total = sum(total))

max_week = weekly %>% filter(total == max(total)) # gives the max week of each year, since we did group_by year

latest_complete_week = weekly %>% 
  ungroup(weekly) %>%
  filter(year == max(year)) %>%
  filter(complete_week == T) %>%
  filter(weekofyear == max(weekofyear))

last_year_compare_week = weekly %>% filter(year == latest_complete_week$year-1, weekofyear == latest_complete_week$weekofyear)
