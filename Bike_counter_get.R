# Bike counter API on Broadway, Cambridge MA

# https://data.cambridgema.gov/resource/gxzm-dpwp.json

# install.packages('RSocrata', dep=T)
library(RSocrata)
library(tidyverse)
library(lubridate) 

setwd("~/Projects/BikeCount")

# Grab data from API ----

# Load historical count once. Takes 7s initially to download, vs. 0.1s to load from working directory. # We will increment the historical data daily
if(length(grep("BikeCountHist.RData", dir()))==0){
  hist_count <- read.socrata("https://data.cambridgema.gov/resource/gxzm-dpwp.csv")
  save("hist_count", file = "BikeCountHist.RData")
} else { load("BikeCountHist.RData") }

rows_hist <- nrow(hist_count)
last_day <- max(hist_count$date)

# Try to get fresher data. Defaults to hist_count if nothing fresher found.
if(Sys.Date() > as.Date(last_day)){
  new_count <- read.socrata(paste0("https://data.cambridgema.gov/resource/gxzm-dpwp.csv?$offset=", rows_hist))
  if(nrow(new_count) > 0){
    count <- rbind(hist_count, new_count)
    } else { count <- hist_count }
  }

# Get some metrics ----

# Day with the most total rides: max_day
# Week witih most todal rides: max_week
# Today compared to same day last year: latest_day and last_year_compare
# Last week compared to same week last year: latest_complete_week and last_year_compare_week
# Year to date total trips and last year's comparison value: latest_ytd and ytd_compare

# read.socrata reads 'date' as date-time, all at midnight. Need to reformat as actualy date only, without time.
count$date <- as.Date(count$date)

daily <- count %>% 
  mutate(year = year(datetime)) %>%
  group_by(year, date) %>%
  summarize(daily_total = sum(total))

max_day = daily %>% filter(daily_total == max(daily_total))

latest_day = daily %>% ungroup(daily) %>% filter(date == max(date))
last_year_compare = daily %>% filter(date == paste(year(latest_day$date)-1, month(latest_day$date), day(latest_day$date), sep="-"))

# Year to date
latest_ytd = daily %>% 
  ungroup(daily) %>% 
  filter(year == max(year)) %>%
  summarize(nrecord = n(), 
            ytd = sum(daily_total))

ytd_compare = daily %>% 
  ungroup(daily) %>%
  filter(year == max(year)-1) %>%
  filter(date <= paste(year(latest_day$date)-1, month(latest_day$date), day(latest_day$date), sep="-")) %>%
  summarize(nrecord = n(),
            ytd = sum(daily_total))

# Weekly counts and comparison

weekly <- count %>%
  mutate(weekofyear = week(datetime),
         year = year(datetime)) %>%
  group_by(year, weekofyear) %>%
  summarize(weekly_total = sum(total),
            complete_week = length(total) == 24*7*4)

max_week = weekly %>% filter(weekly_total == max(weekly_total)) # gives the max week of each year, since we did group_by year

latest_complete_week = weekly %>% 
  ungroup(weekly) %>%
  filter(year == max(year)) %>%
  filter(complete_week == T) %>%
  filter(weekofyear == max(weekofyear))

last_year_compare_week = weekly %>% filter(year == latest_complete_week$year-1, weekofyear == latest_complete_week$weekofyear)


# Testing some visualizations ----

ggplot(daily, aes(x = date, y = daily_total)) +
  geom_point()


# Make a Shiny Dashboard