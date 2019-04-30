# Bike counter API on Broadway, Cambridge MA

# https://data.cambridgema.gov/resource/gxzm-dpwp.json # Broadway
# https://data.cambridgema.gov/resource/gqic-86ts.json # Hampshire, 6 months of data, not formatted the same as Broadway
# https://data.edmonton.ca/resource/tq23-qn4m.json # Edmonton

fetchtime <- Sys.time()
library(RSocrata)
library(dplyr)

# Grab data from API ----

api_get <- 'https://data.cambridgema.gov/resource/gxzm-dpwp.csv' # this will be a variable that the user selects by selecting a station or city name

pattern <- ".{9}(?=\\.csv$|\\.json)" # perl=TRUE
parse_socrata_ID <- regexpr(pattern, api_get, perl = T)
socrata_ID <- substring(api_get, first = parse_socrata_ID, last = parse_socrata_ID + attr(parse_socrata_ID, 'match.length') - 1)

# Load historical count once. Takes 7s initially to download, vs. 0.1s to load from working directory
hist_count_name = paste0("BikeCountHist_", socrata_ID, ".RData")

# We will increment the historical data daily
if(length(grep(hist_count_name, dir()))==0){
  hist_count <- read.socrata(api_get)
  hist_count <- hist_count[order(hist_count$datetime),]
  original_rownum <- nrow(hist_count)
  hist_count <- hist_count[!duplicated(hist_count[,c('datetime','day','entries','exits','total')]),]
  dedup_rownum <- nrow(hist_count); dup_rows = original_rownum - dedup_rownum
  save(list=c("hist_count", "dup_rows"), file = hist_count_name)
} else { load(hist_count_name) }

rows_hist <- nrow(hist_count)
last_day <- max(hist_count$date)

# Try to get fresher data. Defaults to hist_count if nothing fresher found.
if(Sys.Date() > as.Date(last_day)){
  # Combine order and offset. Default is to order new to old, while our RSocrata query returns results old to new.
  response <- httr::GET(paste0(api_get, "?$order=date&$offset=", rows_hist + dup_rows))
  
  if(grepl('csv', api_get)){
    # read_csv parses datetime, so need to provide correct time zone as well
    r_df <- readr::read_csv(httr::content(response, 
                                          as = "text", 
                                          type = "text/csv", 
                                          encoding = "utf-8"),
                   locale = readr::locale(tz = 'America/New_York')
                   )
  }  
  # Check to see if there is actually new data or if just delayed compared to Sys.Date()
  if(nrow(r_df) > 0){
    new_count <- as.data.frame(r_df)
    new_count <- new_count[order(new_count$datetime),]
    original_rownum <- nrow(new_count)
    new_count <- new_count[!duplicated(new_count[,c('datetime','day','entries','exits','total')]),]
    new_dedup_rownum <- nrow(new_count); new_dup_rows = original_rownum - new_dedup_rownum
    dup_rows = dup_rows + new_dup_rows

    hist_count <- rbind(hist_count, new_count)
    count <- hist_count
  } else { 
      count <- hist_count }
}
save(list=c('hist_count', 'dup_rows'), file = hist_count_name)
fetchtimediff <- Sys.time() - fetchtime

# Get some metrics ----
metrictime <- Sys.time()

# read.socrata reads 'date' as date-time, all at midnight. Need to reformat as actualy date only, without time.
# Also, rename variables here -- still fetch and store with original. Ensure they are numeric
count <- count %>% 
  mutate(date = as.Date(date),
         year = format(datetime, '%Y')) %>%
  rename(Total = total,
         Eastbound = exits,
         Westbound = entries) %>%
  mutate(Total = as.numeric(Total),
         Eastbound = as.numeric(Eastbound),
         Westbound = as.numeric(Westbound))

# Count by year, month, day of week, hour of day, for each day. This is is the input for the ZINB and RF models.
hourly_day = count %>%
  mutate(hour = as.numeric(format(datetime, '%H')),
         month = format(datetime, '%m')) %>%
  group_by(year, date, month, day, hour) %>%
  dplyr::summarise(Total = sum(Total),
                   Westbound = sum(Westbound),
                   Eastbound = sum(Eastbound))

# Count by hour of day and month.  Used in the hourly view. 
hourly_hour_month <- count %>%
  mutate(hour = as.numeric(format(datetime, '%H')),
         month = format(datetime, '%m')) %>%
  group_by(hour, month) %>%
  summarize(Total = mean(Total),
            Westbound = mean(Westbound),
            Eastbound = mean(Eastbound))

# Daily summary, no hourly breakdown. This is used in the default daily view, the day of week view, and the time series model.
daily = count %>%
  group_by(year, date, day_of_week = as.factor(day)) %>%
  dplyr::summarise(Total = sum(Total),
                   Westbound = sum(Westbound),
                   Eastbound = sum(Eastbound))

max_day = daily %>% dplyr::filter(Total == max(Total))

latest_day = daily %>% ungroup(daily) %>% dplyr::filter(date == max(date))

# Order day of week factor better
# levels(daily$day_of_week)
levels(daily$day_of_week) <- paste(c(6, 2, 7, 1, 5, 3, 4),
                                   levels(daily$day_of_week))
daily$day_of_week <- as.factor(as.character(daily$day_of_week))
levels(daily$day_of_week) <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                               'Thursday', 'Friday', 'Saturday')

#calculate monthly totals
monthly <- count %>%
  mutate(month = format(datetime, '%m')) %>%
  group_by(year, month_of_year = as.factor(month)) %>%
  dplyr::summarise(Total = sum(Total),
                   Westbound = sum(Westbound),
                   Eastbound = sum(Eastbound))

metrictimediff <- Sys.time() - metrictime
# cat('Fetch took:', fetchtimediff, attr(fetchtimediff, 'units'), '\n') # Use these for profiling run time
# cat('Metrics took:', metrictimediff, attr(metrictimediff, 'units'), '\n')
