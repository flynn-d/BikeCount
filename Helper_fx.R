# Helper functions for bike_counter_get
library(jsonlite)

# Define station characteristics. Will define these for each station
station_meta = data.frame(
  url = 'https://data.cambridgema.gov/resource/gxzm-dpwp.csv',
  name = 'Broadway',
  id_name = NA, # To be used when there is a station ID inside the data
  city_state = 'Cambridge, MA',
  lat = 42.363464,
  lon = -71.086202,
  Total = 'total',
  dir1 = 'exits',
  dir2 = 'entries',
  dir1name = 'Eastbound',
  dir2name = 'Westbound',
  tz = 'America/New_York')

# Get historical weather for a station from DarkSky. Must have a DarkSky API key. Assumes Bike_counter_get.R has already been run and 'api_get' and 'daily' are in working memory 
get_historical_wx <- function(limit_1000 = F, chunk = c(NA, 'iter_date_range2')){
  # Get key.
  if(!exists('key')) key = scan('../keys/DarkSky_Key.txt', what = 'character')
  # Get lat and long from station_meta
  ll = station_meta %>% filter(url == api_get) %>% select(lat, lon)
  tz = station_meta %>% filter(url == api_get) %>% select(tz)
  
  # Find date range to look obtain
  hist_date_range <- range(daily$date)

  iter_date_range = seq(hist_date_range[1], hist_date_range[2], by = 1)
  
  # If limiting to 1000 queries, get the appropriate 1000 query chunk
  if(limit_1000){
    # Make this more flexible at some point, works for now
    if(length(iter_date_range) > 1000 & length(iter_date_range) <= 2000) {
      iter_date_range2 = iter_date_range[1001:length(iter_date_range)]
      iter_date_range = iter_date_range[1:1000]
    }
    
    if(!is.na(chunk)){
      iter_date_range = get(chunk)
      }
    } 
  
  hist_wx <- vector()
  for(i in 1:length(iter_date_range)){ # i = 1
    query_time = format(
                        as.numeric(
                                   as.POSIXct(as.character(iter_date_range[i]), format = '%F', tz = as.character(tz$tz))
                                   ), 
                        scientific = F)
    
    query = paste('https://api.darksky.net/forecast',
                   key, 
                   paste(as.numeric(ll[1]), as.numeric(ll[2]), query_time, sep = ','),
                   sep = '/')
    
    hist_response <- fromJSON(query)
    
    # Add any additional columns if new ones found in this iteration.
    # Since hist_response can have different columns, just use full_join with no messages rather than specifying columns with 'by'.
    if(i == 1) { 
      hist_wx = hist_response$hourly$data
    } else {
      hist_wx <- suppressMessages( full_join(hist_wx, hist_response$hourly$data) )
    }
  if(i %% 100 == 0) cat(as.character(iter_date_range[i]), " . ")
  }
  
  # Format datetime
  hist_wx <- hist_wx %>%
    mutate(datetime = as.POSIXct(time, origin = '1970-01-01', tz = as.character(tz$tz)),
           date = format(datetime, '%F'),
           year = format(datetime, '%Y'),
           month = format(datetime, '%m'),
           day = format(datetime, '%A'),
           hour = format(datetime, '%H'))
  
  # Prepare variable types and fill NA with zero for specific columns
  hist_wx <- hist_wx %>% 
    mutate(date = as.Date(date),
           hour = as.numeric(hour),
           day = as.character(day)) %>%
    tidyr::replace_na(list(precipIntensity = 0,
                           precipProbability = 0,
                           precipAccumulation = 0,
                           precipType = 'None'))
  
  hist_wx
  }

# Get current and forecast weather 
get_curr_forecast_wx <- function(){
  # Get key.
  if(!exists('key')) key = scan('../keys/DarkSky_Key.txt', what = 'character')
  # Get lat and long from station_meta
  ll = station_meta %>% filter(url == api_get) %>% select(lat, lon)
  tz = station_meta %>% filter(url == api_get) %>% select(tz)
  
  # Find date range to obtain: Start from last day in hist_wx (or today, whichever is minimum, and go to tomorrow
  last_day_in_hist = max(as.Date(hist_wx$date))
  start_day = min(Sys.Date(), last_day_in_hist)
  get_date_range <- c(start_day, Sys.Date() + 1)
  
  if(length(get_date_range) > 2) {
    iter_date_range = seq(get_date_range[1], get_date_range[2], by = 1) 
    } else { iter_date_range = get_date_range }
  
  curr_wx <- vector()
  for(i in 1:length(iter_date_range)){ # i = 1
    query_time = format(
      as.numeric(
        as.POSIXct(as.character(iter_date_range[i]), format = '%F', tz = as.character(tz$tz))
      ), 
      scientific = F)
    
    query = paste('https://api.darksky.net/forecast',
                  key, 
                  paste(as.numeric(ll[1]), as.numeric(ll[2]), query_time, sep = ','),
                  sep = '/')
    
    curr_response <- fromJSON(query)
    
    # Add any additional columns if new ones found in this iteration.
    # Since hist_response can have different columns, just use full_join with no messages rather than specifying columns with 'by'.
    if(i == 1) { 
      curr_wx = curr_response$hourly$data
    } else {
      curr_wx <- suppressMessages( full_join(curr_wx, curr_response$hourly$data) )
    }
  }
  
  # Format datetime
  curr_wx <- curr_wx %>%
    mutate(datetime = as.POSIXct(time, origin = '1970-01-01', tz = as.character(tz$tz)),
           date = format(datetime, '%F'),
           year = format(datetime, '%Y'),
           month = format(datetime, '%m'),
           day = format(datetime, '%A'),
           hour = format(datetime, '%H'))
  
  # Prepare variable types and fill NA with zero for specific columns
  curr_wx <- curr_wx %>% 
    mutate(date = as.Date(date),
           hour = as.numeric(hour),
           day = as.character(day)) %>%
    tidyr::replace_na(list(precipIntensity = 0,
                           precipProbability = 0,
                           precipType = 'None'))
  
  curr_wx
}
# as.POSIXct(1555902000, origin = '1970-01-01', tz = 'America/New_York') 

# Detect gaps, interpolate values for missing data if the gap is small

# Aim to run on hist_count in initial data collection or on new_count when incrementing
# also... there are duplicates! need to filter out for double-counted data.
# Ok, for now, not filling gaps, just deleting duplicate rows. Consider imputation later.
NOTRUN = T
if(!NOTRUN){
# fill_gaps <- function(count_dat = hist_count){
    count_dat <- count_dat[order(count_dat$datetime),]
    # detect any gaps, e.g. count_dat %>% filter(date > '2017-07-29' & date < '2017-08-02')
    dt <- diff(count_dat$date)
    dt_gaps <- which(as.numeric(dt, 'days') > 1.05)
    count_dat[dt_gaps,]
    
    count_dat[73594:(73594+70),]
    
    as.numeric(diff(count_dat[37171:(37171+24),'date']), 'days')
  
}
  
# Not a function, but a snippet to make an animated 'Fork Me' badge in the dashboard
# From https://codepen.io/Rplus/pen/wKZOBo

octocat_badge = '<a href="https://github.com/flynn-d/BikeCount" class="github-corner" aria-label="View source on GitHub"><svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;" aria-hidden="true"><path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path><path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path><path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path></svg></a><style>.github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}</style>'

