# Scratch plots

# Text ideas ----
# Link to City site: https://data.cambridgema.gov/dataset/Eco-Totem-Broadway-Bicycle-Counts-by-Date/9yzv-hx4u
# Add info on what to do with the buttons, other actions
# Add link to GitHub repo

# Plot ideas ----
# Daily
# ideas: interactive time series plots with different time units as options on x: 
# date, day of week, year
# y: Westbound, Eastbound, Total

# Hourly
# Hour along x
# color for month
# drop-down filter for year... with compare?

# Metrics moved from Bike_counter_get, since we're not using these right now.
# Day with the most Total rides: max_day
# Week with most todal rides: max_week
# Today compared to same day last year: latest_day and last_year_compare
# Last week compared to same week last year: latest_complete_week and last_year_compare_week
# Year to date Total trips and last year's comparison value: latest_ytd and ytd_compare

# Count by year, month, day of week, and hour of day.
hourly = count %>%
  mutate(hour = as.numeric(format(datetime, '%H')),
         month = format(datetime, '%m')) %>%
  group_by(year, month, day, hour) %>%
  dplyr::summarise(Total = mean(Total),
                   Westbound = mean(Westbound),
                   Eastbound = mean(Eastbound))

last_year_compare = daily %>% dplyr::filter(date == paste(as.numeric(format(latest_day$date, '%Y'))-1, format(latest_day$date, '%m'), format(latest_day$date, '%d'), sep="-"))

# Year to date
latest_ytd = daily %>% 
  ungroup(daily) %>% 
  dplyr::filter(year == max(year)) %>%
  dplyr::summarize(nrecord = n(), 
                   ytd = sum(Total))

ytd_compare = daily %>% 
  ungroup(daily) %>%
  dplyr::filter(year == max(as.numeric(year))-1) %>%
  dplyr::filter(date <= paste(as.numeric(format(latest_day$date, '%Y'))-1, format(latest_day$date, '%m'), format(latest_day$date, '%d'), sep="-")) %>%
  dplyr::summarize(nrecord = n(),
                   ytd = sum(Total))

# Weekly counts and comparison

weekly <- daily %>%
  mutate(weekofyear = format(date, '%U')) %>%
  group_by(year, weekofyear) %>%
  dplyr::summarize(complete_week = length(Total) == 7,
                   Total = sum(Total))

max_week = weekly %>% dplyr::filter(Total == max(Total)) # gives the max week of each year, since we did group_by year

latest_complete_week = weekly %>% 
  ungroup(weekly) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::filter(complete_week == T) %>%
  dplyr::filter(weekofyear == max(weekofyear))

last_year_compare_week = weekly %>% dplyr::filter(year == as.numeric(latest_complete_week$year)-1, weekofyear == latest_complete_week$weekofyear)


# System time zone: set in command line: sudo timedatectl set-timezone America/New_York
gp = ggplot(daily,
            aes_string(x = 'date', 
                       y = as.name('Total'), 
                       color = 'day_of_week')) +
  geom_point() + 
  xlab('Date') +
  theme_bw() 

ggp <- ggplotly(gp) %>% layout(xaxis = list(rangeslider = list(type = "date") ) )

ggp

# Heat map: columns for months, rows for hour of day, color for count or intensity of riders

gp <- ggplot(hourly_hour_month, aes(x = as.numeric(hour), y = Total, color = month)) +
  geom_point() + geom_smooth(se = F, span = 0.3) +
  xlab('Hour of day') + ylab('Average count') + 
  theme_bw()

ggplotly(gp, tooltip= c('Total', 'month')) %>% layout(xaxis = list(rangeslider = list(type = "date") ) )

# Westbound should be West bound -- much lower in 2016-2018, when Longfellow bridge was closed.
# Eastbound is East bound, going in to boston
# Confirmed by comparing with the source https://data.cambridgema.gov/dataset/Eco-Totem-Broadway-Bicycle-Counts-by-Date/9yzv-hx4u

daily %>%
  group_by(year) %>%
  summarize(mean(Westbound),
            mean(Eastbound))


# Weekly view
day_of_week = daily %>%
  group_by(year, day_of_week) %>%
  dplyr::summarize(Total = mean(Total, na.rm=T),
                   Westbound = mean(Westbound, na.rm=T),
                   Eastbound = mean(Eastbound, na.rm=T))

ggplot(daily, aes(x = day_of_week, y = Total, group = year)) + 
  geom_point()

gp <- ggplot(daily, aes(x = day_of_week, y = Total, color = as.factor(year))) +
  geom_point(aes(text = date)) +
  xlab('Day of week') + 
  theme_bw()

ggplotly(gp, tooltip= c('Total', 'date'))

  