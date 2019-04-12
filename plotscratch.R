# Scratch plots

# Text ideas ----
# Link to City site: https://data.cambridgema.gov/dataset/Eco-Totem-Broadway-Bicycle-Counts-by-Date/9yzv-hx4u
# Add info on what to do with the buttons, other actions
# Add link to GitHub repo

# Plot ideas ----
# Daily
# ideas: interactive time series plots with different time units as options on x: 
# date, day of week, year
# y: entries, exits, total

# Hourly
# Hour along x
# color for month
# drop-down filter for year... with compare?


# System time zone: set in command line: sudo timedatectl set-timezone America/New_York
gp = ggplot(daily,
            aes_string(x = 'date', 
                       y = as.name('total'), 
                       color = 'day_of_week')) +
  geom_point() + 
  xlab('Date') +
  theme_bw() 

ggp <- ggplotly(gp) %>% layout(xaxis = list(rangeslider = list(type = "date") ) )

ggp

# Heat map: columns for months, rows for hour of day, color for count or intensity of riders

gp <- ggplot(hourly_hour_month, aes(x = as.numeric(hour), y = total, color = month)) +
  geom_point() + geom_smooth(se = F, span = 0.3) +
  xlab('Hour of day') + ylab('Average count') + 
  theme_bw()

ggplotly(gp, tooltip= c('total', 'month')) %>% layout(xaxis = list(rangeslider = list(type = "date") ) )

# Entries should be West bound -- much lower in 2016-2018, when Longfellow bridge was closed.
# Exits is East bound, going in to boston

daily %>%
  group_by(year) %>%
  summarize(mean(entries),
            mean(exits))


# Weekly view
day_of_week = daily %>%
  group_by(year, day_of_week) %>%
  dplyr::summarize(total = mean(total, na.rm=T),
                   entries = mean(entries, na.rm=T),
                   exits = mean(exits, na.rm=T))

ggplot(daily, aes(x = day_of_week, y = total, group = year)) + 
  geom_point()

gp <- ggplot(daily, aes(x = day_of_week, y = total, color = as.factor(year))) +
  geom_point(aes(text = date)) +
  xlab('Day of week') + 
  theme_bw()

ggplotly(gp, tooltip= c('total', 'date'))

  