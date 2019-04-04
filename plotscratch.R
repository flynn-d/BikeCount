# Scratch plots

# Daily
# ideas: interactive time series plots with different time units as options on x: 
# date, day of week, year
# y: entries, exits, total

# Hourly
# Hour along x
# color for month
# drop-down filter for year... with compare?

# Heat map: columns for months, rows for hour of day, color for count or intensity of riders

gp <- ggplot(hourly_hour_month, aes(x = as.numeric(hour), y = total, color = month)) +
  geom_point() + geom_smooth(se = F, span = 0.3) +
  xlab('Hour of day') + ylab('Average count') + 
  theme_bw()

ggplotly(gp, tooltip= c('total', 'month'))

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

  