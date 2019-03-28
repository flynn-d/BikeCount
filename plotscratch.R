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
