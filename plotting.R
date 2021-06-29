## Plotting ######
## Viet Dao ######
## Jun 23, 2020 ##

require(ggplot2)
require(ggimage)

########################
#### Aggregate data ####
########################
hanoi_plot <- hanoi
# get means by week
hanoi_plot$WEEK <- as.numeric(strftime(hanoi_plot$DATE, format="%V"))
hanoi_plot <- hanoi_plot %>% group_by(WEEK) %>% summarise_at(vars(-c('DATE', 'NAME')), list(mean=mean))
hanoi_plot$CITY <- 'HANOI'

stpeter_plot <- stpeter
# stpeter_plot <- stpeter_plot[stpeter_plot$DATE >= '2016-06-01',]
stpeter_plot$WEEK <- as.numeric(strftime(stpeter_plot$DATE, format="%V"))
# stpeter_plot['WEEK'][stpeter_plot$WEEK==53,] <- 52
stpeter_plot <- stpeter_plot %>% group_by(WEEK) %>% summarise_at(vars(-c('DATE', 'NAME')), funs(mean=mean(., na.rm=TRUE)))
stpeter_plot$CITY <- 'STPETER'

sf_plot <- sf
sf_plot$WEEK <- as.numeric(strftime(sf_plot$DATE, format="%V"))
sf_plot <- sf_plot %>% group_by(WEEK) %>% summarise_at(vars(-c('DATE', 'NAME')), funs(mean=mean(., na.rm=TRUE)))
sf_plot$CITY <- 'SF'

oakland_plot <- oakland
oakland_plot$WEEK <- as.numeric(strftime(oakland_plot$DATE, format="%V"))
oakland_plot <- oakland_plot %>% group_by(WEEK) %>% summarise_at(vars(-c('DATE', 'NAME')), list(mean=mean))
oakland_plot$CITY <- 'OAKLAND'

swarthmore_plot <- swarthmore
swarthmore_plot$WEEK <- as.numeric(strftime(swarthmore_plot$DATE, format="%V"))
swarthmore_plot <- swarthmore_plot %>% group_by(WEEK) %>% summarise_at(vars(-c('DATE', 'NAME')), list(mean=mean))
swarthmore_plot$CITY <- 'SWARTHMORE'

victoria_plot <- victoria
victoria_plot$WEEK <- as.numeric(strftime(victoria_plot$DATE, format="%V"))
victoria_plot <- victoria_plot %>% group_by(WEEK) %>% summarise_at(vars(-c('DATE', 'NAME')), funs(mean=mean(., na.rm=TRUE)))
victoria_plot$CITY <- 'VICTORIA'


####################
#### Make plots ####
####################

# 1 Temperature plot
# bind all cities together
temp_plot_data <- bind_rows(hanoi_plot, stpeter_plot, sf_plot, oakland_plot, swarthmore_plot, victoria_plot)

# ggplot(temp_plot_data, aes(x=WEEK, y=TMAX_mean, group=CITY, colour=CITY)) + 
#       geom_ribbon(aes(ymin=TMIN_mean, ymax=TMAX_mean, fill=CITY), alpha=0.6, linetype=0) +
#       scale_x_continuous(breaks = c(1, 14, 27, 40, 52),
#                          labels = c("1"="1 (Jan)", "14"="14 (Apr)", "27"="27 (Jul)", "40"="40 (Oct)", "52"="52 (Dec)")) + 
#       scale_y_continuous(breaks = pretty(temp_plot_data$TMAX_mean, n = 5)) +
#       labs(
#         x = 'Week',
#         y = 'Celcius',
#         title = 'Weekly Average Temperatures'
#       ) +
#       theme(plot.title = element_text(hjust = 0.5))

# 2 Snow plot
# snow days over year  
stpeter_snow_days <- nrow(stpeter %>% filter(SNWD > 0 | SNOW > 0))
swarthmore_snow_days <- nrow(swarthmore %>% filter(SNWD > 0 | SNOW > 0))
victoria_snow_days <- nrow(victoria %>% filter(SNWD > 0 | SNOW > 0))

# total snow fall over year
stpeter_snow_total <- sum(stpeter['SNOW'][stpeter$SNOW > 0, ])
swarthmore_snow_total <- sum(swarthmore['SNOW'][swarthmore$SNOW > 0, ])
victoria_snow_total <- sum(victoria['SNOW'][victoria$SNOW > 0, ])

CITY <- c("Hanoi/SF/Oakland", "St. Peter", "Swarthmore", "Victoria")
SNOW_TOTALS <- c(0, stpeter_snow_total, swarthmore_snow_total, victoria_snow_total)
SNOW_DAYS <- c(0, stpeter_snow_days, swarthmore_snow_days, victoria_snow_days)
snow_plot_data <- data.frame(CITY, SNOW_TOTALS, SNOW_DAYS)

# ggplot(snow_plot_data, aes(x=SNOW_TOTALS, y=SNOW_DAYS, label=CITY)) + 
#   geom_point(size=3) +
#   geom_image(aes(image=c("./snowflake.png"))) +
#   geom_text(hjust=-.15,vjust=.3) +
#   xlim(0, 650) +
#   ylim(0, 50) +
#   labs(
#     x = 'Total snow fall over year (mm)',
#     y = 'Number of snow days over year',
#     title = 'Snow Fall'
#   ) +
#   theme(plot.title = element_text(hjust = 0.5))

# 3 Precipitation plot
# ggplot(temp_plot_data, aes(x=WEEK, y=PRCP_mean, group=CITY, colour=CITY)) +
#       geom_ribbon(aes(ymin=0, ymax=PRCP_mean, fill=CITY), alpha=0.6, linetype=0) +
#       scale_x_continuous(breaks = c(1, 14, 27, 40, 52),
#                          labels = c("1"="1 (Jan)", "14"="14 (Apr)", "27"="27 (Jul)", "40"="40 (Oct)", "52"="52 (Dec)")) + 
#       scale_y_continuous(breaks = pretty(temp_plot_data$PRCP_mean, n = 5)) +
#       labs(
#         x = 'Week',
#         y = 'mm',
#         title = 'Weekly Average Precipitation'
#       ) +
#       theme(plot.title = element_text(hjust = 0.5))

