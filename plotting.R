## Plotting ######
## Viet Dao ######
## Jun 23, 2020 ##

library(ggplot2)

########################
#### Aggregate data ####
########################
hanoi_plot <- hanoi
# get means by week
hanoi_plot$WEEK <- as.numeric(strftime(hanoi_plot$DATE, format="%V"))
hanoi_plot <- hanoi_plot %>% group_by(WEEK) %>% summarise_at(vars(-c('DATE', 'NAME')), list(mean=mean))
hanoi_plot$CITY <- 'HANOI'

stpeter_plot <- stpeter
stpeter_plot$WEEK <- as.numeric(strftime(stpeter_plot$DATE, format="%V"))
stpeter_plot['WEEK'][stpeter_plot$WEEK==53,] <- 52
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

# bind all cities together
data_plot <- bind_rows(hanoi_plot, stpeter_plot, sf_plot, oakland_plot, swarthmore_plot, victoria_plot)


####################
#### Make plots ####
####################

# 1 Temperature plot
ggplot(data_plot, aes(x=WEEK, y=TMAX_mean, group=CITY, colour=CITY)) + 
      geom_ribbon(aes(ymin=TMIN_mean, ymax=TMAX_mean, fill=CITY), alpha=0.6, linetype=0) +
      scale_fill_manual(values=c("tan1", "red3", "royalblue3", "palegreen3", "brown2", "purple")) +
      scale_x_continuous(breaks = c(1, 14, 27, 40, 52),
                         labels = c("1"="Winter (Jan)", "14"="Spring (Apr)", "27"="Summer (Jul)", "40"="Fall (Oct)", "52"="Winter (Dec)")) + 
      scale_y_continuous(breaks = pretty(data_plot$TMAX_mean, n = 5)) +
      labs(
        x = 'Month',
        y = 'Celcius',
        title = 'Weekly Temperatures'
      ) +
      theme(plot.title = element_text(hjust = 0.5))

# 2 Snow plot  

# 3 Precipitation plot

