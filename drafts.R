library(tidyverse)

hanoi_plot <- hanoi

ggplot(hanoi) +
  geom_ribbon(aes(ymin=hanoi$TMIN, ymax=hanoi$TMAX, fill="red"))
  # geom_line(mapping = aes(x = DATE, y = TMAX), colour = 'steelblue') +
  # geom_line(mapping = aes(x = DATE, y = TMIN), colour = 'steelblue')

# smoothing
ggplot(mtcars, aes(wt, mpg)) + geom_point() +
  geom_smooth(method = "gam", formula = y ~ poly(x, 2)) 
ggplot(mtcars, aes(wt, mpg)) + 
  geom_smooth(method = "loess", span = 0.3, se = FALSE) 

# area graph
ggplot(hanoi_plot, aes(x=hanoi_plot$WEEK, y=hanoi_plot$TMAX_mean, group=1)) + 
  geom_ribbon(aes(ymin=hanoi_plot$TMIN_mean, ymax=hanoi_plot$TMAX_mean, fill="grey80"), alpha=0.4) + 
  geom_line(aes(color="grey80")) +
  scale_fill_manual(values=c("tan1", "red3", "royalblue3", "palegreen3")) +
  scale_color_manual(values=c("brown", "red", "blue", "green4"))


dt <- data.frame(x=rep(1:7,2), group=rep(letters[1:2], each=7), value=runif(14))
dt$lwr <- dt$value*.9
dt$upr <- dt$value*1.1

# build plot in ggplot, don't want lines at the edge
pl <- ggplot(data=dt, aes(y=value, x=x, group=group, colour=group,
                          fill=group)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3, linetype=0) +
  theme_minimal()

# looks ok, no lines at the edges
pl

# no lines at edges
dd = ggplotly(pl)
dd$x$data[[3]]$line$color = "rgba(248,118,109,0.0)"
dd$x$data[[4]]$line$color = "rgba(0,191,196,0.0)"
dd