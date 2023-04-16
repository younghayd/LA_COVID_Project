require(readxl)
require(lubridate)
require(dplyr)
require(ggplot2)
require(bfsl)
require(grob)
require(ggiraph)

setwd("H:/LA_flask_observations/Final_outputs/data/Indy_flask_data/")

onroad_2015 <- 9.1
offroad_2015 <- 96.7
res_2015 <- 3.3
com_2015 <- 1.9
ind_2015 <- 1.2
air_2015 <- 8.5
rail_2015 <- 2.9
total_2015 <- 9.4

onroad_2011 <- 13.2
offroad_2011 <- 101.3
res_2011 <- 2.8
com_2011 <- 2.5
ind_2011 <- 1.3
air_2011 <- 9.3
rail_2011 <- 3.7
total_2011 <- 11.1


data <- data.frame(onroad = c("onroad", onroad_2011, onroad_2015), 
                   offroad = c("offroad", offroad_2011, offroad_2015),
                   res = c("res", res_2011, res_2015),
                   com = c("com", com_2011, com_2015),
                   ind = c("ind", ind_2011, ind_2015),
                   air = c("air", air_2011, air_2015),
                   rail = c("rail", rail_2011, rail_2015),
                   total = c("total", total_2011, total_2015),
                   year = c("year", 2011, 2015))

data <- data.frame(sector = c("onroad", "onroad","offroad", "offroad", "res", "res", "com", "com", "ind", "ind",
                            "air", "air", "rail", "rail", "total", "total"),
                     year = c(2011, 2015,2011, 2015,2011, 2015,2011, 2015,2011, 2015,2011, 2015,2011, 2015,2011, 2015),
                     RCO = c(onroad_2011, onroad_2015, offroad_2011, offroad_2015, res_2011, res_2015, com_2011, com_2015,
                             ind_2011, ind_2015, air_2011, air_2015, rail_2011, rail_2015, total_2011, total_2015))

plot <- ggplot() + theme_linedraw()

for(type in unique(data$sector)){
  type_data <- filter(data, sector == type)
  plot <- plot + geom_line(data = data, aes(x = year, y = RCO, colour = sector))
  
}


plot <- ggplot() + theme_linedraw() +
  geom_line(data = data, aes(x=year, y=onroad, color = year)) +
  geom_line(data = data, aes(x=year, y=offroad, color = year)) +
  geom_line(data = data, aes(x=year, y=res, color = year)) +
  geom_line(data = data, aes(x=year, y=com, color = year)) +
  geom_line(data = data, aes(x=year, y=ind, color = year)) +
  geom_line(data = data, aes(x=year, y=air, color = year)) +
  geom_line(data = data, aes(x=year, y=rail, color = year)) +
  geom_line(data = data, aes(x=year, y=total, color = year)) +
  labs(y = "RCO", x = 'year', title = "RCO_time_series") +
  theme(plot.title = element_text(hjust = 0.5))

yticks <- c(0, 5, 10, 15, 20, 90, 95, 100)

plot <- plot + scale_y_continuous(breaks=trans(yticks))

