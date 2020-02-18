# Tidy Tuesday
# Feb 18th

library(tidyverse)

food_consumption <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

ggplot(food_consumption, aes(x = food_category, y = co2_emmission))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Food category") + ylab("CO2 emissions per capita")

ggplot(food_consumption, aes(x = food_category, y = consumption))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Food category") + ylab("Consumption per capita")

