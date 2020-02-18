# Tidy Tuesday contribution
# Feb 18th, 2020
# Geraldine Klarenberg, PhD
# R-Ladies Gainesville

# Load packages
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# Load data
food_consumption <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Quick plots to explore the data
ggplot(food_consumption, aes(x = consumption, y = co2_emmission))+
  geom_point(aes(color=food_category))+
  scale_color_viridis_d()

ggplot(food_consumption, aes(x = food_category, y = co2_emmission))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Food category") + ylab("CO2 emissions per capita")

ggplot(food_consumption, aes(x = food_category, y = consumption))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Food category") + ylab("Consumption per capita")

ggplot(food_consumption, aes(x = country, y = co2_emmission))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Country") + ylab("CO2 emissions per capita")
# Whew, that last one is a lot. 

# Let's try and find the worst offenders. Workflow:
# 1. Find total emissions
# 2. Select the top 10
# 3. Plot the contribution of each food category to the CO2 emissions for those countries

# Sum emissions per country
sum_emissions <- food_consumption %>% 
  group_by(country) %>% 
  summarize(all_emission = sum(co2_emmission))

# Order the total emissions
sum_emissions_order <- arrange(sum_emissions, desc(all_emission)) 
# The top 10 CO2 emitters (based on agricultural products) are:
sum_emissions_order$country[1:10]

# Go back to the original data to extract the top 10 countries
select_countries_data <- food_consumption %>% 
  filter(country %in% sum_emissions_order$country[1:10])

# I want to have the countries in order of most emissions to less on my plots,
# so I have to make factors
select_countries_data$country <- factor(select_countries_data$country,
                                           levels = sum_emissions_order$country[1:10])

# Quick plot to see what the data looks like
# CO2 emissions per country, per food category
ggplot(select_countries_data, aes(x = country, y = co2_emmission))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  facet_wrap(.~food_category)+
  xlab("Country") + ylab("CO2 emissions per capita")

# Consumption per country per food category
ggplot(select_countries_data, aes(x = country, y = consumption, group=food_category))+
  geom_point()+
  scale_color_manual(values = terrain.colors(11))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  facet_wrap(.~food_category)+
  xlab("Country") + ylab("CO2 emissions per capita")+
  theme_bw()

# Make colors for the different types of food_categories
# make colors for animal products
animal_colors <- rev(brewer.pal(n = 5, name = "Reds")) # Beef, lamb & goat, pork, poultry, fish
animal_derived_colors <- c("#ffffd4", "#fed98e") # milk and eggs
# colors for plant products
plant_colors <- rev(brewer.pal(n=4, name = "Greens")) # wheat, rice, soybeans, nuts

all_colors <- c(animal_colors, animal_derived_colors, plant_colors)

# Reorder the food categories so it follows the colors
select_countries_data$food_category <- factor(select_countries_data$food_category,
                                              levels = c("Beef", "Lamb & Goat", "Pork",
                                                         "Poultry", "Fish",
                                                         "Milk - inc. cheese", "Eggs",
                                                         "Wheat and Wheat Products", "Rice",
                                                         "Soybeans", "Nuts inc. Peanut Butter"))

# Change milk to dairy, nuts just nuts, shorten wheat
levels(select_countries_data$food_category)[6] <- "Dairy"
levels(select_countries_data$food_category)[11] <- "Nuts"
levels(select_countries_data$food_category)[8] <- "Wheat/wheat products"

CO2 <- ggplot(select_countries_data, aes(x = country, y = co2_emmission))+
  geom_bar(stat = "identity", position = "stack", aes(fill=food_category))+
  scale_fill_manual(name = "Food category", values = all_colors)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.text = element_text(size = 14),
        text = element_text(size = 16))+
  xlab("") + ylab("CO2 emissions (kg/capita/yr)")+
  theme(legend.position = "none")+
  ggtitle("Food production related CO2 emissions, \nand food consumption",
          subtitle = "Top 10 CO2 emitting countries\n")+
  theme(plot.title = element_text(face = "bold"))


consumption <- ggplot(select_countries_data, aes(x = country, y = consumption))+
  geom_bar(stat = "identity", position = "stack", aes(fill=food_category))+
  scale_fill_manual(name = "", values = all_colors)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.text = element_text(size = 14),
        text = element_text(size = 16))+
  xlab("") + ylab("Consumption (kg/capita/yr)")+
  theme(legend.position = "top")

top_10 <- plot_grid(CO2, 
                    consumption , 
                    align = "h")

# pdf("top10_offenders.pdf", width = 20, height =10)
# top_10
# dev.off()

########### Do the reverse! ##########
# Go back to the original data to extract the top 10 countries
select_countries_data_least <- food_consumption %>% 
  filter(country %in% sum_emissions_order$country[(nrow(sum_emissions_order)-9):nrow(sum_emissions_order)])

# I want to have the countries in order of most emissions to less on my plots,
# so I have to make factors
select_countries_data_least$country <- factor(select_countries_data_least$country,
                                        levels = sum_emissions_order$country[(nrow(sum_emissions_order)-9):nrow(sum_emissions_order)])

# Reorder the food categories so it follows the colors
select_countries_data_least$food_category <- factor(select_countries_data_least$food_category,
                                              levels = c("Beef", "Lamb & Goat", "Pork",
                                                         "Poultry", "Fish",
                                                         "Milk - inc. cheese", "Eggs",
                                                         "Wheat and Wheat Products", "Rice",
                                                         "Soybeans", "Nuts inc. Peanut Butter"))

# Change milk to dairy, nuts just nuts, shorten wheat
levels(select_countries_data_least$food_category)[6] <- "Dairy"
levels(select_countries_data_least$food_category)[11] <- "Nuts"
levels(select_countries_data_least$food_category)[8] <- "Wheat/wheat products"

CO2_least <- ggplot(select_countries_data_least, aes(x = country, y = co2_emmission))+
  geom_bar(stat = "identity", position = "stack", aes(fill=food_category))+
  scale_fill_manual(name = "Food category", values = all_colors)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.text = element_text(size = 14),
        text = element_text(size = 16))+
  xlab("") + ylab("CO2 emissions (kg/capita/yr)")+
  theme(legend.position = "none")+
  ggtitle("",
          subtitle = "Bottom 10 CO2 emitting countries\n")+
  theme(plot.title = element_text(face = "bold"))


consumption_least <- ggplot(select_countries_data_least, aes(x = country, y = consumption))+
  geom_bar(stat = "identity", position = "stack", aes(fill=food_category))+
  scale_fill_manual(name = "", values = all_colors)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.text = element_text(size = 14),
        text = element_text(size = 16))+
  xlab("") + ylab("Consumption (kg/capita/yr)")+
  theme(legend.position = "none")

bottom_10 <- plot_grid(CO2_least, 
                    consumption_least, 
                    align = "h")

# pdf("bottom10_offenders.pdf", width = 20, height =10)
# bottom_10
# dev.off()

####### All together ######
all_countries <- plot_grid(top_10, bottom_10, ncol = 1, align = "v")
pdf("bottom_and_top10_offenders.pdf", width = 15, height =15)
all_countries
dev.off()

