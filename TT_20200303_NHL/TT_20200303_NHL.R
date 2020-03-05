# Tidy Tuesday


# Load packages
library(tidyverse)
library(cowplot)
# load functions to add shade to plot
source("add_shade.R")

# Get the Data

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')
top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')
season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

###### Look at differences between home and away games ######

##### Players ######

# home goals
game_goals_home <- game_goals %>% 
  filter(location == "Home")

game_goals_home2 <- game_goals_home %>% 
  group_by(player) %>% 
  summarize(goals_home_all = sum(goals, na.rm = TRUE), goals_home_ave = mean(goals, na.rm = TRUE))

# away goals
game_goals_away <- game_goals %>% 
  filter(location == "Away")

game_goals_away2 <- game_goals_away %>% 
  group_by(player) %>% 
  summarize(goals_away_all = sum(goals, na.rm = TRUE), goals_away_ave = mean(goals, na.rm = TRUE))

# merge
game_goals_summ <-  inner_join(game_goals_away2, game_goals_home2, by = "player")

# single out the players that do better
better_players <- game_goals_summ %>% 
  filter(goals_away_ave > goals_home_ave)

library(ggrepel)

player_plot <- ggplot(data = game_goals_summ, aes(x = goals_home_ave, y = goals_away_ave)) +
  geom_point() +
  geom_smooth(method=lm, color = "purple", lwd=1) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", lwd=1)+
  xlab("Average number of goals per home game") +
  ylab(xlab("Average number of goals per away game"))+
  geom_section(slope=1, intercept=0, above=TRUE, fill = "orange")+
  theme_bw() +
  theme(text = element_text(size = 16)) +
  geom_label_repel(data = better_players, aes(x = goals_home_ave, y = goals_away_ave, label = player),
                   nudge_y = .06) +
  annotate("text", x = 0.39, y = 0.65, label = "Shaded area:\nave away goals > ave home goals", size = 5)

########## Team ##########
# home
game_goals_home_team <- game_goals_home %>% 
  group_by(team) %>% 
  summarize(goals_home_all = sum(goals, na.rm = TRUE), goals_home_ave = mean(goals, na.rm = TRUE))

# away goals
game_goals_away_team <- game_goals_away %>% 
  group_by(team) %>% 
  summarize(goals_away_all = sum(goals, na.rm = TRUE), goals_away_ave = mean(goals, na.rm = TRUE))

game_goals_summ_team <-  inner_join(game_goals_away_team, game_goals_home_team, by = "team")

# single out the teams that do better
better_teams <- game_goals_summ_team %>% 
  filter(goals_away_ave > goals_home_ave)

team_plot <- ggplot(data = game_goals_summ_team, aes(x = goals_home_ave, y = goals_away_ave)) +
  geom_point() +
  geom_smooth(method=lm, color = "purple", lwd = 1) +
  geom_abline(intercept = 0, slope = 1, color = "darkorange", lwd = 1)+
  xlab("Average number of goals per home game") +
  ylab(xlab("Average number of goals per away game"))+
  geom_section(slope=1, intercept=0, above=TRUE, fill = "orange")+
  theme_bw()+
  theme(text = element_text(size = 16)) +
  geom_label_repel(data = better_teams, aes(x = goals_home_ave, y = goals_away_ave, label = team),
                                            nudge_y = .06)+
  annotate("text", x = 0.35, y = 0.6, label = "Shaded area:\nave away goals > ave home goals", size = 5)

########### Together ###########
all_plots <- plot_grid(player_plot, team_plot, ncol = 2)

pdf("away_home.pdf", width = 12, height = 6)
all_plots
dev.off()

jpeg("away_home.jpg", width = 960, height = 480)
all_plots
dev.off()


