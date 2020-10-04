library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
plays <- data %>%
  filter(posteam == "DAL" & down <= 4 & yardline_100 <= 15) %>%
  summarize(home_team, away_team, down, ydstogo, yardline_100, goal_to_go, play_type, desc)

types <- plays %>%
  group_by(play_type) %>%
  summarize(number = n())
# View(plays)
View(types)
