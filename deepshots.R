library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
screens <- data %>%
  filter(down <= 4 & air_yards >= 25 & home_wp <= .75 & away_wp <= .75) %>%
  group_by(posteam) %>%
  summarize(deep_balls = n(),
            total_yards_gained = sum(yards_gained),
            gain_pp = total_yards_gained / deep_balls,
            total_epa = sum(epa),
            epa_pp = total_epa / deep_balls)

ggplot(screens, aes(x=deep_balls, y=epa_pp, label=posteam)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE)
