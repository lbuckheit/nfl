library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)
# TODO - Would be nice to be able to break out TEs (https://github.com/samhoppen/NFL_Positions could help)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
air_yards <- data %>%
  filter(!is.na(receiver) & play_type == "pass" & down <= 4) %>%
  group_by(game_id, receiver) %>%
  # TODO - One issue here is that if a receiver played but didn't receive a target, it won't count, when really it should count as a terrible game
  mutate(game_played = ifelse(row_number()==1,1,0)) %>%
  ungroup %>%
  group_by(receiver, posteam) %>%
  summarize(games = sum(game_played),
            targets = n(),
            total_air_yards = sum(air_yards),
            aDoT = total_air_yards / targets,
            air_yards_pg = total_air_yards / games) %>%
  filter(targets >= 10)
  # filter(total_air_yards >= 200) %>%
  # ggplot(aes(x = reorder(receiver, -total_air_yards), y = total_air_yards)) +
  # geom_bar(stat="identity") +
  # theme(axis.text.x = element_text(angle = -90)) +
  # labs(x = "Player",
  #      y = "Total Air Yards",
  #      title = "Air Yards (Wk 1-2)",
  #      subtitle = "Minimum 200 Air Yards",
  #      caption = "Data from nflscrapR"
  # )

view(air_yards)


