library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
options(scipen = 9999)
# TODO - per game rather than total, now that the bye weeks have started

# define which seasons shall be loaded
seasons <- 2010:2019
historical_pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

xtd_historical <- historical_pbp %>%
  filter(play_type == "run" & qb_scramble == 0) %>%
  group_by(yardline_100) %>%
  summarize(n(), xtd = sum(rush_touchdown) / n()) %>%
  summarize(yardline_100, xtd)

# 2020 pbp data
pbp_df_2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

xtd_2020 <- pbp_df_2020 %>%
  filter(play_type == "run" & qb_scramble == 0 & season_type == "REG") %>%
  select(rusher, posteam, yardline_100, rusher_id, game_id) %>%
  # Assign an xfp to each carry based on where the carry took place
  mutate(xtd = xtd_historical[c(yardline_100), 2])

xtd_2020_players <- xtd_2020 %>%
  group_by(rusher, rusher_id) %>%
  summarize(carries = n(), games = length(unique(game_id)), xtd = sum(xtd)) %>%
  filter(carries >= 20)

xtd_2020_teams <- xtd_2020 %>%
  group_by(posteam) %>%
  summarize(carries = n(), games = length(unique(game_id)), xtd = sum(xtd))

ggplot(xtd_2020_teams, aes(x = reorder(posteam, -xtd), y = xtd)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Team",
       y = "Expected TDs",
       title = "2020 Expected Rushing Touchdowns",
       caption = "Via nflFastR"
  )
