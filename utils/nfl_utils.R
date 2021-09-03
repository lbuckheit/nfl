library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)

load_historical_pbp_seasons <- function(start_season, end_season) {
  seasons <- start_season:end_season
  historical_pbp <- purrr::map_df(seasons, function(x) {
    readRDS(
      url(
        glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
      )
    )
  })
  
  return(historical_pbp)
}

calculate_rush_xfp_by_game <- function(pbp_data) {
  xfp_rushes <- pbp_data %>%
    filter(play_type == "run" & !is.na(rusher) & qb_scramble == 0 & season_type == "REG") %>%
    select(
      rusher,
      desc,
      yardline_100,
      yards_gained,
      touchdown,
      gsis_id = rusher_id,
      game_id
    ) %>%
    # Assign an xfp to each carry based on where the carry took place (using the earlier calculated data)
    mutate(xfp = xfp_carry[c(yardline_100), 2]) %>%
    group_by(
      rusher,
      gsis_id,
      game_id
    ) %>%
    summarize(
      carries = n(),
      exp_rush_pts = sum(xfp),
      games = length(unique(game_id)),
      rush_xfp_pp = exp_rush_pts / carries,
      actual_rush_pts = 0.1 * sum(yards_gained) + 6 * sum(touchdown)
    )
  
  return(xfp_rushes)
}
