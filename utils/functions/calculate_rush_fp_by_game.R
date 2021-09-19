library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)

calculate_rush_fp_by_game <- function(pbp_data) {
  xfp_rushes <- pbp_data %>%
    filter(play_type == "run" & !is.na(rusher) & qb_scramble == 0 & season_type == "REG") %>%
    group_by(
      player = rusher,
      gsis_id = rusher_id
    ) %>%
    summarize(
      carries = n(),
      games = length(unique(game_id)),
      rush_yds = sum(yards_gained),
      rush_tds = sum(touchdown),
      actual_rush_pts = (0.1 * rush_yds) + (6 * rush_tds)
    )
  
  return(xfp_rushes)
}
