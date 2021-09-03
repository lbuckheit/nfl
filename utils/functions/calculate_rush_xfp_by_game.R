library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)

source("utils/functions/calculate_rush_xfp_by_play.R")

calculate_rush_xfp_by_game <- function(pbp_data) {
  xfp_rushes <- calculate_rush_xfp_by_play(pbp_data) %>%
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
