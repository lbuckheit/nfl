library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)

calculate_rush_xfp_by_play <- function(pbp_data) {
  # Calculate the expected points from a carry at a given yardline
  xfp_carry <- read.csv("helpful_csvs/2000_2020_xfp_by_rush_location.csv")
  
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
    mutate(xfp = xfp_carry[c(yardline_100), 2])
  
  return(xfp_rushes)
}