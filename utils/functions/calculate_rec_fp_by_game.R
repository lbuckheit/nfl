library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)

calculate_rec_fp_by_game <- function(pbp_data) {
  fp_targets <- pbp_data %>%
    select(
      game_id,
      play_id,
      posteam,
      receiver,
      receiver_id,
      complete_pass,
      yards_gained,
      touchdown
    ) %>%
    filter(complete_pass == 1) %>%
    mutate(
      actual_pts = PTS_PER_RECEPTION + (yards_gained / 10) + (touchdown * 6),
      # Columns for use a few lines down
      target = 0,
      game_played = 0
    )  %>%
    # Indicating a single receiver game for use in the games played summary later
    # TODO - The games played count is a little fucked because it only registers games in which the player had a target I think
    group_by(
      game_id,
      receiver
    ) %>%
    mutate(game_played = ifelse(row_number()==1,1,0)) %>%
    ungroup %>%
    # TODO - NOT ACTUALLY SURE WHAT'S GOING ON HERE.  IT HAS SOMETHING TO DO WITH ONLY RECORDING A SINGLE TARGET FOR THE MANY ROWS OF POTENTIAL YAC OUTCOMES
    group_by(
      game_id,
      play_id,
      receiver
    ) %>%
    mutate(target = ifelse(row_number()==1,1,0)) %>%
    ungroup %>%
    group_by(
      player = receiver,
      gsis_id = receiver_id
    ) %>%
    # Summary columns
    summarize(
      # Games played by a receiver (na.rm removed N/A entries)
      games = sum(game_played, na.rm = T),
      targets = sum(target, na.rm = T),
      catches = sum(complete_pass),
      rec_yards = sum(yards_gained),
      rec_tds = sum(touchdown),
      # Total fantasy points by a receiver
      actual_rec_pts = sum(actual_pts),
    )
  
  return(fp_targets)
}