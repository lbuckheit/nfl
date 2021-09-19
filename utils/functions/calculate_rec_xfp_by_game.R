library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)

calculate_rec_xfp_by_game <- function(pbp_data_with_xyac, PTS_PER_RECEPTION) {
  xfp_targets <- pbp_data_with_xyac %>%
    select(
      season = season.x,
      game_id, play_id,
      posteam = posteam.x,
      receiver,
      receiver_id,
      yardline_100 = yardline_100.x,
      air_yards = air_yards.x,
      actual_yards_gained = yards_gained,
      complete_pass,
      cp,
      yac_prob = prob,
      gain
    ) %>%
    mutate(
      # Check whether the target is in the end zone (yardline_100==air_yards implies an end zone target)
      # The YAC function creates a copy of each play with a different gain number and then assigns a probability to the potential of that YAC happening
      gain = ifelse(yardline_100==air_yards, yardline_100, gain),
      yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
      # Calculate fantasy points that a completion would be worth with a given YAC (air_yards + YAC = gain)
      pts = PTS_PER_RECEPTION + (gain / 10) + ifelse(gain == yardline_100, 6, 0),
      # Can only run after the catch if you catch the ball first (cp = completion probability, a built-in nflfastR model)
      catch_run_prob = cp * yac_prob,
      # Calculate the chance of the run after the catch happening
      exp_pts = pts * catch_run_prob,
      exp_yards = gain * catch_run_prob,
      # Check whether the given YAC actually happened
      actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
      actual_pts = ifelse(actual_outcome==1, pts, 0),
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
      posteam,
      receiver,
      receiver_id,
      game_id
    ) %>%
    # Summary columns
    summarize(
      # Games played by a receiver (na.rm removed N/A entries)
      games = sum(game_played, na.rm = T),
      # Targets by a receiver
      targets = sum(target, na.rm = T),
      # Catches by a receiver
      catches = sum(actual_outcome, na.rm = T),
      # Total yards by a receiver
      yards = sum(ifelse(actual_outcome==1, gain, 0), na.rm = T),
      # Total touchdowns by a receiver (if the actual gain is = to the yardline it must be a touchdown)
      td = sum(ifelse(gain==yardline_100, actual_outcome, 0), na.rm = T),
      # Total points by a receiver
      pts = sum(actual_pts, na.rm = T),
      # Expected catches by a receiver = sum of (target * completion probability of the target)
      exp_catches = sum(ifelse(target==1, cp, NA), na.rm = T),
      # Expected yards for a receiver
      exp_yards = sum(exp_yards, na.rm = T),
      # Expected touchdowns for a receiver
      exp_td = sum(ifelse(gain==yardline_100, catch_run_prob, 0), na.rm = T),
      # Expected  points for a receiver
      exp_pts = sum(exp_pts, na.rm = T),
      # Difference between actual and expected 0.5 PPR points
      pts_over_exp = pts - exp_pts
    ) %>%
    ungroup
  
  return(xfp_targets)
}