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

xfp_carry <- historical_pbp %>%
  filter(play_type == "run" & down <= 4 & qb_scramble == 0) %>%
  group_by(yardline_100) %>%
  summarize(n(), rush_fp_pp = sum(yards_gained) * 0.1 / n(), td_fp_pp = sum(rush_touchdown) * 6 / n(), xfp_pp = rush_fp_pp + td_fp_pp) %>%
  summarize(yardline_100, xfp_pp)

# 2020 pbp data
pbp_df_2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

xfp_rushes_2020 <- pbp_df_2020 %>%
  filter(play_type == "run" & !is.na(rusher) & qb_scramble == 0 & season_type == "REG") %>%
  select(rusher, desc, yardline_100, yards_gained, touchdown, rusher_id, game_id) %>%
  # Assign an xfp to each carry based on where the carry took place (using the earlier calculated data)
  mutate(xfp = xfp_carry[c(yardline_100), 2]) %>%
  group_by(rusher, rusher_id) %>%
  summarize(carries = n(), total_rush_xfp = sum(xfp), games = length(unique(game_id)), rush_xfp_pp = total_rush_xfp / carries, actual_rush_fp = 0.1 * sum(yards_gained) + 6 * sum(touchdown)) %>%
  filter(carries >= 20)

# -------------
# Sourced almost entirely from: https://www.opensourcefootball.com/posts/2020-08-30-calculating-expected-fantasy-points-for-receivers/
# -------------

source('https://raw.githubusercontent.com/mrcaseb/nflfastR/a26830822df59b6f8d82e65c9723a141957d1da3/R/helper_add_xyac.R')
source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_nflscrapr_mutations.R')

# duplicate the add_xyac() function that we sourced above
add_xyac_dist <- add_xyac

# separate each block of code in the add_xyac_dist() function into blocks
add_xyac_blocks <- body(add_xyac_dist) %>% as.list

# we want to remove lines 51 to 62 from the 5th item in the list
add_xyac_blocks[[5]] <- add_xyac_blocks[[5]] %>% 
  format %>% 
  .[-(51:62)] %>% 
  paste(collapse = '\n') %>% 
  str2lang

# replace the body of add_xyac_dist() with our new edited function
body(add_xyac_dist) <- add_xyac_blocks %>% as.call

avg_exp_fp_df <- pbp_df_2020 %>%
  # Get passing plays
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>%
  # Add our custom xyac columns
  add_xyac_dist %>%
  # Select relevant columns
  select(season = season.x, game_id, play_id, posteam = posteam.x, receiver, receiver_id, yardline_100 = yardline_100.x, air_yards = air_yards.x, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = prob, gain) %>%
  mutate(
    # Check whether the target is in the end zone (yardline_100==air_yards implies an end zone target)
    # The YAC function creates a copy of each play with a different gain number and then assigns a probability to the potential of that YAC happening
    gain = ifelse(yardline_100==air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
    # Calculate 0.5 PPR fantasy points that a completion would be worth with a given YAC (air_yards + YAC = gain)
    hPPR_points = 0.5 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    # Can only run after the catch if you catch the ball first (cp = completion probability, a built-in nflfastR model)
    catch_run_prob = cp * yac_prob,
    # Calculate the chance of the run after the catch happening
    exp_hPPR_points = hPPR_points * catch_run_prob,
    exp_yards = gain * catch_run_prob,
    # Check whether the given YAC actually happened
    actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
    actual_hPPR_points = ifelse(actual_outcome==1, hPPR_points, 0),
    # Columns for use a few lines down
    target = 0,
    game_played = 0
  )  %>%
  # Indicating a single receiver game for use in the games played summary later
  # TODO - The games played count is a little fucked because it only registers games in which the player had a target I think
  group_by(game_id, receiver) %>%
  mutate(game_played = ifelse(row_number()==1,1,0)) %>%
  ungroup %>%
  # TODO - NOT ACTUALLY SURE WHAT'S GOING ON HERE.  IT HAS SOMETHING TO DO WITH ONLY RECORDING A SINGLE TARGET FOR THE MANY ROWS OF POTENTIAL YAC OUTCOMES
  group_by(game_id, play_id, receiver) %>%
  mutate(target = ifelse(row_number()==1,1,0)) %>%
  ungroup %>%
  group_by(posteam, receiver, receiver_id) %>%
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
    # Total 0.5 PPR points by a receiver
    hPPR_pts = sum(actual_hPPR_points, na.rm = T),
    # Expected catches by a receiver = sum of (target * completion probability of the target)
    exp_catches = sum(ifelse(target==1, cp, NA), na.rm = T),
    # Expected yards for a receiver
    exp_yards = sum(exp_yards, na.rm = T),
    # Expected touchdowns for a receiver
    exp_td = sum(ifelse(gain==yardline_100, catch_run_prob, 0), na.rm = T),
    # Expected 0.t PPR points for a receiver
    exp_hPPR_pts = sum(exp_hPPR_points, na.rm = T),
    # Difference between actual and expected 0.5 PPR points
    hPPR_over_exp = hPPR_pts - exp_hPPR_pts
  ) %>%
  ungroup

short_xfp_targets_2020 <- avg_exp_fp_df %>%
  select(receiver, receiver_id, games, exp_hPPR_pts, hPPR_pts) %>%
  summarize(player = receiver, gsis_id = receiver_id, rec_games = games, exp_hPPR_pts, actual_catch_fp = hPPR_pts)

short_xfp_rushes_2020 <- xfp_rushes_2020 %>%
  summarize(player = rusher, rush_games = games, total_rush_xfp, actual_rush_fp, gsis_id = rusher_id) %>%
  subset(select = -c(rusher)) # TODO - SHOULDN'T HAVE TO DO THIS

# TODO - Games column is a little fucked up because it only registers games in which the player had a target
# TODO - Malcolm and Marquise Brown are being combined (can fix by grouping by posteam or ID as well as player)
xfp_rb <- merge(short_xfp_rushes_2020, short_xfp_targets_2020)
xfp_rb$games <- pmax(xfp_rb$rush_games, xfp_rb$rec_games)
xfp_rb <- mutate(xfp_rb,
    total_xfp = total_rush_xfp + exp_hPPR_pts,
    actual_fp = actual_rush_fp + actual_catch_fp,
    total_xfp_pg = total_xfp / games
    ) %>%
  filter(games >= 3) # TODO - TEMP FIX FOR THE GAMES PLAYED ISSUES

short_xfp_rb <- xfp_rb %>%
  select(player, gsis_id, games, total_rush_xfp, actual_rush_fp, exp_hPPR_pts, actual_catch_fp, total_xfp, actual_fp, total_xfp_pg)
  
ggplot(short_xfp_rb, aes(x=total_xfp, y=actual_fp, label=player)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE)

write.csv(short_xfp_rb, "./draft_analysis/2020_xfp_rb.csv")
