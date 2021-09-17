library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)
source("utils/nfl_utils.R")

### Background Work ###

# Define variables
PTS_PER_RECEPTION <- 1
SEASON_TO_ANALYZE <- 2020
START_WEEK <- 1
END_WEEK <- 17
XFP_PER_WEEK_THRESHOLD = 10

# Load ADP data
adp_data <- read.csv(file = "helpful_csvs/2021_clean_adp_data.csv") %>%
  select(gsis_id, adp)

# Grab the rosters for use in filtering by position
players <- nflfastR::fast_scraper_roster(SEASON_TO_ANALYZE) %>%
  subset(select = c(team, position, first_name, last_name, gsis_id))

# Load annual PBP Data
pbp_df <- load_pbp(SEASON_TO_ANALYZE) %>%
  filter(season_type == "REG", week >= START_WEEK, week <= END_WEEK)

### Expected points from rushing, grouped by game ###

xfp_rushes <- calculate_rush_xfp_by_game(pbp_df)

### Expected points from receiving ###

# Add xyac data to pbp
pbp_with_xyac <- add_xyac_to_pbp(pbp_df)

# Calculate xfp using xyac data
xfp_targets <- calculate_rec_xfp_by_game(pbp_with_xyac)

# Prune the dataframe only to what's necessary
concise_xfp_targets <- xfp_targets %>%
  select(
    game_id,
    player = receiver,
    gsis_id = receiver_id,
    rec_games = games,
    exp_rec_pts = exp_pts,
    actual_rec_pts = pts
  )

# Prune the dataframe only to what's necessary
concise_xfp_rushes <- xfp_rushes %>%
  select(
    player = rusher,
    gsis_id,
    game_id,
    rush_games = games,
    exp_rush_pts,
    actual_rush_pts
  )

# Get the total (season-long) combined rush/rec xfp for players (for use in determining relevant players and graph ordering)
combined_xfp_aggregate <- dplyr::bind_rows(concise_xfp_rushes, concise_xfp_targets) %>%
  group_by(gsis_id, player) %>%
  summarise(
    total_xfp = sum(exp_rec_pts, exp_rush_pts, na.rm=TRUE),
    actual_fp = sum(actual_rec_pts, actual_rush_pts, na.rm=TRUE)  
  )

# Capture only players above a certain threshold for eventual graphing
players_meeting_points_threshold <- combined_xfp_aggregate %>%
  filter(total_xfp > 125) %>%
  select(player, total_xfp)