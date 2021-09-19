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

#### Plotting expected FP vs actual FP ####

### Background Work ###

# Define variables
SELECTED_POSITION <- "TE"
SEASON_TO_ANALYZE <- 2021
START_WEEK <- 1
END_WEEK <- 1
PTS_PER_RECEPTION <- 0.5
TOTAL_XFP_THRESHOLD <- 5

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
xfp_targets <- calculate_rec_xfp_by_game(pbp_with_xyac, PTS_PER_RECEPTION)

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

concise_xfp_targets <- xfp_targets %>%
  select(
    game_id,
    player = receiver,
    gsis_id = receiver_id,
    rec_games = games,
    exp_rec_pts = exp_pts,
    actual_rec_pts = pts
  )

# Get the total (season-long) combined rush/rec xfp for players (for use in determining relevant players and graph ordering)
combined_xfp_aggregate <- dplyr::bind_rows(concise_xfp_rushes, concise_xfp_targets) %>%
  group_by(gsis_id, player) %>%
  summarise(
    total_xfp = sum(exp_rec_pts, exp_rush_pts, na.rm=TRUE),
    total_fp = sum(actual_rec_pts, actual_rush_pts, na.rm=TRUE)
  )

# Capture only players above a certain threshold for eventual graphing
players_meeting_points_threshold <- combined_xfp_aggregate %>%
  filter(total_xfp > TOTAL_XFP_THRESHOLD) %>%
  select(player, total_xfp, total_fp)

# Combine a list of all running back with a list of all players meeting the graphing threshold
# to produce a list of all players that will be graphed
relevant_players <- merge(players_meeting_points_threshold, players) %>%
  filter(position == SELECTED_POSITION) %>%
  select(gsis_id, player, total_xfp)

# Then merge the above list with the list of all games to get all games played by relevant players
player_xfp_vs_fp <- merge(players_meeting_points_threshold, relevant_players)

# Plot
ggplot(player_xfp_vs_fp, aes(x=total_xfp, y=total_fp, label=player)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = str_glue("Expected {PTS_PER_RECEPTION}PPR Pts."),
    y = str_glue("Actual {PTS_PER_RECEPTION}PPR Pts."),
    title = str_glue("{SEASON_TO_ANALYZE} {PTS_PER_RECEPTION}PPR {SELECTED_POSITION} XFP vs. FP (Weeks {START_WEEK}-{END_WEEK})"),
    caption = "Via nflFastR"
  )

