library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
options(scipen = 9999)

### Check the correlation between expected FP one year and actual FP the following year ###

# Define variables
SELECTED_POSITION <- "RB"
ACTUAL_FP_SEASON <- 2020
EXPECTED_FP_SEASON <- ACTUAL_FP_SEASON - 1
PTS_PER_RECEPTION <- 0.5
TOTAL_XFP_THRESHOLD <- 100

# Grab the rosters for use in filtering by position
players <- nflfastR::fast_scraper_roster(ACTUAL_FP_SEASON) %>%
  subset(select = c(team, position, first_name, last_name, gsis_id))

# Load annual PBP Data
actual_pbp_df <- load_pbp(ACTUAL_FP_SEASON) %>%
  filter(season_type == "REG")

exp_pbp_df <- load_pbp(EXPECTED_FP_SEASON) %>%
  filter(season_type == "REG")

### Actual scoring ###

actual_fp_rec <- calculate_rec_fp_by_game(actual_pbp_df)

actual_fp_rush <- calculate_rush_fp_by_game(actual_pbp_df)

combined_fp_aggregate <- dplyr::bind_rows(actual_fp_rec, actual_fp_rush) %>%
  group_by(player, gsis_id) %>%
  summarise(
    total_fp = sum(actual_rec_pts, actual_rush_pts, na.rm=TRUE)
  )

### Expected scoring, grouped by game ###

xfp_rushes <- calculate_rush_xfp_by_game(exp_pbp_df)### Expected points from receiving ###

# Add xyac data to pbp
pbp_with_xyac <- add_xyac_to_pbp(exp_pbp_df)

# Calculate xfp using xyac data
xfp_targets <- calculate_rec_xfp_by_game(pbp_with_xyac, PTS_PER_RECEPTION)

# Prune the dataframe only to what's necessary
concise_xfp_rushes <- xfp_rushes %>%
  select(
    player = rusher,
    gsis_id,
    game_id,
    rush_games = games,
    exp_rush_pts
  )

concise_xfp_targets <- xfp_targets %>%
  select(
    game_id,
    player = receiver,
    gsis_id = receiver_id,
    rec_games = games,
    exp_rec_pts = exp_pts
  )

# Get the total (season-long) combined rush/rec xfp for players (for use in determining relevant players and graph ordering)
combined_xfp_aggregate <- dplyr::bind_rows(concise_xfp_rushes, concise_xfp_targets) %>%
  group_by(gsis_id, player) %>%
  summarise(
    total_xfp = sum(exp_rec_pts, exp_rush_pts, na.rm=TRUE)
  )

combined_seasons <- merge(combined_xfp_aggregate, combined_fp_aggregate)

# Capture only players above a certain threshold for eventual graphing
players_meeting_points_threshold <- combined_seasons %>%
  filter(total_xfp > TOTAL_XFP_THRESHOLD) %>%
  select(player, gsis_id, total_xfp, total_fp)

# Combine a list of all players by position with a list of all players meeting the graphing threshold
# to produce a list of all players that will be graphed
relevant_players <- merge(players_meeting_points_threshold, players) %>%
  filter(position == SELECTED_POSITION) %>%
  select(gsis_id, player, total_xfp, total_fp)

# Plot
ggplot(relevant_players, aes(x=total_xfp, y=total_fp, label=player)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = str_glue("{EXPECTED_FP_SEASON} Expected {PTS_PER_RECEPTION}PPR Pts."),
    y = str_glue("{ACTUAL_FP_SEASON} Actual {PTS_PER_RECEPTION}PPR Pts."),
    title = str_glue("{EXPECTED_FP_SEASON}-{ACTUAL_FP_SEASON} {PTS_PER_RECEPTION}PPR {SELECTED_POSITION} XFP-FP Correlation"),
    caption = "Via nflFastR"
  )
