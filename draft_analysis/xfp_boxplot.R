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

# TODO - Split this into RB and WR files

### Background Work ###

# Define variables
SEASON_TO_ANALYZE <- 2020
START_WEEK <- 1
END_WEEK <- 17
PTS_PER_RECEPTION <- 1

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


## Boxplots for receivers

# Grab only the receivers above a certain season points threshold
relevant_players <- concise_xfp_targets %>%
  group_by(gsis_id) %>%
  summarize(total_xfp = sum(exp_rec_pts)) %>%
  filter(total_xfp > 50)

# Filter by receiver type if you wish
relevant_receivers <- merge(relevant_players, players) %>%
  filter(position == "TE")

# Create list of season-long/other data to merge with the game-by-game data
relevant_receivers_with_adp <- merge(relevant_receivers, adp_data)

# Create a df of all the games by relevant receivers
receivers_to_plot = merge(concise_xfp_targets, relevant_receivers_with_adp)

# Plot
# To order by avg. xfp per game use reorder(player, -exP-rec_pts)
# To order by total season xfp, use reorder(player, -total_xfp)
# To order by IQR size use reorder(player, exp_rec_pts, IQR)
# To order by ADP use reorder(player, adp)
ggplot(receivers_to_plot, aes(x=reorder(player, adp), y=exp_rec_pts, label=player)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Player",
       y = str_glue("Exp.{PTS_PER_RECEPTION}PPR Pts."),
       title = str_glue("{SEASON_TO_ANALYZE} Expected {PTS_PER_RECEPTION}PPR Pts. Boxplots"),
       caption = "Via nflFastR"
  )

## Boxplots for RBs

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
  summarise(total_xfp = sum(exp_rec_pts, exp_rush_pts, na.rm=TRUE))

# Capture only players above a certain threshold for eventual graphing
players_meeting_points_threshold <- combined_xfp_aggregate %>%
  filter(total_xfp > 125) %>%
  select(player, total_xfp)

# Create list of season-long/other data to merge with the game-by-game data
rbs_to_merge <- merge(players_meeting_points_threshold, adp_data)

# Build a list of each player's combined rush/rec xfp on a game-by-game basis
combined_xfp_by_game <- dplyr::bind_rows(concise_xfp_rushes, concise_xfp_targets) %>%
  group_by(gsis_id, player, game_id) %>%
  summarise(
    xfp = sum(exp_rec_pts, exp_rush_pts, na.rm=TRUE)
  )

# Combine a list of all running back with a list of all players meeting the graphing threshold
# to produce a list of all running backs that will be graphed
relevant_rbs <- merge(rbs_to_merge, players) %>%
  filter(position == "RB") %>%
  select(gsis_id, player, total_xfp, adp)

# Then merge the above list with the list of all games to get all games played by relevant RBs
rb_xfp_by_game <- merge(combined_xfp_by_game, relevant_rbs)

# Plot
# To order by avg. xfp per game use reorder(player, -xfp)
# To order by total season xfp, use reorder(player, -total_xfp)
# To order by IQR size use reorder(player, xfp, IQR)
# To order by ADP use reorder(player, adp)
ggplot(rb_xfp_by_game, aes(x=reorder(player, -xfp), y=xfp, label=player)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Player",
       y = str_glue("Exp.{PTS_PER_RECEPTION}PPR Pts."),
       title = str_glue("{SEASON_TO_ANALYZE} Expected {PTS_PER_RECEPTION}PPR Pts. Boxplots"),
       caption = "Via nflFastR"
  )

