library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)

### Background Work ###

# Define variables
SEASON_TO_ANALYZE <- 2020
PTS_PER_RECEPTION <- 0.5

# Load Historical Data
seasons <- 2010:2019
historical_pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

# Calculate the expected points from a carry at a given yardline
xfp_carry <- historical_pbp %>%
  filter(play_type == "run" & down <= 4 & qb_scramble == 0) %>%
  group_by(yardline_100) %>%
  summarize(
    n(),
    rush_fp_pp = sum(yards_gained) * 0.1 / n(),
    td_fp_pp = sum(rush_touchdown) * 6 / n(),
    xfp_pp = rush_fp_pp + td_fp_pp
  ) %>%
  summarize(yardline_100, xfp_pp)

# Load 2020 PBP Data
pbp_df <- readRDS(url(str_glue('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{SEASON_TO_ANALYZE}.rds')))


### Expected points from rushing ###

xfp_rushes <- pbp_df %>%
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
  mutate(xfp = xfp_carry[c(yardline_100), 2]) %>%
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


### Expected points from receiving ###

# -------------
# Sourced almost entirely from: https://www.opensourcefootball.com/posts/2020-08-30-calculating-expected-fantasy-points-for-receivers/
# -------------

## Loading helper functions
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

pbp_with_xyac <- pbp_df %>%
  # Get passing plays
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>%
  # Add our custom xyac columns
  add_xyac_dist

xfp_targets <- pbp_with_xyac %>%
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

concise_xfp_targets <- xfp_targets %>%
  select(
    game_id,
    player = receiver,
    gsis_id = receiver_id,
    rec_games = games,
    exp_rec_pts = exp_pts,
    actual_rec_pts = pts
  ) %>%
  summarize(
    game_id,
    player,
    gsis_id,
    rec_games,
    exp_rec_pts,
    actual_rec_pts,
  )


## Boxplots for receivers

relevant_receivers <- concise_xfp_targets %>%
  group_by(gsis_id) %>%
  summarize(total_xfp = sum(exp_rec_pts)) %>%
  filter(total_xfp > 125)

receivers_to_plot = merge(concise_xfp_targets, relevant_receivers)

ggplot(receivers_to_plot, aes(x=reorder(player, -total_xfp), y=exp_rec_pts, label=player)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Player",
       y = str_glue("Exp.{PTS_PER_RECEPTION}PPR Pts."),
       title = str_glue("{SEASON_TO_ANALYZE} Expected {PTS_PER_RECEPTION}PPR Pts. Boxplots"),
       caption = "Via nflFastR"
  )

## Boxplots for RBs

# Build an easier to navigate DF
concise_xfp_rushes <- xfp_rushes %>%
  summarize(
    game_id,
    player = rusher,
    rush_games = games,
    exp_rush_pts,
    actual_rush_pts,
    gsis_id
  ) %>%
  subset(select = -c(rusher)) # TODO - SHOULDN'T HAVE TO DO THIS

# Get the combined rush/rec xfp for players
combined_xfp_aggregate <- dplyr::bind_rows(concise_xfp_rushes, concise_xfp_targets) %>%
  group_by(gsis_id, player) %>%
  summarise(total_xfp = sum(exp_rec_pts, exp_rush_pts, na.rm=TRUE))

# Capture only players above a certain threshold for eventual graphing
players_meeting_points_threshold <- combined_xfp_aggregate %>%
  filter(total_xfp > 125) %>%
  select(player, total_xfp)

# Build a list of each player's combined rush/rec xfp on a game-by-game basis
combined_xfp_by_game <- dplyr::bind_rows(concise_xfp_rushes, concise_xfp_targets) %>%
  group_by(gsis_id, player, game_id) %>%
  summarise(
    xfp = sum(exp_rec_pts, exp_rush_pts, na.rm=TRUE)
  )

# Grab the rosters for use in filtering by position
players <- nflfastR::fast_scraper_roster(SEASON_TO_ANALYZE)
players <- subset(players, select = c(team, position, first_name, last_name, gsis_id))

# Combine a list of all running back with a list of all players meeting the graphing threshold
# to produce a list of all running backs that will be graphed
relevant_rbs <- merge(players_meeting_points_threshold, players) %>%
  filter(position == "RB") %>%
  select(gsis_id, player, total_xfp)

# Then merge the above list with the list of all games to get all games played by relevant RBs
rb_xfp_by_game <- merge(combined_xfp_by_game, relevant_rbs)

# Plot
ggplot(rb_xfp_by_game, aes(x=reorder(player, -total_xfp), y=xfp, label=player)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Player",
       y = str_glue("Exp.{PTS_PER_RECEPTION}PPR Pts."),
       title = str_glue("{SEASON_TO_ANALYZE} Expected {PTS_PER_RECEPTION}PPR Pts. Boxplots"),
       caption = "Via nflFastR"
  )
