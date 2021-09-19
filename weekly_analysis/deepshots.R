library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

# Define variables
SEASON_TO_ANALYZE <- 2021
START_WEEK <- 1
END_WEEK <- 1
DEEP_YDS_THRESHOLD <- 20
WP_THRESHOLD <- .75

# Load annual PBP Data
pbp_df <- load_pbp(SEASON_TO_ANALYZE) %>%
  filter(season_type == "REG", week >= START_WEEK, week <= END_WEEK)

deep_balls <- pbp_df %>%
  filter(down <= 4 & air_yards >= DEEP_YDS_THRESHOLD & home_wp <= WP_THRESHOLD & away_wp <= WP_THRESHOLD) %>%
  group_by(posteam) %>%
  summarize(deep_balls = n(),
            total_yards_gained = sum(yards_gained),
            gain_pp = total_yards_gained / deep_balls,
            total_epa = sum(epa),
            epa_pp = total_epa / deep_balls)

ggplot(deep_balls, aes(x=deep_balls, y=epa_pp, label=posteam)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE)
