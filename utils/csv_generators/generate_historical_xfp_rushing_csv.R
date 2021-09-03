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

# Load Historical Data
historical_pbp <- load_historical_pbp_seasons(2000, 2020)

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

write.csv(xfp_carry, "./helpful_csvs/2000_2020_xfp_by_rush_location.csv", row.names=FALSE)
