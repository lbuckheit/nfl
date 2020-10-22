library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)
# TODO - Be able to break down by week chunks (this probably involves just downloading a specific csv, since season-long anya and dbpg is likely better to use for the scaling)
# TODO - Is this scaling against average the best way to do it?

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

anya <- data %>%
  filter(down <= 4 & pass_attempt == 1 & qb_spike == 0) %>%
  group_by(posteam) %>%
  summarize(yards = sum(yards_gained),
            touchdowns = sum(pass_touchdown),
            interceptions = sum(interception),
            sacks = sum(sack),
            attempts = n())

sack_yards <- data %>%
  filter(down <= 4 & pass_attempt == 1 & sack) %>%
  group_by(posteam) %>%
  summarize(sack_yards = sum(yards_gained))

# PFR ANY/A = (pass yards + 20*(pass TD) - 45*(interceptions thrown) - sack yards)/(passing attempts + sacks)
# NOTE: right now pass_attempt == 1 covers sacks, so no need to add in sacks in the final term
full_anya <- merge(anya, sack_yards, by="posteam") %>%
  mutate(anya = (yards + (20 * touchdowns) - (45 * interceptions) - sack_yards) / (attempts)) %>%
  mutate(nfl_avg_anya = (sum(yards) + (20 * sum(touchdowns)) - (45 * sum(interceptions)) - sum(sack_yards)) / (sum(attempts)))
names(full_anya)[names(full_anya) == "posteam"] <- "team"

passes <- data %>%
  # TODO - pass_attempt == 1 isn't perfect but will do for now
  filter(down <= 4 & pass_attempt == 1 & qb_spike == 0) %>%
  group_by(game_id, posteam) %>%
  mutate(game_played = ifelse(row_number()==1,1,0)) %>%
  ungroup() %>%
  group_by(posteam) %>%
  summarize(games_played = sum(game_played),
            dropbacks = n(),
            dropbacks_pg = dropbacks / games_played) %>%
  mutate(nfl_avg_dbpg = sum(dropbacks) / sum(games_played))
names(passes)[names(passes) == "posteam"] <- "team"

team_data <- merge(passes, full_anya, by="team")

airyards_data <- read.csv("./airyards_thru_6.csv")

# Since the airyards API is dead just gotta grab the CSV until I get my own WOPR generator up
merge <- merge(team_data, airyards_data, by="team")

awopr <- merge %>%
  mutate(dbpg_scale = dropbacks_pg / nfl_avg_dbpg,
         anya_scale = anya / nfl_avg_anya,
         db_adj_wopr = wopr * dbpg_scale,
         anya_adj_wopr = wopr * anya_scale,
         double_adj_wopr = wopr * dbpg_scale * anya_scale) %>%
  summarize(full_name,
            team,
            team_dropbacks_pg = round(dropbacks_pg, digits = 2),
            team_anya = round(anya, digits = 2),
            dbpg_against_nfl_avg = round(dbpg_scale, digits = 2),
            anya_against_nfl_avg = round(anya_scale, digits = 2),
            wopr,
            dropback_adj_wopr = round(db_adj_wopr, digits = 2),
            anya_adj_wopr = round(anya_adj_wopr, digits = 2),
            double_adj_wopr = round(double_adj_wopr, digits = 2))

write.csv(awopr, "./awopr.csv")
