library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)
# TODO - Be able to break down by week chunks
# TODO - Maybe add another adjustment to scale it by epa/pass play to try and capture volume and efficiency?

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

passes <- data %>%
  # TODO - pass_attempt == 1 isn't perfect but will do for now
  filter(down <=4 & pass_attempt == 1 & qb_spike == 0) %>%
  group_by(game_id, posteam) %>%
  mutate(game_played = ifelse(row_number()==1,1,0)) %>%
  ungroup() %>%
  group_by(posteam) %>%
  summarize(games_played = sum(game_played),
            dropbacks = n(),
            dropbacks_pg = dropbacks / games_played) %>%
  mutate(nfl_avg_dbpg = sum(dropbacks) / sum(games_played))
names(passes)[names(passes) == "posteam"] <- "team"

airyards <- read.csv("./airyards_thru_6.csv")

# Since the airyards API is dead just gotta grab the CSV until I get my own WOPR generator up
merge <- merge(passes, airyards, by="team")

awopr <- merge %>%
  mutate(dbpg_scale = dropbacks_pg / nfl_avg_dbpg,
         awopr = wopr * dbpg_scale) %>%
  summarize(full_name,
            team,
            dropbacks_pg,
            dbpg_scale,
            wopr,
            awopr)

write.csv(awopr, "./awopr.csv")
