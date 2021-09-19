library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
options(scipen = 9999)

# 2020 pbp data
pbp_df_2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

success_rate <- pbp_df_2020 %>%
  filter(play_type == "run" & qb_scramble == 0 & season_type == "REG") %>%
  select(posteam, ydstogo, down, yards_gained) %>%
  mutate(success = case_when(down == 1 & yards_gained / ydstogo >= .6 ~ 1,
                             down == 2 & yards_gained / ydstogo >= .4 ~ 1,
                             down == 3 & yards_gained / ydstogo >= 1 ~ 1,
                             down == 4 & yards_gained / ydstogo >= 1 ~ 1,
                             TRUE ~ 0)) %>%
  group_by(posteam) %>%
  summarize(total_success_rate = sum(success) / n())

# Pulling from my expected rushing TDs script here
merge = merge(xtd_2020_teams, success_rate)

## Graphing

# Success rate by team
ggplot(success_rate, aes(x = reorder(posteam, -total_success_rate), y = total_success_rate)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Team",
       y = "Success Rate",
       title = "2020 Success Rate by Team",
       caption = "Via nflFastR"
  )  

# This requires having the xtd_2020_teams dataframe loaded from the other script
# Expected rush TDs vs. success rate
ggplot(merge, aes(x=xtd, y=total_success_rate, label=posteam)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE)  


