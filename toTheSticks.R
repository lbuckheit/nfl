library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
third_down_passes <- data %>%
  filter(down == 3 & play_type == "pass" & !is.na(air_yards)) %>%
  mutate(short_of_sticks = as.integer(air_yards < ydstogo)) %>%
  group_by(passer) %>%
  summarize(plays = n(),
            plays_short = sum(short_of_sticks),
            pass_conversions = sum(first_down_pass),
            pct_converted = pass_conversions / plays,
            pct_short = plays_short / plays) %>%
  filter(plays >= 20)

ggplot(third_down_passes, aes(x=pct_short, y=pct_converted, label=passer)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE)
