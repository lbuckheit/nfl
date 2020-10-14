library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
data %>%
  filter(!is.na(receiver) & week == "4" & posteam == "DET" & play_type == "pass" & down <= 4) %>%
  group_by(receiver) %>%
  summarize(total_air_yards = sum(air_yards)) %>%
  ggplot(aes(x = reorder(receiver, -total_air_yards), y = total_air_yards)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Player",
       y = "Total Air Yards",
       title = "Air Yards (Wk 1-2)",
       subtitle = "Minimum 200 Air Yards",
       caption = "Data from nflscrapR"
  )


