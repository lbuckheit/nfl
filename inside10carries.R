library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
data %>%
  filter(!is.na(rusher) & play_type == "run" & down <= 4 & qb_scramble == 0 & yardline_100 <= 10) %>%
  group_by(rusher) %>%
  summarize(carries = n()) %>%
  ggplot(aes(x = reorder(rusher, -carries), y = carries)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Player",
       y = "Inside 10 Carries",
       title = "High Value Carries (2020 Wk 1-3)",
       caption = "Data from nflscrapR"
  )
