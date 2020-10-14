library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
targs <- data %>%
  filter(!is.na(receiver) & week == 5 & play_type == "pass" & down <= 4) %>%
  group_by(receiver) %>%
  summarize(targets = n(), 
            adot = sum(air_yards) / n(), 
            air_yards = sum(air_yards),
            yards = sum(yards_gained))
  # ggplot(aes(x = reorder(receiver, -adot), y = adot)) +
  # geom_bar(stat="identity") +
  # theme(axis.text.x = element_text(angle = -90)) +
  # labs(x = "Player",
  #      y = "Total Air Yards",
  #      title = "Air Yards (Wk 1-2)",
  #      subtitle = "Minimum 200 Air Yards",
  #      caption = "Data from nflscrapR"
  # )

View(targs)


