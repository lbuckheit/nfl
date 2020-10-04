library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
plays <- data %>%
  filter(!is.na(receiver) & play_type == "no_play" & down <= 4)
  # group_by(receiver) %>%
  # summarize(receiver, air_yards)

View(plays)

