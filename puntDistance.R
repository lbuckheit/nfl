library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

data20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
punts20 <- data20 %>%
  filter(play_type == "punt" & !is.na(kick_distance) & yardline_100 > 50) %>%
  summarize(punts = n(), 
            avg_distance = sum(kick_distance) / punts)

data05 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2005.rds'))
punts05 <- data05 %>%
  filter(play_type == "punt" & !is.na(kick_distance) & yardline_100 > 50) %>%
  summarize(punts = n(), 
            avg_distance = sum(kick_distance) / punts)

View(punts20)
View(punts05)


