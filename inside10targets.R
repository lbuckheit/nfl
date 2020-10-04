library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))
data %>%
  filter(!is.na(receiver) & posteam == "DAL" & play_type == "pass" & down <= 4 & yardline_100 <= 15) %>%
  group_by(receiver) %>%
  summarize(targets = n()) %>%
  ggplot(aes(x = reorder(receiver, -targets), y = targets)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Player",
       y = "Inside 15 Targets",
       title = "High Value Targets (2020 Wk 1-3)",
       caption = "Data from nflfastR"
  )

# q <- pl %>%
#   select(desc, yardline_100)
# 
# View(q)