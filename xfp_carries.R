library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

# define which seasons shall be loaded
seasons <- 2010:2019
historical_pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

xfp_carry <- historical_pbp %>%
  filter(play_type == "run" & down <= 4 & qb_scramble == 0) %>%
  group_by(yardline_100) %>%
  summarize(n(), rush_fp_pp = sum(yards_gained) * 0.1 / n(), td_fp_pp = sum(rush_touchdown) * 6 / n(), xfp_pp = rush_fp_pp + td_fp_pp) %>%
  summarize(yardline_100, xfp_pp)
  # ggplot(aes(x = yardline_100, y = xfp_pp)) +
  # geom_bar(stat="identity") +
  # theme(axis.text.x = element_text(angle = -90)) +
  # labs(x = "Distance From Goal Line",
  #      y = "Expected FP",
  #      title = "Carry XFP 2010-19",
  #      caption = "Data from nflscrapR"
  # )

# 2020 pbp data
pbp_df_2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

xfp_rushes_2020 <- pbp_df_2020 %>%
  filter(play_type == "run" & !is.na(rusher) & qb_scramble == 0) %>%
  select(rusher, desc, yardline_100) %>%
  # Assign an xfp to each carry based on where the carry took place (using the earlier calculated data)
  mutate(xfp = xfp_carry[c(yardline_100), 2]) %>%
  group_by(rusher) %>%
  summarize(carries = n(), total_xfp = sum(xfp), xfp_pp = total_xfp / carries) %>%
  filter(carries >= 10)

view(xfp_rushes_2020)
