library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

# define which seasons shall be loaded
seasons <- 2010:2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

targets <- pbp %>%
  filter(!is.na(air_yards) & !is.na(yardline_100) & play_type == "pass" & down <= 4) %>%
  summarize(loc = yardline_100, dot = air_yards, fp = ((yards_gained * 0.1) + 0.5) + (pass_touchdown * 6) )

fit <- lm(fp ~ loc + dot, data = targets)
summary(fit)
# fit2 <- lm(fp ~ dot, data = targets)

# sample <- head(targets, 500)
# 
# plot(fp ~ dot, data = sample)
# abline(fit)
