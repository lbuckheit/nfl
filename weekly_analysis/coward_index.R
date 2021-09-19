library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

### Analyze which QBs leave it short of the sticks on money downs

SEASON_TO_ANALYZE <- 2020
START_WEEK <- 1
# TODO - Remember that now seasons have 18 weeks
END_WEEK <- 17
MINIMUM_PLAYS <- 64

pbp_df <- load_pbp(SEASON_TO_ANALYZE)

critical_downs <- pbp_df %>%
  filter(down >= 3 & play_type == "pass" & !is.na(air_yards)) %>%
  mutate(short_of_sticks = as.integer(air_yards < ydstogo)) %>%
  group_by(passer) %>%
  summarize(
    plays = n(),
    plays_short = sum(short_of_sticks),
    pass_conversions = sum(first_down_pass),
    pct_converted = pass_conversions / plays,
    pct_short = plays_short / plays
  ) %>%
  filter(plays >= MINIMUM_PLAYS)

ggplot(critical_downs, aes(x=pct_short, y=pct_converted, label=passer)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "% of Critical-down Throws Short of Line to Gain",
       y = "% of Critical Downs Converted",
       title = str_glue("{SEASON_TO_ANALYZE} Coward Index (Wk. {START_WEEK}-{END_WEEK})"),
       caption = "Via nflFastR"
  )
