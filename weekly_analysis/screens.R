library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

### Analyze which teams utilize the most screens ###

SEASON_TO_ANALYZE <- 2020
START_WEEK <- 1
END_WEEK <- 17
MINIMUM_PLAYS <- 64
WP_THRESHOLD <- .75

pbp_df <- load_pbp(SEASON_TO_ANALYZE)

# FUTURE TODO - Separate out WR/TE vs RB screens?
screens <- pbp_df %>%
  filter(down <= 4 & air_yards <= 1 & home_wp <= WP_THRESHOLD & away_wp <= WP_THRESHOLD) %>%
  group_by(posteam) %>%
  summarize(
    screens = n(),
    total_yards_gained = sum(yards_gained),
    gain_pp = total_yards_gained / screens,
    total_epa = sum(epa),
    epa_pp = total_epa / screens
  )

ggplot(screens, aes(x=screens, y=epa_pp, label=posteam)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Number of Screens Run",
       y = "EPA/play on Screens",
       title = str_glue("{SEASON_TO_ANALYZE} Screen Pass Analysis (Wk. {START_WEEK}-{END_WEEK})"),
       caption = "Via nflFastR"
  )
