library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

### Analyze run/pass tendencies in the red zone ###

SEASON_TO_ANALYZE <- 2020
START_WEEK <- 1
END_WEEK <- 17

pbp_df <- load_pbp(SEASON_TO_ANALYZE)

# FUTURE TODO - Maybe an optional win prob filter to get neutral script data?
rz_plays <- pbp_df %>%
  filter(week >= START_WEEK, week <= END_WEEK, down <= 4, yardline_100 <= 20, play_type == "run" | play_type == "pass") %>%
  summarize(
    posteam,
    down,
    ydstogo,
    yardline_100,
    goal_to_go,
    play_type,
    is_rush = play_type == "run",
    is_pass = play_type == "pass",
    desc
  )

tendencies <- rz_plays %>%
  group_by(posteam) %>%
  summarize(
    plays = n(),
    rushes = sum(is_rush),
    passes = sum(is_pass),
    pass_ratio = passes/rushes
  )

ggplot(tendencies, aes(x = reorder(posteam, -pass_ratio), y = pass_ratio)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = -90)) +
  labs(x = "Team",
       y = "RZ Pass/Run Ratio",
       title = str_glue("{SEASON_TO_ANALYZE} RZ Pass/Run Ratio (Wk. {START_WEEK}-{END_WEEK})"),
       caption = "Via nflFastR"
  )
