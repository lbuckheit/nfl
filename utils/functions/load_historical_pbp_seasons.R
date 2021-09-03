library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)

load_historical_pbp_seasons <- function(start_season, end_season) {
  seasons <- start_season:end_season
  historical_pbp <- purrr::map_df(seasons, function(x) {
    readRDS(
      url(
        glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
      )
    )
  })
  
  return(historical_pbp)
}