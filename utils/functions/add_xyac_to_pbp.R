library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
options(scipen = 9999)

# -------------
# Sourced almost entirely from: https://www.opensourcefootball.com/posts/2020-08-30-calculating-expected-fantasy-points-for-receivers/
# -------------

add_xyac_to_pbp <- function(pbp_data) {
  ## Loading helper functions
  source('https://raw.githubusercontent.com/mrcaseb/nflfastR/a26830822df59b6f8d82e65c9723a141957d1da3/R/helper_add_xyac.R')
  source('https://github.com/mrcaseb/nflfastR/raw/master/R/helper_add_nflscrapr_mutations.R')
  
  
  # duplicate the add_xyac() function that we sourced above
  add_xyac_dist <- add_xyac
  
  # separate each block of code in the add_xyac_dist() function into blocks
  add_xyac_blocks <- body(add_xyac_dist) %>% as.list
  
  # we want to remove lines 51 to 62 from the 5th item in the list
  add_xyac_blocks[[5]] <- add_xyac_blocks[[5]] %>% 
    format %>% 
    .[-(51:62)] %>% 
    paste(collapse = '\n') %>% 
    str2lang
  
  # replace the body of add_xyac_dist() with our new edited function
  body(add_xyac_dist) <- add_xyac_blocks %>% as.call
  
  
  pbp_with_xyac <- pbp_data %>%
    # Get passing plays
    filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id)) %>%
    # Add our custom xyac columns
    add_xyac_dist
  
  return(pbp_with_xyac)
}