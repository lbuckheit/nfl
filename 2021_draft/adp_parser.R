library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

# Read in the csv
espn_adp <- read.csv(file = "./2021_draft/2021_espn_adp.csv") %>% 
  # Get rid of kickers and defenses
  filter(!grepl('K', position)) %>%
  filter(!grepl('D/ST', last_name))

# Capitalize all the team abbreviations and rename the mismatches
espn_adp$team <- toupper(espn_adp$team)
espn_adp$team <- recode(espn_adp$team, LAR = "LA")
espn_adp$team <- recode(espn_adp$team, WSH = "WAS")

# Note: Players who changed teams:
# Sony Michel
# Kenyan Drake
# Corey Davis
# Marvin Jones
# Jonnu Smith
# Hunter Henry
# James Conner
# Curtis Samuel
# Jamaal Williams
# Phillip Lindsay
# Tyrell Williams
# Randall Cobb
# AJ Green
# Nelson Agholor
# Malcolm Brown
# Jared Cook

players <- nflfastR::fast_scraper_roster(2020)
players <- subset( players, select = c(team, position, first_name, last_name, gsis_id) )

merged <- merge(espn_adp, players)

View(merged)
