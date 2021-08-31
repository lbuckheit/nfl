library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

xfp_rb_2020 <- read.csv(file = "./2021_draft/2020_xfp_rb.csv")

adp_data <- read.csv(file = "./2021_draft/clean_adp_data.csv")

merge <- merge(xfp_rb_2020, adp_data, by="gsis_id") %>%
  filter(grepl('RB', position))

ggplot(merge, aes(x=adp, y=total_xfp_pg, label=player)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE)  
