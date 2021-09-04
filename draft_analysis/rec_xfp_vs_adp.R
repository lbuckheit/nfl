library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)

xfp_rec_2020 <- read.csv(file = "./draft_analysis/2020_xfp_rec.csv")

adp_data <- read.csv(file = "./draft_analysis/clean_adp_data.csv")

merge <- merge(xfp_rec_2020, adp_data, by="gsis_id") %>%
  # Filter in/out particular positions here
  filter(grepl('WR', position))

ggplot(merge, aes(x=adp, y=exp_hPPR_pts_pg, label=receiver)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE)  
