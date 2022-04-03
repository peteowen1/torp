## code to prepare `DATASET` dataset goes here

# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(tidyverse)
library(zoo)
library(tidymodels)
library(mgcv)

chains <- readRDS("./data-raw/chains_2021.rds") %>%
  bind_rows(readRDS("./data-raw/chains_2021.rds")) %>%
  janitor::clean_names()%>%
  mutate(goal_x = venue_length / 2 - x)

###
shot_df <- chains %>% filter(shot_at_goal == T, goal_x < 65)
shot_df$clanger <- ifelse(shot_df$disposal == "clanger",1,0)
shot_df$effective <- ifelse(shot_df$disposal == "effective",1,0)
shot_df$player_position_fac <- forcats::fct_explicit_na(shot_df$player_position)

# goal
shot_goal_mdl <- bam(effective ~ ti(goal_x,y) + s(goal_x, bs="ts") + s(y, bs="ts")
                     #+ s(play_type, bs="re")
                     + s(player_position_fac, bs="re")
                     #+ s(player_full, bs="re")
                     ,
                     data = shot_df %>% filter(clanger == 0), family = "binomial"  , nthreads = 4 ,select = T, discrete = T )


usethis::use_data(shot_goal_mdl, overwrite = TRUE)
