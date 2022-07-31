library(dplyr)
library(forcats)
library(mgcv)
source('./R/load_chains.R')
source('./R/helper_functions.R')

chains <- load_chains(2021:lubridate::year(Sys.Date())) %>%
  janitor::clean_names()

# chains <- bind_rows(chains_2021,chains_2022) %>% janitor::clean_names()

shots <- chains %>%
  clean_pbp() %>%
  filter(shot_at_goal == T, goal_x < 65) %>% droplevels()

###
shots$clanger <- ifelse(shots$disposal == "clanger",1,0)
shots$effective <- ifelse(shots$disposal == "effective",1,0)
shots$player_position_fac <- forcats::fct_explicit_na(shots$player_position)
shots$player_name <- forcats::fct_lump_min(shots$player_name , 1)
shots$venue_name_fac <- forcats::fct_lump_min(shots$venue_name , 5)

##### Direction Variales
goal_width <- 6.4

shots$abs_y <- abs(shots$y)
shots$side_b <- sqrt((shots$goal_x)^2 + (shots$y + goal_width/2 )^2)
shots$side_c <- sqrt((shots$goal_x)^2 + (shots$y - goal_width/2)^2)
shots$angle <-  acos((shots$side_b^2 + shots$side_c^2 - goal_width^2)/(2*shots$side_b*shots$side_c))
shots$distance <- ifelse(shots$y >= -goal_width/2 & shots$y <= goal_width/2,
                         shots$goal_x, pmin(shots$side_b ,shots$side_c ))

# clanger
shot_clanger_mdl <- bam(clanger ~ ti(goal_x,y,by = play_type) + ti(goal_x,y)
                        + offset(log(goal_x)) + s(y, bs="ts")
                        + ti(lag_goal_x,y) + s(lag_goal_x, bs="ts") + s(lag_y, bs="ts")
                        + s(play_type, bs="re")
                        + s(player_position_fac, bs="re"),
                        data = shots, family = "binomial"  , nthreads = 4 ,select = T, discrete = T )

# goal
shot_goal_mdl <- bam(effective ~ ti(goal_x,y,by = play_type) + ti(goal_x,y)
                     + s(goal_x, bs="ts") + s(y, bs="ts")
                     + ti(lag_goal_x,y) + s(lag_goal_x, bs="ts") + s(lag_y, bs="ts")
                     + s(play_type, bs="re")
                     + s(player_position_fac, bs="re")
                     ,
                     data = shots, family = "binomial"  , nthreads = 4 ,select = T, discrete = T )


### save data
usethis::use_data(shot_clanger_mdl, overwrite = TRUE)
usethis::use_data(shot_goal_mdl, overwrite = TRUE)

