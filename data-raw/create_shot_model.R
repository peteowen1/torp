library(dplyr)
library(forcats)
library(mgcv)
source('./R/load_chains.R')

chains <- load_chains(2021:lubridate::year(Sys.Date())) %>%
  janitor::clean_names()

# chains <- bind_rows(chains_2021,chains_2022) %>% janitor::clean_names()

shots <- chains %>%
  dplyr::mutate(goal_x = venue_length / 2 - x,
         y = -y,
         lag_desc = as.factor(dplyr::lag(description)),
         lag_y = dplyr::lag(y),
         lag_goal_x = dplyr::lag(goal_x),
         lag_time5 = dplyr::lag(period_seconds,5),
         lag_goal_x5 = dplyr::lag(goal_x,5),
         speed5 = (lag_goal_x5 - goal_x)/pmax((period_seconds - lag_time5),1),
         lag_time15 = dplyr::lag(period_seconds,15),
         lag_goal_x15 = dplyr::lag(goal_x,15),
         speed15 = (lag_goal_x15 - goal_x)/pmax((period_seconds - lag_time15),1),
         player_name = paste(player_name_given_name,player_name_surname)) %>%
  filter(shot_at_goal == T, goal_x < 65) %>% droplevels()

###
shots$clanger <- ifelse(shots$disposal == "clanger",1,0)
shots$effective <- ifelse(shots$disposal == "effective",1,0)
shots$player_position_fac <- forcats::fct_explicit_na(shots$player_position)
shots$player_name_fac <- forcats::fct_lump_min(shots$player_name , 5)
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
shot_clanger_mdl <- mgcv::bam(clanger ~ s(abs_y, bs="ts")
                        + ti(angle,distance, bs="ts") + s(angle, bs="ts") + s(distance, bs="ts")
                     + s(lag_desc, bs="re")
                     + ti(lag_goal_x,lag_y) + s(lag_goal_x, bs="ts") + s(lag_y, bs="ts")
                     + s(speed5) + s(speed15)
                     + s(player_name_fac, bs="re")
                     + s(venue_name_fac, bs="re")
                     + s(player_position_fac, bs="re")
                     ,
                     data = shots , family = "binomial"  , nthreads = 4 ,select = T, discrete = T )
# goal
shot_goal_mdl <- mgcv::bam(effective ~ s(abs_y, bs="ts")
                     + te(angle,distance, bs="ts") #+ ti(angle, bs="ts") + ti(distance, bs="ts")
                     + s(period_seconds, bs="ts")
                     + s(chain_number, bs="ts")
                     + s(as.numeric(date) , bs="ts")
                     + s(lag_desc, bs="re")
                     + ti(lag_goal_x,lag_y, bs="ts") + s(lag_goal_x, bs="ts") + s(lag_y, bs="ts")
                     + s(speed5, bs="ts") + s(speed15, bs="ts")
                     + s(player_name_fac, bs="re")
                     + s(venue_name_fac, bs="re")
                     #+ s(player_position_fac, bs="re")
                     ,
                     data = shots , family = "binomial"  , nthreads = 4 ,select = T, discrete = T )


usethis::use_data(shot_clanger_mdl, overwrite = TRUE)
usethis::use_data(shot_goal_mdl, overwrite = TRUE)

