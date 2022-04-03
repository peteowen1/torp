# devtools::install_github("jimmyday12/fitzRoy")
library(dplyr)
library(mgcv)
source('R/scraper_functions.R')

chains_2021 <- get_match_chains(2021)
chains_2022 <- get_match_chains(2022)

usethis::use_data(chains_2021, overwrite = TRUE)
usethis::use_data(chains_2022, overwrite = TRUE)

chains_total <- chains_2021 %>%
  bind_rows(chains_2022)%>%
  mutate(goal_x = venue_length / 2 - x)

###
shot_df <- chains %>% filter(shot_at_goal == T, goal_x < 65)
shot_df$clanger <- ifelse(shot_df$disposal == "clanger",1,0)
shot_df$effective <- ifelse(shot_df$disposal == "effective",1,0)
shot_df$player_position_fac <- forcats::fct_explicit_na(shot_df$player_position)
# clanger
shot_clanger_mdl <- bam(clanger ~ ti(goal_x,y) + offset(log(goal_x)) + s(y, bs="ts")
                        #+ s(play_type, bs="re")
                        + s(player_position_fac, bs="re"),
                        data = shot_df, family = "binomial"  , nthreads = 4 ,select = T, discrete = T )

summary(shot_clanger_mdl)
#plot(mgcViz::getViz(shot_clanger_mdl))

# goal
shot_goal_mdl <- bam(effective ~ ti(goal_x,y) + s(goal_x, bs="ts") + s(y, bs="ts")
                     #+ s(play_type, bs="re")
                     + s(player_position_fac, bs="re")
                     #+ s(player_full, bs="re")
                     ,
                     data = shot_df %>% filter(clanger == 0), family = "binomial"  , nthreads = 4 ,select = T, discrete = T )

summary(shot_goal_mdl)
# mixedup::extract_random_effects(shot_goal_mdl) %>% view()
#plot(mgcViz::getViz(shot_goal_mdl))

chains$player_position_fac <- as.factor(chains$player_position)

chains$shot_clanger_pred <- predict.bam(shot_clanger_mdl,chains,type="response")
chains$shot_goal_pred <- predict.bam(shot_goal_mdl,chains,type="response")
chains$shot_exp_pred <- (1-chains$shot_clanger_pred)*
  (chains$shot_goal_pred*6+(1-chains$shot_goal_pred))
chains$clanger <- ifelse(chains$disposal == "clanger" & chains$shot_at_goal == T,1,0)
chains$effective <- ifelse(chains$disposal == "effective" & chains$shot_at_goal == T,1,0)

###
chains %>% filter(goal_x < 80) %>%
  group_by(cut_number(goal_x,15)) %>%
  summarise(goals = sum(effective,na.rm=T),
            clang = sum(clanger , na.rm=T),
            shots = sum(ifelse(shot_at_goal==TRUE,1,0),na.rm=TRUE),
            g_pct = goals/shots,
            cl_pct = clang / shots,
            xpoints = mean(shot_exp_pred,na.rm=TRUE),
            xclang = mean(shot_clanger_pred,na.rm=TRUE),
            xgoal = mean(shot_goal_pred,na.rm=TRUE),
            rows = n())




usethis::use_data(DATASET, overwrite = TRUE)
