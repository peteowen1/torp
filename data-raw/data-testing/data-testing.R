library(tidyverse)
devtools::load_all()

xg_df <- load_xg(T)

xg_df %>%
  group_by(substr(match_id,1,8)) %>%
  summarise(
    home_score = sum(home_shots_score),
    home_xscore = sum(home_xscore),
    away_score = sum(away_shots_score),
    away_xscore = sum(away_xscore),
    )

##### HMMMMMM
tot_res <- xg_df %>%
  left_join(results %>%
              select(matchId,res_hs = homeTeamScore.matchScore.totalScore,res_as = awayTeamScore.matchScore.totalScore),
            by = c('match_id'='matchId')) %>%
  mutate(hdiff = home_shots_score - res_hs,
         adiff = away_shots_score - res_as)

View(tot_res)


#########
devtools::load_all()

df <- load_chains(2025,0)

tst_df <- df %>% filter(matchId == 'CD_M20240142306')

View(tst_df)

# df %>%
#   filter(match_id == 'CD_M20240142306') %>%
#   select(home_points,away_points) %>% unique()
#
# chain_end <-
#   chains %>%
#   group_by(matchId,chain_number) %>%
#   mutate(mdo = max(displayOrder)) %>%
#   filter(mdo == displayOrder)
#
# # View(chain_end)
#
# chain_end %>%
#   group_by(finalState,description) %>%
#   summarise(
#     n = n()
#   ) %>%
#   View()


df  %>%
  # mutate()
  group_by(match_id,chain_number) %>%
  mutate(mdo = max(display_order)) %>%
  filter(mdo == display_order) %>%
  group_by(final_state,description, is.na(team_id),lead_desc_tot) %>%
  summarise(
    n = n()
  ) %>%
  View()

####
# CD_M20240142306 - 2nd half chains missing
# CD_M20240142308 - only quarter 1 data in chains
#

#
devtools::load_all()
library(tidyverse)
library(fitzRoy)

match <- 'CD_M20220141404'
season <- substr(match,5,8) %>% as.integer()
round <- substr(match,12,13) %>% as.integer()

# tst_pbp <- load_pbp()
df <- load_chains(season, round) %>% filter(matchId == match)
swdf <- fetch_score_worm_data(match)

### clean chains (1.5 secs per round)
df <- clean_pbp(df)

df <- clean_model_data_epv(df)

df <- df %>%
  clean_shots_data() %>%
  add_shot_vars() %>%
  add_epv_vars() %>%
  clean_model_data_wp() %>%
  add_wp_vars()


df %>%
  select(c(display_order:match_id,
           scoring_team_id,points_team_id,pos_points_team_id,team_id_mdl,
           home_points, away_points,
           points_row,pos_points,model_points,next_score,
           points_diff,xpoints_diff,team_change,delta_epv)) %>%
  View()

# mdl_tst <- tst_pbp %>%
#   select_epv_model_vars(label=TRUE)
#
# View(mdl_tst)
