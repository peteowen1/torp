library(tidyverse)
library(devtools)
devtools::load_all()
library(rvest)

url <- 'https://www.afl.com.au/matches/injury-list'

inj_df <- read_html(url) %>% html_table() %>% list_rbind() %>% janitor::clean_names()

#
tr <- torp_ratings(2024, get_afl_week("next")) %>%
  left_join(inj_df, by = c('player_name'='player')) %>%
  mutate(estimated_return = replace_na(estimated_return, 'None'))

view(tr)

### coming week
missing<- tr %>% anti_join(teams %>% filter( season == get_afl_season() , round.roundNumber == get_afl_week("next")), by = c('player_id'='player.playerId'))

View(missing)

starting <- tr %>% inner_join(teams %>% filter( season == get_afl_season() , round.roundNumber == get_afl_week("next")), by = c('player_id'='player.playerId'))

View(starting)

###
this_week <- player_game_ratings()
view(this_week)

this_season <- player_game_ratings(round_num = 0:get_afl_week())
view(this_season)

tot_season <- player_game_ratings(season = 2021:2024, round_num = 0:get_afl_week())
view(tot_season)

ssn_24 <- player_season_ratings(2024)
view(ssn_24)
ssn_23 <- player_season_ratings(2023)
# view(ssn_23)
ssn_22 <- player_season_ratings(2022)
# view(ssn_22)
ssn_21 <- player_season_ratings(2021)
# view(ssn_21)

sd(tr$torp_hitout, na.rm = T)

tr %>%
  group_by(position) %>%
  summarise(
    sd = sd(torp, na.rm = T),
    avg = mean(torp, na.rm = T),
    repl = quantile(torp, 0.2, na.rm = T),
    n = n()
  )

tr %>%
  filter(
    # !is.na(torp),
    torp > 0,
    # is.na(injury),
    estimated_return != "Season"
  ) %>%
  group_by(team) %>%
  mutate(tm_rnk = rank(-torp)) %>%
  filter(tm_rnk <= 23) %>%
  summarise(
    val = sum(pmax(torp, 0), na.rm = T),
    ply = n(),
    age = sum(age * torp) / sum(torp)
  ) %>%
  arrange(-val) #%>% summarise(sum(val)) #1361


tr %>%
  filter(
    # !is.na(torp),
    torp > 0,
    is.na(injury),
    #estimated_return != "Season"
         ) %>%
  group_by(team) %>%
  mutate(tm_rnk = rank(-torp)) %>%
  filter(tm_rnk <= 23) %>%
  summarise(
    val = sum(pmax(torp, 0), na.rm = T),
    ply = n(),
    age = sum(age * torp) / sum(torp)
  ) %>%
  arrange(-val) #%>% summarise(sum(val)) #1234


tr %>%
  left_join(teams %>%
              filter(season == get_afl_season(),round.roundNumber == get_afl_week('next')),
            by = c('player_id'='player.playerId')
            ) %>% View()
