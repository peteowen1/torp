library(tidyverse)
options(digits = 3)

## iteration sims
sims <- 500
season_val <- get_afl_season()
# sims_per_round <- max(ceiling(simulations / future::availableCores() * 2), 100)
# sim_round <- 1
# iter_sims <- sims_per_round * (sim_round - 1) + seq_len(sims_per_round)
# iter_sims <- iter_sims[iter_sims <= simulations]
# iter_sims_num <- length(iter_sims)

#
sim_games <-
  fixtures %>%
  mutate(result = home.score.totalScore - away.score.totalScore) %>%
  filter(compSeason.year == season_val) %>%
  select(providerId,
    season = compSeason.year,
    roundnum = round.roundNumber,
    home.team.providerId, home_team = home.team.club.name,
    away.team.providerId, away_team = away.team.club.name,
    utcStartTime, venue.name, result
  ) %>%
  mutate(
    torp_home_round = NA_real_,
    torp_away_round = NA_real_
  )

sim_games_pivot <-
  sim_games %>%
  dplyr::select(
    providerId, season, roundnum, home.team.providerId, away.team.providerId, utcStartTime, venue.name,
    result
  ) %>%
  tidyr::pivot_longer(
    cols = ends_with("team.providerId"),
    names_to = "team_type",
    values_to = "team.providerId"
  ) %>%
  dplyr::mutate(
    venue = replace_venues(venue.name),
    team_type = substr(team_type, 1, 4),
    result = ifelse(team_type == "away", -result, result)
  ) %>%
  dplyr::select(providerId, season, roundnum, team_type,
    teamId = team.providerId, utcStartTime, venue,
    result
  ) %>%
  dplyr::left_join(team_map) %>%
  dplyr::mutate(team_name_season = as.factor(paste(team_name, season)))

#
sim_teams <-
  tr %>%
  filter(
    torp > 0,
    is.na(injury),
  ) %>%
  group_by(team) %>%
  mutate(tm_rnk = rank(-torp)) %>%
  filter(tm_rnk <= 23) %>%
  summarise(
    torp = sum(pmax(torp, 0), na.rm = T)
  ) %>%
  mutate(
    torp = ifelse(team == 'Hawthorn',71,torp)
  )

orig_ratings <- sim_teams %>%
  arrange(-torp)

max_ratings <-
  tr %>%
  filter(
    torp > 0,
    estimated_return != "Season"
  ) %>%
  group_by(team) %>%
  mutate(tm_rnk = rank(-torp)) %>%
  filter(tm_rnk <= 23) %>%
  summarise(
    torp = sum(pmax(torp, 0), na.rm = T)
  ) %>%
  arrange(-torp)

#
library(fitzRoy)
max_ratings$team <- replace_teams(max_ratings$team)
sim_teams$team <- replace_teams(sim_teams$team)
orig_ratings$team <- replace_teams(orig_ratings$team)
sim_games$home_team <- replace_teams(sim_games$home_team)
sim_games$away_team <- replace_teams(sim_games$away_team)
sim_games_pivot$team_name <- replace_teams(sim_games_pivot$team_name)
#
library(furrr)
source("R/sim-helpers.R")

plan("multisession") #, workers = (parallelly::availableCores() - 2))

tictoc::tic('Running Sim')
tst_sims <- furrr::future_map(.x = 1:sims, .f = ~ sim_season(sim_teams, sim_games), .progress = T, .options = furrr::furrr_options(seed = TRUE))
tictoc::toc()

tst_df <- tst_sims %>% list_rbind(., names_to = "sim")

# Create separate rows for home and away teams
home_teams <- tst_df %>%
  select(sim, providerId, season, roundnum, teamId = home.team.providerId, team = home_team, utcStartTime, venue = venue.name, result, estimate, wp, outcome) %>%
  mutate(team_type = "home")

away_teams <- tst_df %>%
  select(sim, providerId, season, roundnum, teamId = away.team.providerId, team = away_team, utcStartTime, venue = venue.name, result, estimate, wp, outcome) %>%
  mutate(
    team_type = "away",
    result = -result,
    estimate = -estimate,
    wp = 1 - wp,
    outcome = 1 - outcome
  )

# Combine home and away teams
pivoted_data <- bind_rows(home_teams, away_teams) %>%
  arrange(sim, providerId, roundnum, team_type) %>%
  mutate(team_name = replace_teams(team))

pivoted_data %>%
  group_by(team, sim) %>%
  summarise(wins = sum(outcome), gms = n()) %>%
  group_by(team) %>%
  summarise(wins = mean(wins), gms = mean(gms)) %>%
  arrange(-wins) %>%
  left_join(sim_teams)


###
# Combine the original data frame with each simulation result
results_list <- furrr::future_map(1:sims, ~ bind_rows(sim_games_pivot %>%
  filter(!is.na(result)), pivoted_data %>% filter(sim == .x)) %>%
  mutate(
    outcome = case_when(
      is.na(result) ~ NA_real_,
      result > 0 ~ 1,
      result < 0 ~ 0,
      TRUE ~ 0.5
    ),
    sim = .x
  ),
.progress = T
)

combined_results_df <- results_list %>% list_rbind()

###
team_record_df <- combined_results_df %>%
  filter(team_name %in% c('Brisbane Lions','Gold Coast','Hawthorn')) %>%
  group_by(sim,team_name) %>%
  mutate(
    wins = cumsum(outcome),
    games = row_number(),
    win_diff = games - wins,
    win_prop = wins/games,
    lost_record = case_when(
      win_diff == 1 & lag(win_diff==0) ~ 1,
      win_diff == 0 & roundnum == 24 ~ 1,
      TRUE ~ 0
    )
  )

betr_df <- team_record_df %>%
  filter(lost_record == 1) %>%
  ungroup() %>%
  group_by(sim) %>%
  mutate(max_round = max(roundnum)) %>%
  filter(max_round==roundnum) %>%
  ungroup() %>%
  select(sim,team_name,roundnum) %>%
  pivot_wider(
    id_cols = sim,
    names_from = team_name,
    values_from = roundnum,
    values_fn = ~mean(.x)
  ) %>%
  mutate(
    teams_undefeated = rowSums(!is.na(across(-sim))),
    max_round = do.call(pmax, c(across(-sim), na.rm = TRUE))
  ) %>% janitor::clean_names()

nrow(betr_df)
# betr_df

betr_df %>%
  filter(
    teams_undefeated == 1
    # max_round == 5
    ) %>%
  summarise(
    avg_teams = mean(teams_undefeated),
    sims = sum(teams_undefeated)/avg_teams,
    avg_round = mean(max_round),
    gc = sum(!is.na(gold_coast), na.rm = TRUE)/sims,
    bl = sum(!is.na(brisbane_lions), na.rm = TRUE)/sims,
    hawk = sum(!is.na(hawthorn), na.rm = TRUE)/sims
  )

# betr_df %>%
#   group_by(max_round) %>%
#   summarise(count = n())

###
# team_record_df %>%
#   group_by(roundnum) %>%
#   summarise(mean(outcome), sd(estimate)) %>% print(n=25)

###
combined_results_df %>%
  filter(team_name == "Brisbane Lions") %>%
  group_by(roundnum) %>%
  summarise(
    mest = mean(estimate),
    mwp = mean(wp),
    mres = mean(result),
    mout = mean(outcome)
  ) %>%
  View()

###
create_ladder <- function(df) {
  df %>%
    group_by(team_name, sim) %>%
    summarise(
      gd = sum(result),
      winz = sum(outcome),
      .groups = "drop"
    ) %>%
    arrange(-winz, -gd) %>%
    mutate(rank = row_number())
}

####
# Define a function that checks if the data frame meets the condition
check_condition <- function(df) {
  # any(df$providerId == "CD_M20240141901" & df$team_name == "Brisbane Lions" & df$outcome == 1)
  TRUE
}

###
# Filter the list of data frames to include only those that meet the condition
filtered_dfs <- results_list %>%
  keep(~ check_condition(.x))

ladders <- furrr::future_map(filtered_dfs,
  ~ create_ladder(.x),
  .progress = T
)

ladders_df <- ladders %>% list_rbind()

# sms <- ladders_df %>%
#   group_by(sim) %>%
#   filter(team_name=="Footscray", winz==14) %>%
#   pull(sim)

ladders_df %>%
  # filter(sim %in% sms) %>% View()
  group_by(team_name) %>%
  summarise(
    wins = mean(winz),
    top_8 = mean(rank <= 8),
    top_4 = mean(rank <= 4),
    rows = n()
  ) %>%
  arrange(-top_8, -top_4) %>%
  # arrange(-winz) %>%
  arrange()

tst_lad <-
  ladders_df %>%
  filter(sim ==1)

# ### Finals
# quali_finals <-
#   tst_lad %>%
#   mutate(
#     opponent_rank = case_when(
#       rank == 1 ~ 4,
#       rank == 2 ~ 3,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   inner_join(tst_lad, by = c("opponent_rank" = "rank", "sim"), suffix = c(".home", ".away")) %>%
#   select(
#     home_team = team_name.home,
#     away_team = team_name.away
#   )
#
# quali_finals
#
# # Display the matches
# elim_finals <-
#   tst_lad %>%
#   mutate(
#     opponent_rank = case_when(
#       rank == 5 ~ 8,
#       rank == 6 ~ 7,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   inner_join(tst_lad, by = c("opponent_rank" = "rank", "sim"), suffix = c(".home", ".away")) %>%
#   select(
#     home_team = team_name.home,
#     away_team = team_name.away
#   )
#
# elim_finals


