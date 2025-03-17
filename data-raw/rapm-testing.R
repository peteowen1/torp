library(tidyverse)
library(glmnet)

pstot <- load_ps()
results <- torp::results

rapm_df <-
  pstot %>%
  left_join(
    results %>%
      dplyr::select(
        match.matchId,
        homeTeamScore.matchScore.totalScore, homeTeamScore.matchScore.goals, homeTeamScore.matchScore.behinds,
        awayTeamScore.matchScore.totalScore, awayTeamScore.matchScore.goals, awayTeamScore.matchScore.behinds,
        match.utcStartTime
      ),
    by = c("provider_id" = "match.matchId")
  ) %>%
  dplyr::mutate(
    home_shots = homeTeamScore.matchScore.goals + homeTeamScore.matchScore.behinds,
    away_shots = awayTeamScore.matchScore.goals + awayTeamScore.matchScore.behinds,
    score_diff = ifelse(team_status == "home",
                        homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore,
                        awayTeamScore.matchScore.totalScore - homeTeamScore.matchScore.totalScore
    ),
    shot_diff = ifelse(team_status == "home",
                       home_shots - away_shots,
                       away_shots - home_shots
    ),
    team_shots = ifelse(team_status == "home",
                        home_shots,
                        away_shots
    ),
    shot_conv = ifelse(team_status == "home",
                       homeTeamScore.matchScore.goals / home_shots,
                       awayTeamScore.matchScore.goals / away_shots
    ),
    win = ifelse(score_diff > 0, 1, ifelse(score_diff == 0, 0.5, 0)),
    ####
    team_type_fac = as.factor(team_status),
    total_score = homeTeamScore.matchScore.totalScore + awayTeamScore.matchScore.totalScore,
    total_shots = home_shots + away_shots,
    opp_name = ifelse(team_status == "home",
                       home_team_name,
                       away_team_name
    ),
    weightz = exp(as.numeric(-(Sys.Date() - as.Date(match.utcStartTime))) / decay),
    weightz = weightz / mean(weightz, na.rm = T),
    row_id = paste0(player_player_id,substr(provider_id,5,8),round_round_number),
    adj_score_diff = score_diff / 18
  ) %>%
  select(player_id = player_player_id, team_id, provider_id , time_on_ground_percentage , score_diff) %>%
  filter(!if_any(everything(), is.na))

rapm_wide <-
  rapm_df %>%
  pivot_wider()

