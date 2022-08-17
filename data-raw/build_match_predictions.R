###
teams <-
  dplyr::bind_rows(fitzRoy::fetch_lineup(season = 2021, comp = "AFLM")) %>%
  dplyr::bind_rows(fitzRoy::fetch_lineup(season = 2022, comp = "AFLM")) %>%
  dplyr::mutate(season = as.numeric(substr(providerId, 5, 8)))

results <-
  dplyr::bind_rows(fitzRoy::fetch_results(season = 2021, comp = "AFLM")) %>%
  dplyr::bind_rows(fitzRoy::fetch_results(season = 2022, comp = "AFLM")) %>%
  dplyr::mutate(season = as.numeric(substr(match.matchId, 5, 8)))

# ############ 2021
##############################
final_21 <- furrr::future_map_dfr(2:28, ~ plyr_ratings(plyr_gm_df, teams, 2021, .))
saveRDS(final_21, "./data/plyr_df_2021.rds")
final_21 <- readRDS("./data/plyr_df_2021.rds")

### 2022
n <- max(teams %>% dplyr::filter(season == lubridate::year(Sys.Date()), utcStartTime < Sys.time() + 350000) %>%
  dplyr::select(round.roundNumber)) + 1

### 2022
final_22 <- furrr::future_map_dfr(1:n, ~ plyr_ratings(plyr_gm_df, teams, 2022, .))
saveRDS(final_22, "./data/plyr_df_2022.rds")
final_22 <- readRDS("./data/plyr_df_2022.rds")

### final
final <- dplyr::bind_rows(final_21, final_22)

final$weight <- exp(as.numeric(-(Sys.Date() - as.Date(final$utcStartTime))) / 250)
final$weight <- final$weight / mean(final$weight, na.rm = T)
#############

team_rt_df <-
  teams %>%
  dplyr::left_join(final %>% dplyr::filter(player_name != "NA NA"), by = c("providerId" = "providerId", "player.playerId" = "player.playerId")) %>%
  dplyr::filter((!is.na(bayes_g) | utcStartTime.x > Sys.time())) %>%
  dplyr::filter((position.x != "EMERG" & position.x != "SUB") | is.na(position.x)) %>%
  dplyr::mutate(
    phase = dplyr::case_when(
      position.x %in% c("BPL", "BPR", "FB", "CHB", "HBFL", "HBFR") ~ "def",
      position.x %in% c("C", "WL", "WR", "R", "RR", "RK") ~ "mid",
      position.x %in% c("FPL", "FPR", "FF", "CHF", "HFFL", "HFFR") ~ "fwd",
      position.x %in% c("INT", "SUB") ~ "int",
      TRUE ~ "other",
    ),
    def = ifelse(phase == "def", bayes_g, NA),
    mid = ifelse(phase == "mid", bayes_g, NA),
    fwd = ifelse(phase == "fwd", bayes_g, NA),
    int = ifelse(phase == "int", bayes_g, NA),
    key_def = ifelse(posn == "KEY_DEFENDER", bayes_g, NA),
    med_def = ifelse(posn == "MEDIUM_DEFENDER", bayes_g, NA),
    midfield = ifelse(posn == "MIDFIELDER", bayes_g, NA),
    mid_fwd = ifelse(posn == "MIDFIELDER_FORWARD", bayes_g, NA),
    med_fwd = ifelse(posn == "MEDIUM_FORWARD", bayes_g, NA),
    key_fwd = ifelse(posn == "KEY_FORWARD", bayes_g, NA),
    rucks = ifelse(posn == "RUCK", bayes_g, NA),
    other_pos = ifelse(is.na(posn), bayes_g, NA)
  ) %>%
  dplyr::group_by(providerId, teamName.x, season, round.roundNumber.x, teamType.x) %>%
  dplyr::summarise(
    bayes_g = sum(bayes_g), # , na.rm = T),
    bayes_recv_g = sum(bayes_recv_g), # , na.rm = T),
    bayes_disp_g = sum(bayes_disp_g), # , na.rm = T),
    def = sum(def, na.rm = T),
    mid = sum(mid, na.rm = T),
    fwd = sum(fwd, na.rm = T),
    int = sum(int, na.rm = T),
    key_def = sum(key_def, na.rm = T),
    med_def = sum(med_def, na.rm = T),
    midfield = sum(midfield, na.rm = T),
    mid_fwd = sum(mid_fwd, na.rm = T),
    med_fwd = sum(med_fwd, na.rm = T),
    key_fwd = sum(key_fwd, na.rm = T),
    rucks = sum(rucks, na.rm = T),
    other_pos = sum(other_pos, na.rm = T),
    count = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(teamName.x) %>%
  tidyr::fill(bayes_g, bayes_recv_g, bayes_disp_g) %>%
  mutate(
    def = ifelse(def == 0, lag(def), def),
    mid = ifelse(mid == 0, lag(mid), mid),
    fwd = ifelse(fwd == 0, lag(fwd), fwd),
    int = ifelse(int == 0, lag(int), int)
  ) %>%
  tidyr::fill(def, mid, fwd, int) %>%
  dplyr::ungroup()

####
team_mdl_df <- team_rt_df %>% # filter(!is.na(bayes_g)) %>%
  dplyr::left_join(team_rt_df %>% mutate(typ2 = ifelse(teamType.x == "home", "away", "home")),
    by = c("providerId" = "providerId", "teamType.x" = "typ2")
  ) %>%
  dplyr::mutate(
    bayes_g_diff = bayes_g.x - bayes_g.y,
    bayes_g_ratio = log(bayes_g.x / bayes_g.y),
    bayes_recv_g_diff = bayes_recv_g.x - bayes_recv_g.y,
    bayes_disp_g_diff = bayes_disp_g.x - bayes_disp_g.y
  ) %>%
  dplyr::left_join(results %>%
    dplyr::select(
      match.matchId,
      homeTeamScore.matchScore.totalScore, homeTeamScore.matchScore.goals, homeTeamScore.matchScore.behinds,
      awayTeamScore.matchScore.totalScore, awayTeamScore.matchScore.goals, awayTeamScore.matchScore.behinds,
      match.utcStartTime
    ),
  by = c("providerId" = "match.matchId")
  ) %>%
  dplyr::mutate(
    home_shots = homeTeamScore.matchScore.goals + homeTeamScore.matchScore.behinds,
    away_shots = awayTeamScore.matchScore.goals + awayTeamScore.matchScore.behinds,
    score_diff = ifelse(teamType.x == "home",
      homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore,
      awayTeamScore.matchScore.totalScore - homeTeamScore.matchScore.totalScore
    ),
    shot_diff = ifelse(teamType.x == "home",
      home_shots - away_shots,
      away_shots - home_shots
    ),
    team_shots = ifelse(teamType.x == "home",
      home_shots,
      away_shots
    ),
    shot_conv = ifelse(teamType.x == "home",
      homeTeamScore.matchScore.goals / home_shots,
      awayTeamScore.matchScore.goals / away_shots
    ),
    win = ifelse(score_diff > 0, 1, ifelse(score_diff == 0, 0.5, 0)),
    hoff_adef = fwd.x - def.y,
    hmid_amid = mid.x - mid.y,
    hdef_afwd = def.x - fwd.y,
    hint_aint = pmax(pmin((int.x - int.y), 10), -10),
    team_type_fac = as.factor(teamType.x),
    total_score = homeTeamScore.matchScore.totalScore + awayTeamScore.matchScore.totalScore,
    total_shots = home_shots + away_shots,
    team_name.x = as.factor(teamName.x.x),
    team_name.y = as.factor(teamName.x.y),
    weightz = exp(as.numeric(-(Sys.Date() - as.Date(match.utcStartTime))) / 250),
    weightz = weightz / mean(weightz, na.rm = T)
  )

#### MODEL
library(mgcViz)
# set.seed("1234")

###
afl_totshots_mdl <- bam(total_shots ~
  s(team_type_fac, bs = "re")
  + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
  + s(abs(bayes_g_diff), bs = "ts", k = 5),
data = team_mdl_df, weights = weightz,
family = quasipoisson(), nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_totshots <- predict(afl_totshots_mdl, newdata = team_mdl_df, type = "response")
# Deviance explained =   22%

###
afl_shot_mdl <- bam(shot_diff ~
  s(team_type_fac, bs = "re")
  + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
  + ti(bayes_g_diff, pred_totshots, bs = c("ts", "ts"), k = 5)
  + s(pred_totshots, bs = "ts")
  + s(bayes_g_diff, bs = "ts"),
data = team_mdl_df, weights = weightz,
family = gaussian(), nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_shot_diff <- predict(afl_shot_mdl, newdata = team_mdl_df, type = "response")
# Deviance explained = 44.5%

###
afl_conv_mdl <- bam(shot_conv ~
  s(team_type_fac, bs = "re")
  #+ s(team_name.x, bs = "re")+ s(team_name.y, bs = "re")
  + ti(bayes_g_diff, pred_totshots, bs = c("ts", "ts"), k = 5)
  + s(pred_totshots, bs = "ts")
  + s(pred_shot_diff, bs = "ts")
  + s(bayes_g_diff, bs = "ts") #+ s(bayes_recv_g_diff, bs = "ts")
  + s(bayes_disp_g_diff, bs = "ts")
  + s(hoff_adef, bs = "ts", k = 5) + s(hmid_amid, bs = "ts", k = 5)
  #+ s(hdef_afwd, bs = "ts")
  + s(hint_aint, bs = "ts", k = 5)
  + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5) #+ s(def.x, bs = "ts")
  #+ s(fwd.y, bs = "ts")
  + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5),
data = team_mdl_df, weights = team_shots * weightz,
family = "binomial", nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_conv <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")

###

afl_score_mdl <- bam(score_diff ~
  s(team_type_fac, bs = "re")
  + s(pred_totshots, bs = "ts")
  + ti(pred_shot_diff, pred_conv, bs = c("ts", "ts"), k = 5)
  + s(pred_conv, bs = "ts")
  + s(bayes_g_diff, bs = "ts")
  + s(bayes_recv_g_diff, bs = "ts")
  + s(bayes_disp_g_diff, bs = "ts")
  + s(pred_shot_diff, bs = "ts"),
data = team_mdl_df, weights = weightz,
family = "gaussian", nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")
# Deviance explained = 42.2%

###
afl_win_mdl <-
  bam(win ~
    s(team_type_fac, bs = "re")
    + ti(pred_totshots, pred_score_diff, bs = c("ts", "ts"), k = 5)
    + s(pred_score_diff, bs = "ts"),
  data = team_mdl_df, weights = weightz,
  family = "binomial", nthreads = 4, select = T, discrete = T
  )

team_mdl_df$pred_win <- predict(afl_win_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$bits <- ifelse(team_mdl_df$win == 1,
  1 + log2(team_mdl_df$pred_win),
  1 + log2(1 - team_mdl_df$pred_win)
)
team_mdl_df$tips <- ifelse(round(team_mdl_df$pred_win) == team_mdl_df$win, 1, 0)
team_mdl_df$mae <- abs(team_mdl_df$score_diff - team_mdl_df$pred_score_diff)
#########
test_df <- team_mdl_df %>% filter(!is.na(win), win != 0.5, teamType.x == "home", season.x >= 2021)
library(MLmetrics)
MLmetrics::LogLoss(test_df$pred_win, test_df$win)
MLmetrics::MAE(test_df$pred_score_diff, test_df$score_diff)
sum(test_df$bits)
mean(test_df$bits)
mean(test_df$tips)

###
week_gms <- team_mdl_df %>%
  dplyr::mutate(totscore = sum(team_mdl_df$total_score, na.rm = T) / sum(team_mdl_df$total_shots, na.rm = T) * pred_totshots) %>%
  dplyr::filter(season.x == lubridate::year(Sys.Date()), round.roundNumber.x.x == (n - 1), teamType.x == "home") %>%
  dplyr::select(
    count.x, providerId, teamName.x.x, bayes_g.x, teamName.x.y, bayes_g.y,
    totscore, pred_shot_diff, pred_score_diff, pred_win, bits, score_diff
  )

week_gms
