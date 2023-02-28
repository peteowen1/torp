###
if (exists("teams") == FALSE) {
  teams <-
    dplyr::bind_rows(fitzRoy::fetch_lineup(season = 2021, comp = "AFLM")) %>%
    dplyr::bind_rows(fitzRoy::fetch_lineup(season = 2022, comp = "AFLM")) %>%
    dplyr::mutate(season = as.numeric(substr(providerId, 5, 8)))
}

if (exists("results") == FALSE) {
results <-
  dplyr::bind_rows(fitzRoy::fetch_results(season = 2021, comp = "AFLM")) %>%
  dplyr::bind_rows(fitzRoy::fetch_results(season = 2022, comp = "AFLM")) %>%
  dplyr::mutate(season = as.numeric(substr(match.matchId, 5, 8)))
}

############# 2021
##############################
if (exists("final_21") == FALSE) {
final_21 <- furrr::future_map_dfr(2:28, ~ plyr_ratings(plyr_gm_df, teams, 2021, .))
saveRDS(final_21, "./data/plyr_df_2021.rds")
final_21 <- readRDS("./data/plyr_df_2021.rds")
}
### 2022
n <- max(teams %>% dplyr::filter(season == lubridate::year(Sys.Date()), utcStartTime < Sys.time() + 350000) %>%
  dplyr::select(round.roundNumber)) + 1

n <- 28
### 2022
if (exists("final_22") == FALSE) {
final_22 <- furrr::future_map_dfr(1:n, ~ plyr_ratings(plyr_gm_df, teams, 2022, .))
saveRDS(final_22, "./data/plyr_df_2022.rds")
final_22 <- readRDS("./data/plyr_df_2022.rds")
}
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
    bayes_g = pmax(bayes_g,0),
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
    # positional lines
    backs = ifelse(position.x == "BPL" | position.x == "BPR"| position.x == "FB" , bayes_g, NA),
    half_backs = ifelse(position.x == "HBFL"|position.x == "HBFR"|position.x == "CHB", bayes_g, NA),
    midfielders = ifelse(position.x == "WL"|position.x == "WR"|position.x == "C", bayes_g, NA),
    followers = ifelse(position.x == "R"|position.x == "RR"|position.x == "RK", bayes_g, NA),
    half_forwards = ifelse(position.x == "HFFL" | position.x == "HFFR" |position.x == "CHF", bayes_g, NA),
    forwards = ifelse(position.x == "FPL" | position.x == "FPR"|position.x == "FF" , bayes_g, NA),
    # individual positions
    BP = ifelse(position.x == "BPL"| position.x == "BPR", bayes_g, NA),
    BPL = ifelse(position.x == "BPL", bayes_g, NA),
    BPR = ifelse(position.x == "BPR", bayes_g, NA),
    FB = ifelse(position.x == "FB", bayes_g, NA),
    HBF = ifelse(position.x == "HBFL" | position.x == "HBFR", bayes_g, NA),
    HBFL = ifelse(position.x == "HBFL", bayes_g, NA),
    HBFR = ifelse(position.x == "HBFR", bayes_g, NA),
    CHB = ifelse(position.x == "CHB", bayes_g, NA),
    W = ifelse(position.x == "WL" | position.x == "WR", bayes_g, NA),
    WL = ifelse(position.x == "WL", bayes_g, NA),
    WR = ifelse(position.x == "WR", bayes_g, NA),
    C = ifelse(position.x == "C", bayes_g, NA),
    R = ifelse(position.x == "R", bayes_g, NA),
    RR = ifelse(position.x == "RR", bayes_g, NA),
    RK = ifelse(position.x == "RK", bayes_g, NA),
    HFF = ifelse(position.x == "HFFL" | position.x == "HFFR", bayes_g, NA),
    HFFL = ifelse(position.x == "HFFL", bayes_g, NA),
    HFFR = ifelse(position.x == "HFFR", bayes_g, NA),
    CHF = ifelse(position.x == "CHF", bayes_g, NA),
    FP = ifelse(position.x == "FPL" | position.x == "FPR", bayes_g, NA),
    FPL = ifelse(position.x == "FPL", bayes_g, NA),
    FPR = ifelse(position.x == "FPR", bayes_g, NA),
    FF = ifelse(position.x == "FF", bayes_g, NA),
    # champion data specific
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
    ###
    backs = sum(backs, na.rm = T),
    half_backs = sum(half_backs, na.rm = T),
    midfielders = sum(midfielders, na.rm = T),
    followers = sum(followers, na.rm = T),
    half_forwards = sum(half_forwards, na.rm = T),
    forwards = sum(forwards, na.rm = T),
    ###
    BPL = sum(BPL, na.rm = T),
    BPR = sum(BPR, na.rm = T),
    FB = sum(FB, na.rm = T),
    HBFL = sum(HBFL, na.rm = T),
    HBFR = sum(HBFR, na.rm = T),
    CHB = sum(CHB, na.rm = T),
    WL = sum(WL, na.rm = T),
    WR = sum(WR, na.rm = T),
    C = sum(C, na.rm = T),
    R = sum(R, na.rm = T),
    RR = sum(RR, na.rm = T),
    RK = sum(RK, na.rm = T),
    HFFL = sum(HFFL, na.rm = T),
    HFFR = sum(HFFR, na.rm = T),
    CHF = sum(CHF, na.rm = T),
    FPL = sum(FPL, na.rm = T),
    FPR = sum(FPR, na.rm = T),
    FF = sum(FF, na.rm = T),
    BP = mean(BP, na.rm = T),
    W = mean(W, na.rm = T),
    HBF = mean(HBF, na.rm = T),
    HFF = mean(HFF, na.rm = T),
    FP = mean(FP, na.rm = T),
    key_def = sum(key_def, na.rm = T),
    med_def = sum(med_def, na.rm = T),
    midfield = sum(midfield, na.rm = T),
    mid_fwd = sum(mid_fwd, na.rm = T),
    med_fwd = sum(med_fwd, na.rm = T),
    key_fwd = sum(key_fwd, na.rm = T),
    rucks = sum(rucks, na.rm = T),
    other_pos = sum(other_pos, na.rm = T),
    count = dplyr::n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(teamName.x) %>%
  tidyr::fill(bayes_g, bayes_recv_g, bayes_disp_g) %>%
  dplyr::mutate(
    def = ifelse(def == 0, dplyr::lag(def), def),
    mid = ifelse(mid == 0, dplyr::lag(mid), mid),
    fwd = ifelse(fwd == 0, dplyr::lag(fwd), fwd),
    int = ifelse(int == 0, dplyr::lag(int), int)
  ) %>%
  tidyr::fill(def, mid, fwd, int) %>%
  dplyr::ungroup()

####
team_mdl_df <- team_rt_df %>% # filter(!is.na(bayes_g)) %>%
  dplyr::left_join(team_rt_df %>%
                     dplyr::mutate(typ2 = dplyr::if_else(teamType.x == "home", "away", "home")),
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
    hoff_adef = pmax(pmin((fwd.x - def.y), 20), -5),
    hmid_amid = pmax(pmin((mid.x - mid.y), 12), -12),
    hdef_afwd = pmax(pmin((def.x - fwd.y), 5), -20),
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
# library(mgcViz)
# set.seed("1234")

###
afl_totshots_mdl <- mgcv::bam(total_shots ~
  s(team_type_fac, bs = "re")
  + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
  + s(abs(bayes_g_diff), bs = "ts", k = 5),
data = team_mdl_df, weights = weightz,
family = quasipoisson(), nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_totshots <- predict(afl_totshots_mdl, newdata = team_mdl_df, type = "response")
# Deviance explained =   22%

###
afl_shot_mdl <- mgcv::bam(shot_diff ~
  s(team_type_fac, bs = "re")
  + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
  + ti(bayes_g_diff, pred_totshots, bs = c("ts", "ts"), k = 4)
  + s(pred_totshots, bs = "ts", k = 5)
  + s(bayes_g_diff, bs = "ts", k = 5),
data = team_mdl_df, weights = weightz,
family = gaussian(), nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_shot_diff <- predict(afl_shot_mdl, newdata = team_mdl_df, type = "response")
# Deviance explained = 44.5%

###
afl_conv_mdl <- mgcv::bam(shot_conv ~
  s(team_type_fac, bs = "re")
  #+ s(team_name.x, bs = "re")+ s(team_name.y, bs = "re")
  + ti(bayes_g_diff, pred_totshots, bs = c("ts", "ts"), k = 4)
  + s(pred_totshots, bs = "ts", k = 5)
  + s(pred_shot_diff, bs = "ts", k = 5)
  + s(bayes_g_diff, bs = "ts", k = 5)
  + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
  + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5),
data = team_mdl_df, weights = team_shots * weightz,
family = "binomial", nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_conv <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")

###

afl_score_mdl <- mgcv::bam(score_diff ~
  s(team_type_fac, bs = "re")
  + s(pred_totshots, bs = "ts", k = 5)
  + ti(pred_shot_diff, pred_conv, bs = c("ts", "ts"), k = 4)
  + s(pred_conv, bs = "ts", k = 5)
  + s(bayes_g_diff, bs = "ts", k = 5)
  + s(bayes_recv_g_diff, bs = "ts", k = 5)
  + s(bayes_disp_g_diff, bs = "ts", k = 5)
  + s(pred_shot_diff, bs = "ts", k = 5),
data = team_mdl_df, weights = weightz,
family = "gaussian", nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")
# Deviance explained = 42.2%

###
afl_win_mdl <-
  mgcv::bam(win ~
    s(team_type_fac, bs = "re")
    + ti(pred_totshots, pred_score_diff, bs = c("ts", "ts"), k = 4)
    + s(pred_score_diff, bs = "ts", k = 5),
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
test_df <- team_mdl_df %>% dplyr::filter(!is.na(win), win != 0.5, teamType.x == "home", season.x >= 2022)
#library(MLmetrics)
MLmetrics::LogLoss(test_df$pred_win, test_df$win)
MLmetrics::MAE(test_df$pred_score_diff, test_df$score_diff)
sum(test_df$bits)
mean(test_df$bits)
mean(test_df$tips)

#################################### GF
team_mdl_df$team_type_fac <- as.factor(ifelse(team_mdl_df$providerId == "CD_M20220142701",
                                    ifelse(team_mdl_df$teamType.x == "home","home","away"),
                                    team_mdl_df$teamType.x))

team_mdl_df$pred_totshots <- predict(afl_totshots_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_shot_diff <- predict(afl_shot_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_conv <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_win <- predict(afl_win_mdl, newdata = team_mdl_df, type = "response")
###
week_gms <- team_mdl_df %>%
  dplyr::mutate(totscore = sum(team_mdl_df$total_score, na.rm = T) / sum(team_mdl_df$total_shots, na.rm = T) * pred_totshots) %>%
  dplyr::filter(season.x == lubridate::year(Sys.Date()), round.roundNumber.x.x == (n-1), teamType.x == "home") %>%
  dplyr::select(
    count.x, providerId, teamName.x.x, bayes_g.x, teamName.x.y, bayes_g.y,
    totscore, pred_shot_diff, pred_score_diff, pred_win, bits, score_diff
  )

week_gms
