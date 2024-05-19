###
library(tidyverse)

skip <- 'no'

# devtools::load_all()

# data(teams, envir = environment())
#
# data(results, envir = environment())
#
# data(torp_df_total, envir = environment())

### this seems to take forever
if(skip == 'no'){
xg_df <- match_xgs(T,T)
}

pred_df <- readRDS("./data-raw/stat_pred_df.rds")

team_preds <- pred_df %>%
  group_by(provider_id, team_name, opp_name) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) # %>% View()

decay <- 1500

#############

team_lineup_df <-
  teams %>% # dplyr::filter(providerId != 'CD_M20230140105' | player.playerId != 'CD_I297373') %>% #tibble::view()
  dplyr::left_join(torp_df_total, by = c("player.playerId" = "player_id", "season" = "season", "round.roundNumber" = "round")) %>%
  # dplyr::filter(!is.na(torp) | (get_afl_season(type = 'next') >= season &  get_afl_week(type = 'next') >= round.roundNumber)) %>% #as.Date(utcStartTime.x) >= lubridate::with_tz(Sys.Date(),"UTC")
  dplyr::filter((position.x != "EMERG" & position.x != "SUB") | is.na(position.x)) %>% # tibble::view()
  dplyr::mutate(
    ###### CHECK
    torp = tidyr::replace_na(torp, 0),
    torp_recv = tidyr::replace_na(torp_recv, 0),
    torp_disp = tidyr::replace_na(torp_disp, 0),
    phase = dplyr::case_when(
      position.x %in% c("BPL", "BPR", "FB", "CHB", "HBFL", "HBFR") ~ "def",
      position.x %in% c("C", "WL", "WR", "R", "RR", "RK") ~ "mid",
      position.x %in% c("FPL", "FPR", "FF", "CHF", "HFFL", "HFFR") ~ "fwd",
      position.x %in% c("INT", "SUB") ~ "int",
      TRUE ~ "other",
    ),
    def = ifelse(phase == "def", torp, NA),
    mid = ifelse(phase == "mid", torp, NA),
    fwd = ifelse(phase == "fwd", torp, NA),
    int = ifelse(phase == "int", torp, NA),
    # positional lines
    backs = ifelse(position.x == "BPL" | position.x == "BPR" | position.x == "FB", torp, NA),
    half_backs = ifelse(position.x == "HBFL" | position.x == "HBFR" | position.x == "CHB", torp, NA),
    midfielders = ifelse(position.x == "WL" | position.x == "WR" | position.x == "C", torp, NA),
    followers = ifelse(position.x == "R" | position.x == "RR" | position.x == "RK", torp, NA),
    half_forwards = ifelse(position.x == "HFFL" | position.x == "HFFR" | position.x == "CHF", torp, NA),
    forwards = ifelse(position.x == "FPL" | position.x == "FPR" | position.x == "FF", torp, NA),
    # individual positions
    BP = ifelse(position.x == "BPL" | position.x == "BPR", torp, NA),
    BPL = ifelse(position.x == "BPL", torp, NA),
    BPR = ifelse(position.x == "BPR", torp, NA),
    FB = ifelse(position.x == "FB", torp, NA),
    HBFL = ifelse(position.x == "HBFL", torp, NA),
    HBFR = ifelse(position.x == "HBFR", torp, NA),
    CHB = ifelse(position.x == "CHB", torp, NA),
    WL = ifelse(position.x == "WL", torp, NA),
    WR = ifelse(position.x == "WR", torp, NA),
    C = ifelse(position.x == "C", torp, NA),
    R = ifelse(position.x == "R", torp, NA),
    RR = ifelse(position.x == "RR", torp, NA),
    RK = ifelse(position.x == "RK", torp, NA),
    HFFL = ifelse(position.x == "HFFL", torp, NA),
    HFFR = ifelse(position.x == "HFFR", torp, NA),
    CHF = ifelse(position.x == "CHF", torp, NA),
    FPL = ifelse(position.x == "FPL", torp, NA),
    FPR = ifelse(position.x == "FPR", torp, NA),
    FF = ifelse(position.x == "FF", torp, NA),
    # grouped positioins
    CB = ifelse(position.x == "CHB" | position.x == "FB", torp, NA),
    BP = ifelse(position.x == "BPL"| position.x == "BPR", torp, NA),
    HBF = ifelse(position.x == "HBFL" | position.x == "HBFR", torp, NA),
    W = ifelse(position.x == "WL" | position.x == "WR", torp, NA),
    MIDS = ifelse(position.x == "C" | position.x == "R" | position.x == "RR", torp, NA),
    HFF = ifelse(position.x == "HFFL" | position.x == "HFFR", torp, NA),
    FP = ifelse(position.x == "FPL" | position.x == "FPR", torp, NA),
    CF = ifelse(position.x == "FF" | position.x == "CHF", torp, NA),
    # champion data specific
    key_def = ifelse(position.y == "KEY_DEFENDER", torp, NA),
    med_def = ifelse(position.y == "MEDIUM_DEFENDER", torp, NA),
    midfield = ifelse(position.y == "MIDFIELDER", torp, NA),
    mid_fwd = ifelse(position.y == "MIDFIELDER_FORWARD", torp, NA),
    med_fwd = ifelse(position.y == "MEDIUM_FORWARD", torp, NA),
    key_fwd = ifelse(position.y == "KEY_FORWARD", torp, NA),
    rucks = ifelse(position.y == "RUCK", torp, NA),
    other_pos = ifelse(is.na(position.y), torp, NA)
  )

team_rt_df <- team_lineup_df %>%
  dplyr::group_by(providerId, venue.name, teamName, season, round.roundNumber, teamType) %>%
  dplyr::summarise(
    torp = sum(torp), # , na.rm = T),
    torp_recv = sum(torp_recv), # , na.rm = T),
    torp_disp = sum(torp_disp), # , na.rm = T),
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
    ###
    CB = sum(CB, na.rm = T),
    BP = sum(BP, na.rm = T),
    HBF = sum(HBF, na.rm = T),
    W = sum(W, na.rm = T),
    MIDS = sum(MIDS, na.rm = T),
    HFF = sum(HFF, na.rm = T),
    FP = sum(FP, na.rm = T),
    CF = sum(CF, na.rm = T),
    ###
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
  dplyr::group_by(teamName) %>%
  tidyr::fill(torp, torp_recv, torp_disp) %>%
  dplyr::mutate(
    def = ifelse(def == 0, dplyr::lag(def), def),
    mid = ifelse(mid == 0, dplyr::lag(mid), mid),
    fwd = ifelse(fwd == 0, dplyr::lag(fwd), fwd),
    int = ifelse(int == 0, dplyr::lag(int), int)
  ) %>%
  tidyr::fill(def, mid, fwd, int) %>%
  dplyr::ungroup()

####
team_mdl_df <- team_rt_df %>% # filter(!is.na(torp)) %>%
  dplyr::left_join(
    team_rt_df %>%
      dplyr::mutate(typ2 = dplyr::if_else(teamType == "home", "away", "home")),
    by = c("providerId" = "providerId", "teamType" = "typ2")
  ) %>%
  dplyr::mutate(
    torp_diff = torp.x - torp.y,
    torp_ratio = log(torp.x / torp.y),
    torp_recv_diff = torp_recv.x - torp_recv.y,
    torp_disp_diff = torp_disp.x - torp_disp.y
  ) %>%
  dplyr::left_join(
    results %>%
      dplyr::select(
        match.matchId,
        homeTeamScore.matchScore.totalScore, homeTeamScore.matchScore.goals, homeTeamScore.matchScore.behinds,
        awayTeamScore.matchScore.totalScore, awayTeamScore.matchScore.goals, awayTeamScore.matchScore.behinds,
        match.utcStartTime
      ),
    by = c("providerId" = "match.matchId")
  ) %>%
  dplyr::left_join(xg_df,
    by = c("providerId" = "match_id")
  ) %>%
  dplyr::mutate(
    home_shots = homeTeamScore.matchScore.goals + homeTeamScore.matchScore.behinds,
    away_shots = awayTeamScore.matchScore.goals + awayTeamScore.matchScore.behinds,
    score_diff = ifelse(teamType == "home",
      homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore,
      awayTeamScore.matchScore.totalScore - homeTeamScore.matchScore.totalScore
    ),
    shot_diff = ifelse(teamType == "home",
      home_shots - away_shots,
      away_shots - home_shots
    ),
    team_shots = ifelse(teamType == "home",
      home_shots,
      away_shots
    ),
    shot_conv = ifelse(teamType == "home",
      homeTeamScore.matchScore.goals / home_shots,
      awayTeamScore.matchScore.goals / away_shots
    ),
    xscore_diff = ifelse(teamType == "home",
      xscore_diff,
      -xscore_diff
    ),
    team_xscore = ifelse(teamType == "home",
      home_xscore,
      away_xscore
    ),
    win = ifelse(score_diff > 0, 1, ifelse(score_diff == 0, 0.5, 0)),
    hoff_adef = pmax(pmin((fwd.x - def.y), 20), -5),
    hmid_amid = pmax(pmin((mid.x - mid.y), 12), -12),
    hdef_afwd = pmax(pmin((def.x - fwd.y), 5), -20),
    hint_aint = pmax(pmin((int.x - int.y), 10), -10),
    # individual positions diff
    BPL_diff = BPL.x - BPL.y,
    BPR_diff = BPR.x - BPR.y,
    FB_diff = FB.x - FB.y,
    HBFL_diff = HBFL.x - HBFL.y,
    HBFR_diff = HBFR.x - HBFR.y,
    CHB_diff = CHB.x - CHB.y,
    WL_diff = WL.x - WL.y,
    WR_diff = WR.x - WR.y,
    C_diff = C.x - C.y,
    R_diff = R.x - R.y,
    RR_diff = RR.x - RR.y,
    RK_diff = RK.x - RK.y,
    HFFL_diff = HFFL.x - HFFL.y,
    HFFR_diff = HFFR.x - HFFR.y,
    CHF_diff = CHF.x - CHF.y,
    FPL_diff = FPL.x - FPL.y,
    FPR_diff = FPR.x - FPR.y,
    FF_diff = FF.x - FF.y,
    int_diff = int.x - int.y,
    ####
    team_type_fac = as.factor(teamType),
    total_score = homeTeamScore.matchScore.totalScore + awayTeamScore.matchScore.totalScore,
    total_shots = home_shots + away_shots,
    team_name.x = as.factor(teamName.x),
    team_name.y = as.factor(teamName.y),
    weightz = exp(as.numeric(-(Sys.Date() - as.Date(match.utcStartTime))) / decay),
    weightz = weightz / mean(weightz, na.rm = T)
  ) %>%
  left_join(team_preds, by = c("providerId" = "provider_id", "teamName.x" = "team_name"))


#### MODEL
# library(mgcViz)
# set.seed("1234")

###
afl_totshots_mdl <- mgcv::bam(
  total_shots ~
    s(team_type_fac, bs = "re") +
  s(hoff_adef, bs='ts') + # = pmax(pmin((fwd.x - def.y), 20), -5),
  s(hmid_amid, bs='ts') + # = pmax(pmin((mid.x - mid.y), 12), -12),
  s(hdef_afwd, bs='ts') + # = pmax(pmin((def.x - fwd.y), 5), -20),
  s(hint_aint, bs='ts') + # = pmax(pmin((int.x - int.y), 10), -10),
  + s(def.x, bs='ts')
  + s(mid.x, bs='ts')
  + s(fwd.x, bs='ts')
  + s(int.x, bs='ts')
    #+ s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    + s(abs(torp_diff), bs = "ts", k = 5)

  ,
  data = team_mdl_df, weights = weightz,
  family = quasipoisson(), nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_totshots <- predict(afl_totshots_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_totshots_mdl)
# Deviance explained =   22%

###
afl_shot_mdl <- mgcv::bam(
  shot_diff ~
    s(team_type_fac, bs = "re")
    #+ s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    + ti(torp_diff, pred_totshots, bs = c("ts", "ts"), k = 4)
    + s(pred_totshots, bs = "ts", k = 5)
    + s(torp_diff, bs = "ts", k = 5),
  data = team_mdl_df, weights = weightz,
  family = gaussian(), nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_shot_diff <- predict(afl_shot_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_shot_mdl)
# mixedup::extract_ranef(afl_shot_mdl) %>% tibble::view()
# plot(mgcViz::getViz(afl_shot_mdl))
# Deviance explained = 44.5%

###
afl_total_xpoints_mdl <- mgcv::bam(
  total_xpoints ~
    s(team_type_fac, bs = "re")
    #+ s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    + s(abs(torp_diff), bs = "ts", k = 5)
    + s(abs(torp_recv_diff), bs = "ts", k = 5)
    + s(abs(torp_disp_diff), bs = "ts", k = 5)
    + s(torp.x, bs = "ts", k = 5) + s(torp.y, bs = "ts", k = 5)
    + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
    + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5),
  data = team_mdl_df, weights = weightz,
  family = gaussian(), nthreads = 4, select = T, discrete = T
)

team_mdl_df$pred_tot_xscore <- predict(afl_total_xpoints_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_total_xpoints_mdl)
# Deviance explained =   22%

###
afl_xscore_diff_mdl <- mgcv::bam(
  xscore_diff ~
    s(team_type_fac, bs = "re")
    #+ s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    + ti(torp_diff, pred_tot_xscore, bs = c("ts", "ts"), k = 4)
    + s(pred_tot_xscore, bs = "ts", k = 5)
    + s(torp_diff, bs = "ts", k = 5),
  data = team_mdl_df, weights = weightz,
  family = gaussian(), nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_xscore_diff <- predict(afl_xscore_diff_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_xscore_diff_mdl)
# mixedup::extract_ranef(afl_xscore_diff_mdl) %>% tibble::view()
# plot(mgcViz::getViz(afl_xscore_diff_mdl))
# Deviance explained = 44.5%


###
afl_conv_mdl <- mgcv::bam(
  shot_conv ~
    s(team_type_fac, bs = "re")
    #+ s(team_name.x, bs = "re")+ s(team_name.y, bs = "re")
    + ti(torp_diff, pred_tot_xscore, bs = c("ts", "ts"), k = 4)
    + s(pred_tot_xscore, bs = "ts", k = 5)
    + s(pred_xscore_diff, bs = "ts", k = 5)
    + s(torp_diff, bs = "ts", k = 5)
    + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
    + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5),
  data = team_mdl_df, weights = team_shots * weightz,
  family = "binomial", nthreads = 4, select = T, discrete = T
)

team_mdl_df$pred_conv <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")
# summary(afl_conv_mdl)
###

afl_score_mdl <- mgcv::bam(
  score_diff ~
    s(team_type_fac, bs = "re")
    + s(pred_tot_xscore, bs = "ts", k = 5)
    + ti(pred_xscore_diff, pred_conv, bs = c("ts", "ts"), k = 4)
    + s(pred_conv, bs = "ts", k = 5)
    + s(torp_diff, bs = "ts", k = 5)
    + s(torp_recv_diff, bs = "ts", k = 5)
    + s(torp_disp_diff, bs = "ts", k = 5)
    + s(pred_xscore_diff, bs = "ts", k = 5),
  data = team_mdl_df, weights = weightz,
  family = "gaussian", nthreads = 4, select = T, discrete = T
)
team_mdl_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_score_mdl)
# Deviance explained = 42.2%

###
afl_win_mdl <-
  mgcv::bam(
    win ~
      s(team_type_fac, bs = "re")
      #+ ti(pred_tot_xscore, pred_score_diff, bs = c("ts", "ts"), k = 4)
      + ti(pred_tot_xscore, pred_xscore_diff, bs = c("ts", "ts"), k = 4)
      + s(pred_score_diff, bs = "ts", k = 5)
    #+ s(pred_xscore_diff, bs = "ts", k = 5)
    ,
    data = team_mdl_df, weights = weightz,
    family = "binomial", nthreads = 4, select = T, discrete = T
  )

# summary(afl_win_mdl)

team_mdl_df$pred_win <- predict(afl_win_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$bits <- ifelse(team_mdl_df$win == 1,
  1 + log2(team_mdl_df$pred_win),
  1 + log2(1 - team_mdl_df$pred_win)
)
team_mdl_df$tips <- ifelse(round(team_mdl_df$pred_win) == team_mdl_df$win, 1, 0)
team_mdl_df$mae <- abs(team_mdl_df$score_diff - team_mdl_df$pred_score_diff)
#########
test_df <- team_mdl_df %>% dplyr::filter(!is.na(win), win != 0.5, teamType == "home", season.x >= 2022)
# library(MLmetrics)
MLmetrics::LogLoss(test_df$pred_win, test_df$win)
MLmetrics::MAE(test_df$pred_score_diff, test_df$score_diff)
sum(test_df$bits)
mean(test_df$bits)
mean(test_df$tips)

#################################### GF
team_mdl_df$team_type_fac <- as.factor(ifelse(team_mdl_df$providerId == "CD_M20220142701",
  ifelse(team_mdl_df$teamType == "home", "home", "away"),
  team_mdl_df$teamType
))

team_mdl_df$pred_totshots <- predict(afl_totshots_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_shot_diff <- predict(afl_shot_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_conv <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_win <- predict(afl_win_mdl, newdata = team_mdl_df, type = "response")

######
n <- get_afl_week(type = "next")

week_gms_home <- team_mdl_df %>%
  dplyr::mutate(
    totscore = pred_tot_xscore
    # totscore = sum(team_mdl_df$total_score, na.rm = T) / sum(team_mdl_df$total_shots, na.rm = T) * pred_totshots
                ) %>%
  dplyr::filter(season.x == lubridate::year(Sys.Date()), round.roundNumber.x == n, teamType == "home") %>%
  dplyr::select(
    players = count.x, providerId,
    home_team = teamName.x, home_rating = torp.x,
    away_team = teamName.y, away_rating = torp.y,
    totscore, pred_shot_diff, pred_score_diff, pred_win, bits, score_diff
  )

week_gms_away <- team_mdl_df %>%
  dplyr::mutate(
    totscore = pred_tot_xscore,
    # totscore = sum(team_mdl_df$total_score, na.rm = T) / sum(team_mdl_df$total_shots, na.rm = T) * pred_totshots,
    pred_shot_diff = -pred_shot_diff,
    pred_score_diff = -pred_score_diff,
    pred_win = 1 - pred_win,
    score_diff = -score_diff
  ) %>%
  dplyr::filter(season.x == lubridate::year(Sys.Date()), round.roundNumber.x == n, teamType == "away") %>%
  dplyr::select(
    players = count.x, providerId,
    home_team = teamName.y, home_rating = torp.y,
    away_team = teamName.x, away_rating = torp.x,
    totscore, pred_shot_diff, pred_score_diff, pred_win, bits, score_diff
  )

week_gms <- dplyr::bind_rows(week_gms_home, week_gms_away) %>%
  dplyr::group_by(providerId, home_team, home_rating, away_team, away_rating) %>%
  dplyr::summarise(
    players = mean(players),
    total = mean(totscore),
    pred_shot_diff = mean(pred_shot_diff),
    pred_score_diff = mean(pred_score_diff),
    pred_win = mean(pred_win),
    score_diff = mean(score_diff)
  )

week_gms
