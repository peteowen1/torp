# ###
library(tidyverse)
library(fitzRoy)
# library(fuzzyjoin)
devtools::load_all()

# Load Tables ----
all_grounds <- arrow::read_parquet("./data-raw/outputs/stadium_data.parquet")
xg_df <- load_xg(TRUE)
fixtures <- load_fixtures(TRUE)
results <- load_results(TRUE)
teams <- load_teams(TRUE)

# Build Fixtures Tables ----
decay <- 1000

team_map <-
  fixtures %>%
  group_by(teamId = home.team.providerId) %>%
  summarise(team_name = get_mode(home.team.name)) %>%
  mutate(team_name = replace_teams(team_name))

fix_df <-
  fixtures %>%
  mutate(result = home.score.totalScore - away.score.totalScore) %>%
  select(
    providerId, compSeason.year, round.roundNumber, home.team.providerId, away.team.providerId, utcStartTime, venue.name, venue.timezone,
    result
  ) %>%
  pivot_longer(
    cols = ends_with("team.providerId"),
    names_to = "team_type",
    values_to = "team.providerId"
  ) %>%
  mutate(
    venue = replace_venues(venue.name),
    team_type = substr(team_type, 1, 4),
    result = ifelse(team_type == "away", -result, result)
  ) %>%
  select(providerId,
    season = compSeason.year, round.roundNumber, team_type, teamId = team.providerId, utcStartTime, venue, venue.timezone,
    result
  ) %>%
  dplyr::left_join(team_map, by = "teamId") %>%
  dplyr::mutate(
    team_name_season = as.factor(paste(team_name, season))
  )

fix_df <- fix_df %>%
  # 1. Parse the UTC time string into a POSIXct object, explicitly stating it's UTC
  mutate(
    utc_dt = ymd_hms(utcStartTime, tz = "UTC")
  ) %>%
  # 2. Process row by row because the target timezone changes
  rowwise() %>%
  # 3. Convert the UTC datetime to the local timezone specified in 'venue.timezone'
  #    and format it as a character string for display
  mutate(
    local_start_time_str = format(
      with_tz(utc_dt, tzone = venue.timezone),
      "%Y-%m-%d %H:%M:%S %Z" # Example format, %Z adds timezone abbreviation
    )
  ) %>%
  # 4. Stop row-wise processing
  ungroup()

## Add Date Variables  ----
fix_df <- fix_df %>%
  # Ensure row-wise processing
  rowwise() %>%
  mutate(
    # --- Calculate core LOCAL time components ONCE per row ---
    # Explicitly use with_tz inside each function for robustness
    game_year = year(with_tz(utc_dt, venue.timezone)),
    game_month = month(with_tz(utc_dt, venue.timezone)), # Need the month number
    game_yday = yday(with_tz(utc_dt, venue.timezone)), # Day of the YEAR
    game_mday = day(with_tz(utc_dt, venue.timezone)), # Day of the month
    game_wday = lubridate::wday(with_tz(utc_dt, venue.timezone), week_start = 1), # Day of the week
    game_wday_fac = as.factor(game_wday), # Day of the week factor
    game_hour = hour(with_tz(utc_dt, venue.timezone)) +
      minute(with_tz(utc_dt, venue.timezone)) / 60 +
      second(with_tz(utc_dt, venue.timezone)) / 3600,

    # --- Numeric representation (UTC based) ---
    game_date_numeric = as.numeric(utc_dt), # Seconds since epoch (UTC)
    # --- Timezone ---
    timezone = venue.timezone,
    # --- Proportion through Year (local time) ---
    game_prop_through_year = game_yday / ifelse(leap_year(game_year), 366, 365),
    # --- Proportion through Month (local time) ---
    game_prop_through_month = game_mday / days_in_month(game_month),
    # --- Proportion through Week (local time) ---
    game_prop_through_week = game_wday / 7,
    # --- Proportion through Day (local time) ---
    game_prop_through_day = game_hour / 24,
    # Adjust Game Year
    game_year_decimal = as.numeric(game_year + game_prop_through_year)
  ) %>%
  # IMPORTANT: Remove row-wise grouping
  ungroup()


# Lineups ----

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
    torp_spoil = tidyr::replace_na(torp_spoil, 0),
    torp_hitout = tidyr::replace_na(torp_hitout, 0),
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
    BP = ifelse(position.x == "BPL" | position.x == "BPR", torp, NA),
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

## Aggregate Lineups ----
team_rt_df <- team_lineup_df %>%
  filter(!is.na(player.playerId)) %>%
  # filter(!is.na(teamAbbr)) %>%
  mutate(team_name_adj = fitzRoy::replace_teams(teamName)) %>%
  dplyr::group_by(providerId, teamId, season, round.roundNumber, teamType) %>%
  dplyr::summarise(
    venue = replace_venues(max(venue.name)),
    team_name_adj = max(team_name_adj),
    torp = sum(torp, na.rm = T),
    torp_recv = sum(torp_recv, na.rm = T),
    torp_disp = sum(torp_disp, na.rm = T),
    torp_spoil = sum(torp_spoil, na.rm = T),
    torp_hitout = sum(torp_hitout, na.rm = T),
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
  dplyr::ungroup()

# Find Home Ground ----
home_ground <-
  team_rt_df %>%
  group_by(teamId, team_name_adj) %>%
  summarise(home_ground = get_mode(venue), .groups = "drop") %>%
  mutate(venue_adj = replace_venues(as.character(home_ground))) %>%
  left_join(
    all_grounds %>%
      mutate(venue_adj = replace_venues(as.character(Ground))),
    by = c("venue_adj" = "venue_adj")
  )

## Calculate Familiarity ----
calculate_proportions <- function(data, team, current_season, current_round) {
  filtered_data <- data %>%
    filter(teamId == team & ((season < current_season) | (season == current_season & round.roundNumber < current_round)))

  if (nrow(filtered_data) == 0) {
    return(data.frame(teamId = team, season = current_season, round.roundNumber = current_round, venue = unique(data$venue), familiarity = 0))
  }

  proportions <- filtered_data %>%
    group_by(venue) %>%
    summarise(games_played = n(), .groups = "drop") %>%
    mutate(total_games = sum(games_played)) %>%
    mutate(familiarity = games_played / total_games) %>%
    mutate(teamId = team, season = current_season, round.roundNumber = current_round) %>%
    select(teamId, season, round.roundNumber, venue, familiarity)

  return(proportions)
}

# Get all unique teams, seasons, and rounds
unique_teams <- unique(team_rt_df$teamId)
unique_seasons <- unique(team_rt_df$season)
unique_rounds <- unique(team_rt_df$round.roundNumber)


tictoc::tic()

# Set up parallel processing with purrr
library(purrr)

grid <- tidyr::expand_grid(
  team   = unique_teams,
  season = unique_seasons,
  round  = unique_rounds
)

f <- purrr::in_parallel(
  \(team, season, round) {
    # attach packages on the worker
    suppressPackageStartupMessages({
      library(magrittr)  # for %>%
      library(dplyr)     # if you use bare filter/mutate/select/etc.
      # library(lubridate) # add any others used inside calculate_proportions()
      # library(stringr)
    })
    calculate_proportions(df, team, season, round)
  },
  df = team_rt_df,
  calculate_proportions = calculate_proportions
)

all_proportions <- purrr::pmap(grid, f, .progress = TRUE)

# If you want one data frame:
ground_prop <- dplyr::bind_rows(all_proportions)

## Distance Traveled  ----

team_dist_df <-
  fix_df %>%
  dplyr::mutate(
    venue = replace_venues(venue),
    venue = ifelse(venue == "Adelaide Arena at Jiangwan Stadium", "Jiangwan Stadium", venue)
  ) %>%
  dplyr::left_join(all_grounds %>% dplyr::select(venue, venue_lat = Latitude, venue_lon = Longitude), by = "venue") %>%
  dplyr::left_join(home_ground %>% dplyr::select(teamId, team_lat = Latitude, team_lon = Longitude), by = "teamId") %>%
  dplyr::mutate(distance = purrr::pmap_dbl(
    list(venue_lon, venue_lat, team_lon, team_lat),
    ~ geosphere::distHaversine(c(..1, ..2), c(..3, ..4))
  )) %>%
  dplyr::mutate(
    
    log_dist = log(distance + 10000),
    log_dist = replace_na(log_dist, 16) ### FIX THIS LATER PLZ
    ,
    log_dist = replace_na(log_dist,16)
    ) %>% # Add 10km as the minimum travel
  dplyr::left_join(ground_prop) %>%
  dplyr::mutate(
    familiarity = replace_na(familiarity, 0)
    )

## Days Rest ----
days_rest <- fix_df %>%
  arrange(teamId, utcStartTime) %>%
  group_by(teamId, season) %>%
  mutate(days_rest = as.numeric(difftime(utcStartTime, lag(utcStartTime), units = "days"))) %>%
  ungroup() %>%
  dplyr::mutate(days_rest = replace_na(days_rest, 21))

###
library(rvest)
url <- "https://www.afl.com.au/matches/injury-list"
inj_df <- read_html(url) %>%
  html_table() %>%
  list_rbind() %>%
  janitor::clean_names() %>%
  mutate(
    player = case_match(
      player,
      "Cam Zurhaar" ~ "Cameron Zurhaar",
      .default = player
    )
  )


tr <- torp_ratings(2025, get_afl_week("next")) %>%
  left_join(inj_df, by = c("player_name" = "player")) %>%
  mutate(estimated_return = replace_na(estimated_return, "None"))

tr_week <-
  tr %>%
  filter(
    torp > 0,
    is.na(injury)
  ) %>%
  mutate(team_name = fitzRoy::replace_teams(team)) %>%
  group_by(team_name, season, round) %>%
  mutate(tm_rnk = rank(-torp)) %>%
  filter(tm_rnk <= 21) %>%
  summarise(
    torp_week = sum(pmax(torp, 0), na.rm = T) * 0.95,
    torp_recv_week = sum(pmax(torp_recv, 0), na.rm = T) * 0.95,
    torp_disp_week = sum(pmax(torp_disp, 0), na.rm = T) * 0.95,
    torp_spoil_week = sum(pmax(torp_spoil, 0), na.rm = T) * 0.95,
    torp_hitout_week = sum(pmax(torp_hitout, 0), na.rm = T) * 0.95
  ) %>%
  arrange(-torp_week) # %>% summarise(sum(val)) #1234

# Torp Ratings ----
team_rt_fix_df <-
  fix_df %>%
  mutate(team_name = fitzRoy::replace_teams(team_name)) %>%
  left_join(team_dist_df) %>%
  left_join(days_rest) %>%
  left_join(team_rt_df) %>%
  left_join(tr_week, by = c("team_name" = "team_name", "season" = "season", "round.roundNumber" = "round")) %>%
  mutate(
    torp = coalesce(torp, torp_week),
    torp_recv = coalesce(torp_recv, torp_recv_week),
    torp_disp = coalesce(torp_disp, torp_disp_week),
    torp_spoil = coalesce(torp_spoil, torp_spoil_week),
    torp_hitout = coalesce(torp_hitout, torp_hitout_week)
  ) %>%
  dplyr::group_by(teamId) %>%
  tidyr::fill(torp, torp_recv, torp_disp, torp_spoil, torp_hitout) %>%
  dplyr::mutate(
    def = ifelse(def == 0, dplyr::lag(def), def),
    mid = ifelse(mid == 0, dplyr::lag(mid), mid),
    fwd = ifelse(fwd == 0, dplyr::lag(fwd), fwd),
    int = ifelse(int == 0, dplyr::lag(int), int),
    team_type_fac = as.factor(team_type)
  ) %>%
  tidyr::fill(def, mid, fwd, int) %>%
  dplyr::ungroup()


# Team mdl df ----
team_mdl_df_tot <- team_rt_fix_df %>% # filter(!is.na(torp)) %>%
  dplyr::left_join(
    team_rt_fix_df %>%
      dplyr::mutate(type_anti = dplyr::if_else(team_type == "home", "away", "home")),
    by = c("providerId" = "providerId", "team_type" = "type_anti")
  ) %>%
  dplyr::mutate(
    torp_diff = torp.x - torp.y,
    torp_ratio = log(torp.x / torp.y),
    torp_recv_diff = torp_recv.x - torp_recv.y,
    torp_disp_diff = torp_disp.x - torp_disp.y,
    torp_spoil_diff = torp_spoil.x - torp_spoil.y,
    torp_hitout_diff = torp_hitout.x - torp_hitout.y
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
    score_diff = ifelse(team_type == "home",
      homeTeamScore.matchScore.totalScore - awayTeamScore.matchScore.totalScore,
      awayTeamScore.matchScore.totalScore - homeTeamScore.matchScore.totalScore
    ),
    shot_diff = ifelse(team_type == "home",
      home_shots - away_shots,
      away_shots - home_shots
    ),
    team_shots = ifelse(team_type == "home",
      home_shots,
      away_shots
    ),
    harmean_shots = harmonic_mean(home_shots,away_shots)
    ,
    shot_conv = ifelse(team_type == "home",
      homeTeamScore.matchScore.goals / home_shots,
      awayTeamScore.matchScore.goals / away_shots
    ),
    shot_conv_diff = ifelse(team_type == "home",
                       (homeTeamScore.matchScore.goals / home_shots) -(awayTeamScore.matchScore.goals / away_shots),
                       (awayTeamScore.matchScore.goals / away_shots) - (homeTeamScore.matchScore.goals / home_shots)
    ),
    xscore_diff = ifelse(team_type == "home",
      xscore_diff,
      -xscore_diff
    ),
    team_xscore = ifelse(team_type == "home",
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
    team_type_fac = team_type_fac.x,
    total_score = homeTeamScore.matchScore.totalScore + awayTeamScore.matchScore.totalScore,
    total_shots = home_shots + away_shots,
    team_name.x = as.factor(team_name.x),
    team_name.y = as.factor(team_name.y),
    log_dist_diff = log_dist.x - log_dist.y,
    familiarity_diff = familiarity.x - familiarity.y,
    days_rest_diff = days_rest.x - days_rest.y,
    days_rest_diff_fac = as.factor(round(ifelse(days_rest_diff > 3, 4, ifelse(days_rest_diff < -3, -4, days_rest_diff)))),
    weightz = exp(as.numeric(-(Sys.Date() - as.Date(match.utcStartTime))) / decay),
    weightz = weightz / mean(weightz, na.rm = T),
    shot_weightz = (harmean_shots/mean(harmean_shots, na.rm = TRUE)) * weightz
  )
# left_join(team_preds %>% mutate(team_name_adj = fitzRoy::replace_teams(team_name)),
#   by = c("providerId" = "provider_id", "team_name_adj.x" = "team_name_adj")
# )

## Filter out early matches ----
team_mdl_df <-
  team_mdl_df_tot %>%
  filter(
    providerId > "CD_M202101409",
    # providerId <= glue::glue("CD_M{get_afl_season()}014{get_afl_week('next')+1}")
  )

## Adjust total_xpoints ----
team_mdl_df <-
  team_mdl_df %>%
  mutate(
    total_xpoints_adj = total_xpoints * (mean(total_points, na.rm = TRUE)/mean(total_xpoints, na.rm = TRUE)),
    venue_fac = as.factor(venue.x)
  )

#  Modelling ----
# library(mgcViz)
# set.seed("1234")

###
# afl_totshots_mdl <- mgcv::bam(
#   total_shots ~
#     s(team_type_fac, bs = "re") +
#   s(hoff_adef, bs='ts') + # = pmax(pmin((fwd.x - def.y), 20), -5),
#   s(hmid_amid, bs='ts') + # = pmax(pmin((mid.x - mid.y), 12), -12),
#   s(hdef_afwd, bs='ts') + # = pmax(pmin((def.x - fwd.y), 5), -20),
#   s(hint_aint, bs='ts') + # = pmax(pmin((int.x - int.y), 10), -10),
#   + s(def.x, bs='ts')
#   + s(mid.x, bs='ts')
#   + s(fwd.x, bs='ts')
#   + s(int.x, bs='ts')
#     + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
#   + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
#     + s(abs(torp_diff), bs = "ts", k = 5)
#
#   ,
#   data = team_mdl_df, weights = weightz,
#   family = quasipoisson(), nthreads = 4, select = T, discrete = T
#   , drop.unused.levels = FALSE
# )
# team_mdl_df$pred_totshots <- predict(afl_totshots_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_totshots_mdl)
# Deviance explained =   22%

###
# afl_shot_mdl <- mgcv::bam(
#   shot_diff ~
#     s(team_type_fac, bs = "re")
#   + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
#   + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
#     + ti(torp_diff, pred_totshots, bs = c("ts", "ts"), k = 4)
#     + s(pred_totshots, bs = "ts", k = 5)
#     + s(torp_diff, bs = "ts", k = 5),
#   data = team_mdl_df, weights = weightz,
#   family = gaussian(), nthreads = 4, select = T, discrete = T
#   , drop.unused.levels = FALSE
# )
# team_mdl_df$pred_shot_diff <- predict(afl_shot_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_shot_mdl)
# mixedup::extract_ranef(afl_shot_mdl) %>% tibble::view()
# plot(mgcViz::getViz(afl_shot_mdl))
# Deviance explained = 44.5%


## Total xPoints Model ----
afl_total_xpoints_mdl <- mgcv::bam(
  total_xpoints_adj ~
    s(team_type_fac.x, bs = "re")
    + s(game_year_decimal.x, bs = "ts")
    + s(game_prop_through_year.x, bs = "cc")
    + s(game_prop_through_month.x, bs = "cc")
    + s(game_wday_fac.x, bs = "re")
    + s(game_prop_through_day.x, bs = "cc")
    + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
    + s(abs(torp_diff), bs = "ts", k = 5)
    + s(abs(torp_recv_diff), bs = "ts", k = 5)
    + s(abs(torp_disp_diff), bs = "ts", k = 5)
    + s(abs(torp_spoil_diff), bs = "ts", k = 5)
    + s(abs(torp_hitout_diff), bs = "ts", k = 5)
    + s(torp.x, bs = "ts", k = 5) + s(torp.y, bs = "ts", k = 5)
    # + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
    # + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
   + s(venue_fac, bs='re')
    + s(log_dist.x, bs = "ts", k = 5) + s(log_dist.y, bs = "ts", k = 5)
    + s(familiarity.x, bs = "ts", k = 5) + s(familiarity.y, bs = "ts", k = 5)
    + s(log_dist_diff, bs = "ts", k = 5)
    + s(familiarity_diff, bs = "ts", k = 5)
    + s(days_rest_diff_fac, bs = "re")
  ,
  data = team_mdl_df, weights = weightz,
  family = gaussian(), nthreads = 4, select = T, discrete = T,
  drop.unused.levels = FALSE
)

team_mdl_df$pred_tot_xscore <- predict(afl_total_xpoints_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_total_xpoints_mdl)
# mixedup::extract_ranef(afl_total_xpoints_mdl, add_group_N = T) %>% tibble::view()
# plot(mgcViz::getViz(afl_total_xpoints_mdl))
# Deviance explained =   16%

## xScore Diff Model ----
afl_xscore_diff_mdl <- mgcv::bam(
  xscore_diff ~
    s(team_type_fac, bs = "re")
    + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
    + ti(torp_diff, pred_tot_xscore, bs = c("ts", "ts"), k = 4)
    + s(pred_tot_xscore, bs = "ts", k = 5)
    + s(torp_diff, bs = "ts", k = 5)
  + s(torp_recv_diff, bs = "ts", k = 5)
  + s(torp_disp_diff, bs = "ts", k = 5)
  + s(torp_spoil_diff, bs = "ts", k = 5)
  + s(torp_hitout_diff, bs = "ts", k = 5)
    # + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
    # + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
    + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re")
  ,
  data = team_mdl_df, weights = weightz,
  family = gaussian(), nthreads = 4, select = T, discrete = T,
  drop.unused.levels = FALSE
)
team_mdl_df$pred_xscore_diff <- predict(afl_xscore_diff_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_xscore_diff_mdl)
# mixedup::extract_ranef(afl_xscore_diff_mdl, add_group_N = T) %>% tibble::view()
# plot(mgcViz::getViz(afl_xscore_diff_mdl))
# Deviance explained = 44.5%


## Conversion Model ----
afl_conv_mdl <- mgcv::bam(
  shot_conv_diff ~
    s(team_type_fac.x, bs = "re")
    + s(game_year_decimal.x, bs = "ts")
    + s(game_prop_through_year.x, bs = "cc")
    + s(game_prop_through_month.x, bs = "cc")
    + s(game_wday_fac.x, bs = "re")
    + s(game_prop_through_day.x, bs = "cc")
    + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
    + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
    + ti(torp_diff, pred_tot_xscore, bs = c("ts", "ts"), k = 4)
    + s(torp_diff, bs = "ts", k = 5)
  + s(torp_recv_diff, bs = "ts", k = 5)
  + s(torp_disp_diff, bs = "ts", k = 5)
  + s(torp_spoil_diff, bs = "ts", k = 5)
  + s(torp_hitout_diff, bs = "ts", k = 5)
    + s(pred_tot_xscore, bs = "ts", k = 5)
    + s(pred_xscore_diff, bs = "ts", k = 5)
    # + s(fwd.x, bs = "ts", k = 5) + s(mid.x, bs = "ts", k = 5) + s(def.x, bs = "ts", k = 5) + s(int.x, bs = "ts", k = 5)
    # + s(fwd.y, bs = "ts", k = 5) + s(mid.y, bs = "ts", k = 5) + s(def.y, bs = "ts", k = 5) + s(int.y, bs = "ts", k = 5)
  + s(venue_fac, bs='re')
    + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re")
  ,
  data = team_mdl_df, weights = shot_weightz,
  family = gaussian(), nthreads = 4, select = T, discrete = T,
  drop.unused.levels = FALSE
)

team_mdl_df$pred_conv_diff <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_conv_mdl)
# mixedup::extract_ranef(afl_conv_mdl, add_group_N = T) %>% tibble::view()
# plot(mgcViz::getViz(afl_conv_mdl))
# Deviance explained = 4.4%

## Score Diff Model ----
afl_score_mdl <- mgcv::bam(
  score_diff ~
    s(team_type_fac, bs = "re")
  + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
  + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
  + ti(pred_xscore_diff, pred_conv_diff, bs = 'ts', k = 5)
  + ti(pred_tot_xscore, pred_conv_diff, bs = 'ts', k = 5)
  # + s(pred_tot_xscore, bs = "ts", k = 5)
  # + s(pred_conv_diff, bs = "ts", k = 5)
  + s(pred_xscore_diff)
  # + s(torp_diff, bs = "ts", k = 5)
  # + s(torp_recv_diff, bs = "ts", k = 5)
  # + s(torp_disp_diff, bs = "ts", k = 5)
  # + s(torp_spoil_diff, bs = "ts", k = 5)
  # + s(torp_hitout_diff, bs = "ts", k = 5)
  + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re")
  ,
  data = team_mdl_df, weights = weightz,
  family = "gaussian", nthreads = 4, select = T, discrete = T,
  drop.unused.levels = FALSE
)

team_mdl_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")

# summary(afl_score_mdl)
# mixedup::extract_ranef(afl_score_mdl, add_group_N = T) %>% tibble::view()
# plot(mgcViz::getViz(afl_score_mdl))
# Deviance explained = 40.4%

## Win Prob Model ----
afl_win_mdl <-
  mgcv::bam(
    win ~
      # s(team_type_fac, bs = "re")
      +s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
      + ti(pred_tot_xscore, pred_score_diff, bs = c("ts", "ts"), k = 4)
      # + ti(pred_tot_xscore, pred_xscore_diff, bs = c("ts", "ts"), k = 4)
      + s(pred_score_diff, bs = "ts", k = 5)
      #+ s(pred_xscore_diff, bs = "ts", k = 5)
      + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re")
    ,
    data = team_mdl_df, weights = weightz,
    family = "binomial", nthreads = 4, select = T, discrete = T,
    drop.unused.levels = FALSE
  )

# summary(afl_win_mdl)
# mixedup::extract_ranef(afl_win_mdl, add_group_N = T) %>% tibble::view()
# plot(mgcViz::getViz(afl_win_mdl))
# Deviance explained = 19.6%

###
team_mdl_df$pred_win <- predict(afl_win_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$bits <- ifelse(team_mdl_df$win == 1,
  1 + log2(team_mdl_df$pred_win),
  ifelse(team_mdl_df$win == 0,
    1 + log2(1 - team_mdl_df$pred_win),
    1 + 0.5 * log2(team_mdl_df$pred_win * (1 - team_mdl_df$pred_win))
  )
)
team_mdl_df$tips <- ifelse(round(team_mdl_df$pred_win) == team_mdl_df$win, 1,
  ifelse(team_mdl_df$win == 0.5, 1, 0)
)
team_mdl_df$mae <- abs(team_mdl_df$score_diff - team_mdl_df$pred_score_diff)

# Model Metrics ----
test_df <- team_mdl_df %>% dplyr::filter(!is.na(win), team_type == "home", season.x == get_afl_season())
# library(MLmetrics)
MLmetrics::LogLoss(test_df$pred_win, test_df$win)
MLmetrics::MAE(test_df$pred_score_diff, test_df$score_diff)
sum(test_df$bits)
sum(test_df$tips)
mean(test_df$bits)
mean(test_df$tips)
nrow(test_df)

#################################### GF
# team_mdl_df$team_type_fac <- as.factor(ifelse(team_mdl_df$providerId == "CD_M20220142701",
#   ifelse(team_mdl_df$team_type == "home", "home", "away"),
#   team_mdl_df$team_type
# ))

# team_mdl_df$pred_totshots <- predict(afl_totshots_mdl, newdata = team_mdl_df, type = "response")
# team_mdl_df$pred_shot_diff <- predict(afl_shot_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_conv_diff <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")
team_mdl_df$pred_win <- predict(afl_win_mdl, newdata = team_mdl_df, type = "response")

# This Weeks Predictions ----
n <- get_afl_week(type = "next")

week_gms_home <- team_mdl_df %>%
  dplyr::mutate(
    totscore = pred_tot_xscore
    # totscore = sum(team_mdl_df$total_score, na.rm = T) / sum(team_mdl_df$total_shots, na.rm = T) * pred_totshots
  ) %>%
  dplyr::filter(season.x == lubridate::year(Sys.Date()), round.roundNumber.x == n, team_type_fac.x == "home") %>%
  dplyr::select(
    players = count.x, providerId,
    home_team = team_name.x, home_rating = torp.x,
    away_team = team_name.y, away_rating = torp.y,
    pred_xtotal = pred_tot_xscore,
    pred_xmargin = pred_xscore_diff ,
    pred_margin = pred_score_diff,
    pred_win,
    bits,
    margin = score_diff
  )

week_gms_away <- team_mdl_df %>%
  dplyr::mutate(
    # totscore = sum(team_mdl_df$total_score, na.rm = T) / sum(team_mdl_df$total_shots, na.rm = T) * pred_totshots,
    # pred_shot_diff = -pred_shot_diff,
    pred_tot_xscore = pred_tot_xscore,
    pred_xscore_diff = -pred_xscore_diff,
    pred_score_diff = -pred_score_diff,
    pred_win = 1 - pred_win,
    score_diff = -score_diff
  ) %>%
  dplyr::filter(season.x == lubridate::year(Sys.Date()), round.roundNumber.x == n, team_type_fac.x == "away") %>%
  dplyr::select(
    players = count.x, providerId,
    home_team = team_name.y, home_rating = torp.y,
    away_team = team_name.x, away_rating = torp.x,
    pred_xtotal = pred_tot_xscore,
    pred_xmargin = pred_xscore_diff ,
    pred_margin = pred_score_diff,
    pred_win,
    bits,
    margin = score_diff
  )

week_gms <- dplyr::bind_rows(week_gms_home, week_gms_away) %>%
  dplyr::group_by(providerId, home_team, home_rating, away_team, away_rating) %>%
  dplyr::summarise(
    # pred_xmargin = mean(pred_xmargin),
    players = mean(players),
    # total = mean(totscore),
    pred_xtotal = mean(pred_xtotal),
    pred_margin = mean(pred_margin),
    pred_win = mean(pred_win),
    margin = mean(margin)
  ) %>%
  mutate(rating_diff = home_rating - away_rating + 4) %>%
  select(providerId:away_rating, rating_diff, players:margin)

inj_df %>%
  filter(str_starts(player, "Upd")) %>%
  unique()

## Final Prediction Table ----
week_gms

####
