# Match Predictions Pipeline ----
#
# Builds weekly AFL match predictions using TORP ratings + GAM models.
# Can be run locally or via CI (GitHub Actions).
#
# Usage:
#   source("data-raw/02-models/build_match_predictions.R")
#   run_predictions_pipeline()           # auto-detect next week
#   run_predictions_pipeline(week = 5)   # specific week

# Setup ----
library(tidyverse)
library(fitzRoy)
library(cli)

devtools::load_all()

# Helper: Scrape Injuries ----

scrape_injuries <- function(timeout = 30) {
  tryCatch({
    library(rvest)
    url <- "https://www.afl.com.au/matches/injury-list"
    session <- rvest::session(url, httr::timeout(timeout))
    session %>%
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
  }, error = function(e) {
    cli::cli_warn("Failed to scrape injury list: {conditionMessage(e)}")
    data.frame(player = character(), injury = character(), estimated_return = character())
  })
}

# Helper: Calculate Familiarity Proportions ----

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

# Main Pipeline ----

run_predictions_pipeline <- function(week = NULL) {

  season <- get_afl_season()
  if (is.null(week)) week <- get_afl_week(type = "next")

  cli::cli_h1("Match Predictions Pipeline")
  cli::cli_inform("Season: {season}, Week: {week}")
  tictoc::tic("predictions_total")

  # Load Tables ----
  cli::cli_h2("Loading data")

  all_grounds <- file_reader("stadium_data", "reference-data")
  xg_df <- load_xg(TRUE)
  fixtures <- load_fixtures(TRUE)
  results <- load_results(TRUE)
  teams <- load_teams(TRUE)
  torp_df_total <- load_torp_ratings()

  cli::cli_inform("Loaded: fixtures={nrow(fixtures)}, results={nrow(results)}, teams={nrow(teams)}, ratings={nrow(torp_df_total)}")

  # Sanity check loaded data
  if (nrow(fixtures) < 100) cli::cli_abort("Fixtures data too small ({nrow(fixtures)} rows) - data may be missing")
  if (nrow(torp_df_total) < 100) cli::cli_abort("Ratings data too small ({nrow(torp_df_total)} rows) - ratings may not have been computed")
  if (nrow(teams) < 100) cli::cli_abort("Teams data too small ({nrow(teams)} rows) - lineup data may be missing")

  # Build Fixtures Tables ----
  cli::cli_h2("Building fixture features")
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
    mutate(
      utc_dt = ymd_hms(utcStartTime, tz = "UTC")
    ) %>%
    rowwise() %>%
    mutate(
      local_start_time_str = format(
        with_tz(utc_dt, tzone = venue.timezone),
        "%Y-%m-%d %H:%M:%S %Z"
      )
    ) %>%
    ungroup()

  ## Add Date Variables ----
  fix_df <- fix_df %>%
    rowwise() %>%
    mutate(
      game_year = year(with_tz(utc_dt, venue.timezone)),
      game_month = month(with_tz(utc_dt, venue.timezone)),
      game_yday = yday(with_tz(utc_dt, venue.timezone)),
      game_mday = day(with_tz(utc_dt, venue.timezone)),
      game_wday = lubridate::wday(with_tz(utc_dt, venue.timezone), week_start = 1),
      game_wday_fac = as.factor(game_wday),
      game_hour = hour(with_tz(utc_dt, venue.timezone)) +
        minute(with_tz(utc_dt, venue.timezone)) / 60 +
        second(with_tz(utc_dt, venue.timezone)) / 3600,
      game_date_numeric = as.numeric(utc_dt),
      timezone = venue.timezone,
      game_prop_through_year = game_yday / ifelse(leap_year(game_year), 366, 365),
      game_prop_through_month = game_mday / days_in_month(game_month),
      game_prop_through_week = game_wday / 7,
      game_prop_through_day = game_hour / 24,
      game_year_decimal = as.numeric(game_year + game_prop_through_year)
    ) %>%
    ungroup()

  # Lineups ----
  cli::cli_h2("Processing lineups")

  team_lineup_df <-
    teams %>%
    dplyr::left_join(torp_df_total, by = c("player.playerId" = "player_id", "season" = "season", "round.roundNumber" = "round")) %>%
    dplyr::filter((position.x != "EMERG" & position.x != "SUB") | is.na(position.x))

  na_torp_count <- sum(is.na(team_lineup_df$torp))
  total_players <- nrow(team_lineup_df)
  if (na_torp_count > 0) {
    na_pct <- round(100 * na_torp_count / total_players, 1)
    cli::cli_inform("Replacing {na_torp_count}/{total_players} ({na_pct}%) NA torp ratings with 0")
    if (na_pct > 25) {
      cli::cli_warn("High proportion of missing ratings ({na_pct}%) - predictions may be less reliable")
    }
  }

  team_lineup_df <- team_lineup_df %>%
    dplyr::mutate(
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
      backs = ifelse(position.x == "BPL" | position.x == "BPR" | position.x == "FB", torp, NA),
      half_backs = ifelse(position.x == "HBFL" | position.x == "HBFR" | position.x == "CHB", torp, NA),
      midfielders = ifelse(position.x == "WL" | position.x == "WR" | position.x == "C", torp, NA),
      followers = ifelse(position.x == "R" | position.x == "RR" | position.x == "RK", torp, NA),
      half_forwards = ifelse(position.x == "HFFL" | position.x == "HFFR" | position.x == "CHF", torp, NA),
      forwards = ifelse(position.x == "FPL" | position.x == "FPR" | position.x == "FF", torp, NA),
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
      CB = ifelse(position.x == "CHB" | position.x == "FB", torp, NA),
      BP = ifelse(position.x == "BPL" | position.x == "BPR", torp, NA),
      HBF = ifelse(position.x == "HBFL" | position.x == "HBFR", torp, NA),
      W = ifelse(position.x == "WL" | position.x == "WR", torp, NA),
      MIDS = ifelse(position.x == "C" | position.x == "R" | position.x == "RR", torp, NA),
      HFF = ifelse(position.x == "HFFL" | position.x == "HFFR", torp, NA),
      FP = ifelse(position.x == "FPL" | position.x == "FPR", torp, NA),
      CF = ifelse(position.x == "FF" | position.x == "CHF", torp, NA),
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
      backs = sum(backs, na.rm = T),
      half_backs = sum(half_backs, na.rm = T),
      midfielders = sum(midfielders, na.rm = T),
      followers = sum(followers, na.rm = T),
      half_forwards = sum(half_forwards, na.rm = T),
      forwards = sum(forwards, na.rm = T),
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
      CB = sum(CB, na.rm = T),
      BP = sum(BP, na.rm = T),
      HBF = sum(HBF, na.rm = T),
      W = sum(W, na.rm = T),
      MIDS = sum(MIDS, na.rm = T),
      HFF = sum(HFF, na.rm = T),
      FP = sum(FP, na.rm = T),
      CF = sum(CF, na.rm = T),
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
  cli::cli_h2("Computing familiarity")

  unique_teams <- unique(team_rt_df$teamId)
  unique_seasons <- unique(team_rt_df$season)
  unique_rounds <- unique(team_rt_df$round.roundNumber)

  tictoc::tic("familiarity")

  library(purrr)

  grid <- tidyr::expand_grid(
    team   = unique_teams,
    season = unique_seasons,
    round  = unique_rounds
  )

  # Use parallel if available, fall back to sequential
  has_in_parallel <- exists("in_parallel", where = asNamespace("purrr"), mode = "function")

  if (has_in_parallel) {
    f <- purrr::in_parallel(
      \(team, season, round) {
        suppressPackageStartupMessages({
          library(magrittr)
          library(dplyr)
        })
        calculate_proportions(df, team, season, round)
      },
      df = team_rt_df,
      calculate_proportions = calculate_proportions
    )
    all_proportions <- purrr::pmap(grid, f, .progress = TRUE)
  } else {
    cli::cli_inform("purrr::in_parallel not available, using sequential pmap")
    all_proportions <- purrr::pmap(grid, \(team, season, round) {
      calculate_proportions(team_rt_df, team, season, round)
    }, .progress = TRUE)
  }

  ground_prop <- dplyr::bind_rows(all_proportions)

  tictoc::toc(log = TRUE)

  ## Distance Traveled ----
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
      log_dist = replace_na(log_dist, 16)
    ) %>%
    dplyr::left_join(ground_prop, by = c("teamId", "season", "round.roundNumber", "venue")) %>%
    dplyr::mutate(
      familiarity = replace_na(familiarity, 0)
    )

  na_dist <- sum(is.na(team_dist_df$distance))
  na_fam <- sum(team_dist_df$familiarity == 0)
  if (na_dist > 0) cli::cli_inform("Distance: {na_dist} rows defaulted to log_dist=16 (missing venue coords)")
  if (na_fam > 0) cli::cli_inform("Familiarity: {na_fam} rows defaulted to 0 (no prior games at venue)")

  ## Days Rest ----
  days_rest <- fix_df %>%
    arrange(teamId, utcStartTime) %>%
    group_by(teamId, season) %>%
    mutate(days_rest = as.numeric(difftime(utcStartTime, lag(utcStartTime), units = "days"))) %>%
    ungroup()

  na_rest <- sum(is.na(days_rest$days_rest))
  if (na_rest > 0) cli::cli_inform("Days rest: {na_rest} rows defaulted to 21 (first game of season)")
  days_rest <- days_rest %>%
    dplyr::mutate(days_rest = replace_na(days_rest, 21))

  ## Injuries ----
  cli::cli_h2("Scraping injuries")
  inj_df <- scrape_injuries()
  cli::cli_inform("Injuries loaded: {nrow(inj_df)} rows")
  if (nrow(inj_df) == 0 && week > 1) {
    cli::cli_warn("0 injuries scraped during active season (week {week}) - all players will be treated as available")
  }

  tr <- torp_ratings(season, week) %>%
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
    arrange(-torp_week)

  # Torp Ratings ----
  cli::cli_h2("Building model dataset")

  team_rt_fix_df <-
    fix_df %>%
    mutate(team_name = fitzRoy::replace_teams(team_name)) %>%
    left_join(team_dist_df, by = c("providerId", "teamId")) %>%
    left_join(days_rest, by = c("providerId", "teamId")) %>%
    left_join(team_rt_df, by = c("providerId", "teamId", "season", "round.roundNumber")) %>%
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
  team_mdl_df_tot <- team_rt_fix_df %>%
    dplyr::left_join(
      team_rt_fix_df %>%
        dplyr::mutate(type_anti = dplyr::if_else(team_type == "home", "away", "home")),
      by = c("providerId" = "providerId", "team_type" = "type_anti")
    ) %>%
    dplyr::mutate(
      torp_diff = torp.x - torp.y,
      torp_ratio = log(pmax(torp.x, 0.01) / pmax(torp.y, 0.01)),
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
      harmean_shots = harmonic_mean(home_shots, away_shots),
      shot_conv = ifelse(team_type == "home",
        homeTeamScore.matchScore.goals / pmax(home_shots, 1),
        awayTeamScore.matchScore.goals / pmax(away_shots, 1)
      ),
      shot_conv_diff = ifelse(team_type == "home",
        (homeTeamScore.matchScore.goals / pmax(home_shots, 1)) - (awayTeamScore.matchScore.goals / pmax(away_shots, 1)),
        (awayTeamScore.matchScore.goals / pmax(away_shots, 1)) - (homeTeamScore.matchScore.goals / pmax(home_shots, 1))
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
      shot_weightz = (harmean_shots / mean(harmean_shots, na.rm = TRUE)) * weightz
    )

  ## Filter out early matches ----
  # CD_M202101409 = first match of 2021 R14 (earliest with reliable TORP + xG data)
  team_mdl_df <-
    team_mdl_df_tot %>%
    filter(providerId > "CD_M202101409")

  ## Adjust total_xpoints ----
  team_mdl_df <-
    team_mdl_df %>%
    mutate(
      total_xpoints_adj = total_xpoints * (mean(total_points, na.rm = TRUE) / mean(total_xpoints, na.rm = TRUE)),
      venue_fac = as.factor(venue.x)
    )

  # Modelling ----
  cli::cli_h2("Training GAM models")

  ## Total xPoints Model ----
  cli::cli_progress_step("Training total xPoints model")
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
      + s(venue_fac, bs = "re")
      + s(log_dist.x, bs = "ts", k = 5) + s(log_dist.y, bs = "ts", k = 5)
      + s(familiarity.x, bs = "ts", k = 5) + s(familiarity.y, bs = "ts", k = 5)
      + s(log_dist_diff, bs = "ts", k = 5)
      + s(familiarity_diff, bs = "ts", k = 5)
      + s(days_rest_diff_fac, bs = "re"),
    data = team_mdl_df, weights = weightz,
    family = gaussian(), nthreads = 4, select = T, discrete = T,
    drop.unused.levels = FALSE
  )
  team_mdl_df$pred_tot_xscore <- predict(afl_total_xpoints_mdl, newdata = team_mdl_df, type = "response")

  ## xScore Diff Model ----
  cli::cli_progress_step("Training xScore diff model")
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
      + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re"),
    data = team_mdl_df, weights = weightz,
    family = gaussian(), nthreads = 4, select = T, discrete = T,
    drop.unused.levels = FALSE
  )
  team_mdl_df$pred_xscore_diff <- predict(afl_xscore_diff_mdl, newdata = team_mdl_df, type = "response")

  ## Conversion Model ----
  cli::cli_progress_step("Training conversion model")
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
      + s(venue_fac, bs = "re")
      + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re"),
    data = team_mdl_df, weights = shot_weightz,
    family = gaussian(), nthreads = 4, select = T, discrete = T,
    drop.unused.levels = FALSE
  )
  team_mdl_df$pred_conv_diff <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")

  ## Score Diff Model ----
  cli::cli_progress_step("Training score diff model")
  afl_score_mdl <- mgcv::bam(
    score_diff ~
      s(team_type_fac, bs = "re")
      + s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
      + ti(pred_xscore_diff, pred_conv_diff, bs = "ts", k = 5)
      + ti(pred_tot_xscore, pred_conv_diff, bs = "ts", k = 5)
      + s(pred_xscore_diff)
      + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re"),
    data = team_mdl_df, weights = weightz,
    family = "gaussian", nthreads = 4, select = T, discrete = T,
    drop.unused.levels = FALSE
  )
  team_mdl_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")

  ## Win Prob Model ----
  cli::cli_progress_step("Training win probability model")
  afl_win_mdl <-
    mgcv::bam(
      win ~
        +s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
        + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
        + ti(pred_tot_xscore, pred_score_diff, bs = c("ts", "ts"), k = 4)
        + s(pred_score_diff, bs = "ts", k = 5)
        + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5) + s(days_rest_diff_fac, bs = "re"),
      data = team_mdl_df, weights = weightz,
      family = "binomial", nthreads = 4, select = T, discrete = T,
      drop.unused.levels = FALSE
    )

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

  # This Week's Predictions ----
  cli::cli_h2("Generating week {week} predictions")

  n <- week

  week_gms_home <- team_mdl_df %>%
    dplyr::mutate(
      totscore = pred_tot_xscore
    ) %>%
    dplyr::filter(season.x == lubridate::year(Sys.Date()), round.roundNumber.x == n, team_type_fac.x == "home") %>%
    dplyr::select(
      players = count.x, providerId,
      home_team = team_name.x, home_rating = torp.x,
      away_team = team_name.y, away_rating = torp.y,
      pred_xtotal = pred_tot_xscore,
      pred_xmargin = pred_xscore_diff,
      pred_margin = pred_score_diff,
      pred_win,
      bits,
      margin = score_diff
    )

  week_gms_away <- team_mdl_df %>%
    dplyr::mutate(
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
      pred_xmargin = pred_xscore_diff,
      pred_margin = pred_score_diff,
      pred_win,
      bits,
      margin = score_diff
    )

  week_gms <- dplyr::bind_rows(week_gms_home, week_gms_away) %>%
    dplyr::group_by(providerId, home_team, home_rating, away_team, away_rating) %>%
    dplyr::summarise(
      players = mean(players),
      pred_xtotal = mean(pred_xtotal),
      pred_margin = mean(pred_margin),
      pred_win = mean(pred_win),
      margin = mean(margin)
    ) %>%
    mutate(rating_diff = home_rating - away_rating + 4) %>%
    select(providerId:away_rating, rating_diff, players:margin)

  # Validate Predictions ----
  cli::cli_h2("Validating predictions")

  if (nrow(week_gms) == 0) {
    cli::cli_abort("No predictions generated for week {week} - aborting upload")
  }
  if (any(is.na(week_gms$pred_win))) {
    cli::cli_abort("NA values in pred_win - model may have failed")
  }
  if (any(week_gms$pred_win < 0 | week_gms$pred_win > 1)) {
    cli::cli_abort("pred_win values out of [0,1] range - model output invalid")
  }
  if (any(is.na(week_gms$pred_margin))) {
    cli::cli_abort("NA values in pred_margin - model may have failed")
  }
  cli::cli_alert_success("Validation passed: {nrow(week_gms)} matches, all predictions valid")

  # Upload Predictions ----
  cli::cli_h2("Uploading predictions")

  pred_file_name <- paste0("predictions_", season, "_", sprintf("%02d", week))
  save_to_release(week_gms, pred_file_name, "predictions")

  # Verify upload
  uploaded <- tryCatch(
    file_reader(pred_file_name, "predictions"),
    error = function(e) NULL
  )
  if (is.null(uploaded) || nrow(uploaded) != nrow(week_gms)) {
    cli::cli_warn("Upload verification failed - file may not be accessible yet (piggyback cache delay)")
  } else {
    cli::cli_alert_success("Upload verified: {nrow(uploaded)} rows match")
  }
  cli::cli_alert_success("Uploaded week {week} predictions ({nrow(week_gms)} matches)")

  tictoc::toc(log = TRUE)

  cli::cli_h2("Pipeline Complete")
  timings <- tictoc::tic.log(format = TRUE)
  for (t in timings) {
    cli::cli_inform(t)
  }
  tictoc::tic.clearlog()

  invisible(week_gms)
}

# Execute if run as script ----

if (sys.nframe() == 0) {
  run_predictions_pipeline()
}
