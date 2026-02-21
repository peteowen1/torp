# Match Predictions Pipeline ----
#
# Builds weekly AFL match predictions using TORP ratings + GAM models.
# Can be run locally or via CI (GitHub Actions).
#
# Usage:
#   source("data-raw/02-models/build_match_predictions.R")
#   run_predictions_pipeline()              # auto-detect next week
#   run_predictions_pipeline(week = 5)      # specific week
#   run_predictions_pipeline(weeks = "all") # all fixture weeks
#   run_predictions_pipeline(weeks = 1:5)   # specific weeks

# Setup ----
library(tidyverse)
library(fitzRoy)
library(cli)
library(purrr)

devtools::load_all()

# Constants ----
WEIGHT_DECAY_DAYS <- 1000
INJURY_DISCOUNT   <- 0.95
HOME_RATING_BOOST <- 4
LOG_DIST_OFFSET   <- 10000
LOG_DIST_DEFAULT  <- 16
MIN_DATA_SEASON   <- 2021
MIN_DATA_ROUND    <- 14

# Position Lookup Tables ----
# Maps field position (position.x) → phase/group/individual/combo columns
PHASE_MAP <- list(
  def = c("BPL", "BPR", "FB", "CHB", "HBFL", "HBFR"),
  mid = c("C", "WL", "WR", "R", "RR", "RK"),
  fwd = c("FPL", "FPR", "FF", "CHF", "HFFL", "HFFR"),
  int = c("INT", "SUB")
)

POS_GROUP_MAP <- list(
  backs        = c("BPL", "BPR", "FB"),
  half_backs   = c("HBFL", "HBFR", "CHB"),
  midfielders  = c("WL", "WR", "C"),
  followers    = c("R", "RR", "RK"),
  half_forwards = c("HFFL", "HFFR", "CHF"),
  forwards     = c("FPL", "FPR", "FF")
)

INDIVIDUAL_POS <- c(
  "BPL", "BPR", "FB", "HBFL", "HBFR", "CHB",
  "WL", "WR", "C", "R", "RR", "RK",
  "HFFL", "HFFR", "CHF", "FPL", "FPR", "FF"
)

COMBO_POS_MAP <- list(
  CB   = c("CHB", "FB"),
  BP   = c("BPL", "BPR"),
  HBF  = c("HBFL", "HBFR"),
  W    = c("WL", "WR"),
  MIDS = c("C", "R", "RR"),
  HFF  = c("HFFL", "HFFR"),
  FP   = c("FPL", "FPR"),
  CF   = c("FF", "CHF")
)

# Maps listed position (position.y) → column name
LISTED_POS_MAP <- list(
  key_def  = "KEY_DEFENDER",
  med_def  = "MEDIUM_DEFENDER",
  midfield = "MIDFIELDER",
  mid_fwd  = "MIDFIELDER_FORWARD",
  med_fwd  = "MEDIUM_FORWARD",
  key_fwd  = "KEY_FORWARD",
  rucks    = "RUCK"
)

# All position columns for aggregation
POS_COLS <- c(
  names(PHASE_MAP), names(POS_GROUP_MAP), INDIVIDUAL_POS,
  names(COMBO_POS_MAP), names(LISTED_POS_MAP), "other_pos"
)

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

# Main Pipeline ----

run_predictions_pipeline <- function(week = NULL, weeks = NULL) {

  season <- get_afl_season()

  # Validate args
  if (!is.null(week) && !is.null(weeks)) {
    cli::cli_abort("Specify either {.arg week} or {.arg weeks}, not both")
  }

  # Auto-detect single week if neither provided
  if (is.null(week) && is.null(weeks)) week <- get_afl_week(type = "next")

  cli::cli_h1("Match Predictions Pipeline")
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

  if (nrow(fixtures) < 100) cli::cli_abort("Fixtures too small ({nrow(fixtures)} rows)")
  if (nrow(torp_df_total) < 100) cli::cli_abort("Ratings too small ({nrow(torp_df_total)} rows)")
  if (nrow(teams) < 100) cli::cli_abort("Teams too small ({nrow(teams)} rows)")

  # Resolve target weeks (after fixtures loaded for "all" support)
  if (!is.null(weeks)) {
    if (identical(weeks, "all")) {
      target_weeks <- sort(unique(fixtures$round.roundNumber[fixtures$compSeason.year == season]))
    } else {
      target_weeks <- weeks
    }
  } else {
    target_weeks <- week
  }
  cli::cli_inform("Season: {season}, Week{?s}: {paste(target_weeks, collapse = ', ')}")

  # Anchor date for time-decay weights (deterministic, not Sys.Date())
  target_fixtures <- fixtures %>%
    filter(compSeason.year == season, round.roundNumber %in% target_weeks)
  weight_anchor_date <- if (nrow(target_fixtures) > 0) {
    as.Date(min(target_fixtures$utcStartTime))
  } else {
    Sys.Date()
  }
  cli::cli_inform("Weight anchor date: {weight_anchor_date}")

  # Build Fixtures Tables ----
  cli::cli_h2("Building fixture features")

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

  ## Add Date Variables ----
  # Vectorized timezone conversion: group by timezone so with_tz() processes
  # each group as a single vectorized call (~5 groups vs ~6K per-row calls)
  fix_df <- fix_df %>%
    mutate(utc_dt = ymd_hms(utcStartTime, tz = "UTC")) %>%
    group_by(venue.timezone) %>%
    mutate(
      local_dt = with_tz(utc_dt, tzone = first(venue.timezone)),
      local_start_time_str = format(local_dt, "%Y-%m-%d %H:%M:%S %Z"),
      game_year = year(local_dt),
      game_month = month(local_dt),
      game_yday = yday(local_dt),
      game_mday = day(local_dt),
      game_wday = lubridate::wday(local_dt, week_start = 1),
      game_wday_fac = as.factor(game_wday),
      game_hour = hour(local_dt) + minute(local_dt) / 60 + second(local_dt) / 3600,
      game_date_numeric = as.numeric(utc_dt),
      timezone = venue.timezone,
      game_prop_through_year = game_yday / ifelse(leap_year(game_year), 366, 365),
      game_prop_through_month = game_mday / days_in_month(game_month),
      game_prop_through_week = game_wday / 7,
      game_prop_through_day = game_hour / 24,
      game_year_decimal = as.numeric(game_year + game_prop_through_year)
    ) %>%
    ungroup() %>%
    select(-local_dt)

  # Lineups ----
  cli::cli_h2("Processing lineups")

  team_lineup_df <-
    teams %>%
    dplyr::left_join(torp_df_total, by = c("player.playerId" = "player_id", "season" = "season", "round.roundNumber" = "round")) %>%
    dplyr::filter((position.x != "EMERG" & position.x != "SUB") | is.na(position.x))

  na_torp_count <- sum(is.na(team_lineup_df$torp))
  if (na_torp_count > 0) {
    na_pct <- round(100 * na_torp_count / nrow(team_lineup_df), 1)
    cli::cli_inform("Replacing {na_torp_count} ({na_pct}%) NA torp ratings with 0")
    if (na_pct > 25) cli::cli_warn("High proportion of missing ratings ({na_pct}%)")
  }

  team_lineup_df <- team_lineup_df %>%
    dplyr::mutate(
      torp = tidyr::replace_na(torp, 0),
      torp_recv = tidyr::replace_na(torp_recv, 0),
      torp_disp = tidyr::replace_na(torp_disp, 0),
      torp_spoil = tidyr::replace_na(torp_spoil, 0),
      torp_hitout = tidyr::replace_na(torp_hitout, 0)
    )

  # Generate position columns from lookup tables (replaces 52 ifelse calls)
  for (col in names(PHASE_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$position.x %in% PHASE_MAP[[col]], team_lineup_df$torp, NA)
  for (col in names(POS_GROUP_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$position.x %in% POS_GROUP_MAP[[col]], team_lineup_df$torp, NA)
  for (pos in INDIVIDUAL_POS)
    team_lineup_df[[pos]] <- ifelse(team_lineup_df$position.x == pos, team_lineup_df$torp, NA)
  for (col in names(COMBO_POS_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$position.x %in% COMBO_POS_MAP[[col]], team_lineup_df$torp, NA)
  for (col in names(LISTED_POS_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$position.y == LISTED_POS_MAP[[col]], team_lineup_df$torp, NA)
  team_lineup_df$other_pos <- ifelse(is.na(team_lineup_df$position.y), team_lineup_df$torp, NA)

  ## Aggregate Lineups ----
  torp_sum_cols <- c("torp", "torp_recv", "torp_disp", "torp_spoil", "torp_hitout")

  team_rt_df <- team_lineup_df %>%
    filter(!is.na(player.playerId)) %>%
    mutate(team_name_adj = fitzRoy::replace_teams(teamName)) %>%
    dplyr::group_by(providerId, teamId, season, round.roundNumber, teamType) %>%
    dplyr::summarise(
      venue = replace_venues(max(venue.name)),
      team_name_adj = max(team_name_adj),
      across(all_of(c(torp_sum_cols, POS_COLS)), ~sum(.x, na.rm = TRUE)),
      count = dplyr::n(),
      .groups = "drop"
    )

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

  tictoc::tic("familiarity")

  # Vectorized familiarity: cumulative venue proportion per team over time
  # For each (team, season, round), familiarity at a venue = proportion of
  # all prior games played at that venue. Computed via cumulative counts.
  ground_prop <- team_rt_df %>%
    arrange(teamId, season, round.roundNumber) %>%
    group_by(teamId) %>%
    mutate(cum_total_games = row_number() - 1) %>%
    group_by(teamId, venue) %>%
    mutate(cum_venue_games = row_number() - 1) %>%
    ungroup() %>%
    mutate(
      familiarity = ifelse(cum_total_games > 0, cum_venue_games / cum_total_games, 0)
    ) %>%
    select(teamId, season, round.roundNumber, venue, familiarity)

  tictoc::toc(log = TRUE)

  ## Distance Traveled ----
  team_dist_df <-
    fix_df %>%
    dplyr::mutate(
      venue = ifelse(venue == "Adelaide Arena at Jiangwan Stadium", "Jiangwan Stadium", venue)
    ) %>%
    dplyr::left_join(all_grounds %>% dplyr::select(venue, venue_lat = Latitude, venue_lon = Longitude), by = "venue") %>%
    dplyr::left_join(home_ground %>% dplyr::select(teamId, team_lat = Latitude, team_lon = Longitude), by = "teamId") %>%
    dplyr::mutate(distance = purrr::pmap_dbl(
      list(venue_lon, venue_lat, team_lon, team_lat),
      ~ geosphere::distHaversine(c(..1, ..2), c(..3, ..4))
    )) %>%
    dplyr::mutate(
      log_dist = log(distance + LOG_DIST_OFFSET),
      log_dist = replace_na(log_dist, LOG_DIST_DEFAULT)
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
  if (nrow(inj_df) == 0 && min(target_weeks) > 1) {
    cli::cli_warn("0 injuries scraped during active season - all players will be treated as available")
  }

  tr <- torp_ratings(season, min(target_weeks))
  if (nrow(tr) == 0 || !"player_name" %in% names(tr)) {
    cli::cli_alert_info("No TORP ratings available for {season} R{min(target_weeks)} (pre-season or fixtures not ready) - skipping predictions")
    tictoc::toc(log = TRUE)
    return(invisible(NULL))
  }
  tr <- tr %>%
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
      torp_week = sum(pmax(torp, 0), na.rm = TRUE) * INJURY_DISCOUNT,
      torp_recv_week = sum(pmax(torp_recv, 0), na.rm = TRUE) * INJURY_DISCOUNT,
      torp_disp_week = sum(pmax(torp_disp, 0), na.rm = TRUE) * INJURY_DISCOUNT,
      torp_spoil_week = sum(pmax(torp_spoil, 0), na.rm = TRUE) * INJURY_DISCOUNT,
      torp_hitout_week = sum(pmax(torp_hitout, 0), na.rm = TRUE) * INJURY_DISCOUNT
    ) %>%
    arrange(-torp_week)

  # Expand estimated ratings across all target weeks (ratings identical for unplayed rounds)
  if (length(target_weeks) > 1) {
    tr_week <- purrr::map_dfr(target_weeks, function(w) {
      tr_week %>% mutate(round = w)
    })
  }

  # Torp Ratings ----
  cli::cli_h2("Building model dataset")

  team_rt_fix_df <-
    fix_df %>%
    mutate(team_name = fitzRoy::replace_teams(team_name)) %>%
    left_join(
      team_dist_df %>% select(providerId, teamId, log_dist, familiarity),
      by = c("providerId", "teamId")
    ) %>%
    left_join(
      days_rest %>% select(providerId, teamId, days_rest),
      by = c("providerId", "teamId")
    ) %>%
    left_join(
      team_rt_df %>% select(providerId, teamId, season, round.roundNumber,
                             all_of(c(torp_sum_cols, POS_COLS, "count"))),
      by = c("providerId", "teamId", "season", "round.roundNumber")
    ) %>%
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
  # Select only needed opponent columns to reduce .x/.y column explosion
  opp_cols <- c(
    "providerId", "team_type",
    "torp", "torp_recv", "torp_disp", "torp_spoil", "torp_hitout",
    "def", "mid", "fwd", "int", INDIVIDUAL_POS,
    "team_name", "team_name_season",
    "log_dist", "familiarity", "days_rest",
    # Must overlap with left side to trigger .x suffixing on columns used downstream
    "team_type_fac", "season", "round.roundNumber", "venue", "count",
    "game_year_decimal", "game_prop_through_year", "game_prop_through_month",
    "game_wday_fac", "game_prop_through_day"
  )

  team_mdl_df_tot <- team_rt_fix_df %>%
    dplyr::left_join(
      team_rt_fix_df %>%
        dplyr::select(all_of(opp_cols)) %>%
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
      weightz = exp(as.numeric(-(weight_anchor_date - as.Date(match.utcStartTime))) / WEIGHT_DECAY_DAYS),
      weightz = weightz / mean(weightz, na.rm = TRUE),
      shot_weightz = (harmean_shots / mean(harmean_shots, na.rm = TRUE)) * weightz
    )

  ## Filter out early matches ----
  # Earliest with reliable TORP + xG data is 2021 R14
  team_mdl_df <-
    team_mdl_df_tot %>%
    filter(
      season.x > MIN_DATA_SEASON |
        (season.x == MIN_DATA_SEASON & round.roundNumber.x >= MIN_DATA_ROUND)
    )

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
    family = gaussian(), nthreads = 4, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  # In-sample: predictions on training data feed into downstream models
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
    family = gaussian(), nthreads = 4, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  # In-sample: uses pred_tot_xscore from above
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
    family = gaussian(), nthreads = 4, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  # In-sample: uses pred_tot_xscore + pred_xscore_diff from above
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
    family = "gaussian", nthreads = 4, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  # In-sample: uses pred_xscore_diff + pred_conv_diff + pred_tot_xscore from above
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
      family = "binomial", nthreads = 4, select = TRUE, discrete = TRUE,
      drop.unused.levels = FALSE
    )

  team_mdl_df$pred_win <- predict(afl_win_mdl, newdata = team_mdl_df, type = "response")
  team_mdl_df$bits <- dplyr::case_when(
    team_mdl_df$win == 1   ~ 1 + log2(team_mdl_df$pred_win),
    team_mdl_df$win == 0   ~ 1 + log2(1 - team_mdl_df$pred_win),
    TRUE                   ~ 1 + 0.5 * log2(team_mdl_df$pred_win * (1 - team_mdl_df$pred_win))
  )
  team_mdl_df$tips <- dplyr::case_when(
    round(team_mdl_df$pred_win) == team_mdl_df$win ~ 1,
    team_mdl_df$win == 0.5                         ~ 1,
    TRUE                                           ~ 0
  )
  team_mdl_df$mae <- abs(team_mdl_df$score_diff - team_mdl_df$pred_score_diff)

  # NOTE: bits/tips/mae are in-sample metrics — each GAM's predictions feed into
  # the next model, and all models predict on their own training data. These metrics
  # are useful for sanity-checking but should NOT be used for model comparison or
  # reported as predictive accuracy. Use held-out evaluation for that.
  cli::cli_alert_warning("bits/tips/mae are in-sample metrics (models predict on training data) — do not use for model comparison")

  # This Week's Predictions ----
  cli::cli_h2("Generating predictions for {length(target_weeks)} week{?s}")

  week_gms_home <- team_mdl_df %>%
    dplyr::filter(season.x == lubridate::year(Sys.Date()), round.roundNumber.x %in% target_weeks, team_type_fac.x == "home") %>%
    dplyr::select(
      round = round.roundNumber.x,
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
      pred_xscore_diff = -pred_xscore_diff,
      pred_score_diff = -pred_score_diff,
      pred_win = 1 - pred_win,
      score_diff = -score_diff
    ) %>%
    dplyr::filter(season.x == lubridate::year(Sys.Date()), round.roundNumber.x %in% target_weeks, team_type_fac.x == "away") %>%
    dplyr::select(
      round = round.roundNumber.x,
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
    dplyr::group_by(round, providerId, home_team, home_rating, away_team, away_rating) %>%
    dplyr::summarise(
      players = mean(players),
      pred_xtotal = mean(pred_xtotal),
      pred_margin = mean(pred_margin),
      pred_win = mean(pred_win),
      margin = mean(margin),
      .groups = "drop"
    ) %>%
    mutate(rating_diff = home_rating - away_rating + HOME_RATING_BOOST) %>%
    select(round, providerId:away_rating, rating_diff, players:margin)

  # Validate Predictions ----
  cli::cli_h2("Validating predictions")

  if (nrow(week_gms) == 0) cli::cli_abort("No predictions generated for week{?s} {paste(target_weeks, collapse = ', ')}")
  if (any(is.na(week_gms$pred_win))) cli::cli_abort("NA values in pred_win")
  if (any(week_gms$pred_win < 0 | week_gms$pred_win > 1)) cli::cli_abort("pred_win values out of [0,1] range")
  if (any(is.na(week_gms$pred_margin))) cli::cli_abort("NA values in pred_margin")

  cli::cli_alert_success("Validation passed: {nrow(week_gms)} matches")

  # Upload Predictions ----
  cli::cli_h2("Uploading predictions")

  week_gms <- week_gms %>% rename(week = round) %>% relocate(week)

  pred_file_name <- paste0("predictions_", season)
  existing <- tryCatch(file_reader(pred_file_name, "predictions"), error = function(e) NULL)

  if (!is.null(existing) && nrow(existing) > 0) {
    combined <- existing %>% filter(!week %in% target_weeks) %>% bind_rows(week_gms) %>% arrange(week)
  } else {
    combined <- week_gms
  }

  save_to_release(combined, pred_file_name, "predictions")
  cli::cli_alert_success("Uploaded {season} predictions ({nrow(combined)} rows, week{?s} {paste(target_weeks, collapse = ', ')} added)")

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
