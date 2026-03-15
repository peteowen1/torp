# Match Model Shared Functions
# ============================
# Shared data preparation and GAM training logic used by both:
#   - data-raw/02-models/build_match_predictions.R  (weekly CI predictions)
#   - torpmodels/data-raw/04-match-model/train_match_models.R (model evaluation)
#
# All functions are internal (not exported). The convenience wrapper
# build_team_mdl_df() chains them end-to-end.

# .build_fixtures_df ----

#' Build fixture dataframe with temporal features
#'
#' Pivots home/away, adds date/time features with timezone-safe conversion.
#'
#' @param fixtures Raw fixtures from load_fixtures()
#' @return Pivoted fixture dataframe with temporal columns
#' @keywords internal
.build_fixtures_df <- function(fixtures) {
  team_map <- fixtures |>
    dplyr::group_by(team_id = home_team_id) |>
    dplyr::summarise(team_name = get_mode(home_team_name)) |>
    dplyr::mutate(team_name = torp_replace_teams(team_name))

  fix_df <- fixtures |>
    dplyr::mutate(result = home_score - away_score) |>
    dplyr::select(
      match_id, season, round_number,
      home_team_id, away_team_id,
      utc_start_time, venue_name, venue_timezone, result
    ) |>
    tidyr::pivot_longer(
      cols = c("home_team_id", "away_team_id"),
      names_to = "team_type",
      values_to = "team_id"
    ) |>
    dplyr::mutate(
      venue = torp_replace_venues(venue_name),
      team_type = substr(team_type, 1, 4),
      result = ifelse(team_type == "away", -result, result)
    ) |>
    dplyr::select(
      match_id, season, round_number,
      team_type, team_id,
      utc_start_time, venue, venue_timezone, result
    ) |>
    dplyr::left_join(team_map, by = "team_id") |>
    dplyr::mutate(team_name_season = as.factor(paste(team_name, season)))

  # Vectorized timezone conversion: group by timezone
  fix_df <- fix_df |>
    dplyr::mutate(utc_dt = lubridate::ymd_hms(utc_start_time, tz = "UTC")) |>
    dplyr::group_by(venue_timezone) |>
    dplyr::mutate(
      local_dt = lubridate::with_tz(utc_dt, tzone = dplyr::first(venue_timezone)),
      local_start_time_str = format(local_dt, "%Y-%m-%d %H:%M:%S %Z"),
      game_year = lubridate::year(local_dt),
      game_month = lubridate::month(local_dt),
      game_yday = lubridate::yday(local_dt),
      game_mday = lubridate::day(local_dt),
      game_wday = lubridate::wday(local_dt, week_start = 1),
      game_wday_fac = as.factor(game_wday),
      game_hour = lubridate::hour(local_dt) + lubridate::minute(local_dt) / 60 +
        lubridate::second(local_dt) / 3600,
      game_date_numeric = as.numeric(utc_dt),
      timezone = venue_timezone,
      game_prop_through_year = game_yday / ifelse(lubridate::leap_year(game_year), 366, 365),
      game_prop_through_month = game_mday / lubridate::days_in_month(game_month),
      game_prop_through_week = game_wday / 7,
      game_prop_through_day = game_hour / 24,
      game_year_decimal = as.numeric(game_year + game_prop_through_year)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-local_dt)

  fix_df
}


# .build_team_ratings_df ----

#' Build team-level ratings from lineups
#'
#' Joins lineups to TORP ratings, imputes missing with priors, applies
#' TOG weighting, generates all position columns, and aggregates to team level.
#'
#' @param teams Raw teams/lineups from load_teams()
#' @param torp_df TORP ratings from load_torp_ratings()
#' @return Team-level aggregated ratings with position columns
#' @keywords internal
.build_team_ratings_df <- function(teams, torp_df, psr_df = NULL) {
  torp_prior_total <- RATING_PRIOR_RATE_RECV + RATING_PRIOR_RATE_DISP +
    RATING_PRIOR_RATE_SPOIL + RATING_PRIOR_RATE_HITOUT

  team_lineup_df <- teams |>
    dplyr::left_join(
      torp_df,
      by = c("player_id" = "player_id", "season" = "season", "round_number" = "round")
    ) |>
    dplyr::filter((position.x != "EMERG" & position.x != "SUB") | is.na(position.x))

  # Impute missing ratings with per-component priors
  na_torp_count <- sum(is.na(team_lineup_df$torp))
  if (na_torp_count > 0) {
    na_pct <- round(100 * na_torp_count / nrow(team_lineup_df), 1)
    cli::cli_inform("Replacing {na_torp_count} ({na_pct}%) NA torp ratings with prior ({round(torp_prior_total, 2)})")
    if (na_pct > 25) cli::cli_warn("High proportion of missing ratings ({na_pct}%)")
    if (na_pct > 50) cli::cli_abort("More than 50% of ratings are missing ({na_pct}%) -- check data pipeline")
  }

  team_lineup_df <- team_lineup_df |>
    dplyr::mutate(
      torp = tidyr::replace_na(torp, torp_prior_total),
      torp_recv = tidyr::replace_na(torp_recv, RATING_PRIOR_RATE_RECV),
      torp_disp = tidyr::replace_na(torp_disp, RATING_PRIOR_RATE_DISP),
      torp_spoil = tidyr::replace_na(torp_spoil, RATING_PRIOR_RATE_SPOIL),
      torp_hitout = tidyr::replace_na(torp_hitout, RATING_PRIOR_RATE_HITOUT),
      lineup_tog = tidyr::replace_na(POSITION_AVG_TOG[position.x], 0.75),
      .unknown_pos = !is.na(position.x) & is.na(POSITION_AVG_TOG[position.x]),
      torp = torp * lineup_tog,
      torp_recv = torp_recv * lineup_tog,
      torp_disp = torp_disp * lineup_tog,
      torp_spoil = torp_spoil * lineup_tog,
      torp_hitout = torp_hitout * lineup_tog
    )

  n_unknown <- sum(team_lineup_df$.unknown_pos, na.rm = TRUE)
  if (n_unknown > 0) {
    unknown_codes <- unique(team_lineup_df$position.x[team_lineup_df$.unknown_pos])
    cli::cli_warn("Unknown position code{?s} not in POSITION_AVG_TOG: {paste(unknown_codes, collapse = ', ')} ({n_unknown} row{?s} defaulted to TOG=0.75)")
  }
  team_lineup_df$.unknown_pos <- NULL

  # Join PSR if provided — use each player's most recent PSR value

  if (!is.null(psr_df)) {
    latest_psr <- psr_df |>
      dplyr::select(player_id, season, round, psr) |>
      dplyr::arrange(player_id, season, round) |>
      dplyr::group_by(player_id) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup() |>
      dplyr::select(player_id, psr)

    team_lineup_df <- team_lineup_df |>
      dplyr::left_join(latest_psr, by = "player_id") |>
      dplyr::mutate(
        psr = tidyr::replace_na(psr, 0),
        psr = psr * lineup_tog
      )
  } else {
    team_lineup_df$psr <- 0
  }

  # Generate position columns from lookup tables
  for (col in names(MATCH_PHASE_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$position.x %in% MATCH_PHASE_MAP[[col]], team_lineup_df$torp, NA)
  for (col in names(MATCH_POS_GROUP_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$position.x %in% MATCH_POS_GROUP_MAP[[col]], team_lineup_df$torp, NA)
  for (pos in MATCH_INDIVIDUAL_POS)
    team_lineup_df[[pos]] <- ifelse(team_lineup_df$position.x == pos, team_lineup_df$torp, NA)
  for (col in names(MATCH_COMBO_POS_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$position.x %in% MATCH_COMBO_POS_MAP[[col]], team_lineup_df$torp, NA)
  for (col in names(MATCH_LISTED_POS_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$position.y %in% MATCH_LISTED_POS_MAP[[col]], team_lineup_df$torp, NA)
  team_lineup_df$other_pos <- ifelse(is.na(team_lineup_df$position.y), team_lineup_df$torp, NA)

  # Verify position columns exist
  missing_pos <- setdiff(MATCH_POS_COLS, names(team_lineup_df))
  if (length(missing_pos) > 0) {
    cli::cli_abort("Missing position columns after generation: {paste(missing_pos, collapse = ', ')}")
  }

  # Aggregate to team level
  torp_sum_cols <- c("torp", "torp_recv", "torp_disp", "torp_spoil", "torp_hitout", "psr")

  team_rt_df <- team_lineup_df |>
    dplyr::filter(!is.na(player_id)) |>
    dplyr::mutate(team_name_adj = torp_replace_teams(team_name)) |>
    dplyr::group_by(match_id, team_id, season, round_number, team_type) |>
    dplyr::summarise(
      team_name_adj = max(team_name_adj),
      dplyr::across(dplyr::all_of(c(torp_sum_cols, MATCH_POS_COLS)), ~ sum(.x, na.rm = TRUE)),
      count = dplyr::n(),
      .groups = "drop"
    )

  team_rt_df
}


# .build_match_features ----

#' Build match-level features (home ground, familiarity, distance, days rest)
#'
#' @param fix_df Pivoted fixture df from .build_fixtures_df()
#' @param team_rt_df Team ratings df from .build_team_ratings_df()
#' @param all_grounds Stadium reference data
#' @return Fixture df enriched with log_dist, familiarity, days_rest, and team ratings
#' @keywords internal
#' @importFrom purrr pmap_dbl
.build_match_features <- function(fix_df, team_rt_df, all_grounds) {
  torp_sum_cols <- c("torp", "torp_recv", "torp_disp", "torp_spoil", "torp_hitout", "psr")

  # Add venue from fixtures (teams data doesn't carry venue)
  team_rt_df <- team_rt_df |>
    dplyr::left_join(
      fix_df |> dplyr::select(match_id, team_id, venue),
      by = c("match_id", "team_id")
    )

  # Home ground detection
  home_ground <- team_rt_df |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(home_ground = get_mode(venue), .groups = "drop") |>
    dplyr::mutate(venue_adj = torp_replace_venues(as.character(home_ground))) |>
    dplyr::left_join(
      all_grounds |>
        dplyr::mutate(venue_adj = torp_replace_venues(as.character(Ground))),
      by = c("venue_adj" = "venue_adj")
    )

  # Familiarity: cumulative venue proportion per team
  ground_prop <- team_rt_df |>
    dplyr::arrange(team_id, season, round_number) |>
    dplyr::group_by(team_id) |>
    dplyr::mutate(cum_total_games = dplyr::row_number() - 1) |>
    dplyr::group_by(team_id, venue) |>
    dplyr::mutate(cum_venue_games = dplyr::row_number() - 1) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      familiarity = ifelse(cum_total_games > 0, cum_venue_games / cum_total_games, 0)
    ) |>
    dplyr::select(team_id, season, round_number, venue, familiarity)

  # Distance traveled (Haversine)
  team_dist_df <- fix_df |>
    dplyr::left_join(
      all_grounds |> dplyr::select(venue, venue_lat = Latitude, venue_lon = Longitude),
      by = "venue"
    ) |>
    dplyr::left_join(
      home_ground |> dplyr::select(team_id, team_lat = Latitude, team_lon = Longitude),
      by = "team_id"
    ) |>
    dplyr::mutate(
      distance = purrr::pmap_dbl(
        list(venue_lon, venue_lat, team_lon, team_lat),
        ~ geosphere::distHaversine(c(..1, ..2), c(..3, ..4))
      ),
      log_dist = log(distance + MATCH_LOG_DIST_OFFSET),
      log_dist = tidyr::replace_na(log_dist, MATCH_LOG_DIST_DEFAULT)
    ) |>
    dplyr::left_join(ground_prop, by = c("team_id", "season", "round_number", "venue")) |>
    dplyr::mutate(familiarity = tidyr::replace_na(familiarity, 0))

  # Days rest (use parsed utc_dt, not character utc_start_time)
  days_rest <- fix_df |>
    dplyr::arrange(team_id, utc_dt) |>
    dplyr::group_by(team_id, season) |>
    dplyr::mutate(days_rest = as.numeric(difftime(utc_dt, dplyr::lag(utc_dt), units = "days"))) |>
    dplyr::ungroup() |>
    dplyr::mutate(days_rest = tidyr::replace_na(days_rest, 21))

  # Combine all features
  team_rt_fix_df <- fix_df |>
    dplyr::mutate(team_name = torp_replace_teams(team_name)) |>
    dplyr::left_join(
      team_dist_df |> dplyr::select(match_id, team_id, log_dist, familiarity),
      by = c("match_id", "team_id")
    ) |>
    dplyr::left_join(
      days_rest |> dplyr::select(match_id, team_id, days_rest),
      by = c("match_id", "team_id")
    ) |>
    dplyr::left_join(
      team_rt_df |> dplyr::select(
        match_id, team_id, season, round_number,
        dplyr::all_of(c(torp_sum_cols, MATCH_POS_COLS, "count"))
      ),
      by = c("match_id", "team_id", "season", "round_number")
    ) |>
    dplyr::group_by(team_id) |>
    tidyr::fill(torp, torp_recv, torp_disp, torp_spoil, torp_hitout, psr) |>
    dplyr::mutate(
      def = ifelse(def == 0, dplyr::lag(def), def),
      mid = ifelse(mid == 0, dplyr::lag(mid), mid),
      fwd = ifelse(fwd == 0, dplyr::lag(fwd), fwd),
      int = ifelse(int == 0, dplyr::lag(int), int),
      team_type_fac = as.factor(team_type)
    ) |>
    tidyr::fill(def, mid, fwd, int) |>
    dplyr::ungroup()

  team_rt_fix_df
}


# .load_match_weather ----

#' Load weather data for match modelling
#'
#' Loads historical weather from parquet and optionally fetches forecast
#' for upcoming matches via Open-Meteo API.
#'
#' @param fixtures Raw fixtures (for identifying upcoming matches)
#' @param all_grounds Stadium reference data (for geocoding)
#' @param target_weeks Numeric vector of target round numbers
#' @param season Target season
#' @param weather_path Path to historical weather parquet file
#' @return Tibble with match_id, temp_avg, precipitation_total, wind_avg, humidity_avg, is_roof
#' @keywords internal
.load_match_weather <- function(fixtures, all_grounds, target_weeks = NULL,
                                season = NULL, weather_path = NULL) {
  if (is.null(weather_path)) {
    weather_path <- file.path("data-raw", "weather_data.parquet")
  }

  if (!file.exists(weather_path)) {
    cli::cli_warn("Weather data not found at {weather_path} -- skipping weather features")
    return(tibble::tibble(match_id = character()))
  }

  historical <- arrow::read_parquet(weather_path)
  # Normalise old weather files with providerId
  if ("providerId" %in% names(historical) && !"match_id" %in% names(historical)) {
    names(historical)[names(historical) == "providerId"] <- "match_id"
  }
  historical <- historical |>
    dplyr::select(match_id, temp_avg, precipitation_total, wind_avg, humidity_avg, is_roof)

  # Forecast for upcoming matches not in historical data
  if (!is.null(target_weeks) && !is.null(season)) {
    upcoming <- fixtures |>
      dplyr::filter(
        season == .env$season,
        round_number %in% target_weeks,
        !match_id %in% historical$match_id
      )

    if (nrow(upcoming) > 0) {
      cli::cli_inform("Fetching forecast weather for {nrow(upcoming)} upcoming fixtures")
      forecast <- .fetch_forecast_weather(upcoming, all_grounds)
      weather_df <- dplyr::bind_rows(historical, forecast)
    } else {
      weather_df <- historical
    }
  } else {
    weather_df <- historical
  }

  weather_df
}

#' Fetch forecast weather from Open-Meteo API
#' @keywords internal
.fetch_forecast_weather <- function(fixtures_upcoming, all_grounds) {
  if (nrow(fixtures_upcoming) == 0) return(tibble::tibble())

  fixtures_geo <- fixtures_upcoming |>
    dplyr::mutate(venue = torp_replace_venues(venue_name)) |>
    dplyr::left_join(
      all_grounds |>
        dplyr::select(venue, Latitude, Longitude) |>
        dplyr::distinct(venue, .keep_all = TRUE),
      by = "venue"
    ) |>
    dplyr::filter(!is.na(Latitude))

  if (nrow(fixtures_geo) == 0) return(tibble::tibble())

  venue_dates <- fixtures_geo |>
    dplyr::mutate(
      match_date = as.Date(as.POSIXct(utc_start_time, format = "%Y-%m-%dT%H:%M", tz = "UTC"))
    ) |>
    dplyr::group_by(venue, Latitude, Longitude) |>
    dplyr::summarise(
      min_date = min(match_date), max_date = max(match_date), .groups = "drop"
    )

  all_hourly <- list()
  for (i in seq_len(nrow(venue_dates))) {
    vd <- venue_dates[i, ]
    hourly <- tryCatch({
      resp <- httr2::request("https://api.open-meteo.com/v1/forecast") |>
        httr2::req_url_query(
          latitude = vd$Latitude, longitude = vd$Longitude,
          start_date = format(vd$min_date, "%Y-%m-%d"),
          end_date = format(vd$max_date, "%Y-%m-%d"),
          hourly = "temperature_2m,precipitation,wind_speed_10m,relative_humidity_2m",
          timezone = "UTC"
        ) |>
        httr2::req_retry(max_tries = 3, backoff = ~ 2) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
      tibble::tibble(
        time = as.POSIXct(unlist(resp$hourly$time), format = "%Y-%m-%dT%H:%M", tz = "UTC"),
        temperature_2m = as.numeric(unlist(resp$hourly$temperature_2m)),
        precipitation = as.numeric(unlist(resp$hourly$precipitation)),
        wind_speed_10m = as.numeric(unlist(resp$hourly$wind_speed_10m)),
        relative_humidity_2m = as.numeric(unlist(resp$hourly$relative_humidity_2m)),
        venue = vd$venue
      )
    }, error = function(e) {
      cli::cli_warn("Forecast failed for {vd$venue}: {conditionMessage(e)}")
      NULL
    })
    if (!is.null(hourly)) all_hourly[[i]] <- hourly
    Sys.sleep(0.3)
  }

  n_failed <- nrow(venue_dates) - length(all_hourly)
  if (n_failed > 0) {
    cli::cli_warn("Weather forecast failed for {n_failed} of {nrow(venue_dates)} venue{?s}")
  }
  if (length(all_hourly) == 0) {
    cli::cli_warn("All weather forecasts failed -- predictions will use median weather imputation")
    return(tibble::tibble())
  }
  hourly_df <- dplyr::bind_rows(all_hourly)

  # Aggregate to match-level (3hr window from kickoff)
  fixtures_geo |>
    dplyr::mutate(
      kickoff_utc = as.POSIXct(utc_start_time, format = "%Y-%m-%dT%H:%M", tz = "UTC")
    ) |>
    dplyr::left_join(hourly_df, by = "venue", relationship = "many-to-many") |>
    dplyr::filter(time >= kickoff_utc, time < kickoff_utc + lubridate::hours(3)) |>
    dplyr::group_by(match_id) |>
    dplyr::summarise(
      temp_avg = mean(temperature_2m, na.rm = TRUE),
      precipitation_total = sum(precipitation, na.rm = TRUE),
      wind_avg = mean(wind_speed_10m, na.rm = TRUE),
      humidity_avg = mean(relative_humidity_2m, na.rm = TRUE),
      is_roof = dplyr::first(venue) == "Docklands",
      .groups = "drop"
    )
}


# .normalise_results_schema ----

#' Normalise results to a common schema
#'
#' Handles three result schemas:
#' \itemize{
#'   \item **Canonical** (normalised fixtures/results): `match_id`, `home_score`, etc.
#'   \item **CFS** (old torpdata releases): `match.matchId`, `homeTeamScore.matchScore.*`
#'   \item **Fixture** (raw API, pre-normalisation): `providerId`, `home.score.*`
#' }
#' Returns a tibble with canonical column names.
#'
#' @param results Results data (may be mixed schema across seasons)
#' @return Tibble with columns: match_id, home_score, home_goals, home_behinds,
#'   away_score, away_goals, away_behinds
#' @keywords internal
.normalise_results_schema <- function(results) {
  has_canonical <- "match_id" %in% names(results) && "home_score" %in% names(results)
  has_cfs <- "match.matchId" %in% names(results)
  has_fix <- "home.score.totalScore" %in% names(results)

  if (has_canonical) {
    return(results |>
      dplyr::select(
        match_id, home_score, home_goals, home_behinds,
        away_score, away_goals, away_behinds
      ))
  }

  if (has_cfs && has_fix) {
    is_fix_row <- !is.na(results[["home.score.totalScore"]])
    parts <- list()
    if (any(!is_fix_row)) {
      parts[[1]] <- results[!is_fix_row, ] |>
        dplyr::transmute(
          match_id = match.matchId,
          home_score = homeTeamScore.matchScore.totalScore,
          home_goals = homeTeamScore.matchScore.goals,
          home_behinds = homeTeamScore.matchScore.behinds,
          away_score = awayTeamScore.matchScore.totalScore,
          away_goals = awayTeamScore.matchScore.goals,
          away_behinds = awayTeamScore.matchScore.behinds
        )
    }
    if (any(is_fix_row)) {
      parts[[2]] <- results[is_fix_row, ] |>
        dplyr::transmute(
          match_id = providerId,
          home_score = home.score.totalScore,
          home_goals = home.score.goals,
          home_behinds = home.score.behinds,
          away_score = away.score.totalScore,
          away_goals = away.score.goals,
          away_behinds = away.score.behinds
        )
    }
    dplyr::bind_rows(parts)
  } else if (has_cfs) {
    results |>
      dplyr::transmute(
        match_id = match.matchId,
        home_score = homeTeamScore.matchScore.totalScore,
        home_goals = homeTeamScore.matchScore.goals,
        home_behinds = homeTeamScore.matchScore.behinds,
        away_score = awayTeamScore.matchScore.totalScore,
        away_goals = awayTeamScore.matchScore.goals,
        away_behinds = awayTeamScore.matchScore.behinds
      )
  } else if (has_fix) {
    results |>
      dplyr::transmute(
        match_id = providerId,
        home_score = home.score.totalScore,
        home_goals = home.score.goals,
        home_behinds = home.score.behinds,
        away_score = away.score.totalScore,
        away_goals = away.score.goals,
        away_behinds = away.score.behinds
      )
  } else {
    cli::cli_abort("Results data has unrecognised schema. Expected match_id, match.matchId, or home.score.totalScore columns.")
  }
}


# .build_team_mdl_df ----

#' Build the full match model dataset
#'
#' Self-joins for opponent features, computes diffs, joins results/xG/weather,
#' adds log-transformed weather, and applies time-decay weights.
#'
#' @param team_rt_fix_df Combined fixture + team ratings from .build_match_features()
#' @param results Match results from load_results()
#' @param xg_df Expected goals data from load_xg()
#' @param weather_df Weather data from .load_match_weather()
#' @param weight_anchor_date Date for time-decay weight anchoring
#' @return Complete model dataset ready for GAM training
#' @keywords internal
.build_team_mdl_df <- function(team_rt_fix_df, results, xg_df, weather_df,
                               weight_anchor_date) {
  # Opponent columns for self-join
  opp_cols <- c(
    "match_id", "team_type",
    "torp", "torp_recv", "torp_disp", "torp_spoil", "torp_hitout", "psr",
    "def", "mid", "fwd", "int", MATCH_INDIVIDUAL_POS,
    "team_name", "team_name_season",
    "log_dist", "familiarity", "days_rest",
    "team_type_fac", "season", "round_number", "venue", "count",
    "game_year_decimal", "game_prop_through_year", "game_prop_through_month",
    "game_wday_fac", "game_prop_through_day"
  )

  team_mdl_df_tot <- team_rt_fix_df |>
    dplyr::left_join(
      team_rt_fix_df |>
        dplyr::select(dplyr::all_of(opp_cols)) |>
        dplyr::mutate(type_anti = dplyr::if_else(team_type == "home", "away", "home")),
      by = c("match_id" = "match_id", "team_type" = "type_anti")
    )

  # Validate self-join completeness
  na_opp <- sum(is.na(team_mdl_df_tot$torp.y))
  if (na_opp > 0) {
    bad_ids <- unique(team_mdl_df_tot$match_id[is.na(team_mdl_df_tot$torp.y)])
    cli::cli_warn("{na_opp} row{?s} have no opponent data after self-join. Affected matches: {paste(utils::head(bad_ids, 5), collapse = ', ')}")
  }

  team_mdl_df_tot <- team_mdl_df_tot |>
    dplyr::mutate(
      torp_diff = torp.x - torp.y,
      torp_ratio = log(pmax(torp.x, 0.01) / pmax(torp.y, 0.01)),
      torp_recv_diff = torp_recv.x - torp_recv.y,
      torp_disp_diff = torp_disp.x - torp_disp.y,
      torp_spoil_diff = torp_spoil.x - torp_spoil.y,
      torp_hitout_diff = torp_hitout.x - torp_hitout.y,
      psr_diff = psr.x - psr.y
    ) |>
    dplyr::left_join(
      .normalise_results_schema(results),
      by = "match_id"
    ) |>
    dplyr::left_join(xg_df, by = c("match_id" = "match_id")) |>
    dplyr::mutate(
      home_shots = home_goals + home_behinds,
      away_shots = away_goals + away_behinds,
      score_diff = ifelse(team_type == "home",
        home_score - away_score,
        away_score - home_score),
      shot_diff = ifelse(team_type == "home",
        home_shots - away_shots, away_shots - home_shots),
      team_shots = ifelse(team_type == "home", home_shots, away_shots),
      harmean_shots = harmonic_mean(home_shots, away_shots),
      shot_conv = ifelse(team_type == "home",
        home_goals / pmax(home_shots, 1),
        away_goals / pmax(away_shots, 1)),
      shot_conv_diff = ifelse(team_type == "home",
        (home_goals / pmax(home_shots, 1)) -
          (away_goals / pmax(away_shots, 1)),
        (away_goals / pmax(away_shots, 1)) -
          (home_goals / pmax(home_shots, 1))),
      xscore_diff = ifelse(team_type == "home", xscore_diff, -xscore_diff),
      team_xscore = ifelse(team_type == "home", home_xscore, away_xscore),
      win = ifelse(score_diff > 0, 1, ifelse(score_diff == 0, 0.5, 0)),
      hoff_adef = pmax(pmin((fwd.x - def.y), 20), -5),
      hmid_amid = pmax(pmin((mid.x - mid.y), 12), -12),
      hdef_afwd = pmax(pmin((def.x - fwd.y), 5), -20),
      hint_aint = pmax(pmin((int.x - int.y), 10), -10)
    )

  # Individual position diffs
  for (pos in MATCH_INDIVIDUAL_POS) {
    diff_col <- paste0(pos, "_diff")
    x_col <- paste0(pos, ".x")
    y_col <- paste0(pos, ".y")
    team_mdl_df_tot[[diff_col]] <- team_mdl_df_tot[[x_col]] - team_mdl_df_tot[[y_col]]
  }

  team_mdl_df_tot <- team_mdl_df_tot |>
    dplyr::mutate(
      int_diff = int.x - int.y,
      team_type_fac = team_type_fac.x,
      total_score = home_score + away_score,
      total_shots = home_shots + away_shots,
      team_name.x = as.factor(team_name.x),
      team_name.y = as.factor(team_name.y),
      log_dist_diff = log_dist.x - log_dist.y,
      familiarity_diff = familiarity.x - familiarity.y,
      days_rest_diff = days_rest.x - days_rest.y,
      days_rest_diff_fac = as.factor(round(ifelse(
        days_rest_diff > 3, 4, ifelse(days_rest_diff < -3, -4, days_rest_diff)
      ))),
      weightz = exp(as.numeric(-(weight_anchor_date - as.Date(utc_start_time))) /
        MATCH_WEIGHT_DECAY_DAYS),
      weightz = weightz / mean(weightz, na.rm = TRUE),
      shot_weightz = (harmean_shots / mean(harmean_shots, na.rm = TRUE)) * weightz
    )

  # Filter early matches with unreliable data
  team_mdl_df <- team_mdl_df_tot |>
    dplyr::filter(
      season.x > MATCH_MIN_DATA_SEASON |
        (season.x == MATCH_MIN_DATA_SEASON & round_number.x >= MATCH_MIN_DATA_ROUND)
    )

  # Adjust total_xpoints scale
  xpoints_scale <- mean(team_mdl_df$total_points, na.rm = TRUE) /
    mean(team_mdl_df$total_xpoints, na.rm = TRUE)
  if (!is.finite(xpoints_scale)) {
    cli::cli_abort(c(
      "Cannot compute xpoints scaling factor (NaN/Inf).",
      "i" = "total_points non-NA: {sum(!is.na(team_mdl_df$total_points))}",
      "i" = "total_xpoints non-NA: {sum(!is.na(team_mdl_df$total_xpoints))}",
      "i" = "Check xg_df join."
    ))
  }
  team_mdl_df <- team_mdl_df |>
    dplyr::mutate(
      total_xpoints_adj = total_xpoints * xpoints_scale,
      venue_fac = as.factor(venue.x)
    )

  # Join weather and apply log transforms
  team_mdl_df <- team_mdl_df |>
    dplyr::left_join(weather_df, by = "match_id") |>
    dplyr::mutate(
      temp_avg = tidyr::replace_na(temp_avg, stats::median(temp_avg, na.rm = TRUE)),
      wind_avg = tidyr::replace_na(wind_avg, stats::median(wind_avg, na.rm = TRUE)),
      humidity_avg = tidyr::replace_na(humidity_avg, stats::median(humidity_avg, na.rm = TRUE)),
      precipitation_total = tidyr::replace_na(precipitation_total, 0),
      is_roof = tidyr::replace_na(is_roof, FALSE),
      log_wind = log1p(wind_avg),
      log_precip = log1p(precipitation_total)
    )

  cli::cli_inform("team_mdl_df: {nrow(team_mdl_df)} rows, {ncol(team_mdl_df)} cols")
  weather_pct <- 100 * sum(!is.na(team_mdl_df$temp_avg)) / nrow(team_mdl_df)
  cli::cli_inform("Weather joined: {sum(!is.na(team_mdl_df$temp_avg))} of {nrow(team_mdl_df)} rows ({round(weather_pct, 1)}%)")
  if (weather_pct < 80) {
    cli::cli_warn("Low weather coverage ({round(weather_pct, 1)}%) -- median imputation may affect predictions")
  }

  team_mdl_df
}


# .train_match_gams ----

#' Train the 5-model sequential GAM pipeline
#'
#' Trains total xPoints, xScore diff, conversion, score diff, and win probability
#' GAMs sequentially (each model's predictions feed the next). Model 1 includes
#' weather smooths (log_wind, log_precip, temp_avg, humidity_avg).
#'
#' @param team_mdl_df Complete model dataset from .build_team_mdl_df()
#' @param train_filter Logical vector indicating training rows (NULL = all completed matches)
#' @param nthreads Number of threads for mgcv::bam() (default 4)
#' @return List with $models (named list of 5 GAMs) and $data (team_mdl_df with predictions)
#' @keywords internal
.train_match_gams <- function(team_mdl_df, train_filter = NULL, nthreads = 4L) {
  loadNamespace("mgcv")

  if (is.null(train_filter)) {
    train_mask <- !is.na(team_mdl_df$win)
  } else {
    train_mask <- train_filter & !is.na(team_mdl_df$win)
  }

  gam_df <- team_mdl_df[train_mask, ]
  cli::cli_inform("Training on {nrow(gam_df)} completed matches")
  if (nrow(gam_df) == 0) {
    cli::cli_abort("Cannot train GAM models: 0 completed matches after filtering")
  }

  # Check which optional smooth terms have sufficient unique values (need >= k)
  # Terms with constant/near-constant data are dropped to prevent mgcv errors
  optional_smooth_terms <- list(
    # Model 1 optional terms (psr + weather)
    "s(psr.x, bs = \"ts\", k = 5)"           = list(var = "psr.x", k = 5),
    "s(psr.y, bs = \"ts\", k = 5)"           = list(var = "psr.y", k = 5),
    "s(log_wind, bs = \"ts\", k = 5)"        = list(var = "log_wind", k = 5),
    "s(log_precip, bs = \"ts\", k = 5)"      = list(var = "log_precip", k = 5),
    "s(temp_avg, bs = \"ts\", k = 5)"        = list(var = "temp_avg", k = 5),
    "s(humidity_avg, bs = \"ts\", k = 5)"     = list(var = "humidity_avg", k = 5),
    # Models 2-4 optional term
    "s(psr_diff, bs = \"ts\", k = 5)"        = list(var = "psr_diff", k = 5)
  )
  drop_terms <- character(0)
  for (term_str in names(optional_smooth_terms)) {
    info <- optional_smooth_terms[[term_str]]
    vals <- gam_df[[info$var]]
    n_unique <- length(unique(vals[!is.na(vals)]))
    if (n_unique < info$k) {
      drop_terms <- c(drop_terms, term_str)
      cli::cli_warn("Dropping smooth {.code {term_str}}: only {n_unique} unique value{?s} (need >= {info$k})")
    }
  }

  # Helper to build formula by conditionally adding optional terms
  .add_optional <- function(base_terms, optional_terms) {
    keep <- setdiff(optional_terms, drop_terms)
    if (length(keep) > 0) {
      paste(base_terms, "+", paste(keep, collapse = " + "))
    } else {
      base_terms
    }
  }

  # Model 1: Total expected points (includes weather smooths)
  cli::cli_progress_step("Training total xPoints model")
  m1_base <- paste(
    "total_xpoints_adj ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(game_year_decimal.x, bs = \"ts\")",
    "+ s(game_prop_through_year.x, bs = \"cc\")",
    "+ s(game_prop_through_month.x, bs = \"cc\")",
    "+ s(game_wday_fac.x, bs = \"re\")",
    "+ s(game_prop_through_day.x, bs = \"cc\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ s(abs(torp_diff), bs = \"ts\", k = 5)",
    "+ s(abs(torp_recv_diff), bs = \"ts\", k = 5)",
    "+ s(abs(torp_disp_diff), bs = \"ts\", k = 5)",
    "+ s(abs(torp_spoil_diff), bs = \"ts\", k = 5)",
    "+ s(abs(torp_hitout_diff), bs = \"ts\", k = 5)",
    "+ s(torp.x, bs = \"ts\", k = 5) + s(torp.y, bs = \"ts\", k = 5)",
    "+ s(venue_fac, bs = \"re\")",
    "+ s(log_dist.x, bs = \"ts\", k = 5) + s(log_dist.y, bs = \"ts\", k = 5)",
    "+ s(familiarity.x, bs = \"ts\", k = 5) + s(familiarity.y, bs = \"ts\", k = 5)",
    "+ s(log_dist_diff, bs = \"ts\", k = 5)",
    "+ s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m1_optional <- c(
    "s(psr.x, bs = \"ts\", k = 5)", "s(psr.y, bs = \"ts\", k = 5)",
    "s(log_wind, bs = \"ts\", k = 5)", "s(log_precip, bs = \"ts\", k = 5)",
    "s(temp_avg, bs = \"ts\", k = 5)", "s(humidity_avg, bs = \"ts\", k = 5)"
  )
  m1_formula <- stats::as.formula(.add_optional(m1_base, m1_optional))

  afl_total_xpoints_mdl <- mgcv::bam(
    m1_formula,
    data = gam_df, weights = gam_df$weightz,
    family = gaussian(), nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  team_mdl_df$pred_tot_xscore <- predict(afl_total_xpoints_mdl, newdata = team_mdl_df, type = "response")

  # Model 2: xScore differential
  cli::cli_progress_step("Training xScore diff model")
  gam_df$pred_tot_xscore <- team_mdl_df$pred_tot_xscore[train_mask]
  m2_base <- paste(
    "xscore_diff ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ ti(torp_diff, pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
    "+ s(pred_tot_xscore, bs = \"ts\", k = 5)",
    "+ s(torp_diff, bs = \"ts\", k = 5)",
    "+ s(torp_recv_diff, bs = \"ts\", k = 5)",
    "+ s(torp_disp_diff, bs = \"ts\", k = 5)",
    "+ s(torp_spoil_diff, bs = \"ts\", k = 5)",
    "+ s(torp_hitout_diff, bs = \"ts\", k = 5)",
    "+ s(log_dist_diff, bs = \"ts\", k = 5) + s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m2_formula <- stats::as.formula(.add_optional(m2_base, "s(psr_diff, bs = \"ts\", k = 5)"))

  afl_xscore_diff_mdl <- mgcv::bam(
    m2_formula,
    data = gam_df, weights = gam_df$weightz,
    family = gaussian(), nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  team_mdl_df$pred_xscore_diff <- predict(afl_xscore_diff_mdl, newdata = team_mdl_df, type = "response")

  # Model 3: Conversion differential
  cli::cli_progress_step("Training conversion model")
  gam_df$pred_xscore_diff <- team_mdl_df$pred_xscore_diff[train_mask]
  m3_base <- paste(
    "shot_conv_diff ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(game_year_decimal.x, bs = \"ts\")",
    "+ s(game_prop_through_year.x, bs = \"cc\")",
    "+ s(game_prop_through_month.x, bs = \"cc\")",
    "+ s(game_wday_fac.x, bs = \"re\")",
    "+ s(game_prop_through_day.x, bs = \"cc\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ ti(torp_diff, pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
    "+ s(torp_diff, bs = \"ts\", k = 5)",
    "+ s(torp_recv_diff, bs = \"ts\", k = 5)",
    "+ s(torp_disp_diff, bs = \"ts\", k = 5)",
    "+ s(torp_spoil_diff, bs = \"ts\", k = 5)",
    "+ s(torp_hitout_diff, bs = \"ts\", k = 5)",
    "+ s(pred_tot_xscore, bs = \"ts\", k = 5)",
    "+ s(pred_xscore_diff, bs = \"ts\", k = 5)",
    "+ s(venue_fac, bs = \"re\")",
    "+ s(log_dist_diff, bs = \"ts\", k = 5) + s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m3_formula <- stats::as.formula(.add_optional(m3_base, "s(psr_diff, bs = \"ts\", k = 5)"))

  afl_conv_mdl <- mgcv::bam(
    m3_formula,
    data = gam_df, weights = gam_df$shot_weightz,
    family = gaussian(), nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  team_mdl_df$pred_conv_diff <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")

  # Model 4: Score differential
  cli::cli_progress_step("Training score diff model")
  gam_df$pred_conv_diff <- team_mdl_df$pred_conv_diff[train_mask]
  m4_base <- paste(
    "score_diff ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ ti(pred_xscore_diff, pred_conv_diff, bs = \"ts\", k = 5)",
    "+ ti(pred_tot_xscore, pred_conv_diff, bs = \"ts\", k = 5)",
    "+ s(pred_xscore_diff)",
    "+ s(log_dist_diff, bs = \"ts\", k = 5) + s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m4_formula <- stats::as.formula(.add_optional(m4_base, "s(psr_diff, bs = \"ts\", k = 5)"))

  afl_score_mdl <- mgcv::bam(
    m4_formula,
    data = gam_df, weights = gam_df$weightz,
    family = "gaussian", nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  team_mdl_df$pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")

  # Model 5: Win probability
  cli::cli_progress_step("Training win probability model")
  gam_df$pred_score_diff <- team_mdl_df$pred_score_diff[train_mask]
  afl_win_mdl <- mgcv::bam(
    win ~
      +s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
      + ti(pred_tot_xscore, pred_score_diff, bs = c("ts", "ts"), k = 4)
      + s(pred_score_diff, bs = "ts", k = 5)
      + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5)
      + s(days_rest_diff_fac, bs = "re"),
    data = gam_df, weights = gam_df$weightz,
    family = "binomial", nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  team_mdl_df$pred_win <- predict(afl_win_mdl, newdata = team_mdl_df, type = "response")

  # Validation
  if (any(is.na(team_mdl_df$pred_win[!is.na(team_mdl_df$win)]))) {
    cli::cli_warn("NA values in pred_win for completed matches")
  }
  pred_win_range <- range(team_mdl_df$pred_win, na.rm = TRUE)
  if (pred_win_range[1] < 0 || pred_win_range[2] > 1) {
    cli::cli_warn("pred_win outside [0,1]: [{round(pred_win_range[1], 4)}, {round(pred_win_range[2], 4)}]")
  }

  # Home/away symmetry check: for each match, H_score_diff ≈ -A_score_diff
  # and H_win + A_win ≈ 1. Large deviations indicate a data pipeline bug.
  sym_check <- team_mdl_df |>
    dplyr::group_by(match_id) |>
    dplyr::summarise(
      score_sum = sum(pred_score_diff),
      win_sum = sum(pred_win),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(n == 2)

  if (nrow(sym_check) > 0) {
    max_score_asym <- max(abs(sym_check$score_sum))
    max_win_asym <- max(abs(sym_check$win_sum - 1))
    if (max_score_asym > 5) {
      cli::cli_abort(c(
        "Home/away prediction asymmetry detected (max score_diff sum: {round(max_score_asym, 1)}).",
        "i" = "For each match, home pred_score_diff + away pred_score_diff should be ~0.",
        "i" = "This usually indicates a column name mismatch in the data pipeline."
      ))
    }
    if (max_win_asym > 0.1) {
      cli::cli_warn("Home/away win probability asymmetry: max |H_win + A_win - 1| = {round(max_win_asym, 4)}")
    }
  }

  # Scoring metrics
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

  models <- list(
    total_xpoints = afl_total_xpoints_mdl,
    xscore_diff   = afl_xscore_diff_mdl,
    conv_diff     = afl_conv_mdl,
    score_diff    = afl_score_mdl,
    win           = afl_win_mdl
  )

  cli::cli_alert_success("GAM pipeline trained on {nrow(gam_df)} matches, predictions generated")

  list(models = models, data = team_mdl_df)
}


# .train_match_xgb ----

#' Train the 5-model sequential XGBoost pipeline
#'
#' Mirrors the GAM pipeline structure: total xPoints -> xScore diff -> conv diff
#' -> score diff -> win probability, each step feeding the next.
#'
#' @param team_mdl_df Complete model dataset (with GAM predictions already added)
#' @param train_filter Logical vector indicating training rows (NULL = all completed matches)
#' @return List with $models (named list of 5 XGBoost models) and $data (team_mdl_df
#'   with xgb_pred_score_diff and xgb_pred_win columns added)
#' @keywords internal
.train_match_xgb <- function(team_mdl_df, train_filter = NULL) {
  loadNamespace("xgboost")

  if (is.null(train_filter)) {
    train_mask <- !is.na(team_mdl_df$win) & !is.na(team_mdl_df$total_xpoints_adj) &
      !is.na(team_mdl_df$xscore_diff) & !is.na(team_mdl_df$shot_conv_diff) &
      !is.na(team_mdl_df$score_diff)
  } else {
    train_mask <- train_filter & !is.na(team_mdl_df$win) &
      !is.na(team_mdl_df$total_xpoints_adj) & !is.na(team_mdl_df$xscore_diff) &
      !is.na(team_mdl_df$shot_conv_diff) & !is.na(team_mdl_df$score_diff)
  }

  xgb_df <- team_mdl_df[train_mask, ]
  cli::cli_inform("XGBoost training on {nrow(xgb_df)} rows")
  if (nrow(xgb_df) == 0) {
    cli::cli_abort("Cannot train XGBoost: 0 complete rows after filtering")
  }

  # Feature columns — diffs only (no .x/.y positional features) to enforce symmetry
  base_cols <- c(
    "team_type_fac",
    "game_year_decimal.x", "game_prop_through_year.x",
    "game_prop_through_month.x", "game_prop_through_day.x",
    "torp_diff", "torp_recv_diff", "torp_disp_diff",
    "torp_spoil_diff", "torp_hitout_diff",
    "psr_diff",
    "log_dist_diff",
    "familiarity_diff",
    "days_rest_diff_fac"
  )

  reg_params <- list(
    objective = "reg:squarederror", eval_metric = "rmse",
    tree_method = "hist", eta = 0.05, subsample = 0.7,
    colsample_bytree = 0.8, max_depth = 3, min_child_weight = 15
  )
  cls_params <- list(
    objective = "binary:logistic", eval_metric = "logloss",
    tree_method = "hist", eta = 0.05, subsample = 0.7,
    colsample_bytree = 0.8, max_depth = 3, min_child_weight = 15
  )

  # Season-grouped CV folds
  train_seasons <- sort(unique(xgb_df$season.x))
  folds <- lapply(train_seasons, function(s) which(xgb_df$season.x == s))

  # Helper: build DMatrix, run CV, train final model
  train_step <- function(df, label, weights, feature_cols, params, step_name) {
    fmat <- stats::model.matrix(~ . - 1, data = df[, feature_cols, drop = FALSE])
    dtrain <- xgboost::xgb.DMatrix(data = fmat, label = label, weight = weights)

    set.seed(1234)
    cv <- xgboost::xgb.cv(
      params = params, data = dtrain, nrounds = 1000, folds = folds,
      early_stopping_rounds = 30, print_every_n = 0, verbose = 0
    )
    metric_col <- paste0("test_", params$eval_metric, "_mean")
    best_n <- which.min(cv$evaluation_log[[metric_col]])
    cv_score <- min(cv$evaluation_log[[metric_col]])

    set.seed(1234)
    model <- xgboost::xgb.train(
      params = params, data = dtrain, nrounds = best_n,
      print_every_n = 0, verbose = 0
    )
    list(model = model, preds = predict(model, dtrain),
         best_n = best_n, cv_score = cv_score)
  }

  # Helper: predict on full dataset
  predict_all <- function(model, df, feature_cols) {
    mat <- stats::model.matrix(~ . - 1, data = df[, feature_cols, drop = FALSE])
    predict(model, xgboost::xgb.DMatrix(data = mat))
  }

  # Step 1: total xPoints
  s1 <- train_step(xgb_df, xgb_df$total_xpoints_adj, xgb_df$weightz, base_cols, reg_params, "total_xpoints")
  xgb_df$xgb_pred_tot_xscore <- s1$preds
  team_mdl_df$xgb_pred_tot_xscore <- predict_all(s1$model, team_mdl_df, base_cols)

  # Step 2: xScore diff
  s2_cols <- c(base_cols, "xgb_pred_tot_xscore")
  s2 <- train_step(xgb_df, xgb_df$xscore_diff, xgb_df$weightz, s2_cols, reg_params, "xscore_diff")
  xgb_df$xgb_pred_xscore_diff <- s2$preds
  team_mdl_df$xgb_pred_xscore_diff <- predict_all(s2$model, team_mdl_df, s2_cols)

  # Step 3: conv diff
  s3_cols <- c(base_cols, "xgb_pred_tot_xscore", "xgb_pred_xscore_diff")
  s3 <- train_step(xgb_df, xgb_df$shot_conv_diff, xgb_df$shot_weightz, s3_cols, reg_params, "conv_diff")
  xgb_df$xgb_pred_conv_diff <- s3$preds
  team_mdl_df$xgb_pred_conv_diff <- predict_all(s3$model, team_mdl_df, s3_cols)

  # Step 4: score diff
  s4_cols <- c(base_cols, "xgb_pred_xscore_diff", "xgb_pred_conv_diff", "xgb_pred_tot_xscore")
  s4 <- train_step(xgb_df, xgb_df$score_diff, xgb_df$weightz, s4_cols, reg_params, "score_diff")
  xgb_df$xgb_pred_score_diff <- s4$preds
  team_mdl_df$xgb_pred_score_diff <- predict_all(s4$model, team_mdl_df, s4_cols)

  # Step 5: win probability — slim features to avoid overfitting binary target
  s5_cols <- c(
    "team_type_fac",
    "xgb_pred_tot_xscore", "xgb_pred_score_diff",
    "log_dist_diff", "familiarity_diff", "days_rest_diff_fac"
  )
  s5 <- train_step(xgb_df, as.numeric(xgb_df$win), xgb_df$weightz, s5_cols, cls_params, "win")
  xgb_df$xgb_pred_win <- s5$preds
  team_mdl_df$xgb_pred_win <- predict_all(s5$model, team_mdl_df, s5_cols)

  cli::cli_alert_success("XGBoost pipeline trained ({s1$best_n}/{s2$best_n}/{s3$best_n}/{s4$best_n}/{s5$best_n} rounds)")

  models <- list(
    total_xpoints = s1$model, xscore_diff = s2$model, conv_diff = s3$model,
    score_diff = s4$model, win = s5$model
  )
  steps <- list(
    total_xpoints = list(best_n = s1$best_n, cv_score = s1$cv_score),
    xscore_diff   = list(best_n = s2$best_n, cv_score = s2$cv_score),
    conv_diff      = list(best_n = s3$best_n, cv_score = s3$cv_score),
    score_diff     = list(best_n = s4$best_n, cv_score = s4$cv_score),
    win            = list(best_n = s5$best_n, cv_score = s5$cv_score)
  )
  list(models = models, steps = steps, data = team_mdl_df)
}


# build_team_mdl_df (convenience wrapper) ----

#' Build complete match model dataset end-to-end
#'
#' Convenience wrapper chaining all internal .build_* functions.
#' Loads data, builds fixtures, ratings, features, weather, and model dataset.
#'
#' @param season Season to build for (default: current via get_afl_season())
#' @param target_weeks Numeric vector of target round numbers (used for weather forecasting)
#' @param weather_path Path to historical weather parquet (default: data-raw/weather_data.parquet)
#' @return Complete team_mdl_df ready for GAM training
#' @keywords internal
build_team_mdl_df <- function(season = NULL, target_weeks = NULL,
                              weather_path = NULL, psr_coef_path = NULL) {
  if (is.null(season)) season <- get_afl_season()

  cli::cli_h2("Loading data")
  all_grounds <- file_reader("stadium_data", "reference-data")
  xg_df <- load_xg(TRUE)
  fixtures <- load_fixtures(TRUE)
  results <- load_results(TRUE)
  teams <- load_teams(TRUE)
  torp_df <- load_torp_ratings()

  # Load PSR (Player Skill Ratings)
  psr_df <- NULL
  tryCatch({
    skills <- load_player_skills(TRUE)
    if (is.null(psr_coef_path)) {
      psr_coef_path <- system.file("extdata", "psr_v2_coefficients.csv", package = "torp")
      if (psr_coef_path == "") {
        psr_coef_path <- file.path(
          find.package("torp", quiet = TRUE)[1] %||% ".",
          "data-raw", "cache-skills", "psr_v2_coefficients.csv"
        )
      }
    }
    if (file.exists(psr_coef_path)) {
      coef_df <- utils::read.csv(psr_coef_path)
      psr_df <- calculate_psr(skills, coef_df)
      cli::cli_inform("PSR computed for {nrow(psr_df)} player-rounds")
    } else {
      cli::cli_warn("PSR coefficient file not found: {psr_coef_path}")
    }
  }, error = function(e) {
    cli::cli_warn("Failed to compute PSR: {e$message}")
  })

  cli::cli_inform(paste0(
    "Loaded: fixtures=", nrow(fixtures), ", results=", nrow(results),
    ", teams=", nrow(teams), ", ratings=", nrow(torp_df)
  ))

  # Input validation
  if (nrow(fixtures) < 100) cli::cli_abort("Fixtures too small ({nrow(fixtures)} rows)")
  if (nrow(torp_df) < 100) cli::cli_abort("Ratings too small ({nrow(torp_df)} rows)")
  if (nrow(teams) < 100) cli::cli_abort("Teams too small ({nrow(teams)} rows)")

  cli::cli_h2("Building fixture features")
  fix_df <- .build_fixtures_df(fixtures)

  cli::cli_h2("Processing lineups")
  team_rt_df <- .build_team_ratings_df(teams, torp_df, psr_df)

  cli::cli_h2("Computing features")
  team_rt_fix_df <- .build_match_features(fix_df, team_rt_df, all_grounds)

  cli::cli_h2("Loading weather")
  weather_df <- .load_match_weather(fixtures, all_grounds, target_weeks, season, weather_path)

  # Weight anchor: most recent fixture date
  weight_anchor_date <- if (!is.null(target_weeks) && !is.null(season)) {
    target_fix <- fixtures |>
      dplyr::filter(season == .env$season, round_number %in% target_weeks)
    if (nrow(target_fix) > 0) as.Date(min(target_fix$utc_start_time)) else Sys.Date()
  } else {
    max(as.Date(fix_df$utc_start_time), na.rm = TRUE)
  }
  cli::cli_inform("Weight anchor date: {weight_anchor_date}")

  cli::cli_h2("Building model dataset")
  team_mdl_df <- .build_team_mdl_df(
    team_rt_fix_df, results, xg_df, weather_df, weight_anchor_date
  )

  team_mdl_df
}


# run_predictions_pipeline ----

#' Run weekly match predictions pipeline
#'
#' Average home/away rows into one match-level prediction
#'
#' Flips away-team predictions to the home-team perspective, then averages
#' home and (flipped) away predictions per match. Expects columns from
#' \code{.train_match_gams()} output (team_mdl_df).
#'
#' @param df Long-form team_mdl_df with both home and away rows per match
#' @return One-row-per-match tibble with averaged predictions from the home
#'   team perspective
#' @keywords internal
.format_match_preds <- function(df) {
  home <- df |>
    dplyr::filter(team_type_fac.x == "home") |>
    dplyr::select(
      season = season.x, round = round_number.x, players = count.x, match_id,
      home_team = team_name.x, home_rating = torp.x, home_psr = psr.x,
      away_team = team_name.y, away_rating = torp.y, away_psr = psr.y,
      pred_xtotal = pred_tot_xscore, pred_xmargin = pred_xscore_diff,
      pred_margin = pred_score_diff, pred_win, bits,
      margin = score_diff, start_time = local_start_time_str, venue = venue.x
    )
  away <- df |>
    dplyr::mutate(
      pred_xscore_diff = -pred_xscore_diff,
      pred_score_diff = -pred_score_diff,
      pred_win = 1 - pred_win,
      score_diff = -score_diff
    ) |>
    dplyr::filter(team_type_fac.x == "away") |>
    dplyr::select(
      season = season.x, round = round_number.x, players = count.x, match_id,
      home_team = team_name.y, home_rating = torp.y, home_psr = psr.y,
      away_team = team_name.x, away_rating = torp.x, away_psr = psr.x,
      pred_xtotal = pred_tot_xscore, pred_xmargin = pred_xscore_diff,
      pred_margin = pred_score_diff, pred_win, bits,
      margin = score_diff, start_time = local_start_time_str, venue = venue.x
    )
  dplyr::bind_rows(home, away) |>
    dplyr::group_by(season, round, match_id, home_team, home_rating, home_psr,
                    away_team, away_rating, away_psr, start_time, venue) |>
    dplyr::summarise(
      players = mean(players), pred_xtotal = mean(pred_xtotal),
      pred_margin = mean(pred_margin), pred_win = mean(pred_win),
      margin = mean(margin), .groups = "drop"
    ) |>
    dplyr::mutate(
      rating_diff = home_rating - away_rating,
      psr_diff = home_psr - away_psr
    ) |>
    dplyr::select(season, round, match_id:away_psr, start_time, venue,
                  rating_diff, psr_diff, players:margin)
}


#' Builds team_mdl_df with injury-adjusted ratings, trains 5 sequential GAMs,
#' generates predictions for target weeks, and uploads to torpdata releases.
#'
#' @param week Single target week (auto-detected if NULL)
#' @param weeks Vector of weeks, or "all" for all fixture weeks
#' @param season Season year (default: current via get_afl_season())
#' @return A list (invisibly) with:
#'   \item{predictions}{All match predictions across all seasons (season, round,
#'     providerId, home_team, away_team, pred_margin, pred_win, margin, etc.)}
#'   \item{models}{Named list of 5 GAM models: total_xpoints, xscore_diff,
#'     conv_diff, score_diff, win}
#' @keywords internal
run_predictions_pipeline <- function(week = NULL, weeks = NULL, season = NULL) {

  if (is.null(season)) season <- get_afl_season()

  if (!is.null(week) && !is.null(weeks)) {
    cli::cli_abort("Specify either {.arg week} or {.arg weeks}, not both")
  }
  if (is.null(week) && is.null(weeks)) week <- get_afl_week(type = "next")

  cli::cli_h1("Match Predictions Pipeline")
  .pipeline_start <- proc.time()

  # Load Data ----
  cli::cli_h2("Loading data")

  all_grounds <- file_reader("stadium_data", "reference-data")
  xg_df <- load_xg(TRUE)
  fixtures <- load_fixtures(TRUE)
  results <- load_results(TRUE)

  # Refresh current season results from AFL API
  tryCatch({
    cli::cli_progress_step("Refreshing {season} results from AFL API")
    fresh_results <- get_afl_results(season)
    if (!is.null(fresh_results) && nrow(fresh_results) > 0) {
      save_to_release(fresh_results, paste0("results_", season), "results-data")
      results <- load_results(TRUE)
      cli::cli_inform("Refreshed results: {nrow(fresh_results)} rows for {season}")
    }
  }, error = function(e) {
    cli::cli_warn("Could not refresh {season} results: {e$message}")
  })

  teams <- load_teams(TRUE)

  # Load TORP ratings with compute-from-scratch fallback
  torp_df_total <- tryCatch(load_torp_ratings(), error = function(e) {
    cli::cli_warn("Could not load TORP ratings from release: {e$message}")
    NULL
  })
  if (is.null(torp_df_total) || nrow(torp_df_total) < 100) {
    cli::cli_warn("TORP ratings unavailable or too small from release - computing from scratch (this may be slow)")
    torp_df_total <- tryCatch(
      calculate_torp_ratings(season_val = season, round_val = get_afl_week(type = "next")),
      error = function(e) {
        cli::cli_abort("Failed to compute TORP ratings from scratch: {e$message}")
      }
    )
  }

  cli::cli_inform("Loaded: fixtures={nrow(fixtures)}, results={nrow(results)}, teams={nrow(teams)}, ratings={nrow(torp_df_total)}")

  if (nrow(fixtures) < 100) cli::cli_abort("Fixtures too small ({nrow(fixtures)} rows)")
  if (nrow(torp_df_total) < 100) cli::cli_abort("Ratings too small ({nrow(torp_df_total)} rows)")
  if (nrow(teams) < 100) cli::cli_abort("Teams too small ({nrow(teams)} rows)")

  # Resolve target weeks
  is_backfill <- identical(weeks, "all")
  if (!is.null(weeks)) {
    if (is_backfill) {
      target_weeks <- sort(unique(fixtures$round_number[fixtures$season == season]))
    } else {
      target_weeks <- weeks
    }
  } else {
    target_weeks <- week
  }
  cli::cli_inform("Season: {season}, Week{?s}: {paste(target_weeks, collapse = ', ')}")

  # Weight anchor date (deterministic)
  target_fixtures <- fixtures |>
    dplyr::filter(season == .env$season, round_number %in% target_weeks)
  weight_anchor_date <- if (nrow(target_fixtures) > 0) {
    as.Date(min(target_fixtures$utc_start_time))
  } else {
    Sys.Date()
  }
  cli::cli_inform("Weight anchor date: {weight_anchor_date}")

  # Build Shared Data ----
  cli::cli_h2("Building fixture features")
  fix_df <- .build_fixtures_df(fixtures)

  cli::cli_h2("Loading PSR")
  psr_df <- NULL

  # Strategy 1: Load pre-computed PSR from torpdata releases

  tryCatch({
    psr_df <- load_psr(TRUE)
    if (nrow(psr_df) > 0) {
      cli::cli_inform("PSR loaded from release: {nrow(psr_df)} player-rounds")
    } else {
      psr_df <- NULL
    }
  }, error = function(e) {
    cli::cli_warn("Could not load PSR from release: {e$message}")
  })

  # Strategy 2: Compute from skills + coefficients
  if (is.null(psr_df)) {
    tryCatch({
      skills <- load_player_skills(TRUE)
      psr_coef_path <- system.file("extdata", "psr_v2_coefficients.csv", package = "torp")
      if (psr_coef_path == "") {
        psr_coef_path <- file.path(
          find.package("torp", quiet = TRUE)[1] %||% ".",
          "data-raw", "cache-skills", "psr_v2_coefficients.csv"
        )
      }
      if (file.exists(psr_coef_path)) {
        coef_df <- utils::read.csv(psr_coef_path)
        psr_df <- calculate_psr(skills, coef_df)
        cli::cli_inform("PSR computed from skills+coefficients: {nrow(psr_df)} player-rounds")
      } else {
        cli::cli_warn("PSR coefficient file not found: {psr_coef_path}")
      }
    }, error = function(e) {
      cli::cli_warn("Failed to compute PSR: {e$message}")
    })
  }

  if (is.null(psr_df)) {
    cli::cli_warn("PSR unavailable - predictions will proceed without PSR features")
  }

  cli::cli_h2("Processing lineups")
  team_rt_df <- .build_team_ratings_df(teams, torp_df_total, psr_df)

  cli::cli_h2("Computing features")
  team_rt_fix_df <- .build_match_features(fix_df, team_rt_df, all_grounds)

  # Injuries ----
  cli::cli_h2("Loading injuries")
  inj_df <- get_all_injuries(season)
  cli::cli_inform("Injuries loaded: {nrow(inj_df)} rows ({sum(inj_df$source == 'weekly', na.rm = TRUE)} weekly, {sum(inj_df$source == 'preseason', na.rm = TRUE)} preseason)")
  if (nrow(inj_df) == 0 && min(target_weeks) > 1) {
    cli::cli_warn("0 injuries loaded during active season - all players will be treated as available")
  }

  tr <- torp_ratings(season, min(target_weeks))
  if (nrow(tr) == 0 || !"player_name" %in% names(tr)) {
    cli::cli_alert_info("No TORP ratings available for {season} R{min(target_weeks)} (pre-season or fixtures not ready) - skipping predictions")
    elapsed <- (proc.time() - .pipeline_start)[["elapsed"]]
    cli::cli_inform("Pipeline aborted after {round(elapsed, 1)}s")
    return(invisible(NULL))
  }
  tr <- match_injuries(tr, inj_df) |>
    dplyr::mutate(estimated_return = tidyr::replace_na(estimated_return, "None"))

  # Join PSR to player-level ratings for injury-adjusted weekly summary
  if (!is.null(psr_df)) {
    tr <- tr |>
      dplyr::left_join(
        psr_df |> dplyr::select(player_id, season, round, psr),
        by = c("player_id", "season", "round")
      ) |>
      dplyr::mutate(psr = tidyr::replace_na(psr, 0))
  } else {
    tr$psr <- 0
  }

  tr_week <- tr |>
    dplyr::filter(!is.na(torp), is.na(injury)) |>
    dplyr::mutate(team_name = torp_replace_teams(team)) |>
    dplyr::group_by(team_name, season, round) |>
    dplyr::mutate(
      n_players = dplyr::n(),
      team_tog_sum = sum(pred_tog, na.rm = TRUE),
      tog_wt = dplyr::if_else(team_tog_sum > 0, pred_tog * 18 / team_tog_sum, 18 / n_players)
    ) |>
    dplyr::summarise(
      torp_week = sum(torp * tog_wt, na.rm = TRUE) * SIM_INJURY_DISCOUNT,
      torp_recv_week = sum(torp_recv * tog_wt, na.rm = TRUE) * SIM_INJURY_DISCOUNT,
      torp_disp_week = sum(torp_disp * tog_wt, na.rm = TRUE) * SIM_INJURY_DISCOUNT,
      torp_spoil_week = sum(torp_spoil * tog_wt, na.rm = TRUE) * SIM_INJURY_DISCOUNT,
      torp_hitout_week = sum(torp_hitout * tog_wt, na.rm = TRUE) * SIM_INJURY_DISCOUNT,
      psr_week = sum(psr * tog_wt, na.rm = TRUE) * SIM_INJURY_DISCOUNT,
      .groups = "drop"
    ) |>
    dplyr::arrange(-torp_week)

  # Expand estimated ratings across all target weeks
  if (length(target_weeks) > 1) {
    tr_week <- purrr::map_dfr(target_weeks, function(w) {
      tr_week |> dplyr::mutate(round = w)
    })
  }

  # Overlay injury-adjusted ratings
  team_rt_fix_df <- team_rt_fix_df |>
    dplyr::left_join(tr_week, by = c("team_name" = "team_name", "season" = "season", "round_number" = "round")) |>
    dplyr::mutate(
      torp = dplyr::coalesce(torp, torp_week),
      torp_recv = dplyr::coalesce(torp_recv, torp_recv_week),
      torp_disp = dplyr::coalesce(torp_disp, torp_disp_week),
      torp_spoil = dplyr::coalesce(torp_spoil, torp_spoil_week),
      torp_hitout = dplyr::coalesce(torp_hitout, torp_hitout_week),
      psr = dplyr::coalesce(psr, psr_week)
    )

  # Weather ----
  cli::cli_h2("Loading weather")
  weather_df <- .load_match_weather(fixtures, all_grounds, target_weeks, season)

  # Build Model Dataset ----
  cli::cli_h2("Building model dataset")
  team_mdl_df <- .build_team_mdl_df(team_rt_fix_df, results, xg_df, weather_df, weight_anchor_date)

  # Train GAMs & Predict ----
  cli::cli_h2("Training GAM models on completed matches")
  gam_result <- .train_match_gams(team_mdl_df)
  team_mdl_df <- gam_result$data

  # Train XGBoost & Blend ----
  cli::cli_h2("Training XGBoost models on completed matches")
  xgb_result <- .train_match_xgb(team_mdl_df)
  team_mdl_df <- xgb_result$data

  # 50/50 blend of GAM and XGBoost predictions
  team_mdl_df$pred_score_diff <- 0.5 * team_mdl_df$pred_score_diff + 0.5 * team_mdl_df$xgb_pred_score_diff
  team_mdl_df$pred_win <- 0.5 * team_mdl_df$pred_win + 0.5 * team_mdl_df$xgb_pred_win
  cli::cli_alert_success("Blended GAM + XGBoost predictions (50/50)")

  # Format Predictions ----
  cli::cli_h2("Generating predictions for {length(target_weeks)} week{?s}")

  # All matches (for analysis)
  all_preds <- .format_match_preds(team_mdl_df)

  # Target week predictions (for upload)
  week_gms <- all_preds |>
    dplyr::filter(season == .env$season, round %in% target_weeks) |>
    dplyr::select(-season)

  # Validate ----
  cli::cli_h2("Validating predictions")
  validation_errors <- character(0)

  if (nrow(week_gms) == 0) cli::cli_abort("No predictions generated for week{?s} {paste(target_weeks, collapse = ', ')}")
  if (any(is.na(week_gms$pred_win))) validation_errors <- c(validation_errors, "NA values in pred_win")
  if (any(week_gms$pred_win < 0 | week_gms$pred_win > 1)) validation_errors <- c(validation_errors, "pred_win values out of [0,1] range")
  if (any(is.na(week_gms$pred_margin))) validation_errors <- c(validation_errors, "NA values in pred_margin")

  # Margin and win probability must agree in direction
  margin_sign <- sign(week_gms$pred_margin)
  win_sign <- sign(week_gms$pred_win - 0.5)
  # Exclude near-zero margins (< 1 point) and near-50/50 win probs where sign can legitimately differ
  meaningful <- abs(week_gms$pred_margin) > 1 & abs(week_gms$pred_win - 0.5) > 0.02
  disagreements <- meaningful & margin_sign != win_sign
  if (any(disagreements)) {
    bad <- week_gms[disagreements, ]
    validation_errors <- c(validation_errors, paste0(
      "Margin/win probability direction disagreement for ", sum(disagreements), " match(es). ",
      "e.g. ", bad$home_team[1], " vs ", bad$away_team[1],
      " (", season, " R", bad$round[1], ", ", bad$match_id[1], ")",
      ": margin=", round(bad$pred_margin[1], 1), ", win=", round(bad$pred_win[1], 3)
    ))
  }

  # Total expected score should be in a plausible range (100-250 points)
  if (any(week_gms$pred_xtotal < 100 | week_gms$pred_xtotal > 250)) {
    bad_xt <- week_gms[week_gms$pred_xtotal < 100 | week_gms$pred_xtotal > 250, ]
    validation_errors <- c(validation_errors, paste0(
      "Implausible pred_xtotal for ", nrow(bad_xt), " match(es) outside 100-250 range. ",
      "e.g. ", bad_xt$home_team[1], " vs ", bad_xt$away_team[1],
      " (", season, " R", bad_xt$round[1], ", ", bad_xt$match_id[1], ")",
      ": pred_xtotal=", round(bad_xt$pred_xtotal[1], 1)
    ))
  }

  if (length(validation_errors) > 0) {
    if (interactive()) {
      cli::cli_warn(c("Prediction validation failed ({length(validation_errors)} issue{?s}):", validation_errors))
      cli::cli_alert_info("Returning models and data for debugging (predictions NOT uploaded)")
      return(invisible(list(
        predictions = all_preds,
        models = gam_result$models,
        xgb_models = xgb_result$models,
        model_data = team_mdl_df,
        validation_errors = validation_errors
      )))
    } else {
      cli::cli_abort(c("Prediction validation failed ({length(validation_errors)} issue{?s}):", validation_errors))
    }
  }

  cli::cli_alert_success("Validation passed: {nrow(week_gms)} matches")

  # Upload ----
  cli::cli_h2("Uploading predictions")
  week_gms <- week_gms |> dplyr::ungroup() |> dplyr::rename(week = round) |> dplyr::relocate(week)

  # --- Locked predictions: frozen at game start, never overwritten ---
  pred_file_name <- paste0("predictions_", season)
  existing <- tryCatch(
    file_reader(pred_file_name, "predictions"),
    error = function(e) {
      cli::cli_warn("Could not load existing predictions ({conditionMessage(e)}). Uploading current weeks only.")
      NULL
    }
  )

  if (!is.null(existing) && nrow(existing) > 0) {
    # Backfill actual margins for completed matches
    completed <- team_mdl_df |>
      dplyr::filter(!is.na(score_diff), team_type_fac.x == "home") |>
      dplyr::distinct(match_id, .keep_all = TRUE) |>
      dplyr::transmute(match_id, .actual_margin = score_diff)

    # Backward compat: existing predictions may use old providerId column
    if (!"match_id" %in% names(existing) && "providerId" %in% names(existing)) {
      data.table::setnames(existing, "providerId", "match_id")
    }

    n_backfilled <- sum(is.na(existing$margin) & existing$match_id %in% completed$match_id)
    if (n_backfilled > 0) {
      existing <- existing |>
        dplyr::left_join(completed, by = "match_id") |>
        dplyr::mutate(margin = dplyr::coalesce(margin, .actual_margin)) |>
        dplyr::select(-.actual_margin)
      cli::cli_alert_success("Backfilled {n_backfilled} match margin{?s} from results")
    }

    # Only replace predictions for games that haven't started yet
    started_ids <- team_mdl_df |>
      dplyr::filter(
        season.x == season,
        round_number.x %in% target_weeks,
        team_type_fac.x == "home",
        utc_dt <= Sys.time()
      ) |>
      dplyr::pull(match_id)

    week_gms <- week_gms |>
      dplyr::filter(!match_id %in% started_ids)

    if (length(started_ids) > 0) {
      cli::cli_alert_info("Keeping locked predictions for {length(started_ids)} already-started match{?es}")
    }

    combined <- existing |>
      dplyr::ungroup() |>
      dplyr::filter(!match_id %in% week_gms$match_id) |>
      dplyr::bind_rows(dplyr::ungroup(week_gms)) |>
      dplyr::arrange(week)
  } else {
    combined <- week_gms
  }

  tryCatch(
    {
      save_to_release(combined, pred_file_name, "predictions", also_csv = TRUE)
      cli::cli_alert_success("Uploaded locked predictions ({nrow(combined)} rows, week{?s} {paste(target_weeks, collapse = ', ')} updated)")
    },
    error = function(e) {
      local_path <- file.path("data-raw", paste0(pred_file_name, ".parquet"))
      arrow::write_parquet(combined, local_path)
      cli::cli_warn(c(
        "Failed to upload locked predictions: {conditionMessage(e)}",
        "i" = "Saved locally to {local_path}",
        "i" = "Check WORKFLOW_PAT if this is a permissions issue"
      ))
    }
  )

  # --- Retrodictions: current model on all matches, fully overwritten each run ---
  # Backfill actual margins for completed matches
  retro_completed <- team_mdl_df |>
    dplyr::filter(!is.na(score_diff), team_type_fac.x == "home") |>
    dplyr::distinct(match_id, .keep_all = TRUE) |>
    dplyr::transmute(match_id, .actual_margin = score_diff)

  retro_all <- all_preds |>
    dplyr::rename(week = round) |>
    dplyr::relocate(week)

  if (nrow(retro_completed) > 0) {
    retro_all <- retro_all |>
      dplyr::left_join(retro_completed, by = "match_id") |>
      dplyr::mutate(margin = dplyr::coalesce(.actual_margin, margin)) |>
      dplyr::select(-.actual_margin)
  }

  # Daily runs: current season only. Full backfill: all seasons (weeks = "all")
  # Daily runs: current season only. Full backfill when weeks = "all"
  retro_seasons <- if (is_backfill) sort(unique(retro_all$season)) else season
  retro_failures <- 0L
  for (retro_s in retro_seasons) {
    retro_preds <- retro_all |> dplyr::filter(season == retro_s)
    if (nrow(retro_preds) == 0) {
      cli::cli_warn("Skipping retrodictions_{retro_s}: 0 rows")
      next
    }
    retro_file_name <- paste0("retrodictions_", retro_s)
    tryCatch(
      {
        save_to_release(retro_preds, retro_file_name, "retrodictions", also_csv = TRUE)
        cli::cli_alert_success("Uploaded retrodictions_{retro_s} ({nrow(retro_preds)} rows)")
      },
      error = function(e) {
        retro_failures <<- retro_failures + 1L
        local_path <- file.path("data-raw", paste0(retro_file_name, ".parquet"))
        arrow::write_parquet(retro_preds, local_path)
        cli::cli_warn(c(
          "Failed to upload retrodictions_{retro_s}: {conditionMessage(e)}",
          "i" = "Saved locally to {local_path}"
        ))
      }
    )
  }
  if (retro_failures > 0) {
    cli::cli_warn("Retrodictions: {retro_failures}/{length(retro_seasons)} season(s) failed to upload")
  } else {
    cli::cli_alert_success("Retrodictions uploaded for {length(retro_seasons)} season{?s}")
  }

  elapsed <- (proc.time() - .pipeline_start)[["elapsed"]]
  cli::cli_h2("Pipeline Complete")
  cli::cli_inform("Total elapsed: {round(elapsed, 1)}s")

  invisible(list(
    predictions = all_preds,
    models = gam_result$models,
    xgb_models = xgb_result$models,
    model_data = team_mdl_df
  ))
}


#' Show TORP Match Predictions
#'
#' Display a formatted summary of TORP match predictions for a given round,
#' including predicted margins, win probabilities, and actual results where
#' available.
#'
#' @param season Season year (default: current via `get_afl_season()`)
#' @param week Round number (default: current via `get_afl_week()`)
#' @param refresh If `TRUE`, run `run_predictions_pipeline()` first
#' @return Predictions tibble with results joined (invisibly)
#' @export
#' @examples
#' \dontrun{
#' show_predictions()
#' show_predictions(2025, 10)
#' show_predictions(refresh = TRUE)
#' }
show_predictions <- function(season = get_afl_season(),
                             week = get_afl_week(),
                             refresh = FALSE) {
  if (isTRUE(refresh)) {
    run_predictions_pipeline(week = week, season = season)
  }

  preds <- tryCatch(
    load_predictions(season, week),
    error = function(e) {
      cli::cli_abort(c(
        "Could not load predictions for {season} Round {week}.",
        "i" = "Run {.code show_predictions(refresh = TRUE)} to generate them.",
        "x" = e$message
      ))
    }
  )

  if (is.null(preds) || nrow(preds) == 0) {
    cli::cli_alert_warning("No predictions found for {season} Round {week}")
    return(invisible(preds))
  }

  # Backfill actual margins for completed games missing results
  now <- Sys.time()
  game_duration_hrs <- 3.5
  preds$start_time_utc <- .parse_start_time(preds$start_time)
  preds$is_complete <- !is.na(preds$start_time_utc) &
    (now > preds$start_time_utc + game_duration_hrs * 3600)

  needs_results <- any(preds$is_complete & is.na(preds$margin))
  if (needs_results) {
    tryCatch({
      fresh <- get_afl_results(season)
      if (!is.null(fresh) && nrow(fresh) > 0) {
        result_margins <- fresh |>
          dplyr::transmute(
            match_id = match_id,
            .actual_margin = home_score - away_score
          )
        # Backward compat: preds may use old providerId column
        if (!"match_id" %in% names(preds) && "providerId" %in% names(preds)) {
          data.table::setnames(preds, "providerId", "match_id")
        }
        preds <- preds |>
          dplyr::left_join(result_margins, by = "match_id") |>
          dplyr::mutate(margin = dplyr::coalesce(margin, .actual_margin)) |>
          dplyr::select(-.actual_margin)
      }
    }, error = function(e) {
      cli::cli_warn("Could not fetch results to backfill: {e$message}")
    })
  }

  # Format and print
  cli::cli_h1("TORP Predictions: {season} Round {week}")

  # Column widths
  w_team <- 20
  header <- paste0(
    format("Home", width = w_team),
    format("Away", width = w_team),
    format("Pred", width = 8, justify = "right"),
    format("Win%", width = 8, justify = "right"),
    format("Result", width = 10, justify = "right")
  )
  cli::cli_text("{.strong {header}}")

  tips_correct <- 0
  tips_total <- 0
  abs_errors <- c()

  for (i in seq_len(nrow(preds))) {
    row <- preds[i, ]
    pred_str <- sprintf("%+.1f", row$pred_margin)
    win_pct <- sprintf("%.1f%%", row$pred_win * 100)

    if (!is.na(row$margin)) {
      result_str <- sprintf("%+.0f", row$margin)
      # Tip correct: predicted winner matches actual winner (same sign)
      tip_ok <- (row$pred_margin > 0 & row$margin > 0) |
        (row$pred_margin < 0 & row$margin < 0) |
        (row$margin == 0)  # draw counts as correct
      icon <- if (tip_ok) cli::col_green("\u2713") else cli::col_red("\u2717")
      result_display <- paste(result_str, icon)
      tips_total <- tips_total + 1
      tips_correct <- tips_correct + as.integer(tip_ok)
      abs_errors <- c(abs_errors, abs(row$pred_margin - row$margin))
    } else if (isTRUE(row$is_complete)) {
      result_display <- "?"
    } else {
      result_display <- "-"
    }

    line <- paste0(
      format(row$home_team, width = w_team),
      format(row$away_team, width = w_team),
      format(pred_str, width = 8, justify = "right"),
      format(win_pct, width = 8, justify = "right"),
      format(result_display, width = 10, justify = "right")
    )
    cli::cli_text(line)
  }

  # Summary
  cli::cli_text("")
  parts <- c()
  if (tips_total > 0) {
    parts <- c(parts, paste0("Tips: ", tips_correct, "/", tips_total, " correct"))
    parts <- c(parts, paste0("MAE: ", sprintf("%.1f", mean(abs_errors))))
  }
  n_complete <- sum(preds$is_complete | !is.na(preds$margin))
  parts <- c(parts, paste0("Completed: ", n_complete, "/", nrow(preds)))
  cli::cli_text(paste(parts, collapse = " | "))

  preds$start_time_utc <- NULL
  preds$is_complete <- NULL
  invisible(preds)
}


#' Parse start_time strings to POSIXct
#'
#' Handles the local-time formatted strings stored in predictions data.
#'
#' @param x Character vector of start time strings
#' @return POSIXct vector
#' @keywords internal
.parse_start_time <- function(x) {
  # start_time is formatted as "YYYY-MM-DD HH:MM:SS TZ"
  # Try parsing with timezone, fall back to UTC
  parsed <- suppressWarnings(lubridate::ymd_hms(x, tz = "Australia/Melbourne"))
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Melbourne"))
  }
  if (all(is.na(parsed)) && length(x) > 0 && !all(is.na(x))) {
    cli::cli_warn("Could not parse any start_time values. Example: {x[!is.na(x)][1]}")
  }
  parsed
}
