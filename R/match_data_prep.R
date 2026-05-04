# Match Data Preparation
# =====================
# Internal helpers that build the match model dataset: fixture pivoting,
# team rating aggregation, match-level features, weather, result normalisation,
# and the final self-join assembly (.build_team_mdl_df).
#
# Called by build_team_mdl_df() and run_predictions_pipeline() in match_model.R.

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
    dplyr::summarise(team_name = get_mode(home_team_name))

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
#' Joins lineups to EPR ratings, imputes missing with priors, applies
#' TOG weighting, generates all position columns, and aggregates to team level.
#'
#' @param teams Raw teams/lineups from load_teams()
#' @param torp_df EPR ratings from load_torp_ratings()
#' @return Team-level aggregated ratings with position columns
#' @keywords internal
.build_team_ratings_df <- function(teams, torp_df, psr_df = NULL) {
  torp_prior_total <- EPR_PRIOR_RATE_RECV + EPR_PRIOR_RATE_DISP +
    EPR_PRIOR_RATE_SPOIL + EPR_PRIOR_RATE_HITOUT

  # Drop PSR columns from torp_df to avoid collision with psr_df join later
  torp_df <- torp_df |> dplyr::select(-dplyr::any_of(c("psr", "osr", "dsr")))

  team_lineup_df <- teams |>
    dplyr::left_join(
      torp_df,
      by = c("player_id" = "player_id", "season" = "season", "round_number" = "round")
    ) |>
    dplyr::filter((lineup_position != "EMERG" & lineup_position != "SUB") | is.na(lineup_position))

  # Impute missing ratings with per-component priors
  na_torp_count <- sum(is.na(team_lineup_df$epr))
  if (na_torp_count > 0) {
    na_pct <- round(100 * na_torp_count / nrow(team_lineup_df), 1)
    cli::cli_inform("Replacing {na_torp_count} ({na_pct}%) NA EPR ratings with prior ({round(torp_prior_total, 2)})")
    if (na_pct > 25) cli::cli_warn("High proportion of missing ratings ({na_pct}%)")
    if (na_pct > 50) cli::cli_abort("More than 50% of ratings are missing ({na_pct}%) -- check data pipeline")
  }

  team_lineup_df <- team_lineup_df |>
    dplyr::mutate(
      epr = tidyr::replace_na(epr, torp_prior_total),
      recv_epr = tidyr::replace_na(recv_epr, EPR_PRIOR_RATE_RECV),
      disp_epr = tidyr::replace_na(disp_epr, EPR_PRIOR_RATE_DISP),
      spoil_epr = tidyr::replace_na(spoil_epr, EPR_PRIOR_RATE_SPOIL),
      hitout_epr = tidyr::replace_na(hitout_epr, EPR_PRIOR_RATE_HITOUT),
      lineup_tog = tidyr::replace_na(POSITION_AVG_TOG[lineup_position], POSITION_AVG_TOG_DEFAULT),
      .unknown_pos = !is.na(lineup_position) & is.na(POSITION_AVG_TOG[lineup_position]),
      epr = epr * lineup_tog,
      recv_epr = recv_epr * lineup_tog,
      disp_epr = disp_epr * lineup_tog,
      spoil_epr = spoil_epr * lineup_tog,
      hitout_epr = hitout_epr * lineup_tog
    )

  n_unknown <- sum(team_lineup_df$.unknown_pos, na.rm = TRUE)
  if (n_unknown > 0) {
    unknown_codes <- unique(team_lineup_df$lineup_position[team_lineup_df$.unknown_pos])
    cli::cli_warn("Unknown position code{?s} not in POSITION_AVG_TOG: {paste(unknown_codes, collapse = ', ')} ({n_unknown} row{?s} defaulted to TOG=0.75)")
  }
  team_lineup_df$.unknown_pos <- NULL

  # Join PSR if provided - use each player's most recent PSR value

  if (!is.null(psr_df)) {
    has_osr_dsr <- all(c("osr", "dsr") %in% names(psr_df))
    select_cols <- if (has_osr_dsr) c("player_id", "season", "round", "psr", "osr", "dsr") else c("player_id", "season", "round", "psr")

    latest_psr <- psr_df |>
      dplyr::select(dplyr::any_of(select_cols)) |>
      dplyr::arrange(player_id, season, round) |>
      dplyr::group_by(player_id) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup() |>
      dplyr::select(-season, -round)

    team_lineup_df <- team_lineup_df |>
      dplyr::left_join(latest_psr, by = "player_id") |>
      dplyr::mutate(
        psr = tidyr::replace_na(psr, PSR_PRIOR_RATE),
        psr = psr * lineup_tog
      )

    if (has_osr_dsr) {
      team_lineup_df <- team_lineup_df |>
        dplyr::mutate(
          osr = tidyr::replace_na(osr, PSR_PRIOR_RATE / 2),
          dsr = tidyr::replace_na(dsr, PSR_PRIOR_RATE / 2),
          osr = osr * lineup_tog,
          dsr = dsr * lineup_tog
        )
    }
  } else {
    team_lineup_df$psr <- PSR_PRIOR_RATE
  }

  # Generate position columns from lookup tables
  for (col in names(MATCH_PHASE_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$lineup_position %in% MATCH_PHASE_MAP[[col]], team_lineup_df$epr, NA)
  for (col in names(MATCH_POS_GROUP_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$lineup_position %in% MATCH_POS_GROUP_MAP[[col]], team_lineup_df$epr, NA)
  for (pos in MATCH_INDIVIDUAL_POS)
    team_lineup_df[[pos]] <- ifelse(team_lineup_df$lineup_position == pos, team_lineup_df$epr, NA)
  for (col in names(MATCH_COMBO_POS_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$lineup_position %in% MATCH_COMBO_POS_MAP[[col]], team_lineup_df$epr, NA)
  for (col in names(MATCH_LISTED_POS_MAP))
    team_lineup_df[[col]] <- ifelse(team_lineup_df$position_group %in% MATCH_LISTED_POS_MAP[[col]], team_lineup_df$epr, NA)
  team_lineup_df$other_pos <- ifelse(is.na(team_lineup_df$position_group), team_lineup_df$epr, NA)

  # Verify position columns exist
  missing_pos <- setdiff(MATCH_POS_COLS, names(team_lineup_df))
  if (length(missing_pos) > 0) {
    cli::cli_abort("Missing position columns after generation: {paste(missing_pos, collapse = ', ')}")
  }

  # Aggregate to team level
  torp_sum_cols <- c("epr", "recv_epr", "disp_epr", "spoil_epr", "hitout_epr", "psr")
  if ("osr" %in% names(team_lineup_df)) torp_sum_cols <- c(torp_sum_cols, "osr", "dsr")

  team_rt_df <- team_lineup_df |>
    dplyr::filter(!is.na(player_id)) |>
    dplyr::mutate(team_name_adj = team_name) |>
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
  torp_sum_cols <- c("epr", "recv_epr", "disp_epr", "spoil_epr", "hitout_epr", "psr")
  if ("osr" %in% names(team_rt_df)) torp_sum_cols <- c(torp_sum_cols, "osr", "dsr")

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
    dplyr::mutate(days_rest = tidyr::replace_na(days_rest, MATCH_DEFAULT_REST_DAYS))

  # Combine all features
  team_rt_fix_df <- fix_df |>
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
    tidyr::fill(epr, recv_epr, disp_epr, spoil_epr, hitout_epr, psr,
                dplyr::any_of(c("osr", "dsr"))) |>
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
#' @return Tibble with match_id, temp_avg, precipitation_total, wind_avg, humidity_avg, is_roof
#' @keywords internal
.load_match_weather <- function(fixtures, all_grounds, target_weeks = NULL,
                                season = NULL, ...) {
  empty_weather <- tibble::tibble(
    match_id = character(), temp_avg = numeric(), wind_avg = numeric(),
    humidity_avg = numeric(), precipitation_total = numeric(), is_roof = logical()
  )

  # Load from torpdata release
  historical <- tryCatch({
    load_weather()
  }, error = function(e) {
    cli::cli_warn("Could not load weather data: {conditionMessage(e)} -- skipping weather features")
    NULL
  })

  if (is.null(historical) || nrow(historical) == 0) {
    cli::cli_warn("Weather data unavailable -- skipping weather features")
    return(empty_weather)
  }
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
    Sys.sleep(OPEN_METEO_RATE_LIMIT_SECONDS)
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
      is_roof = dplyr::first(venue) %in% AFL_ROOF_VENUES,
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

  out <- if (has_cfs && has_fix) {
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

  # Guard against silent empty output (bind_rows(list()) yields a 0-col tibble when upstream flakes).
  if (!"match_id" %in% names(out) || nrow(out) == 0) {
    cli::cli_abort(c(
      "Results normalisation produced an invalid tibble.",
      "i" = "Input: {nrow(results)} row{?s}, cols: {paste(names(results), collapse = ', ')}",
      "i" = "Output: {nrow(out)} row{?s}, cols: {paste(names(out), collapse = ', ')}",
      "!" = "Check load_results() - likely a transient upstream failure."
    ))
  }

  out
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
    "epr", "recv_epr", "disp_epr", "spoil_epr", "hitout_epr", "psr",
    intersect(c("osr", "dsr"), names(team_rt_fix_df)),
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
  na_opp <- sum(is.na(team_mdl_df_tot$epr.y))
  if (na_opp > 0) {
    bad_ids <- unique(team_mdl_df_tot$match_id[is.na(team_mdl_df_tot$epr.y)])
    cli::cli_warn("{na_opp} row{?s} have no opponent data after self-join. Affected matches: {paste(utils::head(bad_ids, 5), collapse = ', ')}")
  }

  team_mdl_df_tot <- team_mdl_df_tot |>
    dplyr::mutate(
      epr_diff = epr.x - epr.y,
      epr_ratio = log(pmax(epr.x, 0.01) / pmax(epr.y, 0.01)),
      epr_recv_diff = recv_epr.x - recv_epr.y,
      epr_disp_diff = disp_epr.x - disp_epr.y,
      epr_spoil_diff = spoil_epr.x - spoil_epr.y,
      epr_hitout_diff = hitout_epr.x - hitout_epr.y,
      psr_diff = psr.x - psr.y,
      osr_diff = if ("osr.x" %in% names(team_mdl_df_tot)) osr.x - osr.y else NA_real_,
      dsr_diff = if ("dsr.x" %in% names(team_mdl_df_tot)) dsr.x - dsr.y else NA_real_,
      torp.x = TORP_EPR_WEIGHT * epr.x + (1 - TORP_EPR_WEIGHT) * psr.x,
      torp.y = TORP_EPR_WEIGHT * epr.y + (1 - TORP_EPR_WEIGHT) * psr.y,
      torp_diff = torp.x - torp.y
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
