# Daily Data Release Script for TORP Package
#
# This script is optimized for daily automated runs via GitHub Actions.
# Key features:
# - Only updates current season data (skips historical)
# - Detects if new games have been played before processing
# - Creates/updates aggregated seasonal files for faster bulk loads
# - Modular functions for each data type
# - Comprehensive logging and error handling
#
# Usage:
#   Rscript data-raw/01-data/daily_release.R
#   Or: source("data-raw/01-data/daily_release.R"); run_daily_release()

# Setup ----

library(dplyr)
library(glue)
library(cli)
library(piggyback)

devtools::load_all()

# Configuration ----

#' Get TORP data repository name
get_torpdata_repo <- function() {
 "peteowen1/torpdata"
}

# Helper Functions ----

#' Check if New Games Have Been Played
#'
#' Compares current fixtures against the last completed round data.
#' Returns TRUE if there are new completed games since the last release.
#'
#' @return Logical indicating if new games are available
has_new_games <- function() {
  tryCatch({
    current_season <- get_afl_season()
    current_round <- get_afl_week()

    # Get fixtures for current season
    fixtures <- get_afl_fixtures(current_season)

    if (nrow(fixtures) == 0) {
      cli::cli_inform("No fixtures found for season {current_season}")
      return(FALSE)
    }

    # Check if any games have been completed in the current round
    time_aest <- lubridate::with_tz(Sys.time(), tzone = "Australia/Brisbane")
    completed_games <- fixtures %>%
      dplyr::filter(
        .data$round_number == current_round,
        .data$utc_start_time < time_aest
      )

    if (nrow(completed_games) > 0) {
      cli::cli_inform("Found {nrow(completed_games)} completed game(s) in round {current_round}")
      return(TRUE)
    }

    cli::cli_inform("No completed games found in round {current_round}")
    return(FALSE)
  }, error = function(e) {
    cli::cli_alert_danger("Error checking for new games: {conditionMessage(e)}")
    # Default to FALSE to avoid releasing stale/partial data when API is down
    return(FALSE)
  })
}

# Module-level cache to avoid double-fetching lineup data within a single run
.release_cache <- new.env(parent = emptyenv())

#' Check if New Team/Lineup Data Exists
#'
#' Compares current lineup data from AFL API against the existing torpdata
#' release. Detects both new rows (new round lineups) and changed rows
#' (player swaps within an existing round).
#'
#' Caches fetched lineup data in `.release_cache` so `update_teams()` can
#' reuse it without a second API call.
#'
#' @return Logical indicating if new or changed lineup data is available
has_new_team_data <- function() {
  tryCatch({
    current_season <- get_afl_season()

    # Fetch fresh lineup data from API
    fresh_teams <- get_afl_lineups(current_season)

    if (is.null(fresh_teams) || nrow(fresh_teams) == 0) {
      cli::cli_inform("No lineup data available for {current_season}")
      return(FALSE)
    }

    # Cache for reuse by update_teams()
    .release_cache$teams <- fresh_teams

    # Compare against existing torpdata release
    existing_teams <- tryCatch({
      load_teams(current_season)
    }, error = function(e) {
      cli::cli_inform("No existing teams data found — treating as new")
      return(NULL)
    })

    if (is.null(existing_teams) || nrow(existing_teams) == 0) {
      cli::cli_inform("No existing teams data — new lineup data available")
      return(TRUE)
    }

    # Check row count difference (new round lineups added)
    if (nrow(fresh_teams) != nrow(existing_teams)) {
      cli::cli_inform(
        "Team data row count changed: {nrow(existing_teams)} -> {nrow(fresh_teams)}"
      )
      return(TRUE)
    }

    # Check for player swaps (same row count but different row_ids)
    fresh_ids <- sort(fresh_teams$row_id)
    existing_ids <- sort(existing_teams$row_id)
    if (!identical(fresh_ids, existing_ids)) {
      n_diff <- sum(!fresh_ids %in% existing_ids)
      cli::cli_inform("Team data has {n_diff} changed row_id(s) (player swaps)")
      return(TRUE)
    }

    cli::cli_inform("No changes in team/lineup data")
    return(FALSE)
  }, error = function(e) {
    cli::cli_alert_danger("Error checking for new team data: {conditionMessage(e)}")
    return(FALSE)
  })
}

#' Get Maximum Round for a Season
#'
#' Returns the maximum round number to process for a given season.
#'
#' @param season Season year
#' @return Integer round number
get_max_round <- function(season) {
  current_season <- get_afl_season()
  if (season == current_season) {
    return(get_afl_week())
  }
  # Historical seasons have up to round 28 (including finals)
  return(28)
}

#' Get Starting Round for a Season
#'
#' Returns the starting round number for a season (0 for 2024+, 1 for earlier).
#'
#' @param season Season year
#' @return Integer round number
get_start_round <- function(season) {
  if (season >= 2024) {
    return(0)
  }
  return(1)
}

# Seasonal Data Update Functions (chains + PBP) ----
# All data stored as _all files (one per season). New rounds are merged into
# the existing _all file and re-uploaded.

#' Update Chains Data for Current Season
#'
#' Fetches new round chains, merges into existing _all file, and re-uploads.
#'
#' @param season Season year
#' @param round Round number
#' @return Invisible NULL
update_season_chains <- function(season, round) {
  cli::cli_h2("Updating chains_data_{season}_all with round {round}")

  # Fetch new round data from AFL API
  new_chains <- tryCatch({
    get_week_chains(season, round)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch chains for {season} R{round}: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(new_chains) || nrow(new_chains) == 0) {
    cli::cli_inform("No chains data for {season} round {round}")
    return(invisible(NULL))
  }

  cli::cli_inform("Fetched {nrow(new_chains)} new chain rows for round {round}")

  # Load existing _all file
  existing <- tryCatch({
    file_reader(glue::glue("chains_data_{season}_all"), "chains-data")
  }, error = function(e) {
    cli::cli_inform("No existing _all file for {season} - creating fresh")
    NULL
  })

  if (!is.null(existing) && nrow(existing) > 0) {
    # Remove stale data for this round (handles re-runs)
    # Normalise existing chains columns (old parquets may use camelCase)
    existing <- data.table::as.data.table(existing)
    .normalise_chains_columns(existing)
    round_col <- if ("round_number" %in% names(existing)) {
      "round_number"
    } else {
      NULL
    }
    if (!is.null(round_col)) {
      existing <- existing[existing[[round_col]] != round, ]
    } else {
      cli::cli_warn("No round column found in existing chains data - cannot de-duplicate round {round}")
    }
    cli::cli_inform("Existing data: {nrow(existing)} rows (after removing round {round})")
  }

  # Normalise new chains to match existing schema
  new_chains <- data.table::as.data.table(new_chains)
  .normalise_chains_columns(new_chains)

  # Combine
  combined <- data.table::rbindlist(
    list(existing, new_chains),
    use.names = TRUE, fill = TRUE
  )

  file_name <- glue::glue("chains_data_{season}_all")
  save_to_release(df = combined, file_name = file_name, release_tag = "chains-data")

  cli::cli_alert_success("Saved {file_name} ({nrow(combined)} rows)")
  invisible(NULL)
}

#' Update PBP Data for Current Season
#'
#' Fetches new round chains, processes into PBP, merges into existing _all file.
#'
#' @param season Season year
#' @param round Round number
#' @return Invisible NULL
update_season_pbp <- function(season, round) {
  cli::cli_h2("Updating pbp_data_{season}_all with round {round}")

  # Fetch new round chains and process into PBP
  new_pbp <- tryCatch({
    chains <- get_week_chains(season, round)
    if (is.null(chains) || nrow(chains) == 0) return(NULL)
    chains |>
      clean_pbp() |>
      clean_model_data_epv() |>
      clean_shots_data() |>
      add_shot_vars() |>
      add_epv_vars() |>
      clean_model_data_wp() |>
      add_wp_vars()
  }, error = function(e) {
    cli::cli_alert_danger("Failed to process PBP for {season} R{round}: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(new_pbp) || nrow(new_pbp) == 0) {
    cli::cli_inform("No PBP data for {season} round {round}")
    return(invisible(NULL))
  }

  cli::cli_inform("Processed {nrow(new_pbp)} new PBP rows for round {round}")

  # Load existing _all file
  existing <- tryCatch({
    file_reader(glue::glue("pbp_data_{season}_all"), "pbp-data")
  }, error = function(e) {
    cli::cli_inform("No existing _all file for {season} - creating fresh")
    NULL
  })

  if (!is.null(existing) && nrow(existing) > 0) {
    existing <- data.table::as.data.table(existing)
    existing <- existing[round_number != round]
    cli::cli_inform("Existing data: {nrow(existing)} rows (after removing round {round})")
  }

  # Combine
  combined <- data.table::rbindlist(
    list(existing, new_pbp),
    use.names = TRUE, fill = TRUE
  )

  file_name <- glue::glue("pbp_data_{season}_all")
  save_to_release(df = combined, file_name = file_name, release_tag = "pbp-data")

  cli::cli_alert_success("Saved {file_name} ({nrow(combined)} rows)")
  invisible(NULL)
}

# Season-Level Data Functions ----

#' Update XG Data
#'
#' @param season Season year
#' @return Invisible NULL
update_xg_data <- function(season) {
  cli::cli_progress_step("Updating xG data for {season}")

  xg_df <- tryCatch({
    match_xgs(season, TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to generate xG data: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(xg_df) || nrow(xg_df) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("xg_data_{season}")
  save_to_release(df = xg_df, file_name = file_name, release_tag = "xg-data")

  cli::cli_inform("Saved xG: {file_name} ({nrow(xg_df)} rows)")
  invisible(NULL)
}

#' Update Fixtures Data
#'
#' @param season Season year
#' @return Invisible NULL
update_fixtures <- function(season) {
  cli::cli_progress_step("Updating fixtures for {season}")

  fixtures <- tryCatch({
    get_afl_fixtures(season)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch fixtures: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(fixtures) || nrow(fixtures) == 0) {
    return(invisible(NULL))
  }

  # Drop list-columns (nested structs from API) that arrow can't serialize
  list_cols <- names(fixtures)[vapply(fixtures, is.list, logical(1))]
  if (length(list_cols) > 0) {
    fixtures <- fixtures[, !names(fixtures) %in% list_cols, drop = FALSE]
  }

  file_name <- glue::glue("fixtures_{season}")
  save_to_release(df = fixtures, file_name = file_name, release_tag = "fixtures-data")

  cli::cli_inform("Saved fixtures: {file_name} ({nrow(fixtures)} rows)")
  invisible(NULL)
}

#' Update Results Data
#'
#' @param season Season year
#' @return Invisible NULL
update_results <- function(season) {
  cli::cli_progress_step("Updating results for {season}")

  results <- tryCatch({
    get_afl_results(season)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch results: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(results) || nrow(results) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("results_{season}")
  save_to_release(df = results, file_name = file_name, release_tag = "results-data")

  cli::cli_inform("Saved results: {file_name} ({nrow(results)} rows)")
  invisible(NULL)
}

#' Update Player Stats Data
#'
#' @param season Season year
#' @return Invisible NULL
update_player_stats <- function(season) {
  cli::cli_progress_step("Updating player stats for {season}")

  player_stats <- tryCatch({
    get_afl_player_stats(season) |>
      dplyr::select(where(~ dplyr::n_distinct(.) > 1)) |>
      janitor::clean_names()
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch player stats: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(player_stats) || nrow(player_stats) == 0) {
    return(invisible(NULL))
  }

  # Normalise column names (strips v2 stats_ prefix, renames player/match IDs)
  player_stats <- .normalise_player_stats_columns(player_stats)

  # Ensure season column exists
  if (!"season" %in% names(player_stats)) {
    player_stats$season <- as.integer(season)
  }

  file_name <- glue::glue("player_stats_{season}")
  save_to_release(df = player_stats, file_name = file_name, release_tag = "player_stats-data")

  cli::cli_inform("Saved player stats: {file_name} ({nrow(player_stats)} rows)")
  invisible(NULL)
}

#' Update Teams/Lineups Data
#'
#' @param season Season year
#' @return Invisible NULL
update_teams <- function(season) {
  cli::cli_progress_step("Updating teams/lineups for {season}")

  # Reuse cached data from has_new_team_data() if available
  teams <- if (!is.null(.release_cache$teams)) {
    cli::cli_inform("Using cached lineup data")
    .release_cache$teams
  } else {
    tryCatch({
      get_afl_lineups(season)
    }, error = function(e) {
      cli::cli_alert_danger("Failed to fetch teams: {conditionMessage(e)}")
      return(NULL)
    })
  }

  if (is.null(teams) || nrow(teams) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("teams_{season}")
  save_to_release(df = teams, file_name = file_name, release_tag = "teams-data")

  cli::cli_inform("Saved teams: {file_name} ({nrow(teams)} rows)")
  invisible(NULL)
}

#' Update Player Game Data
#'
#' Rebuilds processed player game data from PBP, player stats, and teams data.
#'
#' @param season Season year
#' @return Invisible NULL
update_player_game_data <- function(season) {
  cli::cli_progress_step("Updating player game data for {season}")

  pgd <- tryCatch({
    pbp <- load_pbp(season, rounds = TRUE)
    pstats <- load_player_stats(season)
    teams_data <- load_teams(season)
    create_player_game_data(pbp, pstats, teams_data)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to create player game data: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(pgd) || nrow(pgd) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("player_game_{season}")
  save_to_release(df = pgd, file_name = file_name, release_tag = "player_game-data")

  cli::cli_inform("Saved player game data: {file_name} ({nrow(pgd)} rows)")
  invisible(NULL)
}

#' Update Player Details Data
#'
#' @param season Season year
#' @return Invisible NULL
update_player_details <- function(season) {
  cli::cli_progress_step("Updating player details for {season}")

  player_details <- tryCatch({
    get_afl_player_details(season)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch player details: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(player_details) || nrow(player_details) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("player_details_{season}")
  save_to_release(df = player_details, file_name = file_name, release_tag = "player_details-data")

  cli::cli_inform("Saved player details: {file_name} ({nrow(player_details)} rows)")
  invisible(NULL)
}

# Derived Data Functions ----
# These run after upstream data is updated, computing derived datasets
# from already-released data (PBP, player_game_data, etc.)

#' Update Player Game Ratings
#'
#' Computes per-game TORP ratings for a season and releases to torpdata.
#'
#' @param season Season year
#' @return Invisible NULL
update_player_game_ratings <- function(season) {
  cli::cli_progress_step("Updating player game ratings for {season}")

  ratings <- tryCatch({
    pgd <- load_player_game_data(season)
    start_round <- get_start_round(season)
    max_round <- get_max_round(season)
    .compute_player_game_ratings(pgd, season, start_round:max_round)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to compute player game ratings: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(ratings) || nrow(ratings) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("player_game_ratings_{season}")
  save_to_release(df = ratings, file_name = file_name, release_tag = "player_game_ratings-data")

  cli::cli_inform("Saved player game ratings: {file_name} ({nrow(ratings)} rows)")
  invisible(NULL)
}

#' Update Player Season Ratings
#'
#' Computes season-total TORP ratings for a season and releases to torpdata.
#'
#' @param season Season year
#' @return Invisible NULL
update_player_season_ratings <- function(season) {
  cli::cli_progress_step("Updating player season ratings for {season}")

  ratings <- tryCatch({
    pgd <- load_player_game_data(season)
    start_round <- get_start_round(season)
    max_round <- get_max_round(season)
    pgr <- .compute_player_game_ratings(pgd, season, start_round:max_round)
    .compute_player_season_ratings(pgr)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to compute player season ratings: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(ratings) || nrow(ratings) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("player_season_ratings_{season}")
  save_to_release(df = ratings, file_name = file_name, release_tag = "player_season_ratings-data")

  cli::cli_inform("Saved player season ratings: {file_name} ({nrow(ratings)} rows)")
  invisible(NULL)
}

#' Update PSR (Player Skill Ratings)
#'
#' Computes PSR from player skills + coefficients and releases to torpdata.
#'
#' @param season Season year
#' @return Invisible NULL
update_psr <- function(season) {
  cli::cli_progress_step("Updating PSR for {season}")

  psr_data <- tryCatch({
    skills <- load_player_skills(season)
    if (nrow(skills) == 0) return(NULL)

    psr_coef_path <- file.path("data-raw", "cache-skills", "psr_v2_coefficients.csv")
    if (!file.exists(psr_coef_path)) {
      psr_coef_path <- system.file("extdata", "psr_v2_coefficients.csv", package = "torp")
    }
    if (!file.exists(psr_coef_path) || nchar(psr_coef_path) == 0) {
      cli::cli_warn("PSR coefficient file not found - skipping PSR update")
      return(NULL)
    }

    coef_df <- utils::read.csv(psr_coef_path)
    calculate_psr(skills, coef_df)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to compute PSR: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(psr_data) || nrow(psr_data) == 0) {
    return(invisible(NULL))
  }

  file_name <- paste0("psr_", season)
  save_to_release(df = psr_data, file_name = file_name, release_tag = "psr-data")

  cli::cli_inform("Saved PSR: {file_name} ({nrow(psr_data)} rows)")
  invisible(NULL)
}

#' Update EP/WP Chart Data
#'
#' Selects charting-relevant columns from PBP and releases as a lightweight file.
#'
#' @param season Season year
#' @return Invisible NULL
update_ep_wp_chart <- function(season) {
  cli::cli_progress_step("Updating EP/WP chart data for {season}")

  chart_data <- tryCatch({
    pbp <- load_pbp(season, rounds = TRUE)
    if (nrow(pbp) == 0) return(NULL)

    chart_cols <- c(
      "match_id", "season", "round_number", "period", "period_seconds",
      "total_seconds", "display_order",
      "home_team_name", "away_team_name", "team", "home",
      "pos_team_points", "opp_team_points", "points_diff",
      "home_points", "away_points",
      "exp_pts", "delta_epv", "wp", "wpa",
      "description", "player_name", "play_type",
      "shot_row", "points_shot"
    )

    # Only keep columns that exist in the data
    available_cols <- intersect(chart_cols, names(pbp))
    missing_cols <- setdiff(chart_cols, names(pbp))
    if (length(missing_cols) > 0) {
      cli::cli_warn("EP/WP chart missing {length(missing_cols)} expected column{?s}: {.val {missing_cols}}")
    }
    pbp[, available_cols, drop = FALSE]
  }, error = function(e) {
    cli::cli_alert_danger("Failed to create EP/WP chart data: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(chart_data) || nrow(chart_data) == 0) {
    return(invisible(NULL))
  }

  file_name <- glue::glue("ep_wp_chart_{season}_all")
  save_to_release(df = chart_data, file_name = file_name, release_tag = "ep_wp_chart-data")

  cli::cli_inform("Saved EP/WP chart: {file_name} ({nrow(chart_data)} rows, {ncol(chart_data)} cols)")
  invisible(NULL)
}

#' Update Weather Data
#'
#' Fetches historical weather for recently completed matches not yet in the
#' weather release, merges with existing data, and re-uploads.
#'
#' @return Invisible NULL
#' @keywords internal
update_weather <- function() {
  cli::cli_progress_step("Updating weather data")

  # Load existing weather from release
  existing <- tryCatch(load_weather(), error = function(e) {
    cli::cli_warn("Could not load existing weather: {conditionMessage(e)}")
    NULL
  })
  existing_ids <- if (!is.null(existing)) existing$match_id else character()

  # Find completed matches missing weather
  fixtures <- load_fixtures(all = TRUE) |>
    dplyr::filter(!is.na(utc_start_time)) |>
    dplyr::mutate(
      venue = torp_replace_venues(venue_name),
      utc_start_time = as.POSIXct(utc_start_time, format = "%Y-%m-%dT%H:%M", tz = "UTC")
    ) |>
    dplyr::filter(
      !is.na(utc_start_time),
      utc_start_time < Sys.time(),
      !match_id %in% existing_ids
    )

  if (nrow(fixtures) == 0) {
    cli::cli_alert_info("Weather already up to date - no new matches to backfill")
    return(invisible(NULL))
  }

  cli::cli_inform("Fetching weather for {nrow(fixtures)} new match{?es}")

  all_grounds <- file_reader("stadium_data", "reference-data") |>
    dplyr::select(venue, Latitude, Longitude) |>
    dplyr::distinct(venue, .keep_all = TRUE)

  fixtures_geo <- fixtures |>
    dplyr::select(match_id, season, round_number,
                  venue_name, venue, venue_timezone, utc_start_time,
                  home_team_name, away_team_name) |>
    dplyr::left_join(all_grounds, by = "venue") |>
    dplyr::mutate(match_date = as.Date(utc_start_time))

  no_geo <- fixtures_geo |> dplyr::filter(is.na(Latitude))
  if (nrow(no_geo) > 0) {
    missing_venues <- unique(no_geo$venue)
    cli::cli_warn("Skipping {nrow(no_geo)} match{?es} at unmapped venue{?s}: {paste(missing_venues, collapse = ', ')}")
  }
  fixtures_geo <- fixtures_geo |> dplyr::filter(!is.na(Latitude))

  # Fetch per venue batch
  venue_groups <- fixtures_geo |>
    dplyr::group_by(venue, Latitude, Longitude) |>
    dplyr::summarise(
      min_date = min(match_date), max_date = max(match_date),
      n_matches = dplyr::n(), .groups = "drop"
    )

  all_hourly <- list()
  for (i in seq_len(nrow(venue_groups))) {
    vg <- venue_groups[i, ]
    hourly <- tryCatch({
      resp <- httr2::request("https://archive-api.open-meteo.com/v1/archive") |>
        httr2::req_url_query(
          latitude = vg$Latitude, longitude = vg$Longitude,
          start_date = format(vg$min_date, "%Y-%m-%d"),
          end_date = format(vg$max_date, "%Y-%m-%d"),
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
        venue = vg$venue
      )
    }, error = function(e) {
      cli::cli_warn("Weather fetch failed for {vg$venue}: {conditionMessage(e)}")
      NULL
    })
    if (!is.null(hourly)) all_hourly[[i]] <- hourly
    Sys.sleep(0.5)
  }

  if (length(all_hourly) == 0) {
    cli::cli_warn("All weather fetches failed - skipping update")
    return(invisible(NULL))
  }

  hourly_df <- dplyr::bind_rows(all_hourly)

  # Aggregate to match-level (3hr window from kickoff)
  new_weather <- fixtures_geo |>
    dplyr::mutate(kickoff_utc = utc_start_time) |>
    dplyr::left_join(hourly_df, by = "venue", relationship = "many-to-many") |>
    dplyr::filter(time >= kickoff_utc, time < kickoff_utc + lubridate::hours(3)) |>
    dplyr::group_by(match_id, season, round_number,
                    venue_name, venue, home_team_name, away_team_name,
                    Latitude, Longitude, kickoff_utc) |>
    dplyr::summarise(
      temp_avg = mean(temperature_2m, na.rm = TRUE),
      precipitation_total = sum(precipitation, na.rm = TRUE),
      wind_avg = mean(wind_speed_10m, na.rm = TRUE),
      humidity_avg = mean(relative_humidity_2m, na.rm = TRUE),
      weather_hours = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      is_rain = precipitation_total > 0.5,
      is_roof = venue == "Docklands"
    )

  # Warn if matches had no hourly data in the kickoff window
  n_no_weather <- nrow(fixtures_geo) - length(unique(new_weather$match_id))
  if (n_no_weather > 0) {
    cli::cli_warn("{n_no_weather} match{?es} had no hourly weather data in the 3hr kickoff window")
  }

  # Merge with existing and upload
  combined <- dplyr::bind_rows(existing, new_weather) |>
    dplyr::distinct(match_id, .keep_all = TRUE) |>
    dplyr::arrange(season, round_number)

  save_to_release(combined, "weather_data", "weather-data")
  cli::cli_alert_success("Weather updated: {nrow(new_weather)} new + {length(existing_ids)} existing = {nrow(combined)} total")
  invisible(NULL)
}

# Main Entry Point ----

#' Run Daily Data Release
#'
#' Update Injury Data
#'
#' Scrapes the current AFL injury list, parses return rounds, and saves
#' a timestamped snapshot to the torpdata injury-data release.
#'
#' @param season Numeric season year.
#' @keywords internal
update_injury_data <- function(season) {
  cli::cli_progress_step("Updating injury data for {season}")
  inj <- get_all_injuries(season)
  if (nrow(inj) == 0) {
    cli::cli_alert_info("No injuries scraped - skipping save")
    return(invisible(NULL))
  }
  current_round <- get_afl_week()
  inj$return_round <- parse_return_round(inj$estimated_return, season, current_round)
  save_injury_data(inj, season)
  cli::cli_alert_success("Saved injury snapshot: {nrow(inj)} injuries")
  invisible(inj)
}


#' Main entry point for daily automated data release.
#' Only processes current season data to minimize runtime.
#'
#' Returns a release mode string:
#' - `"full"`: new games detected (or forced) — all data types updated
#' - `"team_only"`: no new games but lineup changes detected — only teams,
#'   fixtures, results, and player_details updated
#' - `"none"`: nothing new to release
#'
#' @param force Logical. If TRUE, skip the new games check and force full update.
#' @return Character string: `"full"`, `"team_only"`, or `"none"` (invisibly)
#' @export
run_daily_release <- function(force = FALSE) {
  cli::cli_h1("Daily Data Release")

  # Ensure cache is cleaned up on exit (even on error)
  on.exit({
    rm(list = ls(.release_cache), envir = .release_cache)
  }, add = TRUE)

  tictoc::tic("total")

  current_season <- get_afl_season()
  current_round <- get_afl_week()

  cli::cli_inform("Season: {current_season}, Round: {current_round}")

  # Determine release mode
  if (force || has_new_games()) {
    release_mode <- "full"
  } else if (has_new_team_data()) {
    release_mode <- "team_only"
  } else {
    cli::cli_alert_info("No new games or lineup changes detected - skipping release")
    tictoc::toc(log = TRUE)
    tictoc::tic.clearlog()
    return(invisible("none"))
  }

  cli::cli_alert_info("Release mode: {release_mode}")

  if (release_mode == "full") {
    # -----------------------------------------------------------------------
    # 1. Update seasonal _all files (chains + pbp) with current round
    # -----------------------------------------------------------------------
    cli::cli_h2("Updating season files with round {current_round}")
    tictoc::tic("season_data")

    update_season_chains(current_season, current_round)
    update_season_pbp(current_season, current_round)

    tictoc::toc(log = TRUE)
  }

  # -------------------------------------------------------------------------
  # 2. Update season-level data files
  # -------------------------------------------------------------------------
  cli::cli_h2("Updating seasonal data")
  tictoc::tic("seasonal_data")

  seasonal_failures <- character()

  if (release_mode == "full") {
    seasonal_fns <- list(
      xg_data = update_xg_data,
      fixtures = update_fixtures,
      results = update_results,
      player_stats = update_player_stats,
      teams = update_teams,
      player_details = update_player_details,
      player_game_data = update_player_game_data
    )
  } else {
    # team_only: skip heavy data types that haven't changed
    seasonal_fns <- list(
      teams = update_teams,
      fixtures = update_fixtures,
      results = update_results,
      player_details = update_player_details
    )
  }

  for (nm in names(seasonal_fns)) {
    tryCatch(seasonal_fns[[nm]](current_season), error = function(e) {
      seasonal_failures <<- c(seasonal_failures, nm)
      cli::cli_alert_danger("Failed: {nm}: {conditionMessage(e)}")
    })
  }

  tictoc::toc(log = TRUE)

  # -------------------------------------------------------------------------
  # 3. Update derived data (full mode only)
  # -------------------------------------------------------------------------
  derived_failures <- character()

  if (release_mode == "full") {
    cli::cli_h2("Updating derived data")
    tictoc::tic("derived_data")

    tryCatch(update_player_game_ratings(current_season), error = function(e) {
      derived_failures <<- c(derived_failures, "player_game_ratings")
      cli::cli_alert_danger("Failed: player_game_ratings: {conditionMessage(e)}")
    })
    tryCatch(update_player_season_ratings(current_season), error = function(e) {
      derived_failures <<- c(derived_failures, "player_season_ratings")
      cli::cli_alert_danger("Failed: player_season_ratings: {conditionMessage(e)}")
    })
    tryCatch(update_ep_wp_chart(current_season), error = function(e) {
      derived_failures <<- c(derived_failures, "ep_wp_chart")
      cli::cli_alert_danger("Failed: ep_wp_chart: {conditionMessage(e)}")
    })
    tryCatch(update_psr(current_season), error = function(e) {
      derived_failures <<- c(derived_failures, "psr")
      cli::cli_alert_danger("Failed: psr: {conditionMessage(e)}")
    })
    tryCatch(update_weather(), error = function(e) {
      derived_failures <<- c(derived_failures, "weather")
      cli::cli_alert_danger("Failed: weather: {conditionMessage(e)}")
    })

    tictoc::toc(log = TRUE)
  }

  # -------------------------------------------------------------------------
  # 4. Injury snapshot (both modes — injuries change without new games)
  # -------------------------------------------------------------------------
  tryCatch(update_injury_data(current_season), error = function(e) {
    cli::cli_alert_danger("Failed: injury_data: {conditionMessage(e)}")
  })

  # -------------------------------------------------------------------------
  # Summary
  # -------------------------------------------------------------------------
  tictoc::toc(log = TRUE)

  all_failures <- c(seasonal_failures, derived_failures)
  if (length(all_failures) > 0) {
    cli::cli_alert_warning("Daily release ({release_mode}) complete with {length(all_failures)} failure{?s}: {paste(all_failures, collapse = ', ')}")
  } else {
    cli::cli_alert_success("Daily release ({release_mode}) complete!")
  }

  # Print timing summary
  cli::cli_h3("Timing Summary")
  timings <- tictoc::tic.log(format = TRUE)
  for (t in timings) {
    cli::cli_inform(t)
  }
  tictoc::tic.clearlog()

  invisible(release_mode)
}

# Execute if run as script ----

if (sys.nframe() == 0) {
  # Running as a script
  args <- commandArgs(trailingOnly = TRUE)
  force_flag <- "--force" %in% args

  run_daily_release(force = force_flag)
}
