#' Save a Data Frame to a GitHub Release via Piggyback
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' Saves a data frame as a `.parquet` file and uploads it to a GitHub release using the `piggyback` package.
#'
#' @param df A data frame to save.
#' @param file_name A string for the file name (without extension).
#' @param release_tag The GitHub release tag to associate with the uploaded file.
#'
#' @return No return value. Used for side effects (file upload).
#' @export
#'
#' @examples
#' \dontrun{
#' my_df <- data.frame(x = 1:3)
#' save_to_release(my_df, "my_data", "v1.0.0")
#' }
save_to_release <- function(df, file_name, release_tag) {
  f_name <- paste0(file_name, ".parquet")
  tf <- tempfile(fileext = ".parquet")
  on.exit(unlink(tf), add = TRUE)

  tryCatch(
    arrow::write_parquet(df, tf),
    error = function(e) {
      cli::cli_abort("Failed to write parquet file {.val {f_name}}: {conditionMessage(e)}")
    }
  )

  tryCatch(
    piggyback::pb_upload(tf,
                         repo = get_torp_data_repo(),
                         tag = release_tag,
                         name = f_name),
    error = function(e) {
      cli::cli_abort("Failed to upload {.val {f_name}} to release {.val {release_tag}}: {conditionMessage(e)}")
    }
  )

  # Also save a local copy if torpdata/data/ is configured
  if (!is.null(get_local_data_dir())) {
    save_locally(df, file_name)
  }
}

#' Read a Parquet File from a GitHub Release via Piggyback
#'
#' Downloads and reads a `.parquet` file from a GitHub release using the `piggyback` package.
#'
#' @param file_name The base name of the file (without `.parquet` extension).
#' @param release_tag The GitHub release tag the file is associated with.
#'
#' @return A data frame read from the downloaded `.parquet` file.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- file_reader("latest_data", "v1.0.0")
#' }
file_reader <- function(file_name, release_tag) {
  f_name <- paste0(file_name, ".parquet")
  tf <- tempfile(fileext = ".parquet")
  on.exit(unlink(tf), add = TRUE)

  tryCatch(
    piggyback::pb_download(f_name,
                           repo = get_torp_data_repo(),
                           tag = release_tag,
                           dest = dirname(tf),
                           overwrite = TRUE),
    error = function(e) {
      cli::cli_abort("Failed to download {.val {f_name}} from release {.val {release_tag}}: {conditionMessage(e)}")
    }
  )

  # pb_download saves with the original filename in the dest directory
  downloaded_path <- file.path(dirname(tf), f_name)
  on.exit(unlink(downloaded_path), add = TRUE)

  tryCatch(
    arrow::read_parquet(downloaded_path),
    error = function(e) {
      cli::cli_abort("Failed to read {.val {f_name}} after download: {conditionMessage(e)}")
    }
  )
}


#' Load Chains Data
#'
#' @description Loads chains data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to all rounds. If set to `TRUE`, returns all available rounds in the given season range.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing chains data.
#' @seealso [load_pbp()], [load_xg()], [load_fixtures()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_chains(2021:2022)
#' })
#' }
#' @export
load_chains <- function(seasons = get_afl_season(), rounds = TRUE, use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("chains-data", "chains_data", seasons, rounds)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load Play By Play Data
#'
#' @description Loads play by play seasons from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to all rounds. If set to `TRUE`, returns all available rounds in the given season range.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing play by play data.
#' @seealso [load_chains()], [load_xg()], [load_fixtures()], [clean_pbp()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_pbp(2021:2022)
#' })
#' }
#' @export
load_pbp <- function(seasons = get_afl_season(), rounds = TRUE, use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("pbp-data", "pbp_data", seasons, rounds)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load Expected Goals (xG) Data
#'
#' @description Loads xg data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing xG data.
#' @seealso [load_pbp()], [load_chains()], [calculate_match_xgs()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_xg(2021:2022)
#' })
#' }
#' @export
load_xg <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("xg-data", "xg_data", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load Player Stats Data
#'
#' @description Loads player stats data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing player stats data.
#' @seealso [load_player_details()], [player_game_ratings()], [player_season_ratings()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_player_stats(2021:2022)
#' })
#' }
#' @export
load_player_stats <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_stats-data", "player_stats", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load Player Game Data
#'
#' @description Loads processed player game data from the [torpdata repository](https://github.com/peteowen1/torpdata).
#' This data contains per-game performance metrics (disposal points, reception points,
#' spoil points, hitout points) adjusted by position, as used by the TORP ratings pipeline.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing player game performance data.
#' @seealso [create_player_game_data()], [player_game_ratings()], [calculate_torp_ratings()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_player_game_data(2024)
#' })
#' }
#' @export
load_player_game_data <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_game-data", "player_game", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load AFL Fixture Data
#'
#' @description Loads AFL fixture and schedule data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param all Logical. If TRUE, loads all available fixture data from 2021 onwards.
#' @param use_cache Logical. If TRUE (default), uses cached data when available to speed up repeated calls.
#' @param cache_ttl Numeric. Time-to-live for cached data in seconds. Default is 3600 (1 hour).
#' @param verbose Logical. If TRUE, prints cache hit/miss information.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk caching. Default is FALSE.
#'
#' @return A data frame containing AFL fixture and schedule data.
#' @seealso [load_results()], [load_teams()], [load_predictions()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_fixtures(2021:2022)
#'
#'   # Load all fixtures with caching disabled
#'   load_fixtures(all = TRUE, use_cache = FALSE)
#'
#'   # Load with verbose cache information
#'   load_fixtures(all = TRUE, verbose = TRUE)
#' })
#' }
#' @export
load_fixtures <- function(seasons = NULL, all = FALSE, use_cache = TRUE, cache_ttl = 3600, verbose = FALSE, columns = NULL, use_disk_cache = FALSE) {
  # Process parameters
  if (all) {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    seasons <- 2021:current_year
  } else if (is.null(seasons)) {
    seasons <- get_afl_season() # Use default season when no season is provided
  } else {
    seasons <- validate_seasons(seasons)
  }

  # Check cache if enabled
  if (use_cache) {
    cache_key <- generate_fixture_cache_key(seasons, all)

    # Try to get from cache
    if (exists(cache_key, envir = .torp_cache)) {
      cache_entry <- get(cache_key, envir = .torp_cache)

      if (is_cache_valid(cache_entry, cache_ttl)) {
        if (verbose) {
          age_seconds <- as.numeric(difftime(Sys.time(), cache_entry$timestamp, units = "secs"))
          cli::cli_inform("Cache HIT for fixtures (age: {round(age_seconds, 1)}s)")
        }
        return(cache_entry$data)
      } else {
        if (verbose) {
          cli::cli_inform("Cache EXPIRED for fixtures, fetching fresh data")
        }
        # Remove expired cache entry
        rm(list = cache_key, envir = .torp_cache)
      }
    } else {
      if (verbose) {
        cli::cli_inform("Cache MISS for fixtures, fetching data")
      }
    }
  }

  # Fetch data from URLs
  urls <- generate_urls("fixtures-data", "fixtures", seasons)
  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  # Store in cache if enabled
  if (use_cache && nrow(out) > 0) {
    store_in_cache(cache_key, out)
    if (verbose) {
      cli::cli_inform("Stored fixtures in cache ({nrow(out)} rows)")
    }
  }

  return(out)
}



#' Load AFL Team and Lineup Data
#'
#' @description Loads AFL team roster and lineup data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing AFL team and player lineup data.
#' @seealso [load_fixtures()], [load_results()], [load_player_details()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_teams(2021:2022)
#' })
#' }
#' @export
load_teams <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls(data_type = "teams-data", file_prefix = "teams", seasons = seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load AFL Match Results Data
#'
#' @description Loads AFL match results and scores from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing AFL match results and final scores.
#' @seealso [load_fixtures()], [load_predictions()], [load_teams()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_results(2021:2022)
#' })
#' }
#' @export
load_results <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("results-data", "results", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load AFL Player Details Data
#'
#' @description Loads AFL player biographical and details data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing AFL player biographical details including names, ages, and team affiliations.
#' @seealso [load_player_stats()], [calculate_torp_ratings()], [player_game_ratings()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_player_details(2021:2022)
#' })
#' }
#' @export
load_player_details <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_details-data", "player_details", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load AFL Match Predictions Data
#'
#' @description Loads AFL match predictions and probability data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to latest round. If set to `TRUE`, returns all available rounds in the given season range.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing AFL match predictions including win probabilities and expected scores.
#' @seealso [load_fixtures()], [load_results()], [simulate_season()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_predictions(2021:2022)
#' })
#' }
#' @export
load_predictions <- function(seasons = get_afl_season(), rounds = get_afl_week(), use_disk_cache = FALSE, columns = NULL) {
  if (isTRUE(seasons) && missing(rounds)) rounds <- TRUE
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("predictions", "predictions", seasons)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load TORP Ratings Data
#'
#' @description Loads pre-computed TORP player ratings from the [torpdata repository](https://github.com/peteowen1/torpdata).
#' This data contains per-round TORP ratings for all players, as generated by the ratings pipeline.
#'
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing TORP ratings with columns including
#'   \code{player_id}, \code{player_name}, \code{torp}, \code{season}, \code{round}, and \code{row_id}.
#' @seealso [calculate_torp_ratings()], [load_player_game_data()], [player_season_ratings()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_torp_ratings()
#' })
#' }
#' @export
load_torp_ratings <- function(columns = NULL) {
  url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download/ratings-data/torp_ratings.parquet")
  out <- parquet_from_url_cached(url, use_cache = FALSE, columns = columns)
  if (nrow(out) == 0) {
    cli::cli_warn("No TORP ratings data loaded. The file may not exist yet or the download failed.")
  }
  tibble::as_tibble(out)
}

#' Load Player Game Ratings Data
#'
#' @description Loads pre-computed per-game TORP ratings from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   This is the output of [player_game_ratings()] — a per-game TORP breakdown
#'   for every player, ready for leaderboards and analysis.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons — defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing per-game player ratings with columns
#'   including `season`, `round`, `match_id`, `player_id`, `player_name`,
#'   `team`, `total_points`, `recv_points`, `disp_points`, `spoil_points`,
#'   and `hitout_points`.
#' @seealso [player_game_ratings()], [load_player_season_ratings()], [load_torp_ratings()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_player_game_ratings(2024)
#' })
#' }
#' @export
load_player_game_ratings <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_game_ratings-data", "player_game_ratings", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load Player Season Ratings Data
#'
#' @description Loads pre-computed season-total TORP ratings from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   This is the output of [player_season_ratings()] — season totals and PPG
#'   leaderboards per player.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons — defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing season-total player ratings with columns
#'   including `season`, `player_id`, `player_name`, `team`, `position`,
#'   `games`, `season_points`, `season_recv`, `season_disp`, `season_spoil`,
#'   `season_hitout`, and `ppg`.
#' @seealso [player_season_ratings()], [load_player_game_ratings()], [load_torp_ratings()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_player_season_ratings(2024)
#' })
#' }
#' @export
load_player_season_ratings <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_season_ratings-data", "player_season_ratings", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load Team Ratings Data
#'
#' @description Loads pre-computed team-level TORP aggregates from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   Aggregates are the sum of TORP ratings for each team's top-21 players
#'   (filtered to TORP > 0) per round, with subcategory breakdowns.
#'   Player-level ratings are already centered relative to average, so
#'   team sums are naturally relative to 0.
#'
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing team-level ratings with columns including
#'   `season`, `round`, `team`, `team_torp`, `team_recv`, `team_disp`,
#'   `team_spoil`, `team_hitout`, `top_player`, `top_torp`, and `n_players`.
#' @seealso [load_torp_ratings()], [load_player_game_ratings()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_team_ratings()
#' })
#' }
#' @export
load_team_ratings <- function(columns = NULL) {
  url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download/team_ratings-data/team_ratings.parquet")
  out <- parquet_from_url_cached(url, use_cache = FALSE, columns = columns)
  if (nrow(out) == 0) {
    cli::cli_warn("No team ratings data loaded. The file may not exist yet or the download failed.")
  }
  out <- tibble::as_tibble(out)

  out
}

#' Load EP/WP Chart Data
#'
#' @description Loads a lightweight subset of play-by-play data optimised for
#'   charting Expected Points (EP) and Win Probability (WP) over a match.
#'   Contains every play but only ~25 columns instead of the full 150+
#'   available from [load_pbp()].
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons — defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param rounds A numeric vector associated with given AFL round — defaults to
#'   all rounds. If set to `TRUE`, returns all available rounds in the given
#'   season range.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing EP/WP chart data with columns including
#'   `match_id`, `season`, `round_number`, `period`, `total_seconds`,
#'   `home_team_team_name`, `away_team_team_name`, `team`, `exp_pts`,
#'   `delta_epv`, `wp`, `wpa`, `description`, `player_name`, `play_type`,
#'   `shot_row`, and `points_shot`.
#' @seealso [load_pbp()], [load_xg()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_ep_wp_charts(2024)
#' })
#' }
#' @export
load_ep_wp_charts <- function(seasons = get_afl_season(), rounds = TRUE, use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("ep_wp_chart-data", "ep_wp_chart", seasons, rounds)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}

#' Load Player Skills Data
#'
#' @description Loads pre-computed Bayesian player skill estimates from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   Skills are per-stat estimates with credible intervals, produced by
#'   \code{estimate_player_skills()}.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons — defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing player skill estimates with columns
#'   including `player_id`, `player_name`, `pos_group`, `n_games`,
#'   `wt_games`, `ref_date`, and `{stat}_skill`, `{stat}_lower`,
#'   `{stat}_upper` for each estimated stat.
#' @seealso [estimate_player_skills()], [player_skill_profile()], [load_player_game_ratings()]
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_player_skills(2024)
#' })
#' }
#' @export
load_player_skills <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_skills-data", "player_skills", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}
