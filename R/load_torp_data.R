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
  .f_name <- paste0(file_name, ".parquet")
  tf <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, tf)

  piggyback::pb_upload(tf,
                       repo = get_torp_data_repo(),
                       tag = release_tag,
                       name = .f_name
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
  td <- tempdir(check = TRUE)
  piggyback::pb_download(f_name,
                         repo = get_torp_data_repo(),
                         tag = release_tag,
                         dest = td
  )

  arrow::read_parquet(file.path(td, f_name))
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
load_fixtures <- function(seasons = NULL, all = FALSE, use_cache = TRUE, cache_ttl = 3600, verbose = FALSE, columns = NULL) {
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
  out <- load_from_url(urls, seasons = seasons, columns = columns)
  
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
#'   This data summarises per-round team ratings derived from individual
#'   player TORP ratings.
#'
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing team-level ratings with columns including
#'   `season`, `round`, `team`, `team_torp`, `team_attack`, `team_defence`,
#'   `top_player`, `top_torp`, and `n_players`.
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
  tibble::as_tibble(out)
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

#' Load parquet files from remote URLs
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' @param url A vector of URLs to load into memory. If more than one URL provided, will row-bind them.
#' @param seasons A numeric vector of years that will be used to filter the dataframe's `season` column. If `TRUE` (default), does not filter.
#' @param rounds A numeric vector of rounds that will be used to filter the dataframe's `round` column. If `TRUE` (default), does not filter.
#' @param peteowen1 TRUE to add peteowen1_data classing and attributes.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for faster repeated loads.
#' @param columns Optional character vector of column names to read. If NULL (default),
#'   reads all columns. Filter columns (season, round, round_number, week) are
#'   auto-included when filtering is active.
#' @param ... Named arguments that will be added as attributes to the data, e.g. `peteowen1_type` = "pbp"
#'
#' @return A tibble, possibly of type `peteowen1_data`
#' @export
#' @importFrom data.table rbindlist setDT
#' @importFrom tibble as_tibble
load_from_url <- function(url, ..., seasons = TRUE, rounds = TRUE, peteowen1 = FALSE, use_disk_cache = FALSE, columns = NULL) {
  url <- as.character(url)

  cache_opts <- get_disk_cache_options()
  use_cache <- use_disk_cache && cache_opts$enabled
  max_age <- cache_opts$max_age_days

  # Auto-include filter columns when columns is specified and filtering is active
  read_cols <- columns
  if (!is.null(read_cols)) {
    if (!isTRUE(seasons)) read_cols <- union(read_cols, "season")
    if (!isTRUE(rounds))  read_cols <- union(read_cols, c("round", "round_number", "week"))
  }

  if (length(url) == 1) {
    out <- parquet_from_url_cached(url, use_cache = use_cache, max_age_days = max_age, columns = read_cols)
  } else {
    out <- parquet_from_urls_parallel(url, use_cache = use_cache, max_age_days = max_age, columns = read_cols)
  }

  if (nrow(out) == 0 && length(url) > 0) {
    cli::cli_warn("No data loaded from {length(url)} URL{?s}. Check seasons/rounds or use {.fn clear_skip_markers} to retry previously failed files.")
  }

  # Filter by season/round if specific values requested
  if (!isTRUE(seasons)) {
    stopifnot(is.numeric(seasons))
    if ("season" %in% names(out)) out <- out[out$season %in% seasons, ]
  }
  if (!isTRUE(rounds)) {
    stopifnot(is.numeric(rounds))
    if ("round" %in% names(out)) {
      out <- out[out$round %in% rounds, ]
    } else if ("round_number" %in% names(out)) {
      out <- out[out$round_number %in% rounds, ]
    } else if ("week" %in% names(out)) {
      out <- out[out$week %in% rounds, ]
    } else if (nrow(out) > 0) {
      cli::cli_warn("Round filtering requested but no round column found in data. Returning unfiltered. Available columns: {.val {head(names(out), 10)}}")
    }
  }

  # Drop auto-added filter columns that weren't originally requested
  if (!is.null(columns)) {
    keep <- intersect(names(out), columns)
    if (length(keep) == 0 && nrow(out) > 0) {
      cli::cli_warn("None of the requested columns ({.val {columns}}) found in data. Available: {.val {head(names(out), 10)}}")
    }
    if (length(keep) > 0) out <- out[, ..keep]
  }

  out <- tibble::as_tibble(out)

  if (peteowen1) {
    class(out) <- c("peteowen1_data", class(out))
    attributes(out) <- c(attributes(out), list(...))
  }

  return(out)
}

#' Download multiple parquet files in parallel using curl
#'
#' Always checks local `torpdata/data/` first for each URL (with smart
#' staleness), then optionally checks disk cache, then downloads missing
#' files in parallel. Local files are batch-read via `arrow::open_dataset()`
#' for speed. Downloaded data is auto-saved to local storage.
#'
#' @param urls Character vector of URLs
#' @param use_cache Logical. If TRUE, also check/write the `~/.torp/cache/` disk cache.
#' @param max_age_days Maximum disk cache age in days.
#' @param columns Optional character vector of column names to select.
#'
#' @return A data.table with all files combined
#' @keywords internal
#' @importFrom curl multi_download
#' @importFrom data.table rbindlist setDT
parquet_from_urls_parallel <- function(urls, use_cache = FALSE, max_age_days = 7, columns = NULL) {
  n <- length(urls)

  # --- Phase 1: Resolve each URL to a local file path or mark for download ---
  local_paths <- character(n)     # local path if available, "" otherwise
  download_indices <- integer(0)

  n_skipped <- 0L
  for (i in seq_len(n)) {
    # Skip known-bad files (negative cache)
    if (is_download_skippable(urls[i])) {
      n_skipped <- n_skipped + 1L
      next
    }

    # Check local torpdata/data/ (smart staleness)
    local_max_age <- local_max_age_for_url(urls[i])
    if (is_locally_stored(urls[i], local_max_age)) {
      lp <- get_local_path(urls[i])
      if (!is.null(lp) && file.exists(lp) && file.size(lp) >= MIN_PARQUET_BYTES) {
        local_paths[i] <- lp
        next
      }
    }

    # Check disk cache (opt-in)
    if (use_cache && is_disk_cached(urls[i], max_age_days)) {
      cp <- get_disk_cache_path(urls[i])
      if (!is.null(cp) && file.exists(cp) && file.size(cp) >= MIN_PARQUET_BYTES) {
        local_paths[i] <- cp
        next
      }
    }

    download_indices <- c(download_indices, i)
  }

  if (n_skipped > 0) {
    cli::cli_inform("Skipped {n_skipped} previously failed URL{?s}. Use {.fn clear_skip_markers} to retry.")
  }

  # --- Phase 2: Batch-read all local files with open_dataset() ---
  valid_paths <- local_paths[nchar(local_paths) > 0]
  local_dt <- data.table::data.table()

  if (length(valid_paths) > 0) {
    local_dt <- tryCatch({
      ds <- arrow::open_dataset(valid_paths, format = "parquet", unify_schemas = TRUE)
      if (!is.null(columns)) {
        ds <- dplyr::select(ds, dplyr::any_of(columns))
      }
      out <- dplyr::collect(ds)
      data.table::setDT(out)
      out
    }, error = function(e) {
      cli::cli_warn("Batch read failed, falling back to sequential: {conditionMessage(e)}")
      NULL
    })

    # Sequential fallback (outside tryCatch to avoid nested error handler issues with Arrow)
    if (is.null(local_dt)) {
      parts <- lapply(valid_paths, function(p) {
        tryCatch({
          if (!is.null(columns)) {
            d <- arrow::read_parquet(p, col_select = dplyr::any_of(columns))
          } else {
            d <- arrow::read_parquet(p)
          }
          data.table::setDT(d)
          d
        }, error = function(e) {
          cli::cli_warn("Failed to read local file {.path {basename(p)}}: {conditionMessage(e)}")
          data.table::data.table()
        })
      })
      n_failed <- sum(vapply(parts, function(p) nrow(p) == 0L, logical(1)))
      if (n_failed > 0) {
        cli::cli_warn("{n_failed} of {length(valid_paths)} local file{?s} failed to read. Results may be incomplete.")
      }
      local_dt <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
    }
  }

  # --- Phase 3: Download missing files in parallel ---
  dl_dt <- data.table::data.table()

  if (length(download_indices) > 0) {
    urls_to_dl <- urls[download_indices]
    tmp_files <- vapply(urls_to_dl, function(u) tempfile(fileext = ".parquet"), character(1))

    cli::cli_inform("Downloading {length(urls_to_dl)} file{?s} in parallel...")
    dl <- tryCatch(
      curl::multi_download(urls_to_dl, destfiles = tmp_files),
      error = function(e) {
        if (nrow(local_dt) == 0) {
          cli::cli_abort("Download failed and no local data available: {conditionMessage(e)}")
        }
        cli::cli_warn("Download failed (using local data only): {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(dl)) {
      dl_parts <- list()

      for (j in seq_along(download_indices)) {
        idx <- download_indices[j]
        if (isTRUE(dl$success[j]) && file.exists(tmp_files[j]) &&
            file.size(tmp_files[j]) >= MIN_PARQUET_BYTES) {
          tryCatch({
            dt <- arrow::read_parquet(tmp_files[j])
            data.table::setDT(dt)

            if (nrow(dt) > 0) {
              # Apply column selection
              if (!is.null(columns)) {
                cols_present <- intersect(columns, names(dt))
                if (length(cols_present) > 0) {
                  dl_parts[[length(dl_parts) + 1L]] <- dt[, ..cols_present]
                } else {
                  cli::cli_warn("None of the requested columns found in {.url {basename(urls[idx])}}. Available: {.val {head(names(dt), 10)}}")
                }
              } else {
                dl_parts[[length(dl_parts) + 1L]] <- dt
              }

              # Save full data to local storage (best-effort)
              write_local_parquet(urls[idx], dt)

              # Also cache to disk if enabled
              if (use_cache) write_disk_cache(urls[idx], dt)
            }
          }, error = function(e) {
            cli::cli_warn("Failed to read downloaded file {.url {basename(urls[idx])}}: {conditionMessage(e)}")
            # Do NOT mark as skippable -- this was a read error, not a missing file (404)
          })
        } else {
          # Only mark as skippable for HTTP 404, not transient errors
          status <- dl$status_code[j]
          if (!is.na(status) && status == 404) {
            mark_download_skippable(urls[idx])
          }
          if (!isTRUE(dl$success[j])) {
            cli::cli_warn("Failed to download {.url {urls[idx]}}")
          }
        }
      }

      unlink(tmp_files)

      if (length(dl_parts) > 0) {
        dl_dt <- data.table::rbindlist(dl_parts, use.names = TRUE, fill = TRUE)
      }
    }
  }

  # --- Phase 4: Combine local + downloaded ---
  if (nrow(local_dt) == 0 && nrow(dl_dt) == 0) return(data.table::data.table())
  if (nrow(dl_dt) == 0) return(local_dt)
  if (nrow(local_dt) == 0) return(dl_dt)

  data.table::rbindlist(list(local_dt, dl_dt), use.names = TRUE, fill = TRUE)
}

#' Load parquet file from a remote connection with local-first loading
#'
#' Always checks local `torpdata/data/` first (with smart staleness), then
#' optionally checks the disk cache, then downloads. Downloaded data is
#' auto-saved to local storage for next time.
#'
#' @param url A character URL
#' @param use_cache Logical. If TRUE, also check/write the `~/.torp/cache/` disk cache.
#' @param max_age_days Maximum age for disk cache files in days.
#' @param columns Optional character vector of column names to select.
#'
#' @return A data frame
#' @keywords internal
#' @importFrom cli cli_warn cli_abort
#' @importFrom data.table data.table setDT
parquet_from_url_cached <- function(url, use_cache = TRUE, max_age_days = 7, columns = NULL) {
  # 0. Negative cache — skip known-bad URLs
  if (is_download_skippable(url)) {
    cli::cli_warn(c(
      "Skipping previously failed URL: {.url {basename(url)}}",
      "i" = "Run {.fn clear_skip_markers} to retry."
    ))
    return(data.table::data.table())
  }

  # 1. ALWAYS check local torpdata/data/ first (smart staleness per URL)
  local_max_age <- local_max_age_for_url(url)
  if (is_locally_stored(url, local_max_age)) {
    local_data <- read_local_parquet(url, columns = columns)
    if (!is.null(local_data)) {
      data.table::setDT(local_data)
      return(local_data)
    }
  }

  # 2. Check disk cache (opt-in)
  if (use_cache && is_disk_cached(url, max_age_days)) {
    cached_data <- read_disk_cache(url, columns = columns)
    if (!is.null(cached_data)) {
      data.table::setDT(cached_data)
      return(cached_data)
    }
  }

  # 3. Download from URL
  result <- parquet_from_url(url)

  # Mark as skippable only for confirmed 404s (not transient errors)
  if (nrow(result) == 0) {
    if (identical(attr(result, "skip_reason"), "not_found")) {
      mark_download_skippable(url)
    }
    return(result)
  }

  # Auto-save full data to local storage
  write_local_parquet(url, result)

  # Also cache to disk cache if enabled
  if (use_cache) {
    write_disk_cache(url, result)
  }

  # Apply column selection on the downloaded data
  if (!is.null(columns)) {
    cols_present <- intersect(columns, names(result))
    if (length(cols_present) > 0) {
      result <- result[, ..cols_present]
    } else {
      cli::cli_warn("None of the requested columns ({.val {columns}}) found in downloaded data. Available: {.val {head(names(result), 10)}}")
    }
  }

  return(result)
}

#' Load parquet file from a remote connection
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' @param url A character URL
#'
#' @return A data frame
#' @export
#' @importFrom cli cli_warn cli_abort
#' @importFrom data.table data.table setDT
parquet_from_url <- function(url) {
  # Validate URL format
  if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
    cli::cli_abort("URL must be a single non-empty character string")
  }

  if (!grepl("^https?://", url)) {
    cli::cli_abort("URL must start with http:// or https://")
  }

  tryCatch({
    # Arrow can read parquet directly from URL
    load <- arrow::read_parquet(url)

    # Validate that we got actual data
    if (is.null(load)) {
      cli::cli_warn("No data returned from {.url {url}}")
      return(data.table::data.table())
    }

    data.table::setDT(load)
    return(load)

  }, error = function(e) {
    error_msg <- conditionMessage(e)
    result <- data.table::data.table()

    if (grepl("404|Not Found", error_msg, ignore.case = TRUE)) {
      cli::cli_warn("Data file not found at {.url {url}} - file may not exist for this season/round combination")
      attr(result, "skip_reason") <- "not_found"
    } else if (grepl("timeout|timed out", error_msg, ignore.case = TRUE)) {
      cli::cli_warn("Connection timeout while downloading from {.url {url}} - please try again")
      attr(result, "skip_reason") <- "transient"
    } else if (grepl("cannot open|connection", error_msg, ignore.case = TRUE)) {
      cli::cli_warn("Failed to connect to {.url {url}} - check internet connection")
      attr(result, "skip_reason") <- "transient"
    } else {
      cli::cli_warn("Failed to load data from {.url {url}}: {error_msg}")
      attr(result, "skip_reason") <- "transient"
    }

    return(result)
  })
}

# Package Configuration

#' Get TORP Data Repository
#'
#' Returns the repository used for TORP data downloads. Can be configured
#' via the TORP_DATA_REPO environment variable or package options.
#'
#' @return Character string of the repository in format "owner/repo"
#' @keywords internal
get_torp_data_repo <- function() {
  getOption("torp.data.repo", "peteowen1/torpdata")
}

#' Set TORP Data Repository
#'
#' Sets the repository used for TORP data downloads in the current session.
#'
#' @param repo Character string of the repository in format "owner/repo"
#' @export
set_torp_data_repo <- function(repo) {
  if (!is.character(repo) || length(repo) != 1 || nchar(repo) == 0) {
    cli::cli_abort("Repository must be a single non-empty character string")
  }

  if (!grepl("^[^/]+/[^/]+$", repo)) {
    cli::cli_abort("Repository must be in format 'owner/repo'")
  }

  options(torp.data.repo = repo)
  cli::cli_inform("TORP data repository set to: {repo}")
}

# Helper functions

#' Validate rounds
#'
#' @param rounds A numeric vector or TRUE
#'
#' @return Validated rounds vector
#' @keywords internal
validate_rounds <- function(rounds) {
  if (isTRUE(rounds)) rounds <- 0:28

  if (!is.numeric(rounds)) {
    cli::cli_abort("Rounds must be numeric values or TRUE")
  }

  invalid_rounds <- rounds[rounds < 0 | rounds > 28]
  if (length(invalid_rounds) > 0) {
    cli::cli_abort("Invalid round numbers: {paste(invalid_rounds, collapse = ', ')}. Rounds must be between 0 and 28")
  }

  return(rounds)
}

#' Validate seasons
#'
#' @param seasons A numeric vector or TRUE
#'
#' @return A validated numeric vector of seasons
#' @keywords internal
validate_seasons <- function(seasons) {
  if (isTRUE(seasons)) seasons <- 2021:(get_afl_season())

  if (!is.numeric(seasons)) {
    cli::cli_abort("Seasons must be numeric values or TRUE")
  }

  current_season <- tryCatch({
    get_afl_season()
  }, error = function(e) {
    fallback <- as.integer(format(Sys.Date(), "%Y"))
    cli::cli_warn("Could not determine current AFL season, using {fallback} as default")
    fallback
  })

  invalid_seasons <- seasons[seasons < 2021 | seasons > current_season]
  if (length(invalid_seasons) > 0) {
    cli::cli_abort("Invalid season years: {paste(invalid_seasons, collapse = ', ')}. Seasons must be between 2021 and {current_season}")
  }

  return(seasons)
}

#' Generate URLs for data download
#'
#' @param data_type Type of data (e.g., "chain-data", "pbp-data")
#' @param file_prefix Prefix for the file name
#' @param seasons A numeric vector of seasons
#' @param rounds A numeric vector of rounds (optional)
#' @param prefer_aggregated Logical. If TRUE, prefer aggregated seasonal files
#'   when loading all rounds for a season. Default is TRUE (aggregated files
#'   are available on torpdata). Set via option `torp.use_aggregated_files`.
#'
#' @return A character vector of URLs
#' @keywords internal
generate_urls <- function(data_type, file_prefix, seasons, rounds = NULL, prefer_aggregated = NULL) {
  base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")

  # Determine if we should try aggregated files
  # Default to TRUE - aggregated files are available on torpdata
  # Can be disabled via option: options(torp.use_aggregated_files = FALSE)
  if (is.null(prefer_aggregated)) {
    prefer_aggregated <- getOption("torp.use_aggregated_files", TRUE)
  }

  current_season <- get_afl_season()
  current_round <- NULL

  if (is.null(rounds)) {
    combinations <- expand.grid(seasons = seasons)

    urls <- paste0(base_url, "/", data_type, "/", file_prefix, "_", combinations$seasons, ".parquet")
    urls <- sort(urls)
  }

  if (!is.null(rounds)) {
    # These data types only have _all files (no per-round files on GitHub)
    # Always load _all and filter by round_number in load_from_url()
    aggregated_only_types <- c("chains-data", "pbp-data", "ep_wp_chart-data")

    if (data_type %in% aggregated_only_types) {
      urls <- paste0(base_url, "/", data_type, "/", file_prefix, "_", seasons, "_all.parquet")
      return(as.character(urls))
    }

    current_season <- get_afl_season()
    current_round <- get_afl_week()

    # Check if we're loading all rounds for any season (rounds 0-28 or TRUE was passed)
    all_rounds <- 0:28
    loading_all_rounds <- length(rounds) >= length(all_rounds) &&
      all(all_rounds %in% rounds)

    if (prefer_aggregated && loading_all_rounds) {
      # Use aggregated files for complete seasons
      urls <- paste0(base_url, "/", data_type, "/", file_prefix, "_", seasons, "_all.parquet")
      return(as.character(urls))
    }

    # Use per-round files (default behavior for other data types)
    rounds_02d <- sprintf("%02d", rounds)
    combinations <- expand.grid(seasons = seasons, rounds = rounds_02d)

    urls <- paste0(base_url, "/", data_type, "/", file_prefix, "_", combinations$seasons, "_", combinations$rounds, ".parquet")
    urls <- sort(urls)
  }

  if (data_type == "fixtures-data") {
    current_round_str <- 99
  } else {
    if (is.null(current_round)) current_round <- get_afl_week()
    current_round_str <- sprintf("%02d", current_round)
  }

  max_url <- paste0(base_url, "/", data_type, "/", file_prefix, "_", current_season, "_", current_round_str, ".parquet")

  # Lexicographic comparison works because rounds are zero-padded via sprintf("%02d", ...)
  n_before <- length(urls)
  urls <- urls[urls <= max_url]
  n_dropped <- n_before - length(urls)

  if (n_dropped > 0) {
    cli::cli_inform("Filtered {n_dropped} future URL{?s} (current season: {current_season}, round: {current_round}).")
  }

  return(as.character(urls))
}

