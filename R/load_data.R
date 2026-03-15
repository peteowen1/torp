#' Save a Data Frame to a GitHub Release via Piggyback
#'
#' @description This function is intended for internal use and may be unexported in a future release.
#' Saves a data frame as a `.parquet` file and uploads it to a GitHub release using the `piggyback` package.
#'
#' @param df A data frame to save.
#' @param file_name A string for the file name (without extension).
#' @param release_tag The GitHub release tag to associate with the uploaded file.
#' @param also_csv Logical. If TRUE, also upload a `.csv` copy alongside parquet.
#'
#' @return No return value. Used for side effects (file upload).
#' @export
#'
#' @examples
#' \dontrun{
#' my_df <- data.frame(x = 1:3)
#' save_to_release(my_df, "my_data", "v1.0.0")
#' save_to_release(my_df, "my_data", "v1.0.0", also_csv = TRUE)
#' }
save_to_release <- function(df, file_name, release_tag, also_csv = FALSE) {
  rlang::check_installed("piggyback", version = "0.1.4", reason = "to upload data to GitHub releases")
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

  if (also_csv) {
    csv_name <- paste0(file_name, ".csv")
    tf_csv <- tempfile(fileext = ".csv")
    on.exit(unlink(tf_csv), add = TRUE)

    tryCatch({
      utils::write.csv(df, tf_csv, row.names = FALSE)
      piggyback::pb_upload(tf_csv,
                           repo = get_torp_data_repo(),
                           tag = release_tag,
                           name = csv_name)
    }, error = function(e) {
      cli::cli_warn("Parquet uploaded but CSV copy failed for {.val {csv_name}}: {conditionMessage(e)}")
    })
  }

  # Also save a local copy if torpdata/data/ is configured
  if (!is.null(get_local_data_dir())) {
    tryCatch(
      save_locally(df, file_name),
      error = function(e) {
        cli::cli_warn("Remote upload succeeded but local save failed: {conditionMessage(e)}")
      }
    )
  }
}

#' Update Player Stats Release
#'
#' Scrapes player stats from the AFL API for a given season, normalises
#' column names, and uploads to the torpdata GitHub release.
#'
#' @param season Season year (numeric).
#' @return Invisible NULL. Called for side effects (upload).
#' @keywords internal
update_player_stats <- function(season) {
  cli::cli_progress_step("Updating player stats for {season}")

  player_stats <- tryCatch({
    get_afl_player_stats(season) |>
      dplyr::select(tidyselect::where(~ dplyr::n_distinct(.) > 1)) |>
      torp_clean_names()
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch player stats: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(player_stats) || nrow(player_stats) == 0) {
    cli::cli_alert_warning("No player stats data for {season}")
    return(invisible(NULL))
  }

  # Normalise column names (strips v2 stats_ prefix, renames player/match IDs)
  player_stats <- .normalise_player_stats_columns(player_stats)

  if (!"season" %in% names(player_stats)) {
    player_stats$season <- as.integer(season)
  }

  file_name <- paste0("player_stats_", season)
  save_to_release(df = player_stats, file_name = file_name, release_tag = "player_stats-data")

  cli::cli_inform("Saved player stats: {file_name} ({nrow(player_stats)} rows)")
  invisible(NULL)
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
  rlang::check_installed("piggyback", version = "0.1.4", reason = "to download data from GitHub releases")
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


# ============================================================================
# Internal helper: load data via API with in-memory cache
# ============================================================================

#' Load data from AFL API with caching
#'
#' Generalised helper that wraps any `get_afl_*()` function with per-season
#' fetching, in-memory caching, and column selection. Used by `load_fixtures()`,
#' `load_results()`, `load_teams()`, `load_player_stats()`, and
#' `load_player_details()`.
#'
#' @param cache_prefix Character. Cache key prefix (e.g. "fixtures", "results").
#' @param seasons Numeric vector of seasons.
#' @param fetch_fn Function that takes a single season year and returns a
#'   data.frame/tibble.
#' @param use_cache Logical. Whether to use in-memory caching.
#' @param cache_ttl Numeric. Cache time-to-live in seconds.
#' @param verbose Logical. Print cache hit/miss info.
#' @param columns Optional character vector of column names to select.
#' @param fetch_all_fn Optional function that takes a vector of seasons and
#'   returns all data in one batch. When provided and `length(seasons) > 1`,
#'   used instead of per-season `lapply(seasons, fetch_fn)` for efficiency
#'   (e.g. one big `curl::multi_download()` instead of N sequential batches).
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache for
#'   completed past seasons. Default is FALSE.
#' @param refresh Logical. If TRUE, clears all caches and fetches fresh
#'   data from the API. Default is FALSE.
#' @return A tibble.
#' @keywords internal
.load_with_cache <- function(cache_prefix, seasons, fetch_fn,
                             use_cache = TRUE, cache_ttl = 3600,
                             verbose = FALSE, columns = NULL,
                             fetch_all_fn = NULL,
                             use_disk_cache = FALSE,
                             refresh = FALSE) {
  cache_key <- paste0(cache_prefix, "_", paste(sort(seasons), collapse = "_"))

  # Force refresh: clear in-memory and disk caches
  if (refresh) {
    if (exists(cache_key, envir = .torp_cache)) {
      rm(list = cache_key, envir = .torp_cache)
    }
    if (use_disk_cache) {
      clear_disk_cache(pattern = sprintf("cfs_%s_", cache_prefix))
    }
  }

  # Check in-memory cache
  if (!refresh && use_cache && exists(cache_key, envir = .torp_cache)) {
    cache_entry <- get(cache_key, envir = .torp_cache)
    if (is_cache_valid(cache_entry, cache_ttl)) {
      if (verbose) {
        age_seconds <- as.numeric(difftime(Sys.time(), cache_entry$timestamp, units = "secs"))
        cli::cli_inform("Cache HIT for {cache_prefix} (age: {round(age_seconds, 1)}s)")
      }
      out <- cache_entry$data
      if (!is.null(columns)) {
        keep <- intersect(columns, names(out))
        out <- out[, keep, drop = FALSE]
      }
      return(tibble::as_tibble(out))
    } else {
      if (verbose) cli::cli_inform("Cache EXPIRED for {cache_prefix}, fetching fresh data")
      rm(list = cache_key, envir = .torp_cache)
    }
  } else if (use_cache && verbose) {
    cli::cli_inform("Cache MISS for {cache_prefix}, fetching data")
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Fetch data — use bulk fetcher if available and multi-season, else per-season
  if (!is.null(fetch_all_fn) && length(seasons) > 1) {
    results <- list(tryCatch(
      suppressMessages(fetch_all_fn(seasons)),
      error = function(e) {
        cli::cli_alert_danger("Bulk fetch failed for {cache_prefix}: {conditionMessage(e)}")
        NULL
      }
    ))
  } else {
    results <- lapply(seasons, function(s) {
      # Check per-season disk cache
      if (use_disk_cache) {
        disk_data <- .read_season_disk_cache(cache_prefix, s, current_year)
        if (!is.null(disk_data)) return(disk_data)
      }

      data <- tryCatch(suppressMessages(fetch_fn(s)), error = function(e) {
        cli::cli_alert_danger("Failed to fetch {cache_prefix} for {s}: {conditionMessage(e)}")
        NULL
      })

      # Save to per-season disk cache (past seasons only)
      if (use_disk_cache && s < current_year && !is.null(data) && nrow(data) > 0) {
        .write_season_disk_cache(cache_prefix, s, data)
      }

      data
    })
  }
  results <- Filter(Negate(is.null), results)

  if (length(results) == 0) {
    cli::cli_warn("No {cache_prefix} data returned for seasons: {paste(seasons, collapse = ', ')}")
    return(tibble::tibble())
  }

  # rbindlist with fill=TRUE handles differing column sets across seasons
  out <- data.table::rbindlist(lapply(results, data.table::as.data.table), fill = TRUE)
  out <- tibble::as_tibble(out)

  # Store in in-memory cache before column selection
  if (use_cache && nrow(out) > 0) {
    store_in_cache(cache_key, out)
    if (verbose) cli::cli_inform("Stored {cache_prefix} in cache ({nrow(out)} rows)")
  }

  # Apply column selection
  if (!is.null(columns)) {
    keep <- intersect(columns, names(out))
    out <- out[, keep, drop = FALSE]
  }

  out
}

#' Read per-season disk cache for CFS data
#'
#' Only caches past seasons (before current year). Current season data
#' may contain provisional lineups or live stats, so it is always
#' fetched fresh from the API.
#'
#' @param prefix Cache prefix (e.g. "teams", "player_stats")
#' @param season Season year
#' @param current_year Current calendar year
#' @return Data frame if cache hit, NULL if miss
#' @keywords internal
.read_season_disk_cache <- function(prefix, season, current_year) {
  # Never use disk cache for current season — data may be provisional
  if (season >= current_year) return(NULL)

  cache_dir <- get_disk_cache_dir()
  disk_path <- file.path(cache_dir, sprintf("cfs_%s_%d.parquet", prefix, season))

  if (!file.exists(disk_path)) return(NULL)

  tryCatch({
    data <- arrow::read_parquet(disk_path)
    if (nrow(data) > 0) {
      cli::cli_inform("Disk cache HIT for {prefix} {season} ({nrow(data)} rows)")
      return(data)
    }
    NULL
  }, error = function(e) {
    cli::cli_warn("Corrupt disk cache for {prefix} {season} -- deleting and re-fetching: {conditionMessage(e)}")
    unlink(disk_path)
    NULL
  })
}

#' Write per-season disk cache for CFS data
#'
#' @param prefix Cache prefix
#' @param season Season year
#' @param data Data frame to cache
#' @keywords internal
.write_season_disk_cache <- function(prefix, season, data) {
  cache_dir <- get_disk_cache_dir()
  disk_path <- file.path(cache_dir, sprintf("cfs_%s_%d.parquet", prefix, season))
  tryCatch(
    arrow::write_parquet(data, disk_path),
    error = function(e) {
      cli::cli_warn("Failed to write disk cache for {prefix} {season}: {conditionMessage(e)}")
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
#' \dontrun{
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

  # Normalise camelCase chains columns (matchId → match_id, etc.)
  if (nrow(out) > 0) {
    if (!data.table::is.data.table(out)) out <- data.table::as.data.table(out)
    .normalise_chains_columns(out)
  }

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
#' \dontrun{
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
#' \dontrun{
#' try({ # prevents cran errors
#'   load_xg(2021:2022)
#' })
#' }
#' @export
load_xg <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("xg-data", "xg_data", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  # Normalise old column names (home_sG → home_scored_goals, etc.)
  if (nrow(out) > 0) {
    .normalise_columns(out, XG_COL_MAP, verbose = TRUE, label = "XG")
  }

  return(out)
}

#' Load Player Stats Data
#'
#' @description Loads player stats data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE (default), caches completed past seasons
#'   to disk so they load instantly on subsequent calls. Current season is always
#'   fetched fresh from the API.
#' @param refresh Logical. If TRUE, clears all caches and fetches fresh data
#'   from the API for all seasons. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing player stats data.
#' @seealso [load_player_details()], [player_game_ratings()], [player_season_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_stats(2021:2022)
#' })
#' }
#' @export
load_player_stats <- function(seasons = get_afl_season(), use_disk_cache = TRUE, refresh = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  out <- .load_with_cache(
    cache_prefix = "player_stats",
    seasons = seasons,
    fetch_fn = get_afl_player_stats,
    columns = columns,
    use_disk_cache = use_disk_cache,
    refresh = refresh
  )

  # Normalise once after retrieval (handles both fresh API data and stale disk cache)
  if (nrow(out) > 0) out <- tibble::as_tibble(.normalise_player_stats_columns(out))

  out
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
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_game_data(2024)
#' })
#' }
#' @export
load_player_game_data <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_game-data", "player_game", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  # Normalise old abbreviated column names (plyr_nm → player_name, etc.)
  if (nrow(out) > 0) .normalise_columns(out, PLAYER_GAME_COL_MAP, verbose = TRUE, label = "Player game")

  return(out)
}

#' Load AFL Fixture Data
#'
#' @description Loads AFL fixture and schedule data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param all Deprecated. Use `seasons = TRUE` instead (consistent with other `load_*()` functions).
#' @param use_cache Logical. If TRUE, uses in-memory cached data when available to speed up repeated calls. Default is TRUE.
#' @param cache_ttl Numeric. Time-to-live for cached data in seconds. Default is 3600 (1 hour).
#' @param verbose Logical. If TRUE, prints cache hit/miss information.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk caching. Default is FALSE.
#'
#' @return A data frame containing AFL fixture and schedule data.
#' @seealso [load_results()], [load_teams()], [load_predictions()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_fixtures(2021:2022)
#'
#'   # Load all fixtures
#'   load_fixtures(seasons = TRUE)
#' })
#' }
#' @export
load_fixtures <- function(seasons = NULL, all = FALSE, use_cache = TRUE, cache_ttl = 3600, verbose = FALSE, columns = NULL, use_disk_cache = FALSE) {
  # Process parameters — `all = TRUE` is equivalent to `seasons = TRUE`
  if (all) seasons <- TRUE
  if (is.null(seasons)) {
    seasons <- get_afl_season()
  } else {
    seasons <- validate_seasons(seasons)
  }

  .load_with_cache(
    cache_prefix = "fixtures",
    seasons = seasons,
    fetch_fn = get_afl_fixtures,
    use_cache = use_cache,
    cache_ttl = cache_ttl,
    verbose = verbose,
    columns = columns,
    use_disk_cache = use_disk_cache
  )
}



#' Load AFL Team and Lineup Data
#'
#' @description Loads AFL team roster and lineup data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE (default), caches completed past seasons
#'   to disk so they load instantly on subsequent calls. Current season is always
#'   fetched fresh from the API.
#' @param refresh Logical. If TRUE, clears all caches and fetches fresh data
#'   from the API for all seasons. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing AFL team and player lineup data.
#' @seealso [load_fixtures()], [load_results()], [load_player_details()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_teams(2021:2022)
#' })
#' }
#' @export
load_teams <- function(seasons = get_afl_season(), use_disk_cache = TRUE, refresh = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  out <- .load_with_cache(
    cache_prefix = "teams",
    seasons = seasons,
    fetch_fn = get_afl_lineups,
    columns = columns,
    use_disk_cache = use_disk_cache,
    refresh = refresh
  )

  # Derive round_number from match_id if absent or NA (disk-cached data may lack it)
  if (nrow(out) > 0 && "match_id" %in% names(out)) {
    needs_round <- !"round_number" %in% names(out) || anyNA(out$round_number)
    if (needs_round) {
      out$round_number <- as.integer(substr(out$match_id, 12L, 13L))
    }
  }

  out
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
#' \dontrun{
#' try({ # prevents cran errors
#'   load_results(2021:2022)
#' })
#' }
#' @export
load_results <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  .load_with_cache(
    cache_prefix = "results",
    seasons = seasons,
    fetch_fn = get_afl_results,
    columns = columns,
    use_disk_cache = use_disk_cache
  )
}

#' Load AFL Player Details Data
#'
#' @description Loads AFL player biographical and details data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param use_disk_cache Logical. If TRUE (default), caches completed past seasons
#'   to disk so they load instantly on subsequent calls. Current season is always
#'   fetched fresh from the API.
#' @param refresh Logical. If TRUE, clears all caches and fetches fresh data
#'   from the API for all seasons. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing AFL player biographical details including names, ages, and team affiliations.
#' @seealso [load_player_stats()], [calculate_torp_ratings()], [player_game_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_details(2021:2022)
#' })
#' }
#' @export
load_player_details <- function(seasons = get_afl_season(), use_disk_cache = TRUE, refresh = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  .load_with_cache(
    cache_prefix = "player_details",
    seasons = seasons,
    fetch_fn = get_afl_player_details,
    fetch_all_fn = .fetch_all_player_details,
    columns = columns,
    use_disk_cache = use_disk_cache,
    refresh = refresh
  )
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
#' \dontrun{
#' try({ # prevents cran errors
#'   load_predictions(2021:2022)
#' })
#' }
#' @export
load_predictions <- function(seasons = get_afl_season(), rounds = get_afl_week(type = "next"), use_disk_cache = FALSE, columns = NULL) {
  if (isTRUE(seasons) && missing(rounds)) rounds <- TRUE
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("predictions", "predictions", seasons)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  # Normalise old column names (providerId → match_id, etc.)
  if (nrow(out) > 0) {
    if (!data.table::is.data.table(out)) out <- data.table::as.data.table(out)
    .normalise_predictions_columns(out)
    out <- tibble::as_tibble(out)
  }

  return(out)
}

#' Load Retrodictions Data
#'
#' @description Loads retrodictions from the [torpdata repository](https://github.com/peteowen1/torpdata).
#' Retrodictions are the current model's predictions for all matches (completed and upcoming),
#' regenerated each pipeline run. Compare with [load_predictions()] which returns locked
#' pre-game predictions frozen at kickoff.
#'
#' @param seasons A numeric vector of 4-digit years - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector of round numbers - defaults to latest round. If set to `TRUE`, returns all available rounds.
#' @param use_disk_cache Logical. If TRUE, uses persistent disk cache. Default is FALSE.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing retrodictions with the same columns as [load_predictions()].
#' @seealso [load_predictions()], [load_results()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_retrodictions(2026)
#' })
#' }
#' @export
load_retrodictions <- function(seasons = get_afl_season(), rounds = get_afl_week(type = "next"), use_disk_cache = FALSE, columns = NULL) {
  if (isTRUE(seasons) && missing(rounds)) rounds <- TRUE
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("retrodictions", "retrodictions", seasons)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds, use_disk_cache = use_disk_cache, columns = columns)

  if (nrow(out) > 0) {
    if (!data.table::is.data.table(out)) out <- data.table::as.data.table(out)
    .normalise_predictions_columns(out)
    out <- tibble::as_tibble(out)
  }

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
#' \dontrun{
#' try({ # prevents cran errors
#'   load_torp_ratings()
#' })
#' }
#' @export
load_torp_ratings <- function(columns = NULL) {
  base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")
  url <- paste0(base_url, "/ratings-data/torp_ratings.parquet")
  out <- load_from_url(url, columns = columns)

  if (nrow(out) == 0) {
    cli::cli_warn("No TORP ratings data loaded. The file may not exist yet or the download failed.")
  }
  out
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
#' @param rounds A numeric vector of round numbers to filter to, or `TRUE`
#'   (default) for all rounds.
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
#' \dontrun{
#' try({ # prevents cran errors
#'   load_player_game_ratings(2024)
#'   load_player_game_ratings(2024, rounds = 1:5)
#' })
#' }
#' @export
load_player_game_ratings <- function(seasons = get_afl_season(), rounds = TRUE, use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("player_game_ratings-data", "player_game_ratings", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  # Post-load round filter (data is stored per-season, not per-round)
  if (!isTRUE(rounds) && "round" %in% names(out)) {
    out <- out[out$round %in% rounds, ]
  }

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
#' \dontrun{
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
#' \dontrun{
#' try({ # prevents cran errors
#'   load_team_ratings()
#' })
#' }
#' @export
load_team_ratings <- function(columns = NULL) {
  base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")
  url <- paste0(base_url, "/team_ratings-data/team_ratings.parquet")
  out <- load_from_url(url, columns = columns)

  if (nrow(out) == 0) {
    cli::cli_warn("No team ratings data loaded. The file may not exist yet or the download failed.")
  }
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
#'   `home_team_name`, `away_team_name`, `team`, `exp_pts`,
#'   `delta_epv`, `wp`, `wpa`, `description`, `player_name`, `play_type`,
#'   `shot_row`, and `points_shot`.
#' @seealso [load_pbp()], [load_xg()]
#' @examples
#' \dontrun{
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
#' \dontrun{
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


#' Load Player Skill Ratings (PSR)
#'
#' @description Loads pre-computed PSR (Player Skill Ratings) from the
#'   [torpdata repository](https://github.com/peteowen1/torpdata).
#'   PSR represents each player's predicted contribution to match margin
#'   based on their skill profile.
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL
#'   seasons — defaults to latest season. If set to `TRUE`, returns all
#'   available data since 2021.
#' @param use_disk_cache Logical. If `TRUE`, uses persistent disk cache for
#'   faster repeated loads. Default is `FALSE`.
#' @param columns Optional character vector of column names to read. If NULL (default), reads all columns.
#'
#' @return A data frame containing PSR data with columns including
#'   \code{player_id}, \code{player_name}, \code{season}, \code{round},
#'   \code{pos_group}, \code{psr_raw}, and \code{psr}.
#' @seealso [calculate_psr()], [load_player_skills()], [load_torp_ratings()]
#' @examples
#' \dontrun{
#' try({ # prevents cran errors
#'   load_psr(2024)
#'   load_psr(TRUE)  # all seasons
#' })
#' }
#' @export
load_psr <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("psr-data", "psr", seasons)

  out <- load_from_url(urls, seasons = seasons, use_disk_cache = use_disk_cache, columns = columns)

  return(out)
}
