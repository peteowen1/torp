# Local Data Storage for torpdata
#
# Provides functions to read/write parquet files locally in the
# torpdata/data/ directory, enabling fast offline access without
# network downloads. Files in torpdata/data/ are gitignored.

# Minimum file size for a valid parquet file. Files smaller than this
# are treated as invalid placeholders (e.g. empty GitHub release assets).
MIN_PARQUET_BYTES <- 100

#' Get Local Data Directory
#'
#' Returns the path to the local torpdata/data/ directory for storing
#' parquet files locally. Checks the `torp.local_data_dir` option first,
#' then auto-detects based on common workspace layouts.
#'
#' @return Character path to local data directory, or NULL if not found
#' @export
#'
#' @examples
#' \dontrun{
#' get_local_data_dir()
#'
#' # Set explicitly
#' set_local_data_dir("path/to/torpdata/data")
#' get_local_data_dir()
#' }
get_local_data_dir <- function() {
  # Explicit option takes precedence
  dir <- getOption("torp.local_data_dir")
  if (!is.null(dir) && dir.exists(dir)) return(normalizePath(dir, winslash = "/"))

  # Auto-detect: check common workspace locations

  candidates <- c(
    file.path(getwd(), "..", "torpdata", "data"),
    file.path(getwd(), "torpdata", "data")
  )

  for (candidate in candidates) {
    if (dir.exists(candidate)) return(normalizePath(candidate, winslash = "/"))
  }

  NULL
}

#' Set Local Data Directory
#'
#' Sets the path to the local torpdata/data/ directory for the current session.
#' Creates the directory if it doesn't exist.
#'
#' @param path Character path to the local data directory
#' @return Invisible normalized path
#' @export
#'
#' @examples
#' \dontrun{
#' set_local_data_dir("path/to/torpdata/data")
#' }
set_local_data_dir <- function(path) {
  if (!dir.exists(path)) {
    created <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!created || !dir.exists(path)) {
      cli::cli_abort("Failed to create local data directory: {path}")
    }
  }
  path <- normalizePath(path, winslash = "/")
  options(torp.local_data_dir = path)
  cli::cli_inform("Local data directory set to: {path}")
  invisible(path)
}

#' Get Local File Path for a URL
#'
#' Maps a GitHub release URL to a local file path in torpdata/data/.
#'
#' @param url Character URL of the remote parquet file
#' @return Character path to the local file, or NULL if local dir not configured
#' @keywords internal
get_local_path <- function(url) {
  local_dir <- get_local_data_dir()
  if (is.null(local_dir)) return(NULL)
  file.path(local_dir, basename(url))
}

#' Determine Max Age for a Local File Based on URL
#'
#' Historical seasons (before current year) never expire. Current season files
#' expire after 1 day to pick up mid-season updates.
#'
#' @param url Character URL of the remote parquet file
#' @return Numeric max age in days, or NULL for no expiration
#' @keywords internal
local_max_age_for_url <- function(url) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  fname <- basename(url)

  # Extract year from patterns like pbp_data_2024_all.parquet or fixtures_2024.parquet
  year_match <- regmatches(fname, regexpr("_(20[0-9]{2})(_|\\.|$)", fname))
  if (length(year_match) == 0 || nchar(year_match) == 0) return(1)

  file_year <- as.numeric(sub("^_(20[0-9]{2}).*", "\\1", year_match))

  if (file_year < current_year) return(NULL)  # historical = never expire
  1  # current season = expire after 1 day
}

#' Check if a File Exists Locally and is Fresh
#'
#' @param url Character URL to check for a local copy
#' @param max_age_days Maximum age in days before the local file is considered stale.
#'   If NULL, no staleness check is performed.
#' @return Logical
#' @keywords internal
is_locally_stored <- function(url, max_age_days = NULL) {
  path <- get_local_path(url)
  if (is.null(path) || !file.exists(path)) return(FALSE)

  if (!is.null(max_age_days)) {
    file_age_days <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "days"))
    if (file_age_days > max_age_days) return(FALSE)
  }

  TRUE
}

#' Read a Parquet File from Local Storage
#'
#' @param url Character URL used as the key to find the local file
#' @param columns Optional character vector of column names to read.
#'   If NULL (default), reads all columns.
#' @return Data frame if local file exists, NULL otherwise
#' @keywords internal
read_local_parquet <- function(url, columns = NULL) {
  path <- get_local_path(url)
  if (is.null(path) || !file.exists(path)) return(NULL)

  tryCatch({
    if (!is.null(columns)) {
      arrow::read_parquet(path, col_select = dplyr::any_of(columns))
    } else {
      arrow::read_parquet(path)
    }
  }, error = function(e) {
    msg <- conditionMessage(e)
    cli::cli_warn("Failed to read local file {path}: {msg}")
    # Only delete on likely corruption, not transient errors (memory, locking, version)
    if (grepl("corrupt|magic number|truncated|not a parquet|unexpected end", msg, ignore.case = TRUE)) {
      cli::cli_inform("Removing potentially corrupt file: {.path {basename(path)}}")
      unlink(path)
    }
    NULL
  })
}

#' Write a Parquet File to Local Storage
#'
#' @param url Character URL used as the key (filename extracted from it)
#' @param data Data frame to write
#' @return Invisible NULL
#' @keywords internal
write_local_parquet <- function(url, data) {
  path <- get_local_path(url)
  if (is.null(path)) return(invisible(NULL))

  tryCatch({
    arrow::write_parquet(data, path)
  }, error = function(e) {
    cli::cli_warn("Failed to write local file {path}: {conditionMessage(e)}")
    if (file.exists(path)) unlink(path)  # clean up partial write
  })

  invisible(NULL)
}

#' Save Data Frame Locally in torpdata/data/
#'
#' Saves a data frame as a parquet file in the local torpdata/data/ directory.
#' This is called automatically by [save_to_release()] but can also be
#' used directly.
#'
#' @param df A data frame to save
#' @param file_name A string for the file name (without extension)
#' @return Invisible logical indicating success
#' @export
#'
#' @examples
#' \donttest{
#' my_df <- data.frame(x = 1:3)
#' save_locally(my_df, "test_data")
#' }
save_locally <- function(df, file_name) {
  local_dir <- get_local_data_dir()
  if (is.null(local_dir)) {
    cli::cli_warn("Local data directory not found. Use {.fn set_local_data_dir} to configure.")
    return(invisible(FALSE))
  }

  path <- file.path(local_dir, paste0(file_name, ".parquet"))

  tryCatch({
    arrow::write_parquet(df, path)
    cli::cli_inform("Saved locally: {basename(path)}")
    invisible(TRUE)
  }, error = function(e) {
    cli::cli_warn("Failed to save locally: {conditionMessage(e)}")
    # Remove stale local file so load functions don't serve outdated data
    if (unlink(path) != 0L) {
      cli::cli_warn("Could not remove stale local file: {.path {basename(path)}}")
    }
    invisible(FALSE)
  })
}

#' Check if a Download Should Be Skipped (Negative Cache)
#'
#' When a URL previously produced an invalid file (too small / read error),
#' a `.skip` marker is written next to the expected local path. This avoids
#' re-downloading known-bad files every call.
#'
#' @param url Character URL to check
#' @return Logical — TRUE if a fresh `.skip` marker exists
#' @keywords internal
is_download_skippable <- function(url) {
  local_path <- get_local_path(url)
  if (is.null(local_path)) return(FALSE)

  skip_path <- paste0(local_path, ".skip")
  if (!file.exists(skip_path)) return(FALSE)

  max_age <- local_max_age_for_url(url)
  if (is.null(max_age)) max_age <- 30  # historical = cap at 30 days

  file_age <- as.numeric(difftime(Sys.time(), file.info(skip_path)$mtime, units = "days"))
  if (is.na(file_age)) return(FALSE)
  file_age <= max_age
}

#' Mark a URL as Skippable (Negative Cache)
#'
#' Writes a tiny `.skip` marker so future calls don't re-download a
#' known-invalid file.
#'
#' @param url Character URL
#' @return Invisible NULL
#' @keywords internal
mark_download_skippable <- function(url) {
  local_path <- get_local_path(url)
  if (is.null(local_path)) return(invisible(NULL))
  skip_path <- paste0(local_path, ".skip")
  tryCatch(writeLines("skip", skip_path), error = function(e) {
    cli::cli_warn("Could not write skip marker for {.url {basename(url)}}: {conditionMessage(e)}")
  })
  invisible(NULL)
}

#' Clear Skip Markers (Negative Cache)
#'
#' Removes all `.skip` marker files from the local data directory,
#' allowing previously skipped URLs to be re-attempted on next load.
#'
#' @return Invisible count of markers removed
#' @export
clear_skip_markers <- function() {
  local_dir <- get_local_data_dir()
  if (is.null(local_dir)) {
    cli::cli_inform("No local data directory configured.")
    return(invisible(0L))
  }

  skip_files <- list.files(local_dir, pattern = "\\.skip$", full.names = TRUE)
  if (length(skip_files) == 0) {
    cli::cli_inform("No skip markers to clear.")
    return(invisible(0L))
  }

  unlink(skip_files)
  cli::cli_inform("Cleared {length(skip_files)} skip marker{?s}.")
  invisible(length(skip_files))
}

#' Download TORP Data for Local Storage
#'
#' Downloads parquet files from GitHub releases to the local `torpdata/data/`
#' directory for fast offline access. Subsequent calls to `load_pbp()`,
#' `load_chains()`, etc. will read from local files instead of downloading.
#'
#' @param data_types Character vector of data types to download. Options:
#'   `"pbp"`, `"chains"`, `"ep_wp_chart"`, `"fixtures"`, `"player_stats"`,
#'   `"player_details"`, `"player_game"`, `"results"`, `"xg"`, `"teams"`,
#'   `"predictions"`, `"player_game_ratings"`, `"player_season_ratings"`,
#'   `"torp_ratings"`, `"team_ratings"`.
#'   Default is `"all"` which downloads all types.
#' @param seasons A numeric vector of 4-digit years, or `TRUE` for all
#'   available seasons (2021 to current). Default is `TRUE`.
#' @param overwrite Logical. If `FALSE` (default), skips files that already
#'   exist locally.
#'
#' @return Invisible NULL. Called for side effects (file downloads).
#' @export
#'
#' @examples
#' \dontrun{
#' # Download all PBP data
#' download_torp_data("pbp")
#'
#' # Download multiple types for specific seasons
#' download_torp_data(c("pbp", "chains"), seasons = 2024:2025)
#'
#' # Download everything
#' download_torp_data()
#' }
download_torp_data <- function(data_types = "all", seasons = TRUE, overwrite = FALSE) {
  local_dir <- get_local_data_dir()
  if (is.null(local_dir)) {
    cli::cli_abort(c(
      "Local data directory not found.",
      "i" = "Set it with {.fn set_local_data_dir} or ensure {.path torpdata/data/} exists in your workspace."
    ))
  }

  seasons <- validate_seasons(seasons)

  # Map data_types to release tags and file prefixes
  type_map <- list(
    pbp            = list(tag = "pbp-data",            prefix = "pbp_data",            has_rounds = TRUE),
    chains         = list(tag = "chains-data",          prefix = "chains_data",          has_rounds = TRUE),
    ep_wp_chart    = list(tag = "ep_wp_chart-data",     prefix = "ep_wp_chart",          has_rounds = TRUE),
    fixtures       = list(tag = "fixtures-data",        prefix = "fixtures",             has_rounds = FALSE),
    player_stats   = list(tag = "player_stats-data",    prefix = "player_stats",         has_rounds = FALSE),
    player_details = list(tag = "player_details-data",  prefix = "player_details",       has_rounds = FALSE),
    player_game    = list(tag = "player_game-data",     prefix = "player_game",          has_rounds = FALSE),
    results        = list(tag = "results-data",         prefix = "results",              has_rounds = FALSE),
    xg             = list(tag = "xg-data",              prefix = "xg_data",              has_rounds = FALSE),
    teams          = list(tag = "teams-data",           prefix = "teams",                has_rounds = FALSE),
    predictions    = list(tag = "predictions",          prefix = "predictions",          has_rounds = FALSE),
    player_game_ratings   = list(tag = "player_game_ratings-data",   prefix = "player_game_ratings",   has_rounds = FALSE),
    player_season_ratings = list(tag = "player_season_ratings-data", prefix = "player_season_ratings", has_rounds = FALSE),
    torp_ratings   = list(tag = "ratings-data",         prefix = "torp_ratings",         has_rounds = FALSE, no_season = TRUE),
    team_ratings   = list(tag = "team_ratings-data",    prefix = "team_ratings",         has_rounds = FALSE, no_season = TRUE)
  )

  if (identical(data_types, "all")) {
    data_types <- names(type_map)
  }

  invalid <- setdiff(data_types, names(type_map))
  if (length(invalid) > 0) {
    cli::cli_abort("Unknown data type{?s}: {.val {invalid}}. Valid types: {.val {names(type_map)}}")
  }

  base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")

  # Build list of URLs and destination paths
  urls <- character(0)
  dest_files <- character(0)

  for (dtype in data_types) {
    info <- type_map[[dtype]]

    if (isTRUE(info$no_season)) {
      # Single file (no season suffix)
      fname <- paste0(info$prefix, ".parquet")
      urls <- c(urls, paste0(base_url, "/", info$tag, "/", fname))
      dest_files <- c(dest_files, file.path(local_dir, fname))
    } else if (info$has_rounds) {
      # Round-based types: download _all aggregated files
      for (s in seasons) {
        fname <- paste0(info$prefix, "_", s, "_all.parquet")
        urls <- c(urls, paste0(base_url, "/", info$tag, "/", fname))
        dest_files <- c(dest_files, file.path(local_dir, fname))
      }
    } else {
      for (s in seasons) {
        fname <- paste0(info$prefix, "_", s, ".parquet")
        urls <- c(urls, paste0(base_url, "/", info$tag, "/", fname))
        dest_files <- c(dest_files, file.path(local_dir, fname))
      }
    }
  }

  # Skip existing files unless overwrite = TRUE
  if (!overwrite) {
    exists_mask <- file.exists(dest_files)
    if (any(exists_mask)) {
      cli::cli_inform("Skipping {sum(exists_mask)} file{?s} that already exist locally.")
      urls <- urls[!exists_mask]
      dest_files <- dest_files[!exists_mask]
    }
  }

  if (length(urls) == 0) {
    cli::cli_inform("All files already exist locally. Use {.arg overwrite = TRUE} to re-download.")
    return(invisible(NULL))
  }

  cli::cli_inform("Downloading {length(urls)} file{?s} to {.path {local_dir}}...")
  dl <- tryCatch(
    curl::multi_download(urls, destfiles = dest_files),
    error = function(e) {
      # Clean up any partially-written files
      written <- dest_files[file.exists(dest_files)]
      if (length(written) > 0) unlink(written)
      cli::cli_abort(c(
        "Download failed: {conditionMessage(e)}",
        "i" = "Check your internet connection and try again."
      ))
    }
  )

  success <- !is.na(dl$success) & dl$success
  n_fail <- sum(!success)

  # Remove invalid files (too small to be valid parquet, e.g. placeholder releases)
  valid_mask <- success & file.exists(dest_files) & file.size(dest_files) >= MIN_PARQUET_BYTES
  invalid_mask <- success & file.exists(dest_files) & file.size(dest_files) < MIN_PARQUET_BYTES

  if (any(invalid_mask)) {
    cli::cli_warn("Removed {sum(invalid_mask)} file{?s} that {?is/are} too small to be valid parquet (likely placeholder{?s}).")
    unlink(dest_files[invalid_mask])
  }

  n_valid <- sum(valid_mask, na.rm = TRUE)
  total_mb <- round(sum(file.size(dest_files[valid_mask]), na.rm = TRUE) / 1024 / 1024, 1)

  # Spot-check: verify at least one downloaded file is valid parquet
  if (n_valid > 0) {
    test_file <- dest_files[valid_mask][1]
    tryCatch(
      arrow::read_parquet(test_file, col_select = 1),
      error = function(e) {
        cli::cli_warn("Downloaded files may not be valid parquet. First file read failed: {conditionMessage(e)}")
      }
    )
  }

  cli::cli_inform("Downloaded {n_valid} file{?s} ({total_mb} MB total).")

  if (n_fail > 0) {
    failed_urls <- urls[!success]
    cli::cli_warn("{n_fail} file{?s} failed to download: {.url {failed_urls}}")
    unlink(dest_files[!success])
  }

  invisible(NULL)
}
