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
#' @importFrom tools file_path_sans_ext
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

  # Filter out future URLs by parsing season/round numerically from filenames
  # Only fetch current_round lazily — when a filename actually has a round to check
  n_before <- length(urls)
  keep <- vapply(urls, function(u) {
    fname <- tools::file_path_sans_ext(basename(u))
    # Extract trailing numeric segments: e.g. "pbp_data_2026_05" -> c(2026, 5)
    parts <- regmatches(fname, gregexpr("[0-9]+", fname))[[1]]
    if (length(parts) == 0) return(TRUE)
    file_season <- as.numeric(parts[length(parts) - (length(parts) > 1)])
    if (is.na(file_season) || file_season < 2020) {
      cli::cli_warn("Could not parse season from URL: {.url {u}}")
      return(TRUE)
    }
    if (file_season < current_season) return(TRUE)
    if (file_season > current_season) return(FALSE)
    # Same season: check round if applicable
    if (data_type == "fixtures-data") return(TRUE)
    if (length(parts) >= 2) {
      file_round <- as.numeric(parts[length(parts)])
      # "_all" files have no numeric round suffix, keep them
      if (grepl("_all\\.parquet$", u)) return(TRUE)
      # Lazy-fetch current round only when actually needed
      if (is.null(current_round)) current_round <<- get_afl_week()
      return(file_round <= current_round)
    }
    TRUE
  }, logical(1), USE.NAMES = FALSE)
  urls <- urls[keep]
  n_dropped <- n_before - length(urls)

  if (n_dropped > 0) {
    cli::cli_inform("Filtered {n_dropped} future URL{?s} (current season: {current_season}, round: {current_round}).")
  }

  return(as.character(urls))
}
