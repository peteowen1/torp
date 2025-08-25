#' Save a Data Frame to a GitHub Release via Piggyback
#'
#' Saves a data frame as an `.rds` file and uploads it to a GitHub release using the `piggyback` package.
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
#' save_to_release(my_df, "latest_data", "v1.0.0")
#' }
save_to_release <- function(df, file_name, release_tag) {
  temp_dir <- tempdir(check = TRUE)
  .f_name <- paste0(file_name, ".rds")
  saveRDS(df, file.path(temp_dir, .f_name))

  piggyback::pb_upload(file.path(temp_dir, .f_name),
                       repo = get_torp_data_repo(),
                       tag = release_tag
  )
}

#' Read an RDS File from a GitHub Release via Piggyback
#'
#' Downloads and reads an `.rds` file from a GitHub release using the `piggyback` package.
#'
#' @param file_name The base name of the file (without `.rds` extension).
#' @param release_tag The GitHub release tag the file is associated with.
#'
#' @return An R object read from the downloaded `.rds` file.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- file_reader("latest_data", "v1.0.0")
#' }
file_reader <- function(file_name, release_tag) {
  f_name <- paste0(file_name, ".rds")
  piggyback::pb_download(f_name,
                         repo = get_torp_data_repo(),
                         tag = release_tag,
                         dest = tempdir()
  )
  temp_dir <- tempdir(check = TRUE)

  readRDS(file.path(temp_dir, f_name))
}


#' Load Chains Data
#'
#' @description Loads chains data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to latest round. If set to `TRUE`, returns all available rounds in the given season range.
#'
#' @return A data frame containing chains data.
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_chains(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_chains <- function(seasons = get_afl_season(), rounds = get_afl_week()) {
  tryCatch({
    seasons <- validate_seasons(seasons)
    rounds <- validate_rounds(rounds)

    urls <- generate_urls("chains-data", "chains_data", seasons, rounds)

    if (length(urls) == 0) {
      cli::cli_warn("No data URLs generated for seasons {paste(seasons, collapse = ', ')} and rounds {paste(rounds, collapse = ', ')}")
      return(data.table::data.table())
    }

    out <- load_from_url(urls, seasons = seasons, rounds = rounds)

    if (nrow(out) == 0) {
      cli::cli_warn("No chains data found for the specified seasons and rounds")
    }

    return(out)
  }, error = function(e) {
    cli::cli_abort("Failed to load chains data: {conditionMessage(e)}")
  })
}

#' Load Play By Play Data
#'
#' @description Loads play by play seasons from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param rounds A numeric vector associated with given AFL round - defaults to latest round. If set to `TRUE`, returns all available rounds in the given season range.
#'
#' @return A data frame containing play by play data.
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_pbp(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_pbp <- function(seasons = get_afl_season(), rounds = get_afl_week()) {
  seasons <- validate_seasons(seasons)
  rounds <- validate_rounds(rounds)

  urls <- generate_urls("pbp-data", "pbp_data", seasons, rounds)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds)

  return(out)
}

#' Load Expected Goals (xG) Data
#'
#' @description Loads xg data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#'
#' @return A data frame containing xG data.
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_xg(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_xg <- function(seasons = get_afl_season()) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("xg-data", "xg_data", seasons)

  out <- load_from_url(urls, seasons = seasons)

  return(out)
}

#' Load Player Stats Data
#'
#' @description Loads player stats data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#'
#' @return A data frame containing player stats data.
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_player_stats(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_player_stats <- function(seasons = get_afl_season()) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_stats-data", "player_stats", seasons)

  out <- load_from_url(urls, seasons = seasons)

  return(out)
}

#' Load AFL Fixture Data
#'
#' @description Loads AFL fixture and schedule data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#' @param all Logical. If TRUE, loads all available fixture data from 2018 onwards.
#' @param use_cache Logical. If TRUE (default), uses cached data when available to speed up repeated calls.
#' @param cache_ttl Numeric. Time-to-live for cached data in seconds. Default is 3600 (1 hour).
#' @param verbose Logical. If TRUE, prints cache hit/miss information.
#'
#' @return A data frame containing AFL fixture and schedule data.
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
#' @importFrom glue glue
load_fixtures <- function(seasons = NULL, all = FALSE, use_cache = TRUE, cache_ttl = 3600, verbose = FALSE) {
  # Process parameters
  if (all) {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    seasons <- 2018:current_year
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
  out <- load_from_url(urls, seasons = seasons)
  
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
#'
#' @return A data frame containing AFL team and player lineup data.
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_teams(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_teams <- function(seasons = get_afl_season()) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls(data_type = "teams-data", file_prefix = "teams", seasons = seasons)

  out <- load_from_url(urls, seasons = seasons)

  return(out)
}

#' Load AFL Match Results Data
#'
#' @description Loads AFL match results and scores from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#'
#' @return A data frame containing AFL match results and final scores.
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_results(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_results <- function(seasons = get_afl_season()) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("results-data", "results", seasons)

  out <- load_from_url(urls, seasons = seasons)

  return(out)
}

#' Load AFL Player Details Data
#'
#' @description Loads AFL player biographical and details data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#'
#' @return A data frame containing AFL player biographical details including names, ages, and team affiliations.
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_player_details(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_player_details <- function(seasons = get_afl_season()) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_details-data", "player_details", seasons)

  out <- load_from_url(urls, seasons = seasons)

  return(out)
}

#' Load AFL Match Predictions Data
#'
#' @description Loads AFL match predictions and probability data from the [torpdata repository](https://github.com/peteowen1/torpdata)
#'
#' @param seasons A numeric vector of 4-digit years associated with given AFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 2021.
#'
#' @return A data frame containing AFL match predictions including win probabilities and expected scores.
#' @examples
#' \donttest{
#' try({ # prevents cran errors
#'   load_predictions(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_predictions <- function(seasons = get_afl_season()) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("predictions", "predictions", seasons)

  out <- load_from_url(urls, seasons = seasons)

  return(out)
}

#' Load any rds/csv/csv.gz/parquet/qs file from a remote URL
#'
#' @param url A vector of URLs to load into memory. If more than one URL provided, will row-bind them.
#' @param seasons A numeric vector of years that will be used to filter the dataframe's `season` column. If `TRUE` (default), does not filter.
#' @param rounds A numeric vector of rounds that will be used to filter the dataframe's `round` column. If `TRUE` (default), does not filter.
#' @param peteowen1 TRUE to add peteowen1_data classing and attributes.
#' @param ... Named arguments that will be added as attributes to the data, e.g. `peteowen1_type` = "pbp"
#'
#' @return A data frame, possibly of type `peteowen1_data`
#' @export
#' @importFrom data.table rbindlist setDT
#' @importFrom progressr progressor
load_from_url <- function(url, ..., seasons = TRUE, rounds = TRUE, peteowen1 = FALSE) {
  url <- as.character(url)

  if (length(url) == 1) {
    out <- rds_from_url(url)
    if (!isTRUE(seasons)) {
      stopifnot(is.numeric(seasons))
      if ("season" %in% names(out)) out <- out[out$season %in% seasons, ]
    }
    if (!isTRUE(rounds)) {
      stopifnot(is.numeric(rounds))
      if ("round" %in% names(out)) out <- out[out$round %in% rounds, ]
    }
  } else {
    # Use parallel processing for multiple URLs
    f <- purrr::in_parallel(
      \(url_single) {
        # Load required packages on worker
        suppressPackageStartupMessages({
          library(data.table)
          library(cli)
        })
        
        # Define rds_from_url functionality directly in worker
        # Validate URL format
        if (!is.character(url_single) || length(url_single) != 1 || nchar(url_single) == 0) {
          cli::cli_abort("URL must be a single non-empty character string")
        }
        
        if (!grepl("^https?://", url_single)) {
          cli::cli_abort("URL must start with http:// or https://")
        }
        
        # Check internet connectivity (simplified check)
        internet_available <- tryCatch({
          con <- url("https://www.google.com", open = "r")
          close(con)
          TRUE
        }, error = function(e) {
          FALSE
        })
        
        if (!internet_available) {
          cli::cli_abort("No internet connection available")
        }
        
        # Load RDS data
        con <- NULL
        tryCatch({
          con <- url(url_single)
          on.exit({
            if (!is.null(con)) {
              try(close(con), silent = TRUE)
            }
          })
          
          load <- readRDS(con)
          
          # Validate that we got actual data
          if (is.null(load)) {
            cli::cli_warn("No data returned from {.url {url_single}}")
            return(data.table::data.table())
          }
          
          data.table::setDT(load)
          return(load)
          
        }, error = function(e) {
          error_msg <- conditionMessage(e)
          
          if (grepl("404|Not Found", error_msg, ignore.case = TRUE)) {
            cli::cli_warn("Data file not found at {.url {url_single}} - file may not exist for this season/round combination")
          } else if (grepl("timeout|timed out", error_msg, ignore.case = TRUE)) {
            cli::cli_warn("Connection timeout while downloading from {.url {url_single}} - please try again")
          } else if (grepl("cannot open|connection", error_msg, ignore.case = TRUE)) {
            cli::cli_warn("Failed to connect to {.url {url_single}} - check internet connection")
          } else {
            cli::cli_warn("Failed to load data from {.url {url_single}}: {error_msg}")
          }
          
          return(data.table::data.table())
        })
      }
    )
    
    # Create data frame with URLs for pmap
    url_grid <- data.frame(url_single = url, stringsAsFactors = FALSE)
    
    # Process URLs in parallel with progress
    out <- purrr::pmap(url_grid, f, .progress = TRUE)
    out <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  }

  if (peteowen1) {
    class(out) <- c("peteowen1_data", class(out))
    attributes(out) <- c(attributes(out), list(...))
  }

  return(out)
}

#' Load .rds file from a remote connection
#'
#' @param url A character URL
#'
#' @return A data frame as created by [`readRDS()`]
#' @export
#' @importFrom cli cli_warn cli_abort
#' @importFrom data.table data.table setDT
rds_from_url <- function(url) {
  # Validate URL format
  if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
    cli::cli_abort("URL must be a single non-empty character string")
  }

  if (!grepl("^https?://", url)) {
    cli::cli_abort("URL must start with http:// or https://")
  }

  # Check internet connectivity
  if (!check_internet_connection()) {
    cli::cli_abort("No internet connection available")
  }

  con <- NULL
  tryCatch({
    con <- url(url)
    on.exit({
      if (!is.null(con)) {
        try(close(con), silent = TRUE)
      }
    })

    load <- readRDS(con)

    # Validate that we got actual data
    if (is.null(load)) {
      cli::cli_warn("No data returned from {.url {url}}")
      return(data.table::data.table())
    }

    data.table::setDT(load)
    return(load)

  }, error = function(e) {
    error_msg <- conditionMessage(e)

    if (grepl("404|Not Found", error_msg, ignore.case = TRUE)) {
      cli::cli_warn("Data file not found at {.url {url}} - file may not exist for this season/round combination")
    } else if (grepl("timeout|timed out", error_msg, ignore.case = TRUE)) {
      cli::cli_warn("Connection timeout while downloading from {.url {url}} - please try again")
    } else if (grepl("cannot open|connection", error_msg, ignore.case = TRUE)) {
      cli::cli_warn("Failed to connect to {.url {url}} - check internet connection")
    } else {
      cli::cli_warn("Failed to load data from {.url {url}}: {error_msg}")
    }

    return(data.table::data.table())
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
  # Default repository
  return("peteowen1/torpdata")
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

#' Check if internet connection is available
#'
#' @return Logical indicating if internet connection is available
#' @keywords internal
check_internet_connection <- function() {
  tryCatch({
    con <- url("https://www.google.com", open = "r")
    close(con)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Validate seasons and rounds
#'
#' @param seasons A numeric vector or TRUE
#' @param rounds A numeric vector or TRUE
#'
#' @return NULL
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
#' @return NULL
#' @keywords internal
validate_seasons <- function(seasons) {
  if (isTRUE(seasons)) seasons <- 2021:(get_afl_season())

  if (!is.numeric(seasons)) {
    cli::cli_abort("Seasons must be numeric values or TRUE")
  }

  current_season <- tryCatch({
    get_afl_season()
  }, error = function(e) {
    cli::cli_warn("Could not determine current AFL season, using 2025 as default")
    2025
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
#'
#' @return A character vector of URLs
#' @keywords internal
#' @importFrom glue glue
generate_urls <- function(data_type, file_prefix, seasons, rounds = NULL) {
  base_url <- paste0("https://github.com/", get_torp_data_repo(), "/releases/download")

  if (is.null(rounds)) {
    combinations <- expand.grid(seasons = seasons)

    urls <- glue::glue("{base_url}/{data_type}/{file_prefix}_{combinations$seasons}.rds")
    urls <- sort(urls)
  }

  if (!is.null(rounds)) {
    rounds_02d <- sprintf("%02d", rounds)
    combinations <- expand.grid(seasons = seasons, rounds = rounds_02d)

    urls <- glue::glue("{base_url}/{data_type}/{file_prefix}_{combinations$seasons}_{combinations$rounds}.rds")
    urls <- sort(urls)
  }

  current_season <- get_afl_season()
  current_round <- 99

  if (data_type != "fixtures-data") {
    current_round <- sprintf("%02d", get_afl_week())
  }

  max_url <- glue::glue("{base_url}/{data_type}/{file_prefix}_{current_season}_{current_round}.rds")

  urls <- urls[urls <= max_url]

  return(urls)
}

#' Check if a package is installed
#'
#' @param pkg Name of the package
#'
#' @return Logical indicating if the package is installed
#' @keywords internal
is_installed <- function(pkg) {
  return(requireNamespace(pkg, quietly = TRUE))
}
