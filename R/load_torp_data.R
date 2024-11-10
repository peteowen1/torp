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
  validate_seasons_and_rounds(seasons, rounds)

  urls <- generate_urls("chains-data", "chains_data", seasons, rounds)

  out <- load_from_url(urls, seasons = seasons, rounds = rounds)

  return(out)
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

  if(seasons == TRUE){
    seasons <- 2021:get_afl_season()
  }

  if(rounds == TRUE){
    rounds <- 0:28
  }

  validate_seasons_and_rounds(seasons, rounds)

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

  urls <- generate_urls("player_stats-data", "player_stats_data", seasons)

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
#'   load_fixtures(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_fixtures <- function(seasons = get_afl_season()) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("fixtures-data", "fixtures", seasons)

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
#'   load_teams(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_teams <- function(seasons = get_afl_season()) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("teams-data", "teams", seasons)

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
#'   load_player_details(2021:2022)
#' })
#' }
#' @export
#' @importFrom glue glue
load_player_details <- function(seasons = get_afl_season()) {
  seasons <- validate_seasons(seasons)

  urls <- generate_urls("player_details-data", "player_details_data", seasons)

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
    p <- NULL
    if (is_installed("progressr")) p <- progressr::progressor(along = url)
    out <- lapply(url, progressively(rds_from_url, p))
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
#' @importFrom cli cli_warn
#' @importFrom data.table data.table setDT
rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)

  if (inherits(load, "try-error")) {
    cli::cli_warn("Failed to readRDS from {.url {url}}")
    return(data.table::data.table())
  }

  data.table::setDT(load)
  return(load)
}

# Helper functions

#' Validate seasons and rounds
#'
#' @param seasons A numeric vector or TRUE
#' @param rounds A numeric vector or TRUE
#'
#' @return NULL
#' @keywords internal
validate_seasons_and_rounds <- function(seasons, rounds) {
  validate_seasons(seasons)

  if (isTRUE(rounds)) rounds <- 0:28

  stopifnot(
    is.numeric(rounds),
    rounds >= 0,
    rounds <= 28
  )
}

#' Validate seasons
#'
#' @param seasons A numeric vector or TRUE
#'
#' @return NULL
#' @keywords internal
validate_seasons <- function(seasons) {
  if (isTRUE(seasons)) seasons <- 2021:get_afl_season()

  stopifnot(
    is.numeric(seasons),
    seasons >= 2021,
    seasons <= get_afl_season()
  )

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
  base_url <- "https://github.com/peteowen1/torpdata/releases/download"

    if(is.null(rounds)){
      combinations <- expand.grid(seasons = seasons)

      urls <- glue::glue("{base_url}/{data_type}/{file_prefix}_{combinations$seasons}.rds")
      urls <- sort(urls)
    }

    if(!is.null(rounds)){
      rounds_02d <- sprintf("%02d", rounds)
      combinations <- expand.grid(seasons = seasons, rounds = rounds_02d)

      urls <- glue::glue("{base_url}/{data_type}/{file_prefix}_{combinations$seasons}_{combinations$rounds}.rds")
      urls <- sort(urls)
    }

  current_season <- get_afl_season()
  current_round <- sprintf("%02d", get_afl_week())
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
