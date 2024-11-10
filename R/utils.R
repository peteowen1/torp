#' Get AFL Season
#'
#' A helper function to choose the most recent season available for a given dataset
#'
#' @param type A character string: "current" returns the current season, "next" returns the upcoming season.
#'
#' @return An integer representing the AFL season year.
#' @export
#' @importFrom lubridate with_tz as_date
#' @importFrom dplyr filter
#' @importFrom cli cli_abort
get_afl_season <- function(type = "current") {
  if (!type %in% c("current", "next")) {
    cli::cli_abort('type must be one of: "current" or "next"')
  }
  time_aest <- lubridate::with_tz(Sys.time(), tzone = "Australia/Brisbane")
  current_day <- lubridate::as_date(time_aest)
  past_fixtures <- torp::fixtures %>% dplyr::filter(.data$utcStartTime < current_day)
  future_fixtures <- torp::fixtures %>% dplyr::filter(.data$utcStartTime >= current_day)
  if (type == "current" | nrow(future_fixtures) == 0) {
    season <- as.numeric(max(past_fixtures$compSeason.year))
  } else {
    season <- as.numeric(min(future_fixtures$compSeason.year))
  }
  return(season)
}

#' Get AFL Week
#'
#' Determine the current or next AFL week (round) based on fixtures.
#'
#' @param type A character string: "current" returns the current week, "next" returns the upcoming week.
#'
#' @return An integer representing the AFL week (round) number.
#' @export
#' @importFrom lubridate with_tz as_date
#' @importFrom dplyr filter
#' @importFrom cli cli_abort
get_afl_week <- function(type = "current") {
  if (!type %in% c("current", "next")) {
    cli::cli_abort('type must be one of: "current" or "next"')
  }
  season <- get_afl_season(type)
  time_aest <- lubridate::with_tz(Sys.time(), tzone = "Australia/Brisbane")
  current_day <- lubridate::as_date(time_aest)
  past_fixtures <- torp::fixtures %>%
    dplyr::filter(.data$utcStartTime < current_day, .data$compSeason.year == season)
  future_fixtures <- torp::fixtures %>%
    dplyr::filter(.data$utcStartTime >= current_day, .data$compSeason.year == season)
  if (type == "current" | nrow(future_fixtures) == 0) {
    round <- as.numeric(max(past_fixtures$round.roundNumber))
  } else {
    round <- as.numeric(min(future_fixtures$round.roundNumber))
  }
  return(round)
}

#' Check if a package is installed
#'
#' @param pkg Name of the package to check.
#'
#' @return A logical value indicating whether the package is installed.
#' @keywords internal
is_installed <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

#' Progressively apply a function
#'
#' This function helps add progress-reporting to any function. Given function `f()` and progressor `p()`,
#' it will return a new function that calls `f()` and then calls `p()` after every iteration.
#'
#' @param f A function to add progressor functionality to.
#' @param p A function such as one created by `progressr::progressor()` - also accepts purrr-style lambda functions.
#'
#' @return A function that does the same as `f` but calls `p()` after each iteration.
#' @export
#' @importFrom rlang as_function
progressively <- function(f, p = NULL) {
  if (is.null(p)) p <- function(...) NULL
  p <- rlang::as_function(p)
  f <- rlang::as_function(f)
  function(...) {
    on.exit(p())
    f(...)
  }
}

#' Get mode of a vector
#'
#' This function returns the mode (most frequent value) of a vector.
#'
#' @param x A vector of values.
#'
#' @return The mode (most frequent value) of the input vector.
#' @export
get_mode <- function(x) {
  t <- table(x)
  names(t)[which.max(t)]
}

utils::globalVariables(c(".data"))
