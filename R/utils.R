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
  # if (!type %in% c("current", "next")) {
  #   cli::cli_abort('type must be one of: "current" or "next"')
  # }
  # time_aest <- lubridate::with_tz(Sys.time(), tzone = "Australia/Brisbane")
  # current_day <- lubridate::as_date(time_aest)
  # past_fixtures <- load_fixtures(all = TRUE) %>% dplyr::filter(.data$utcStartTime < current_day)
  # future_fixtures <- load_fixtures(all = TRUE) %>% dplyr::filter(.data$utcStartTime >= current_day)
  # if (type == "current" | nrow(future_fixtures) == 0) {
  #   season <- as.numeric(max(past_fixtures$compSeason.year))
  # } else {
  #   season <- as.numeric(min(future_fixtures$compSeason.year))
  # }
  # return(season)

  season <- lubridate::year(Sys.Date())

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
  past_fixtures <- load_fixtures() %>%
    dplyr::filter(.data$utcStartTime < current_day, .data$compSeason.year == season)
  future_fixtures <- load_fixtures() %>%
    dplyr::filter(.data$utcStartTime >= current_day, .data$compSeason.year == season)
  if ((type == "current" & nrow(past_fixtures) > 0) | nrow(future_fixtures) == 0) {
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
get_proportion_through_day <- function(datetime) {
  # Ensure datetime is POSIXct
  if (!inherits(datetime, "POSIXct")) {
    stop("Input must be a POSIXct datetime object.")
  }

  # Calculate seconds since midnight
  midnight <- as.POSIXct(format(datetime, "%Y-%m-%d 00:00:00"), tz = tz(datetime))
  seconds_since_midnight <- as.numeric(difftime(datetime, midnight, units = "secs"))

  # Proportion through the day
  proportion <- seconds_since_midnight / (24 * 60 * 60)
  return(proportion)
}

#' Get mode of a vector
#'
#' This function returns the mode (most frequent value) of a vector.
#'
#' @param x A vector of values.
#'
#' @return The mode (most frequent value) of the input vector.
#' @export
get_proportion_through_year <- function(datetime) {
  # Ensure datetime is POSIXct
  if (!inherits(datetime, "POSIXct")) {
    stop("Input must be a POSIXct datetime object.")
  }

  year <- year(datetime)

  # Start and end of the year
  start_of_year <- as.POSIXct(paste0(year, "-01-01 00:00:00"), tz = tz(datetime))
  start_of_next_year <- as.POSIXct(paste0(year + 1, "-01-01 00:00:00"), tz = tz(datetime))

  total_seconds <- as.numeric(difftime(start_of_next_year, start_of_year, units = "secs"))
  seconds_elapsed <- as.numeric(difftime(datetime, start_of_year, units = "secs"))

  proportion <- seconds_elapsed / total_seconds
  return(proportion)
}


#' Convert datetime to decimal hours
#'
#' Convert a POSIXct datetime to its decimal hour representation.
#'
#' @param datetime A POSIXct datetime object.
#'
#' @return Numeric decimal hour.
#' @importFrom lubridate hour minute second
#' @export
decimal_hour <- function(datetime) {
  # Ensure datetime is POSIXct
  if (!inherits(datetime, "POSIXct")) {
    stop("Input must be a POSIXct datetime object.")
  }

  h <- hour(datetime)
  m <- minute(datetime)
  s <- second(datetime)

  h + m / 60 + s / 3600
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

#' Vectorized Harmonic Mean of Two Numeric Vectors
#'
#' Computes the row-wise harmonic mean of two numeric vectors.
#'
#' @param x Numeric vector (e.g. home_shots).
#' @param y Numeric vector (e.g. away_shots).
#' @return A numeric vector of harmonic means.
#' @export
harmonic_mean <- function(x, y) {
  if (length(x) != length(y)) stop("Vectors x and y must be the same length.")

  result <- ifelse(x == 0 | y == 0, NA_real_, 2 / (1 / x + 1 / y))
  return(result)
}

# Add Globals Variables
utils::globalVariables(c(".data"))



