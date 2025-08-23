#' Get AFL Season
#'
#' A helper function to choose the most recent season available for a given dataset.
#' Currently simplified to return the current calendar year - future enhancement
#' should implement proper AFL season detection based on fixture data.
#'
#' @param type A character string: "current" returns the current season, "next" returns the upcoming season.
#'
#' @return An integer representing the AFL season year.
#' @export
#' @importFrom lubridate year
#' @importFrom cli cli_abort
#' @examples
#' # Get the current AFL season
#' get_afl_season("current")
#'
#' # Get the next AFL season
#' get_afl_season("next")
get_afl_season <- function(type = "current") {
  if (!type %in% c("current", "next")) {
    cli::cli_abort('type must be one of: "current" or "next"')
  }

  # TODO: Implement proper AFL season detection based on fixture data
  # This would involve checking fixture dates to determine if we're in the
  # current season, off-season, or upcoming season period

  current_year <- lubridate::year(Sys.Date())

  # Simple implementation - assumes calendar year equals AFL season
  if (type == "next") {
    return(current_year + 1L)
  }

  return(current_year)
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
  season <- get_afl_season('current')
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

#' Get Proportion Through Day
#'
#' Calculates what proportion of the day has elapsed for a given datetime.
#'
#' @param datetime A POSIXct datetime object.
#'
#' @return A numeric value representing the proportion of the day elapsed (0-1).
#' @export
get_proportion_through_day <- function(datetime) {
  # Ensure datetime is POSIXct
  if (!inherits(datetime, "POSIXct")) {
    cli::cli_abort("Input must be a POSIXct datetime object.")
  }

  # Calculate seconds since midnight
  midnight <- as.POSIXct(format(datetime, "%Y-%m-%d 00:00:00"), tz = tz(datetime))
  seconds_since_midnight <- as.numeric(difftime(datetime, midnight, units = "secs"))

  # Proportion through the day
  proportion <- seconds_since_midnight / (24 * 60 * 60)
  return(proportion)
}

#' Get Proportion Through Year
#'
#' Calculates what proportion of the year has elapsed for a given datetime.
#'
#' @param datetime A POSIXct datetime object.
#'
#' @return A numeric value representing the proportion of the year elapsed (0-1).
#' @export
get_proportion_through_year <- function(datetime) {
  # Ensure datetime is POSIXct
  if (!inherits(datetime, "POSIXct")) {
    cli::cli_abort("Input must be a POSIXct datetime object.")
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
    cli::cli_abort("Input must be a POSIXct datetime object.")
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
#' Computes the row-wise harmonic mean of two numeric vectors. The harmonic mean
#' is particularly useful in AFL analytics for averaging rates and proportions,
#' giving less weight to extreme values than the arithmetic mean.
#'
#' @param x Numeric vector (e.g. home_shots).
#' @param y Numeric vector (e.g. away_shots).
#' @return A numeric vector of harmonic means. Returns NA for pairs where either value is 0.
#' @export
#' @examples
#' # Calculate harmonic mean of shot attempts
#' home_shots <- c(10, 15, 20)
#' away_shots <- c(12, 18, 25)
#' harmonic_mean(home_shots, away_shots)
#'
#' # Returns NA when one value is zero
#' harmonic_mean(c(10, 0, 20), c(15, 10, 25))
harmonic_mean <- function(x, y) {
  if (length(x) != length(y)) {
    cli::cli_abort("Vectors x and y must be the same length.")
  }

  if (!is.numeric(x) || !is.numeric(y)) {
    cli::cli_abort("Both x and y must be numeric vectors.")
  }

  result <- ifelse(x == 0 | y == 0, NA_real_, 2 / (1 / x + 1 / y))
  return(result)
}


#' Normalize Player Names
#'
#' Converts input character strings into a standardized format by:
#' \itemize{
#'   \item Converting accented characters to ASCII (e.g., "José" → "Jose")
#'   \item Lowercasing all text
#'   \item Removing non-alphabetic characters (keeping spaces)
#'   \item Collapsing multiple spaces into a single space
#' }
#'
#' Useful for preparing player names (or similar text fields) before
#' joining tables where names may not match exactly due to case,
#' punctuation, or diacritics.
#'
#' @param x A character vector of names (or other strings).
#'
#' @return A character vector with normalized names.
#' @examples
#' norm_name(c("Cam Zurhaar", "Cameron   Zúrhär", "  José López "))
#' # Returns: "cam zurhaar" "cameron zurhar" "jose lopez"
#'
#' @seealso [stringi::stri_trans_general()], [stringr::str_to_lower()]
#' @export
norm_name <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z ]", " ") |>
    stringr::str_squish()
}

# Add Globals Variables
utils::globalVariables(c(".data"))



