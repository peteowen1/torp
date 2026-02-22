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
  season <- get_afl_season("current")
  time_aest <- lubridate::with_tz(Sys.time(), tzone = "Australia/Brisbane")
  current_day <- lubridate::as_date(time_aest)

  # Try to load fixtures for current season, handle missing data gracefully
  all_fixtures <- tryCatch(
    {
      load_fixtures(season) |>
        dplyr::filter(.data$compSeason.year == season)
    },
    error = function(e) {
      cli::cli_warn("Could not load fixtures for season {season}: {e$message}")
      return(data.frame())
    }
  )

  # Handle empty fixtures (pre-season or missing data)
  if (nrow(all_fixtures) == 0) {
    cli::cli_warn("No fixtures found for season {season}. Returning round 0.")
    return(0)
  }

  past_fixtures <- all_fixtures |>
    dplyr::filter(.data$utcStartTime < current_day)
  future_fixtures <- all_fixtures |>
    dplyr::filter(.data$utcStartTime >= current_day)

  # Pre-season: no past fixtures yet
  if (nrow(past_fixtures) == 0) {
    round <- as.numeric(min(future_fixtures$round.roundNumber))
    if (type == "current") {
      return(0)
    }
    return(round)
  }

  # Post-season: no future fixtures
  if (nrow(future_fixtures) == 0) {
    return(as.numeric(max(past_fixtures$round.roundNumber)))
  }

  # Mid-season: both past and future fixtures exist
  if (type == "current") {
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


#' Get Proportion Through Day
#'
#' Calculates what proportion of the day has elapsed for a given datetime.
#'
#' @param datetime A POSIXct datetime object.
#'
#' @return A numeric value representing the proportion of the day elapsed (0-1).
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
get_mode <- function(x) {
  t <- table(x)
  names(t)[which.max(t)]
}

#' Vectorized Harmonic Mean of Two Numeric Vectors
#'
#' @description This function is intended for internal use and may be unexported in a future release.
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

  result <- ifelse(x <= 0 | y <= 0, NA_real_, 2 / (1 / x + 1 / y))
  return(result)
}


#' Normalize Player Names
#'
#' @description This function is intended for internal use and may be unexported in a future release.
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
#' @importFrom stringi stri_trans_general
#' @export
norm_name <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z ]", " ") |>
    stringr::str_squish()
}

#' Clean column names to snake_case
#'
#' Lightweight replacement for janitor::clean_names(). Converts column names to
#' snake_case by handling CamelCase, dots, spaces, and special characters.
#'
#' @param df A data frame.
#' @return The data frame with cleaned column names.
#' @keywords internal
torp_clean_names <- function(df) {
  nms <- names(df)
  # CamelCase to snake_case
  nms <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nms)
  # Replace dots, spaces, and special chars with underscores
  nms <- gsub("[. ]+", "_", nms)
  nms <- tolower(nms)
  nms <- gsub("[^a-z0-9_]", "_", nms)
  nms <- gsub("_+", "_", nms)
  nms <- gsub("^_|_$", "", nms)
  names(df) <- nms
  df
}

#' Convert NA factor levels to a named level
#'
#' Lightweight replacement for forcats::fct_na_value_to_level().
#' Converts a vector to factor and replaces NA values with the specified level.
#'
#' @param x A vector (character or factor).
#' @param level The level name to use for NA values.
#' @return A factor with NAs replaced by the specified level.
#' @keywords internal
fct_na_to_level <- function(x, level = "(Missing)") {
  x <- as.character(x)
  x[is.na(x)] <- level
  factor(x)
}

#' Create dummy columns for categorical variables
#'
#' Lightweight replacement for fastDummies::dummy_cols(). Creates binary 0/1
#' columns for each level of the specified factor/character columns.
#'
#' @param df A data frame.
#' @param select_columns Character vector of column names to create dummies for.
#' @param remove_first_dummy Logical; if TRUE, omits the first level (useful for regression).
#' @return The data frame with added dummy columns.
#' @keywords internal
torp_dummy_cols <- function(df, select_columns, remove_first_dummy = FALSE) {
  for (col in select_columns) {
    if (col %in% names(df)) {
      vals <- as.factor(df[[col]])
      lvls <- levels(vals)
      if (remove_first_dummy) lvls <- lvls[-1]
      for (lvl in lvls) {
        df[[paste0(col, "_", lvl)]] <- as.integer(!is.na(vals) & vals == lvl)
      }
    }
  }
  df
}

#' Bound a probability to avoid log(0) and numeric edge cases
#'
#' @param p Numeric vector of probabilities
#' @param lower Lower bound (default 0.001)
#' @param upper Upper bound (default 0.999)
#' @return Numeric vector bounded to \[lower, upper\]
#' @keywords internal
bound_probability <- function(p, lower = 0.001, upper = 0.999) {
  pmax(lower, pmin(upper, p))
}

#' Calculate time remaining in an AFL match (seconds)
#'
#' @param period Numeric vector of period numbers (1-4)
#' @param period_seconds Numeric vector of seconds elapsed in the current period
#' @return Numeric vector of seconds remaining in the match
#' @keywords internal
calculate_time_remaining <- function(period, period_seconds) {
  (AFL_MAX_PERIODS - period) * AFL_QUARTER_DURATION +
    (AFL_QUARTER_DURATION - period_seconds)
}

#' Calculate time remaining as a proportion of total game time
#'
#' @param period Numeric vector of period numbers (1-4)
#' @param period_seconds Numeric vector of seconds elapsed in the current period
#' @return Numeric vector of proportions (0 to 1)
#' @keywords internal
calculate_time_remaining_pct <- function(period, period_seconds) {
  calculate_time_remaining(period, period_seconds) / AFL_TOTAL_GAME_SECONDS
}

# Add Globals Variables
utils::globalVariables(c(".data", ".SD", "disp", "season.x", "tm"))




