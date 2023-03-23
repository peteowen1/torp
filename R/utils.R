#' Get AFL Season
#'
#' A helper function to choose the most recent season available for a given dataset
#'
#' @param type a TRUE/FALSE flag: if TRUE, returns the current year if March 1st or later. if FALSE, returns the current year if September 1st or later. Otherwise returns current year minus 1.
#'
#'
#' @rdname get_afl_season
#' @export
get_afl_season <- function(type="current") {
  time_aest <- lubridate::with_tz(Sys.time(),tzone = "Australia/Brisbane")
  current_day <- lubridate::as_date(time_aest)

  past_fixtures <- torp::fixtures %>% dplyr::filter(utcStartTime < current_day)
  future_fixtures <- torp::fixtures %>% dplyr::filter(utcStartTime >= current_day)

  if (!type %in% c('current','next')) {
    cli::cli_abort('type must be one of: "current" or "next"')
  }
  if (type == "current") {
    season <- as.numeric(max(past_fixtures$compSeason.year))
  }
  if (type == "next") {
    season <- as.numeric(min(future_fixtures$compSeason.year))
  }

  return(season)
}


#' Get AFL Week
#'
#' Note that the date heuristic will count a new week starting on Thursdays, while
#' the schedule-based method will count a new week after the last game of the previous
#' week.
#'
#' @param type a logical to determine whether to use date-based heuristics to
#' determine current week, default TRUE
#'
#' @export
get_afl_week <- function(type="current") {

  season <- get_afl_season(type)

  time_aest <- lubridate::with_tz(Sys.time(),tzone = "Australia/Brisbane")
  current_day <- lubridate::as_date(time_aest)

  past_fixtures <- torp::fixtures %>% dplyr::filter(utcStartTime < current_day, compSeason.year == season)
  future_fixtures <- torp::fixtures %>% dplyr::filter(utcStartTime >= current_day, compSeason.year == season)

  if (!type %in% c('current','next')) {
    cli::cli_abort('type must be one of: "current" or "next"')
  }
  if (type == "current") {
    round <- as.numeric(max(past_fixtures$round.roundNumber))
  }
  if (type == "next") {
    round <- as.numeric(min(future_fixtures$round.roundNumber))
  }

  return(round)
  }


is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)


#' Progressively
#'
#' This function helps add progress-reporting to any function - given function `f()` and progressor `p()`,
#' it will return a new function that calls `f()` and then (on exiting) will call `p()` after every iteration.
#' This is inspired by purrr's `safely`, `quietly`, and `possibly` function decorators.
#'
#' @param f a function to add progressor functionality to.
#' @param p a function such as one created by `progressr::progressor()` - also accepts purrr-style lambda functions.
#'
#'
#' @return a function that does the same as `f` but it calls `p()` after iteration.
#'
#' @export
progressively <- function(f, p = NULL){
  if(is.null(p)) p <- function(...) NULL
  p <- rlang::as_function(p)
  f <- rlang::as_function(f)

  function(...){
    on.exit(p())
    f(...)
  }

}
