#' Get Latest Season
#'
#' A helper function to choose the most recent season available for a given dataset
#'
#' @param roster a TRUE/FALSE flag: if TRUE, returns the current year if March 1st or later. if FALSE, returns the current year if September 1st or later. Otherwise returns current year minus 1.
#'
#'
#' @rdname latest_season
#' @return most recent season (a four digit numeric)
#' @family Date utils
#' @export
most_recent_season <- function(roster = FALSE) {
  today <- Sys.Date()
  current_year <- as.integer(format(today, format = "%Y"))
  current_month <- as.integer(format(today, format = "%m"))

  if ((isFALSE(roster) && current_month >= 9) ||
      (isTRUE(roster) && current_month >= 3)) {

    return(current_year)
  }

  return(current_year - 1)
}

#' @rdname latest_season
#' @export
get_latest_season <- most_recent_season

#' @rdname latest_season
#' @export
get_current_season <- most_recent_season


#' Get Current Week
#'
#' A helper function that returns the upcoming NFL regular season week based on either
#' the nflverse schedules file (as found in `load_schedules()`)
#' or some date-based heuristics (number of weeks since the first Monday of September)
#'
#' Note that the date heuristic will count a new week starting on Thursdays, while
#' the schedule-based method will count a new week after the last game of the previous
#' week, e.g. after MNF is completed. Tan and Ben argued for a while about this.
#'
#' @param use_date a logical to determine whether to use date-based heuristics to
#' determine current week, default FALSE (i.e. uses schedule file)
#'
#' @examples {
#'   \donttest{
#'     try({ # schedules file as per default requires online access
#'     get_current_week()
#'     })
#'   }
#'   # using the date method works offline
#'   get_current_week(use_date = TRUE)
#' }
#'
#' @family Date utils
#' @return current nfl regular season week as a numeric
#' @export
get_current_week <- function(use_date = TRUE) {

  if(!use_date){
    season <- NULL
    week <- NULL
    result <- NULL
    current_season <- data.table::as.data.table(load_schedules())[season == most_recent_season()]

    if(all(!is.na(current_season$result))) return(max(current_season$week, na.rm = TRUE))

    current_week <- current_season[is.na(result), week]
    return(min(current_week, na.rm = TRUE))
  }

  if(use_date){
    # Find first Monday of September in current season
    week1_sep <- as.POSIXlt(paste0(most_recent_season(),"-09-0",1:7), tz = "GMT")
    monday1_sep <- week1_sep[week1_sep$wday == 1]

    # NFL season starts 3 days later
    first_game <- monday1_sep
    first_game$mday <- first_game$mday + 3

    # current week number of nfl season is 1 + how many weeks have elapsed since first game
    current_week <- as.numeric(Sys.Date() - as.Date(first_game)) %/% 7 + 1

    # hardcoded week bounds because this whole date based thing has assumptions anyway
    if(current_week < 1) current_week <- 1
    if(current_week > 22) current_week <- 22

    return(current_week)
  }
}

is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

#' @keywords internal
"_PACKAGE"

#' @importFrom data.table `:=`
NULL

`%c%` <- function(x,y){
  ifelse(!is.na(x),x,y)
}

#' Progressively
#'
#' This function helps add progress-reporting to any function - given function `f()` and progressor `p()`,
#' it will return a new function that calls `f()` and then (on exiting) will call `p()` after every iteration.
#' This is inspired by purrr's `safely`, `quietly`, and `possibly` function decorators.
#'
#' @param f a function to add progressor functionality to.
#' @param p a function such as one created by `progressr::progressor()` - also accepts purrr-style lambda functions.
#'
#' @examples
#'
#' \donttest{
#' try({ # prevents cran errors
#'
#' urls <- rep("https://github.com/nflverse/nflverse-data/releases/download/test/combines.csv",3)
#'
#' lapply(urls, progressively(read.csv, ~cli::cli_progress_step('Loading...')))
#'
#' read_rosters <- function(urls){
#'   p <- progressr::progressor(along = urls)
#'   lapply(urls, progressively(read.csv, p))
#' }
#'
#' progressr::with_progress(read_rosters())
#'
#' })
#' }
#'
#' @return a function that does the same as `f` but it calls `p()` after iteration.
#'
#' @seealso <https://nflreadr.nflverse.com/articles/exporting_nflreadr.html> for vignette on exporting nflreadr in packages
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
