#' AFL Season Fixtures
#'
#' Complete fixture list for the current AFL season including match scheduling,
#' team matchups, venues, and round information. Useful as offline fallback
#' when torpdata is unavailable.
#'
#' @format A data frame with fixture information including match IDs, start times,
#'   round details, home/away team information, venue details, and season metadata.
#'   Key columns include `match_id`, `utc_start_time`, `status`,
#'   `round_number`, `home_team_name`, `away_team_name`, `venue_name`,
#'   and `season`.
#' @source AFL fixture data via AFL API
#' @note For live/current fixtures, use `load_fixtures()` from torpdata.
#'   This bundled copy serves as offline fallback reference.
"fixtures"

