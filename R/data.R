#' AFL Season Fixtures
#'
#' Complete fixture list for the current AFL season including match scheduling,
#' team matchups, venues, and round information. Useful as offline fallback
#' when torpdata is unavailable.
#'
#' @format A data frame with fixture information including match IDs, start times,
#'   round details, home/away team information, venue details, and season metadata.
#'   Key columns include `id`, `providerId`, `utcStartTime`, `status`,
#'   `round.roundNumber`, `home.team.name`, `away.team.name`, `venue.name`,
#'   and `compSeason.year`.
#' @source AFL fixture data via fitzRoy package
#' @note For live/current fixtures, use `load_fixtures()` from torpdata.
#'   This bundled copy serves as offline fallback reference.
"fixtures"

