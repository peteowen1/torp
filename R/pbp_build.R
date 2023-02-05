#' Build pbp
#'
#' This function builds play-by-play data
#'
#' @param season a numeric vector of seasons that defaults to most recent season. Must be later than 2006.
#' @param version an EP model version - one of "latest" (default) or "v1.0.0" (these are currently identical)
#'
#' @examples
#' \donttest{
#' try({ # prevents cran-related errors
#'   pbp_build(season = 2022)
#' })
#' }
#'
#' @return a list containing three dataframes: `ep_weekly` provides a game-level summary by player, `ep_pbp_pass` provides EP data on pass plays, and `ep_pbp_rush` provides EP data on rush plays.
#'
#' @family main
#'
#' @export
pbp_build <- function(season = lubridate::year(Sys.Date()) , version = "latest"){

  version <- rlang::arg_match0(version, c("latest", "v1.0.0"))

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2021
  )

  vcli_rule("Starting pbp build for {paste(unique(range(season)),collapse = '-')} season(s)! {Sys.time()}")

  vcli_alert("Loading pbp {Sys.time()}")
  pbp <- torp::load_pbp(season)

  vcli_alert("Preprocessing pbp {Sys.time()}")
  pbp_preprocessed <- ep_preprocess(pbp)

  pbp_preds <- ep_predict(pbp_preprocessed, version = version)
  vcli_alert("Generating predictions {Sys.time()}")

  weekly_ep <- ep_summarize(pbp_preds)
  vcli_alert("Summarizing data {Sys.time()}")

  vcli_rule("Finished building ep for {paste(unique(range(season)),collapse = '-')} season(s)! {Sys.time()}")

  out <- structure(
    list(
      ep_weekly = weekly_ep,
      ep_pbp_pass = pbp_preds$pass_df,
      ep_pbp_rush = pbp_preds$rush_df,
      ep_version = version,
      timestamp = Sys.time()
    ),
    class = "ffopps_build"
  )
  return(out)
}
