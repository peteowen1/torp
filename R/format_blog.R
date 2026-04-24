#' Canonical blog predictions schema
#'
#' Column order matters less than presence — OJS reads by name — but keeping a
#' canonical order makes `preds_blog[, PREDICTIONS_BLOG_COLS]` style checks
#' trivial and parquet schemas diff-clean across runs.
#' @keywords internal
PREDICTIONS_BLOG_COLS <- c(
  "season", "round", "home_team", "away_team",
  "home_epr", "away_epr",
  "pred_margin", "home_win_prob", "pred_total",
  "actual_margin",
  "start_time", "venue",
  "xscore_home", "xscore_away"
)


#' Format match predictions for the blog / R2 parquet
#'
#' Single source of truth for the `predictions.parquet` schema consumed by the
#' inthegame-blog `afl/matches.qmd` page. Called by both the CI pipeline
#' (`torpdata/scripts/build_blog_data.R`) and the local convenience pusher
#' (`torp/data-raw/02-models/push_predictions_to_r2.R`) so schema drift between
#' the two producers is impossible.
#'
#' @param preds Data frame with the core blog columns: `season`, `round`,
#'   `home_team`, `away_team`, `home_epr`, `away_epr`, `pred_margin`,
#'   `home_win_prob`, `pred_total`, `actual_margin`, `start_time`, `venue`.
#' @param xg Optional lookup with columns `season`, `round`, `home_team`,
#'   `away_team`, `xscore_home`, `xscore_away`. Use [xg_to_blog_lookup()] to
#'   build this from [load_xg()] or [get_xg()] output. If `NULL`, xscore
#'   columns are written as `NA_real_` so the blog reader never hits
#'   `undefined`.
#' @return Tibble with exactly the columns in `PREDICTIONS_BLOG_COLS`, in
#'   canonical order. Fails loudly if input is missing any required column.
#' @export
format_predictions_blog <- function(preds, xg = NULL) {
  req_cols <- setdiff(PREDICTIONS_BLOG_COLS, c("xscore_home", "xscore_away"))
  missing <- setdiff(req_cols, names(preds))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "{.arg preds} missing required column{?s}: {.val {missing}}",
      "i" = "Expected blog schema: {.val {req_cols}}"
    ))
  }

  out <- tibble::as_tibble(preds)[, req_cols]

  if (!is.null(xg) && nrow(xg) > 0) {
    xg_req <- c("season", "round", "home_team", "away_team",
                "xscore_home", "xscore_away")
    xg_missing <- setdiff(xg_req, names(xg))
    if (length(xg_missing) > 0) {
      cli::cli_abort(c(
        "{.arg xg} missing required column{?s}: {.val {xg_missing}}",
        "i" = "Build {.arg xg} with {.fn xg_to_blog_lookup}"
      ))
    }
    out <- dplyr::left_join(
      out, tibble::as_tibble(xg)[, xg_req],
      by = c("season", "round", "home_team", "away_team")
    )
  } else {
    out$xscore_home <- NA_real_
    out$xscore_away <- NA_real_
  }

  out[, PREDICTIONS_BLOG_COLS]
}


#' Convert xG release / live data into a blog-ready xscore lookup
#'
#' Takes the match-level output of [load_xg()] or [get_xg()] and reshapes it
#' into the `(season, round, home_team, away_team, xscore_home, xscore_away)`
#' shape that [format_predictions_blog()] expects as its `xg` argument.
#'
#' Team names are normalised via [torp_replace_teams()] so that live
#' [get_xg()] output (raw `home_team_name`) joins cleanly against predictions
#' that already went through normalisation.
#'
#' @param xg Output of [load_xg()] (single season) or [get_xg()] (single
#'   round). Must contain `match_id`, `home_team`, `away_team`, `home_xscore`,
#'   `away_xscore`.
#' @param season_val Integer season to stamp onto every row — `load_xg()` does
#'   not emit a `season` column and match_id parsing alone would hide schema
#'   bugs if the caller mixes seasons by accident.
#' @return Tibble with canonical lookup columns, or `NULL` if `xg` is empty.
#' @export
xg_to_blog_lookup <- function(xg, season_val) {
  if (is.null(xg) || nrow(xg) == 0) return(NULL)
  req <- c("match_id", "home_team", "away_team", "home_xscore", "away_xscore")
  missing <- setdiff(req, names(xg))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg xg} missing column{?s}: {.val {missing}}")
  }
  xg |>
    dplyr::mutate(
      season = as.integer(season_val),
      round = as.integer(substr(.data$match_id, 12L, 13L)),
      home_team = torp_replace_teams(.data$home_team),
      away_team = torp_replace_teams(.data$away_team),
      xscore_home = round(.data$home_xscore, 1),
      xscore_away = round(.data$away_xscore, 1)
    ) |>
    dplyr::select("season", "round", "home_team", "away_team",
                  "xscore_home", "xscore_away")
}
