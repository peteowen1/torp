# Match Margin Recalibration
# ===========================
# Post-hoc scaling of the blended match-model margin prediction (2026-07,
# FABLE-MATCH-MAE-PLAN.md WS1 "V1a" -- a leak-safe expanding-window slope
# fit on OOS predictions). Mirrors the WP calibration sidecar pattern
# (wp_calibration.rds, FABLE-RECAL-PLAN.md): fit once per retrain on a
# temporal holdout, applied at serve time with an identity (b=1) fallback
# when absent, network-down, or there's insufficient OOS history.
#
# Design note: WS1's original experiment fit this via a full rolling
# week-by-week reconstruction (expensive -- retrains 5 GAMs + 5 XGBoost
# models per round). Production instead uses a single temporal holdout (the
# same pattern already proven for wp_calibration/fit_wp_temporal_variant()):
# train on all seasons except the most recently completed one, score that
# season out-of-sample, fit the scale on those predictions. Cheaper (one
# extra full training pass per retrain, not dozens), and the WP precedent
# shows this pattern ships reliably.

# fit_match_margin_calibration ----

#' Fit a leak-safe margin recalibration scale via one temporal holdout
#'
#' Trains the GAM+XGBoost chain on all seasons strictly before the most
#' recently completed season in `team_mdl_df`, scores that held-out season
#' with the resulting models (same 50/50 Input Blend used in production,
#' \code{run_predictions_pipeline()}'s blend block), and fits
#' \code{b = coef(lm(margin ~ pred_margin + 0))} on those honest OOS
#' predictions (V1a's "slope_only" mode).
#'
#' @param team_mdl_df Complete model dataset from \code{.build_team_mdl_df()}
#' @param nthreads Threads for the holdout GAM fit (default 4)
#' @return A list: \code{b} (recalibration scale, 1 = identity),
#'   \code{slope_raw} (the OOS margin calibration slope BEFORE recalibration
#'   -- feeds the release gate, see \code{MATCH_MARGIN_SLOPE_GATE}),
#'   \code{n_oos} (holdout match count), \code{holdout_season}, and
#'   \code{fitted_at} (timestamp). \code{b = 1} (identity) if
#'   \code{n_oos < MATCH_RECAL_MIN_N} or fewer than 2 seasons of history
#'   exist to hold one out.
#' @keywords internal
fit_match_margin_calibration <- function(team_mdl_df, nthreads = 4L) {
  seasons <- sort(unique(team_mdl_df$season.x[!is.na(team_mdl_df$win)]))
  if (length(seasons) < 2) {
    cli::cli_warn("fit_match_margin_calibration: fewer than 2 completed seasons available -- identity fallback (b=1)")
    return(list(b = 1, slope_raw = NA_real_, n_oos = 0L, holdout_season = NA_integer_,
                fitted_at = Sys.time()))
  }

  holdout_season <- max(seasons)
  train_filter <- team_mdl_df$season.x < holdout_season
  test_mask <- !is.na(team_mdl_df$win) & team_mdl_df$season.x == holdout_season

  cli::cli_inform("fit_match_margin_calibration: holdout season {holdout_season} ({sum(test_mask)/2} matches)")

  gam_result <- suppressMessages(.train_match_gams(team_mdl_df, train_filter = train_filter, nthreads = nthreads))
  gam_data <- gam_result$data
  xgb_result <- suppressMessages(.train_match_xgb(gam_data, train_filter = train_filter))
  xgb_data <- xgb_result$data

  # Same 50/50 Input Blend as run_predictions_pipeline()'s blend block
  xgb_data$pred_score_diff_blend <- 0.5 * xgb_data$gam_pred_score_diff + 0.5 * xgb_data$xgb_pred_score_diff

  oos <- data.frame(
    pred_margin = xgb_data$pred_score_diff_blend[test_mask],
    margin      = xgb_data$score_diff[test_mask]
  )
  oos <- oos[stats::complete.cases(oos), ]
  n_oos <- nrow(oos)

  slope_raw <- if (n_oos >= 2) {
    tryCatch(unname(stats::coef(stats::lm(margin ~ pred_margin, data = oos))[["pred_margin"]]),
             error = function(e) NA_real_)
  } else {
    NA_real_
  }

  if (n_oos < MATCH_RECAL_MIN_N) {
    cli::cli_warn("fit_match_margin_calibration: only {n_oos} OOS holdout rows (< {MATCH_RECAL_MIN_N}) -- identity fallback (b=1)")
    b <- 1
  } else {
    b <- tryCatch(
      unname(stats::coef(stats::lm(margin ~ pred_margin + 0, data = oos))[1]),
      error = function(e) {
        cli::cli_warn("fit_match_margin_calibration: slope fit failed ({conditionMessage(e)}) -- identity fallback (b=1)")
        1
      }
    )
    if (!is.finite(b)) {
      cli::cli_warn("fit_match_margin_calibration: non-finite slope -- identity fallback (b=1)")
      b <- 1
    }
  }

  cli::cli_inform("fit_match_margin_calibration: b={round(b, 3)}, raw OOS slope={round(slope_raw, 3)}, n_oos={n_oos}")

  list(b = b, slope_raw = slope_raw, n_oos = n_oos, holdout_season = holdout_season,
       fitted_at = Sys.time())
}

# apply_match_margin_calibration ----

#' Apply a fitted margin recalibration scale, with identity fallback
#'
#' @param pred_margin Numeric vector of blended margin predictions
#' @param calib Output of \code{fit_match_margin_calibration()}, or `NULL`
#'   (serves uncalibrated -- identity)
#' @return Recalibrated \code{pred_margin}
#' @keywords internal
apply_match_margin_calibration <- function(pred_margin, calib) {
  if (is.null(calib) || is.null(calib$b) || !is.finite(calib$b)) {
    return(pred_margin)
  }
  calib$b * pred_margin
}

# load_match_margin_calibration ----

#' Load the match margin calibration sidecar, if available
#'
#' Mirrors \code{load_model_with_fallback("wp_calibration")}'s contract:
#' absence, a network failure, or torpmodels not being installed all
#' degrade to a single \code{cli_warn()} and `NULL` (identity fallback at
#' the call site) -- never an error. Cached per session so repeated calls
#' (e.g. across several weeks' predictions in one session) don't re-hit the
#' network.
#'
#' @return The calibration list (see \code{fit_match_margin_calibration()}),
#'   or `NULL` if unavailable.
#' @keywords internal
load_match_margin_calibration <- function() {
  if (exists("match_margin_calibration", envir = .torp_model_cache)) {
    return(get("match_margin_calibration", envir = .torp_model_cache))
  }

  if (!requireNamespace("torpmodels", quietly = TRUE)) {
    cli::cli_warn("torpmodels package not available -- serving uncalibrated match margins (match_margin_calibration unavailable)")
    assign("match_margin_calibration", NULL, envir = .torp_model_cache)
    return(NULL)
  }

  calib <- tryCatch(
    torpmodels::load_torp_model("match_margin_calibration", verbose = FALSE),
    error = function(e) {
      cli::cli_warn(c(
        "match_margin_calibration unavailable -- serving uncalibrated match margins",
        "x" = e$message
      ))
      NULL
    }
  )

  assign("match_margin_calibration", calib, envir = .torp_model_cache)
  calib
}
