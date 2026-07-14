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
#'   \code{n_oos} (holdout MATCH count -- \code{team_mdl_df} is long, two rows
#'   per match, so this is the OOS row count halved), \code{cold_start}
#'   (\code{TRUE} when \code{b} was forced to identity because
#'   \code{n_oos < MATCH_RECAL_MIN_N} or fewer than 2 seasons of history
#'   exist -- lets callers, e.g. \code{run_predictions_pipeline()}'s upload
#'   gate, distinguish a low-confidence cold-start identity fit from a
#'   genuine slope-gate breach), \code{holdout_season}, and \code{fitted_at}
#'   (timestamp). \code{b = 1} (identity) if \code{n_oos < MATCH_RECAL_MIN_N}
#'   or fewer than 2 seasons of history exist to hold one out.
#' @keywords internal
fit_match_margin_calibration <- function(team_mdl_df, nthreads = 4L) {
  seasons <- sort(unique(team_mdl_df$season.x[!is.na(team_mdl_df$win)]))
  if (length(seasons) < 2) {
    cli::cli_warn("fit_match_margin_calibration: fewer than 2 completed seasons available -- identity fallback (b=1)")
    return(list(b = 1, slope_raw = NA_real_, n_oos = 0L, cold_start = TRUE,
                holdout_season = NA_integer_, fitted_at = Sys.time()))
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
  # team_mdl_df (and therefore xgb_data) is long -- one row per team per
  # match -- so nrow(oos) is a ROW count. n_oos must mean MATCH count
  # everywhere below (MATCH_RECAL_MIN_N and the release gate are both
  # documented in match counts), so halve it here, once, at the source.
  n_oos <- nrow(oos) / 2

  slope_raw <- if (n_oos >= 2) {
    tryCatch(unname(stats::coef(stats::lm(margin ~ pred_margin, data = oos))[["pred_margin"]]),
             error = function(e) NA_real_)
  } else {
    NA_real_
  }

  cold_start <- n_oos < MATCH_RECAL_MIN_N
  if (cold_start) {
    cli::cli_warn("fit_match_margin_calibration: only {n_oos} OOS holdout matches (< {MATCH_RECAL_MIN_N}) -- identity fallback (b=1)")
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

  list(b = b, slope_raw = slope_raw, n_oos = n_oos, cold_start = cold_start,
       holdout_season = holdout_season, fitted_at = Sys.time())
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
#' degrade to a \code{cli_warn()} and `NULL` (identity fallback at the call
#' site) -- never an error. A successful load is cached per session so
#' repeated calls (e.g. across several weeks' predictions in one session)
#' don't re-hit the network; a failed load is deliberately NOT cached, so
#' the next call retries rather than a one-off transient failure
#' permanently downgrading the rest of the session to uncalibrated margins.
#' The warning itself is still deduped to once per session via a separate
#' cache key.
#'
#' @return The calibration list (see \code{fit_match_margin_calibration()}),
#'   or `NULL` if unavailable.
#' @keywords internal
load_match_margin_calibration <- function() {
  if (exists("match_margin_calibration", envir = .torp_model_cache)) {
    return(get("match_margin_calibration", envir = .torp_model_cache))
  }

  # Warn-once dedup lives on its own key, separate from the model cache slot
  # itself -- a failed load must NEVER populate the "match_margin_calibration"
  # cache slot (see below), so gating the warning on that slot would silently
  # turn a one-off transient failure into "sidecar absent" for the rest of
  # the session. Deliberately NOT dot-prefixed: ls() (used by
  # clear_model_cache()/get_model_cache_info()) defaults to
  # all.names = FALSE and would silently skip a dot-prefixed key, leaking
  # warn-once state across a clear_model_cache() call.
  warned_key <- "match_margin_calibration__warned"
  warn_once <- function(msg) {
    if (!exists(warned_key, envir = .torp_model_cache)) {
      cli::cli_warn(msg)
      assign(warned_key, TRUE, envir = .torp_model_cache)
    }
  }

  if (!requireNamespace("torpmodels", quietly = TRUE)) {
    warn_once("torpmodels package not available -- serving uncalibrated match margins (match_margin_calibration unavailable)")
    return(NULL)
  }

  calib <- tryCatch(
    torpmodels::load_torp_model("match_margin_calibration", verbose = FALSE),
    error = function(e) {
      warn_once(c(
        "match_margin_calibration unavailable -- serving uncalibrated match margins",
        "x" = e$message
      ))
      NULL
    }
  )

  # Only cache a successful load -- see load_model_with_fallback()'s wp_calibration
  # path in add_variables.R for the identical fix and rationale.
  if (!is.null(calib)) {
    assign("match_margin_calibration", calib, envir = .torp_model_cache)
  }
  calib
}
