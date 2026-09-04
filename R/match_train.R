# Match Model Training
# ====================
# GAM and XGBoost training pipelines for match predictions.
# Called by run_predictions_pipeline() in match_model.R.

# .pre_extract_random_effects ----

#' Pre-extract all random effects from a GAM before stripping
#'
#' Extracts coefficients and SEs for every `bs = "re"` smooth in the model,
#' storing them as a named list of data.tables keyed by variable name.
#' This allows `.strip_gam()` to safely remove `$Vp` and `$model` while
#' preserving the random effect information needed by
#' [extract_gam_random_effects()].
#'
#' @param model A fitted gam/bam object
#' @return Named list of data.tables (one per random effect variable), or
#'   empty list if extraction fails
#' @keywords internal
.pre_extract_random_effects <- function(model) {
  if (is.null(model$smooth)) return(list())

  re_smooths <- Filter(function(s) inherits(s, "random.effect"), model$smooth)
  if (length(re_smooths) == 0L) return(list())

  result <- list()
  for (s in re_smooths) {
    vn <- s$vn[1]
    coef_idx <- s$first.para:s$last.para
    coefs <- tryCatch(stats::coef(model)[coef_idx], error = function(e) {
      cli::cli_warn("Failed to extract RE coefficients for smooth '{vn}': {conditionMessage(e)}")
      NULL
    })
    se <- tryCatch(sqrt(diag(stats::vcov(model)[coef_idx, coef_idx, drop = FALSE])),
                   error = function(e) {
      cli::cli_warn("Failed to extract RE std errors for smooth '{vn}': {conditionMessage(e)}")
      NULL
    })
    if (is.null(coefs)) next

    # Recover level names from model frame
    level_names <- NULL
    if (!is.null(model$model) && vn %in% names(model$model) &&
        is.factor(model$model[[vn]])) {
      lvls <- levels(model$model[[vn]])
      if (length(lvls) == length(coefs)) level_names <- lvls
    }
    if (is.null(level_names)) level_names <- gsub("^.*\\.", "", names(coefs))

    result[[vn]] <- data.table::data.table(
      level = level_names,
      coefficient = unname(coefs),
      se = if (!is.null(se)) unname(se) else rep(NA_real_, length(coefs))
    )
  }
  result
}

# .strip_gam ----

#' Strip a GAM model to prediction-only components
#'
#' Removes large components (model frame, residuals, fitted values, etc.)
#' that are not needed for predict.gam(). Typically shrinks models 5-10x.
#'
#' @param model A fitted gam/bam object
#' @return The same model with bulky diagnostic components removed
#' @keywords internal
.strip_gam <- function(model) {
  # Pre-extract random effects before removing $Vp and $model
  # (both are needed by extract_gam_random_effects but are bulky)
  re_tables <- .pre_extract_random_effects(model)
  if (length(re_tables) > 0) model$pre_extracted_re <- re_tables

  model$y <- NULL
  model$model <- NULL
  model$residuals <- NULL
  model$fitted.values <- NULL
  model$linear.predictors <- NULL
  model$weights <- NULL
  model$prior.weights <- NULL
  model$working.weights <- NULL
  model$hat <- NULL
  model$offset <- NULL
  model$R <- NULL
  model$Ve <- NULL
  model$Vp <- NULL
  model$qrx <- NULL
  model$db.drho <- NULL
  model$gcv.ubre <- NULL
  if (!is.null(model$family$data)) model$family$data <- NULL
  attr(model, "predict.gam.env") <- NULL
  model
}

#' Strip a list of GAM models for compact serialisation
#' @param models Named list of gam/bam objects
#' @return Same list with stripped models
#' @keywords internal
.strip_gam_models <- function(models) {
  lapply(models, function(m) {
    if (inherits(m, c("gam", "bam"))) .strip_gam(m) else m
  })
}

# .train_match_gams ----

#' Out-of-fold prediction for one stacked-cascade GAM stage
#'
#' Refits `formula` once per season fold, holding that season's rows out of
#' training and predicting only onto them, then assembles the held-out
#' predictions into one vector aligned to `data`'s row order. Used so a
#' cascade stage's own in-sample fit is never fed forward as the next
#' stage's input feature (see the header comment on `.train_match_gams()`
#' and `.train_match_xgb()` for why this matters).
#'
#' @param formula Model formula, identical to the one used for the full fit.
#' @param data Training data (one cascade stage's `gam_df`).
#' @param weights Numeric weights vector, same length as `data`.
#' @param family GAM family (e.g. `gaussian()`).
#' @param gamma_arg Smoothness penalty multiplier, matching the full fit.
#' @param nthreads Threads for `mgcv::bam()`.
#' @param folds List of integer row-index vectors, one per held-out fold.
#' @return Numeric vector, `length(data)` rows, out-of-fold predictions.
#' @keywords internal
.oof_predict_gam <- function(formula, data, weights, family, gamma_arg, nthreads, folds) {
  # bam()/gam() resolve `weights =`/`subset =` NSE using environment(formula)
  # -- the LEXICAL scope where the formula was built (.train_match_gams()'s
  # frame) -- not this function's call frame. Rebinding it here makes `data`,
  # `weights` and the fold index `f` all resolve from THIS frame instead,
  # where they actually live; otherwise bam() aborts with "object 'f' not
  # found" the moment it tries to evaluate `weights[-f]`.
  environment(formula) <- environment()
  oof <- rep(NA_real_, nrow(data))
  for (f in folds) {
    fit <- mgcv::bam(
      formula, data = data[-f, ], weights = weights[-f], family = family,
      select = TRUE, discrete = TRUE, drop.unused.levels = FALSE,
      gamma = gamma_arg, nthreads = nthreads
    )
    oof[f] <- predict(fit, newdata = data[f, ], type = "response")
  }
  oof
}

#' Train the 5-model sequential GAM pipeline
#'
#' Trains total xPoints, xScore diff, conversion, score diff, and win probability
#' GAMs sequentially (each model's predictions feed the next). Model 1 includes
#' weather smooths (log_wind, log_precip, temp_avg, humidity_avg).
#'
#' @param team_mdl_df Complete model dataset from .build_team_mdl_df()
#' @param train_filter Logical vector indicating training rows (NULL = all completed matches)
#' @param nthreads Number of threads for mgcv::bam() (default 4)
#' @param gamma_arg Smoothness penalty multiplier passed to every mgcv::bam()
#'   call (default 1.4). 1.0 = fREML's own choice; >1 forces smoother fits.
#'   Tuned via rolling-eval sweep on 2025+2026 (n=306) where 1.4 improved MAE
#'   (−0.44), Brier (−0.003), and bits (+4.1 total) over baseline 1.0 with no
#'   regressions on either season — see data-raw/debug/gamma_full_pipeline_rolling.R.
#' @return List with $models (named list of 5 GAMs) and $data (team_mdl_df with predictions)
#' @keywords internal
.train_match_gams <- function(team_mdl_df, train_filter = NULL, nthreads = 4L, gamma_arg = 1.4) {
  # Smooth-term basis size convention used throughout this file:
  #   bs = "ts" thin-plate splines: k = 5
  #     ~5K-team-game training rows / ~16K when including upcoming fixtures,
  #     and shrinkage smooths (`ts`) penalise unused dimensions to zero — so
  #     k = 5 gives effective DF in the 2–4 range without overfitting.
  #   ti(...) tensor interactions: k = 4 (per marginal)
  #     k=4 caps the interaction grid at 16 basis functions before penalisation,
  #     which is enough to capture monotone curvature in the joint EPR x xScore
  #     surface without exploding rank.
  # If you change either constant, change both the m{1..5}_base/optional blocks
  # below AND the entries in optional_smooth_terms so the unique-value guard
  # still matches.
  loadNamespace("mgcv")

  if (is.null(train_filter)) {
    train_mask <- !is.na(team_mdl_df$win)
  } else {
    train_mask <- train_filter & !is.na(team_mdl_df$win)
  }

  gam_df <- team_mdl_df[train_mask, ]
  cli::cli_inform("Training on {nrow(gam_df)} completed matches")
  if (nrow(gam_df) == 0) {
    cli::cli_abort("Cannot train GAM models: 0 completed matches after filtering")
  }

  # Season-grouped out-of-fold assignment. Used only to de-leak the stacked
  # cascade features below (models 1, 2 and 4's predictions feed later
  # stages as inputs) -- NOT a substitute for the caller's own train_filter,
  # which still controls what counts as "training" at all.
  #
  # Known limitation: the m1/m2/m4 formulas include a team_name_season
  # random effect (s(team_name_season.x/.y, bs = "re")), which is scoped to
  # a single season by construction (match_data_prep.R:
  # paste(team_name, season)). Season-grouped folds therefore give that
  # term ZERO training/held-out overlap on every fold, every retrain -- mgcv
  # predicts it at the population mean rather than erroring or NA'ing.
  # Measured harmless on the served blend (423-match rolling comparison,
  # 2026-08-27, "a wash, not a win" -- see NEWS.md), but this is a real,
  # deterministic gap in the OOF-corrected training features for models
  # 2/4/5, not a hypothetical edge case.
  stopifnot(!anyNA(gam_df$season.x))
  gam_seasons <- sort(unique(gam_df$season.x))
  gam_folds <- lapply(gam_seasons, function(s) which(gam_df$season.x == s))

  # Check which optional smooth terms have sufficient unique values (need >= k)
  # Terms with constant/near-constant data are dropped to prevent mgcv errors
  optional_smooth_terms <- list(
    # Model 1 optional terms (psr + weather)
    "s(psr.x, bs = \"ts\", k = 5)"           = list(var = "psr.x", k = 5),
    "s(psr.y, bs = \"ts\", k = 5)"           = list(var = "psr.y", k = 5),
    "s(log_wind, bs = \"ts\", k = 5)"        = list(var = "log_wind", k = 5),
    "s(log_precip, bs = \"ts\", k = 5)"      = list(var = "log_precip", k = 5),
    "s(temp_avg, bs = \"ts\", k = 5)"        = list(var = "temp_avg", k = 5),
    "s(humidity_avg, bs = \"ts\", k = 5)"     = list(var = "humidity_avg", k = 5),
    # Model 1 abs() terms
    "s(abs(psr_diff), bs = \"ts\", k = 5)"   = list(var = "psr_diff", k = 5),
    "s(abs(osr_diff), bs = \"ts\", k = 5)"   = list(var = "osr_diff", k = 5),
    "s(abs(dsr_diff), bs = \"ts\", k = 5)"   = list(var = "dsr_diff", k = 5),
    # Models 2-4 optional terms
    "s(psr_diff, bs = \"ts\", k = 5)"        = list(var = "psr_diff", k = 5),
    "s(osr_diff, bs = \"ts\", k = 5)"        = list(var = "osr_diff", k = 5),
    "s(dsr_diff, bs = \"ts\", k = 5)"        = list(var = "dsr_diff", k = 5),
    "s(xelo_diff, bs = \"ts\", k = 5)"       = list(var = "xelo_diff", k = 5),
    # The EPR CHANNEL smooths. These used to sit in the base formulas, outside
    # this guard, and that made a whole engine untrainable: under
    # EPV3_CHANNELS = 3L the hitout slot is zeroed, so epr_hitout_diff is
    # IDENTICALLY CONSTANT and mgcv aborts the entire fit with "a term has fewer
    # unique covariate combinations than specified maximum degrees of freedom".
    # A channel that does not exist should not be a model term; it should drop
    # out the same way an absent psr_diff does.
    #
    # Under v2 every one of these has thousands of unique values, so nothing is
    # dropped and the fitted formulas are unchanged -- the terms simply move
    # from the base string to the optional list, and a sum of smooths does not
    # care about order.
    "s(abs(epr_diff), bs = \"ts\", k = 5)"        = list(var = "epr_diff", k = 5),
    "s(abs(epr_recv_diff), bs = \"ts\", k = 5)"   = list(var = "epr_recv_diff", k = 5),
    "s(abs(epr_disp_diff), bs = \"ts\", k = 5)"   = list(var = "epr_disp_diff", k = 5),
    "s(abs(epr_spoil_diff), bs = \"ts\", k = 5)"  = list(var = "epr_spoil_diff", k = 5),
    "s(abs(epr_hitout_diff), bs = \"ts\", k = 5)" = list(var = "epr_hitout_diff", k = 5),
    "s(epr_diff, bs = \"ts\", k = 5)"             = list(var = "epr_diff", k = 5),
    "s(epr_recv_diff, bs = \"ts\", k = 5)"        = list(var = "epr_recv_diff", k = 5),
    "s(epr_disp_diff, bs = \"ts\", k = 5)"        = list(var = "epr_disp_diff", k = 5),
    "s(epr_spoil_diff, bs = \"ts\", k = 5)"       = list(var = "epr_spoil_diff", k = 5),
    "s(epr_hitout_diff, bs = \"ts\", k = 5)"      = list(var = "epr_hitout_diff", k = 5)
  )
  drop_terms <- character(0)
  for (term_str in names(optional_smooth_terms)) {
    info <- optional_smooth_terms[[term_str]]
    vals <- gam_df[[info$var]]
    n_unique <- length(unique(vals[!is.na(vals)]))
    if (n_unique < info$k) {
      drop_terms <- c(drop_terms, term_str)
      cli::cli_warn("Dropping smooth {.code {term_str}}: only {n_unique} unique value{?s} (need >= {info$k})")
    }
  }

  # Helper to build formula by conditionally adding optional terms
  .add_optional <- function(base_terms, optional_terms) {
    keep <- setdiff(optional_terms, drop_terms)
    if (length(keep) > 0) {
      paste(base_terms, "+", paste(keep, collapse = " + "))
    } else {
      base_terms
    }
  }

  # Model 1: Total expected points (includes weather smooths)
  cli::cli_progress_step("Training total xPoints model")
  m1_base <- paste(
    "total_xpoints_adj ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(game_year_decimal.x, bs = \"ts\")",
    "+ s(game_prop_through_year.x, bs = \"cc\")",
    "+ s(game_prop_through_month.x, bs = \"cc\")",
    "+ s(game_wday_fac.x, bs = \"re\")",
    "+ s(game_prop_through_day.x, bs = \"cc\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ s(epr.x, bs = \"ts\", k = 5) + s(epr.y, bs = \"ts\", k = 5)",
    "+ s(abs(torp_diff), bs = \"ts\", k = 5)",
    "+ s(torp.x, bs = \"ts\", k = 5) + s(torp.y, bs = \"ts\", k = 5)",
    "+ s(venue_fac, bs = \"re\")",
    "+ s(log_dist.x, bs = \"ts\", k = 5) + s(log_dist.y, bs = \"ts\", k = 5)",
    "+ s(familiarity.x, bs = \"ts\", k = 5) + s(familiarity.y, bs = \"ts\", k = 5)",
    "+ s(log_dist_diff, bs = \"ts\", k = 5)",
    "+ s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m1_optional <- c(
    "s(abs(epr_diff), bs = \"ts\", k = 5)",
    "s(abs(epr_recv_diff), bs = \"ts\", k = 5)",
    "s(abs(epr_disp_diff), bs = \"ts\", k = 5)",
    "s(abs(epr_spoil_diff), bs = \"ts\", k = 5)",
    "s(abs(epr_hitout_diff), bs = \"ts\", k = 5)",
    "s(psr.x, bs = \"ts\", k = 5)", "s(psr.y, bs = \"ts\", k = 5)",
    "s(abs(psr_diff), bs = \"ts\", k = 5)",
    "s(abs(osr_diff), bs = \"ts\", k = 5)", "s(abs(dsr_diff), bs = \"ts\", k = 5)",
    "s(log_wind, bs = \"ts\", k = 5)", "s(log_precip, bs = \"ts\", k = 5)",
    "s(temp_avg, bs = \"ts\", k = 5)", "s(humidity_avg, bs = \"ts\", k = 5)"
  )
  m1_formula <- stats::as.formula(.add_optional(m1_base, m1_optional))

  afl_total_xpoints_mdl <- mgcv::bam(
    m1_formula,
    data = gam_df, weights = gam_df$weightz,
    family = gaussian(), nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE,
    gamma = gamma_arg
  )
  team_mdl_df$gam_pred_tot_xscore <- predict(afl_total_xpoints_mdl, newdata = team_mdl_df, type = "response")
  # Out-of-fold correction for training rows only: models 2, 3 and 5 consume
  # gam_pred_tot_xscore as an input feature, so the value they train on must
  # not come from a fit that already saw that row's own outcome. Non-training
  # rows (upcoming fixtures) keep the full-model prediction above -- there is
  # no "held-out fold" for them, and it is already legitimately out-of-sample.
  team_mdl_df$gam_pred_tot_xscore[train_mask] <- .oof_predict_gam(
    m1_formula, gam_df, gam_df$weightz, gaussian(), gamma_arg, nthreads, gam_folds
  )

  # Model 2: xScore differential
  cli::cli_progress_step("Training xScore diff model")
  gam_df$gam_pred_tot_xscore <- team_mdl_df$gam_pred_tot_xscore[train_mask]
  # Formula simplification (2026-07, FABLE-MATCH-MAE-PLAN.md WS4/WS5 "V4b"):
  # models 2-4 previously included ti(epr_diff/torp_diff/psr_diff,
  # gam_pred_tot_xscore) interaction tensors on the theory that they'd
  # capture rating-diff x total-score amplification effects. Rolling-OOS
  # ablation testing showed they were a "clean null" (no measurable slope or
  # MAE effect) -- dropping them, plus model 4's second-order stack tensors
  # below, was the single best-performing structural simplification tested,
  # and is part of the confirmed "C6" ship-gate win. Do not re-add without
  # re-running the rolling harness ablation.
  m2_base <- paste(
    "xscore_diff ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ s(gam_pred_tot_xscore, bs = \"ts\", k = 5)",
    "+ s(torp_diff, bs = \"ts\", k = 5)",
    "+ s(log_dist_diff, bs = \"ts\", k = 5) + s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m2_optional <- c(
    "s(epr_diff, bs = \"ts\", k = 5)",
    "s(epr_recv_diff, bs = \"ts\", k = 5)",
    "s(epr_disp_diff, bs = \"ts\", k = 5)",
    "s(epr_spoil_diff, bs = \"ts\", k = 5)",
    "s(epr_hitout_diff, bs = \"ts\", k = 5)",
    "s(psr_diff, bs = \"ts\", k = 5)",
                    "s(osr_diff, bs = \"ts\", k = 5)", "s(dsr_diff, bs = \"ts\", k = 5)",
                    "s(xelo_diff, bs = \"ts\", k = 5)")
  m2_formula <- stats::as.formula(.add_optional(m2_base, m2_optional))

  afl_xscore_diff_mdl <- mgcv::bam(
    m2_formula,
    data = gam_df, weights = gam_df$weightz,
    family = gaussian(), nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE,
    gamma = gamma_arg
  )
  team_mdl_df$gam_pred_xscore_diff <- predict(afl_xscore_diff_mdl, newdata = team_mdl_df, type = "response")
  # Out-of-fold correction, same reasoning as model 1: models 3 and 4 both
  # consume gam_pred_xscore_diff as an input feature.
  team_mdl_df$gam_pred_xscore_diff[train_mask] <- .oof_predict_gam(
    m2_formula, gam_df, gam_df$weightz, gaussian(), gamma_arg, nthreads, gam_folds
  )

  # Model 3: Conversion differential
  cli::cli_progress_step("Training conversion model")
  gam_df$gam_pred_xscore_diff <- team_mdl_df$gam_pred_xscore_diff[train_mask]
  # V4b simplification (see model 2's comment); model 3 does not get elo_diff
  # (round 1's WS2(b) only added it to models 2 and 4 -- kept identical here).
  m3_base <- paste(
    "shot_conv_diff ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(game_year_decimal.x, bs = \"ts\")",
    "+ s(game_prop_through_year.x, bs = \"cc\")",
    "+ s(game_prop_through_month.x, bs = \"cc\")",
    "+ s(game_wday_fac.x, bs = \"re\")",
    "+ s(game_prop_through_day.x, bs = \"cc\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ s(torp_diff, bs = \"ts\", k = 5)",
    "+ s(gam_pred_tot_xscore, bs = \"ts\", k = 5)",
    "+ s(gam_pred_xscore_diff, bs = \"ts\", k = 5)",
    "+ s(venue_fac, bs = \"re\")",
    "+ s(log_dist_diff, bs = \"ts\", k = 5) + s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m3_optional <- c(
    "s(epr_diff, bs = \"ts\", k = 5)",
    "s(epr_recv_diff, bs = \"ts\", k = 5)",
    "s(epr_disp_diff, bs = \"ts\", k = 5)",
    "s(epr_spoil_diff, bs = \"ts\", k = 5)",
    "s(epr_hitout_diff, bs = \"ts\", k = 5)",
    "s(psr_diff, bs = \"ts\", k = 5)",
                    "s(osr_diff, bs = \"ts\", k = 5)", "s(dsr_diff, bs = \"ts\", k = 5)")
  m3_formula <- stats::as.formula(.add_optional(m3_base, m3_optional))

  afl_conv_mdl <- mgcv::bam(
    m3_formula,
    data = gam_df, weights = gam_df$shot_weightz,
    family = gaussian(), nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE,
    gamma = gamma_arg
  )
  team_mdl_df$gam_pred_conv_diff <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")

  # Model 4: Score differential
  cli::cli_progress_step("Training score diff model")
  gam_df$gam_pred_conv_diff <- team_mdl_df$gam_pred_conv_diff[train_mask]
  # V4b simplification (see model 2's comment) -- also drops model 4's
  # second-order stack tensors (ti(gam_pred_xscore_diff, gam_pred_conv_diff),
  # ti(gam_pred_tot_xscore, gam_pred_conv_diff)), keeping only the main-effect
  # s(gam_pred_xscore_diff). Plus elo_diff (added to models 2 and 4 only).
  m4_base <- paste(
    "score_diff ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ s(gam_pred_xscore_diff)",
    "+ s(torp_diff, bs = \"ts\", k = 5)",
    "+ s(log_dist_diff, bs = \"ts\", k = 5) + s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m4_optional <- c(
    "s(epr_diff, bs = \"ts\", k = 5)",
    "s(epr_recv_diff, bs = \"ts\", k = 5)",
    "s(epr_disp_diff, bs = \"ts\", k = 5)",
    "s(epr_spoil_diff, bs = \"ts\", k = 5)",
    "s(epr_hitout_diff, bs = \"ts\", k = 5)",
    "s(psr_diff, bs = \"ts\", k = 5)",
                    "s(osr_diff, bs = \"ts\", k = 5)", "s(dsr_diff, bs = \"ts\", k = 5)",
                    "s(xelo_diff, bs = \"ts\", k = 5)")
  m4_formula <- stats::as.formula(.add_optional(m4_base, m4_optional))

  afl_score_mdl <- mgcv::bam(
    m4_formula,
    data = gam_df, weights = gam_df$weightz,
    family = "gaussian", nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE,
    gamma = gamma_arg
  )
  team_mdl_df$gam_pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")
  # Out-of-fold correction: model 5 consumes gam_pred_score_diff as an input
  # feature, AND it is served directly (run_predictions_pipeline() blends
  # gam_pred_score_diff/xgb_pred_score_diff for every row, including
  # completed matches) -- so training-row honesty here also matters for
  # historical reporting, not only for model 5's own fit.
  team_mdl_df$gam_pred_score_diff[train_mask] <- .oof_predict_gam(
    m4_formula, gam_df, gam_df$weightz, "gaussian", gamma_arg, nthreads, gam_folds
  )

  # Model 5: Win probability — trained on bare pred_* names so the blend step
  # can re-feed the same model with blended values via newdata.
  cli::cli_progress_step("Training win probability model")
  gam_df$pred_tot_xscore  <- gam_df$gam_pred_tot_xscore
  gam_df$pred_score_diff  <- team_mdl_df$gam_pred_score_diff[train_mask]
  afl_win_mdl <- mgcv::bam(
    win ~
      +s(team_name.x, bs = "re") + s(team_name.y, bs = "re")
      + s(team_name_season.x, bs = "re") + s(team_name_season.y, bs = "re")
      + ti(pred_tot_xscore, pred_score_diff, bs = c("ts", "ts"), k = 4)
      + s(pred_score_diff, bs = "ts", k = 5)
      + s(log_dist_diff, bs = "ts", k = 5) + s(familiarity_diff, bs = "ts", k = 5)
      + s(days_rest_diff_fac, bs = "re"),
    data = gam_df, weights = gam_df$weightz,
    family = "binomial", nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE,
    gamma = gamma_arg
  )
  # Seed bare pred_* columns on team_mdl_df with GAM-only values so the
  # win-model predict() below has the columns it expects, and so downstream
  # consumers (and the blend step in run_predictions_pipeline) start from a
  # well-defined GAM baseline.
  team_mdl_df$pred_tot_xscore  <- team_mdl_df$gam_pred_tot_xscore
  team_mdl_df$pred_xscore_diff <- team_mdl_df$gam_pred_xscore_diff
  team_mdl_df$pred_conv_diff   <- team_mdl_df$gam_pred_conv_diff
  team_mdl_df$pred_score_diff  <- team_mdl_df$gam_pred_score_diff

  team_mdl_df$gam_pred_win <- predict(afl_win_mdl, newdata = team_mdl_df, type = "response")
  team_mdl_df$pred_win     <- team_mdl_df$gam_pred_win

  # Validation
  if (any(is.na(team_mdl_df$pred_win[!is.na(team_mdl_df$win)]))) {
    cli::cli_warn("NA values in pred_win for completed matches")
  }
  pred_win_range <- range(team_mdl_df$pred_win, na.rm = TRUE)
  if (pred_win_range[1] < 0 || pred_win_range[2] > 1) {
    cli::cli_warn("pred_win outside [0,1]: [{round(pred_win_range[1], 4)}, {round(pred_win_range[2], 4)}]")
  }

  # Home/away symmetry check: for each match, H_score_diff ≈ -A_score_diff
  # and H_win + A_win ≈ 1. Large deviations indicate a data pipeline bug.
  sym_check <- team_mdl_df |>
    dplyr::group_by(match_id) |>
    dplyr::summarise(
      score_sum = sum(pred_score_diff),
      win_sum = sum(pred_win),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(n == 2)

  if (nrow(sym_check) > 0) {
    max_score_asym <- max(abs(sym_check$score_sum), na.rm = TRUE)
    max_win_asym <- max(abs(sym_check$win_sum - 1), na.rm = TRUE)
    if (max_score_asym > 5) {
      cli::cli_abort(c(
        "Home/away prediction asymmetry detected (max score_diff sum: {round(max_score_asym, 1)}).",
        "i" = "For each match, home pred_score_diff + away pred_score_diff should be ~0.",
        "i" = "This usually indicates a column name mismatch in the data pipeline."
      ))
    }
    if (max_win_asym > 0.1) {
      cli::cli_warn("Home/away win probability asymmetry: max |H_win + A_win - 1| = {round(max_win_asym, 4)}")
    }
  }

  # Scoring metrics
  team_mdl_df$bits <- dplyr::case_when(
    team_mdl_df$win == 1   ~ 1 + log2(team_mdl_df$pred_win),
    team_mdl_df$win == 0   ~ 1 + log2(1 - team_mdl_df$pred_win),
    TRUE                   ~ 1 + 0.5 * log2(team_mdl_df$pred_win * (1 - team_mdl_df$pred_win))
  )
  team_mdl_df$tips <- dplyr::case_when(
    round(team_mdl_df$pred_win) == team_mdl_df$win ~ 1,
    team_mdl_df$win == 0.5                         ~ 1,
    TRUE                                           ~ 0
  )
  team_mdl_df$mae <- abs(team_mdl_df$score_diff - team_mdl_df$pred_score_diff)

  models <- list(
    total_xpoints = afl_total_xpoints_mdl,
    xscore_diff   = afl_xscore_diff_mdl,
    conv_diff     = afl_conv_mdl,
    score_diff    = afl_score_mdl,
    win           = afl_win_mdl
  )

  cli::cli_alert_success("GAM pipeline trained on {nrow(gam_df)} matches, predictions generated")

  list(models = models, data = team_mdl_df)
}


# .predict_all_rows ----

#' Score every row of a frame, refusing to return a misaligned vector
#'
#' `model.matrix()`'s default `na.action` is `na.omit`, so ANY row carrying an
#' NA in a feature column is silently DROPPED and the caller gets back fewer
#' predictions than the frame has rows. `.train_match_xgb()` assigns the result
#' straight onto the frame (`team_mdl_df$xgb_pred_... <- ...`), and that fails
#' two different ways depending on arithmetic:
#'
#' * lengths do not divide — `"replacement has N rows, data has M"`, raised far
#'   from the cause and naming neither the NAs nor the matches involved;
#' * lengths DO divide — R **recycles silently**, attaching every prediction
#'   after the first gap to the wrong match, with nothing logged.
#'
#' The second is why this is a guard and not a tidy-up.
#'
#' Not hypothetical: placeholder finals fixtures (teams TBD) carry NA rating
#' features every year from the moment the AFL publishes the finals schedule,
#' which is exactly when predictions matter most. Bit twice on 2026-07-29 in two
#' different places.
#'
#' **The obvious one-line fix does not work.** Passing `na.action = na.pass` to
#' `model.matrix()` still drops the rows — measured, 5-row frame in, 3 rows out.
#' The `na.action` has to reach `model.frame()`, hence the detour below.
#' XGBoost then routes NA down the default branch it learned at training time,
#' so the vector comes back full length and finite.
#'
#' Training is unaffected — `train_step()` is fed completed matches only.
#'
#' @param model A trained `xgb.Booster`.
#' @param df Frame to score. Every row gets a prediction.
#' @param feature_cols Character vector of feature columns.
#' @return Numeric vector, guaranteed `length() == nrow(df)`.
#' @keywords internal
.predict_all_rows <- function(model, df, feature_cols) {
  fdf <- df[, feature_cols, drop = FALSE]
  mf <- stats::model.frame(~ . - 1, data = fdf, na.action = stats::na.pass)
  mat <- stats::model.matrix(~ . - 1, data = mf)
  if (nrow(mat) != nrow(df)) {
    cli::cli_abort(c(
      "Feature matrix has {nrow(mat)} row{?s} for a {nrow(df)}-row frame.",
      "x" = "Rows were dropped building the design matrix, so predictions cannot be aligned to matches.",
      "i" = "Expected na.pass to preserve every row -- check for a non-numeric feature column."
    ))
  }
  preds <- predict(model, xgboost::xgb.DMatrix(data = mat))
  # Belt and braces: the entire failure mode is a short vector reaching an
  # assignment, so refuse to return one. An abort here names the cause; the
  # recycling it prevents names nothing.
  if (length(preds) != nrow(df)) {
    cli::cli_abort(
      "Predicted {length(preds)} value{?s} for {nrow(df)} row{?s} -- refusing to return a vector that would recycle."
    )
  }
  preds
}


# .train_match_xgb ----

#' Train the 5-model sequential XGBoost pipeline
#'
#' Mirrors the GAM pipeline structure: total xPoints -> xScore diff -> conv diff
#' -> score diff -> win probability, each step feeding the next.
#'
#' @param team_mdl_df Complete model dataset (with GAM predictions already added)
#' @param train_filter Logical vector indicating training rows (NULL = all completed matches)
#' @param xgb_nthread Thread cap for every xgb.train()/xgb.cv() call (default
#'   MATCH_XGB_NTHREAD = 4L). XGBoost's `tree_method = "hist"` is not
#'   deterministic across different thread counts even with a fixed seed, so
#'   pinning this makes retrains reproducible across machines/CI runners with
#'   different core counts (see docs/plans/FABLE-MATCH-MAE-PLAN.md §8).
#' @return List with $models (named list of 5 XGBoost models) and $data (team_mdl_df
#'   with xgb_pred_score_diff and xgb_pred_win columns added)
#' @keywords internal
.train_match_xgb <- function(team_mdl_df, train_filter = NULL, xgb_nthread = MATCH_XGB_NTHREAD) {
  loadNamespace("xgboost")

  if (is.null(train_filter)) {
    train_mask <- !is.na(team_mdl_df$win) & !is.na(team_mdl_df$total_xpoints_adj) &
      !is.na(team_mdl_df$xscore_diff) & !is.na(team_mdl_df$shot_conv_diff) &
      !is.na(team_mdl_df$score_diff)
  } else {
    train_mask <- train_filter & !is.na(team_mdl_df$win) &
      !is.na(team_mdl_df$total_xpoints_adj) & !is.na(team_mdl_df$xscore_diff) &
      !is.na(team_mdl_df$shot_conv_diff) & !is.na(team_mdl_df$score_diff)
  }

  xgb_df <- team_mdl_df[train_mask, ]
  cli::cli_inform("XGBoost training on {nrow(xgb_df)} rows")
  if (nrow(xgb_df) == 0) {
    cli::cli_abort("Cannot train XGBoost: 0 complete rows after filtering")
  }

  # Feature columns — diffs only for rating/context features (no .x/.y splits)
  # to enforce symmetry. Temporal .x features are shared per match, not team-specific.
  # Include osr_diff/dsr_diff only if available (requires PSR decomposition)
  osr_dsr_cols <- character(0)
  if (all(c("osr_diff", "dsr_diff") %in% names(team_mdl_df)) &&
      !all(is.na(team_mdl_df$osr_diff))) {
    osr_dsr_cols <- c("osr_diff", "dsr_diff")
  }

  # xelo_diff (2026-07-28, FABLE-MATCH-FEATURES-PLAN.md WS1b): the xScore team
  # power rating, replacing the win-based elo_diff that shipped with C6 -- a
  # dynamic team-strength signal absent from the player-rating diffs above, now
  # driven by expected rather than actual score. Included unconditionally (not
  # behind an availability check like osr_dsr_cols) because
  # .build_team_mdl_df() always adds it, with a neutral 0 fallback on failure
  # -- see match_data_prep.R and xscore_rating.R.
  base_cols <- c(
    "team_type_fac",
    "game_year_decimal.x", "game_prop_through_year.x",
    "game_prop_through_month.x", "game_prop_through_day.x",
    "epr_diff", "epr_recv_diff", "epr_disp_diff",
    "epr_spoil_diff", "epr_hitout_diff",
    "torp_diff", "psr_diff", osr_dsr_cols,
    "xelo_diff",
    # xrapm_diff (2026-08-25): decay-weighted, SPM-shrunk RAPM. Wired at Pete's
    # explicit direction despite FAILING the g7 gate (p = 0.075, CI spans zero)
    # -- see team_rapm_match_feature.R's header. Included unconditionally on the
    # same reasoning as xelo_diff: .build_team_mdl_df() always produces the
    # column, falling back to a flat 0 when no snapshot is available.
    #
    # DELIBERATELY XGBOOST-ONLY -- do not add s(xrapm_diff) to the GAM formulas
    # without gating it first. The sec22/sec23 gate fed this feature through
    # rolling_lib.R's `extra_feature_cols`, which appends to the XGBoost
    # base_cols and never touches a GAM formula. So the p = 0.075 result
    # describes the XGBoost half alone; putting it in the GAMs would ship a
    # configuration with no measurement behind it at all, which is strictly
    # worse-evidenced than the already-sub-threshold number we do have.
    #
    # Attempted 2026-08-27: adding it was trivial (mirrors xelo_diff exactly),
    # but every locally-cached team_mdl_df snapshot available for gating has
    # xrapm_diff constant at 0 (no real xRAPM backfill present -- see
    # torpverse/docs/NEXT-STEPS.md). Reverted rather than ship ungated.
    "xrapm_diff",
    # Listed-position splits. Only usable as features because the published EPR
    # is position-centred (EPR_POSITION_CENTRE); uncentred bucket sums encode
    # roster shape, and teams differ in bucket counts in 40-76% of matches.
    MATCH_LISTED_POS_DIFF_COLS,
    "log_dist_diff",
    "familiarity_diff",
    "days_rest_diff_fac"
  )

  # Weather enters Step 1 only, matching the GAM structure (Model 1 has
  # weather smooths, Models 2-5 inherit the signal via gam_pred_tot_xscore).
  # Downstream XGBoost steps likewise consume xgb_pred_tot_xscore.
  weather_cols <- character(0)
  weather_candidates <- c("log_wind", "log_precip", "temp_avg", "humidity_avg", "is_roof")
  if (all(weather_candidates %in% names(team_mdl_df))) {
    weather_cols <- weather_candidates
  }
  s1_cols <- c(base_cols, weather_cols)

  reg_params <- list(
    objective = "reg:squarederror", eval_metric = "rmse",
    tree_method = "hist", eta = 0.05, subsample = 0.7,
    colsample_bytree = 0.8, max_depth = 3, min_child_weight = 15,
    nthread = xgb_nthread
  )
  cls_params <- list(
    objective = "binary:logistic", eval_metric = "logloss",
    tree_method = "hist", eta = 0.05, subsample = 0.7,
    colsample_bytree = 0.8, max_depth = 3, min_child_weight = 15,
    nthread = xgb_nthread
  )

  # Season-grouped CV folds. Also reused below by oof_predict_xgb() to
  # de-leak the stacked cascade -- a training row with an unmapped season.x
  # would silently sit in no fold and never get corrected, so guard it here
  # rather than downstream.
  stopifnot(!anyNA(xgb_df$season.x))
  train_seasons <- sort(unique(xgb_df$season.x))
  folds <- lapply(train_seasons, function(s) which(xgb_df$season.x == s))

  # Helper: build DMatrix, run CV, train final model
  train_step <- function(df, label, weights, feature_cols, params, step_name) {
    fmat <- stats::model.matrix(~ . - 1, data = df[, feature_cols, drop = FALSE])
    dtrain <- xgboost::xgb.DMatrix(data = fmat, label = label, weight = weights)

    withr::local_seed(1234)
    cv <- xgboost::xgb.cv(
      params = params, data = dtrain, nrounds = 1000, folds = folds,
      early_stopping_rounds = 30, print_every_n = 0, verbose = 0
    )
    metric_col <- paste0("test_", params$eval_metric, "_mean")
    best_n <- which.min(cv$evaluation_log[[metric_col]])
    cv_score <- min(cv$evaluation_log[[metric_col]])

    withr::local_seed(1234)
    model <- xgboost::xgb.train(
      params = params, data = dtrain, nrounds = best_n,
      print_every_n = 0, verbose = 0
    )
    # colnames(fmat) is the ONLY reliable record of what this model expects:
    # an xgb.Booster in xgboost 3.x is a bare external pointer, carries no
    # feature_names, and predict() on a matrix with the wrong columns returns
    # numbers instead of erroring. Callers that rebuild a design matrix later
    # (build_matchup_table()) assert against this.
    list(model = model, preds = predict(model, dtrain),
         best_n = best_n, cv_score = cv_score,
         feature_names = colnames(fmat))
  }

  # Helper: predict on the full frame. Defined at file scope as
  # .predict_all_rows() so its row-alignment guard is testable without training
  # a model; see the comment on that function for why the guard exists.
  predict_all <- .predict_all_rows

  # Helper: out-of-fold prediction for one cascade stage, using the SAME
  # season folds as train_step()'s xgb.cv() and the ALREADY-CHOSEN best_n
  # (not re-run per fold -- nrounds was already selected honestly via
  # cross-validated early stopping on the full training set, so refitting
  # xgb.cv per fold too would only add compute, not honesty). Used so a
  # stage's own in-sample fit is never fed forward as the next stage's
  # input feature (see header comment on this function and
  # .train_match_gams()/.oof_predict_gam() for the GAM-side equivalent).
  oof_predict_xgb <- function(df, label, weights, feature_cols, params, best_n) {
    oof <- rep(NA_real_, nrow(df))
    for (f in folds) {
      fmat_tr <- stats::model.matrix(~ . - 1, data = df[-f, feature_cols, drop = FALSE])
      dtr <- xgboost::xgb.DMatrix(data = fmat_tr, label = label[-f], weight = weights[-f])
      withr::local_seed(1234)
      fit <- xgboost::xgb.train(
        params = params, data = dtr, nrounds = best_n, print_every_n = 0, verbose = 0
      )
      fmat_te <- stats::model.matrix(~ . - 1, data = df[f, feature_cols, drop = FALSE])
      oof[f] <- predict(fit, xgboost::xgb.DMatrix(data = fmat_te))
    }
    oof
  }

  # Step 1: total xPoints (includes weather features)
  s1 <- train_step(xgb_df, xgb_df$total_xpoints_adj, xgb_df$weightz, s1_cols, reg_params, "total_xpoints")
  # Out-of-fold correction for training rows: steps 2-4 all consume
  # xgb_pred_tot_xscore as an input feature, so what they train on must not
  # come from a fit that already saw that row's own outcome. team_mdl_df's
  # non-training rows (upcoming fixtures) keep the full-model prediction
  # below -- there is no held-out fold for them, and it is already
  # legitimately out-of-sample.
  xgb_df$xgb_pred_tot_xscore <- oof_predict_xgb(
    xgb_df, xgb_df$total_xpoints_adj, xgb_df$weightz, s1_cols, reg_params, s1$best_n
  )
  team_mdl_df$xgb_pred_tot_xscore <- predict_all(s1$model, team_mdl_df, s1_cols)
  team_mdl_df$xgb_pred_tot_xscore[train_mask] <- xgb_df$xgb_pred_tot_xscore

  # Step 2: xScore diff
  s2_cols <- c(base_cols, "xgb_pred_tot_xscore")
  s2 <- train_step(xgb_df, xgb_df$xscore_diff, xgb_df$weightz, s2_cols, reg_params, "xscore_diff")
  # Out-of-fold correction: steps 3 and 4 both consume xgb_pred_xscore_diff.
  xgb_df$xgb_pred_xscore_diff <- oof_predict_xgb(
    xgb_df, xgb_df$xscore_diff, xgb_df$weightz, s2_cols, reg_params, s2$best_n
  )
  team_mdl_df$xgb_pred_xscore_diff <- predict_all(s2$model, team_mdl_df, s2_cols)
  team_mdl_df$xgb_pred_xscore_diff[train_mask] <- xgb_df$xgb_pred_xscore_diff

  # Step 3: conv diff
  s3_cols <- c(base_cols, "xgb_pred_tot_xscore", "xgb_pred_xscore_diff")
  s3 <- train_step(xgb_df, xgb_df$shot_conv_diff, xgb_df$shot_weightz, s3_cols, reg_params, "conv_diff")
  # Out-of-fold correction: step 4 consumes xgb_pred_conv_diff.
  xgb_df$xgb_pred_conv_diff <- oof_predict_xgb(
    xgb_df, xgb_df$shot_conv_diff, xgb_df$shot_weightz, s3_cols, reg_params, s3$best_n
  )
  team_mdl_df$xgb_pred_conv_diff <- predict_all(s3$model, team_mdl_df, s3_cols)
  team_mdl_df$xgb_pred_conv_diff[train_mask] <- xgb_df$xgb_pred_conv_diff

  # Step 4: score diff
  s4_cols <- c(base_cols, "xgb_pred_xscore_diff", "xgb_pred_conv_diff", "xgb_pred_tot_xscore")
  s4 <- train_step(xgb_df, xgb_df$score_diff, xgb_df$weightz, s4_cols, reg_params, "score_diff")
  # Out-of-fold correction: step 5 consumes xgb_pred_score_diff, AND it is
  # served directly (run_predictions_pipeline() blends gam_pred_score_diff/
  # xgb_pred_score_diff for every row, including completed matches) -- so
  # training-row honesty here also matters for historical reporting, not
  # only for step 5's own fit.
  xgb_df$xgb_pred_score_diff <- oof_predict_xgb(
    xgb_df, xgb_df$score_diff, xgb_df$weightz, s4_cols, reg_params, s4$best_n
  )
  team_mdl_df$xgb_pred_score_diff <- predict_all(s4$model, team_mdl_df, s4_cols)
  team_mdl_df$xgb_pred_score_diff[train_mask] <- xgb_df$xgb_pred_score_diff

  # Step 5: win probability — computed for diagnostics only. Not used in
  # final pred_win: tree models can't represent the smooth saturating logit
  # shape that AFL margins follow (XGB tends to overconfidence at moderate
  # margins, e.g. ~0.72 at +11pts vs GAM's ~0.62, implying a residual SD of
  # ~19 pts when AFL's true value is ~35). The blended margin is fed back
  # through the GAM win head instead — see match_model.R blend block.
  s5_cols <- c(
    "team_type_fac",
    "xgb_pred_tot_xscore", "xgb_pred_score_diff",
    "log_dist_diff", "familiarity_diff", "days_rest_diff_fac"
  )
  s5 <- train_step(xgb_df, as.numeric(xgb_df$win), xgb_df$weightz, s5_cols, cls_params, "win")
  xgb_df$xgb_pred_win <- s5$preds
  team_mdl_df$xgb_pred_win <- predict_all(s5$model, team_mdl_df, s5_cols)

  cli::cli_alert_success("XGBoost pipeline trained ({s1$best_n}/{s2$best_n}/{s3$best_n}/{s4$best_n}/{s5$best_n} rounds)")

  models <- list(
    total_xpoints = s1$model, xscore_diff = s2$model, conv_diff = s3$model,
    score_diff = s4$model, win = s5$model
  )
  steps <- list(
    total_xpoints = list(best_n = s1$best_n, cv_score = s1$cv_score),
    xscore_diff   = list(best_n = s2$best_n, cv_score = s2$cv_score),
    conv_diff      = list(best_n = s3$best_n, cv_score = s3$cv_score),
    score_diff     = list(best_n = s4$best_n, cv_score = s4$cv_score),
    win            = list(best_n = s5$best_n, cv_score = s5$cv_score)
  )
  feature_names <- list(
    total_xpoints = s1$feature_names, xscore_diff = s2$feature_names,
    conv_diff = s3$feature_names, score_diff = s4$feature_names,
    win = s5$feature_names
  )
  list(models = models, steps = steps, data = team_mdl_df,
       feature_names = feature_names)
}
