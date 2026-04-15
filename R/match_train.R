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

#' Train the 5-model sequential GAM pipeline
#'
#' Trains total xPoints, xScore diff, conversion, score diff, and win probability
#' GAMs sequentially (each model's predictions feed the next). Model 1 includes
#' weather smooths (log_wind, log_precip, temp_avg, humidity_avg).
#'
#' @param team_mdl_df Complete model dataset from .build_team_mdl_df()
#' @param train_filter Logical vector indicating training rows (NULL = all completed matches)
#' @param nthreads Number of threads for mgcv::bam() (default 4)
#' @return List with $models (named list of 5 GAMs) and $data (team_mdl_df with predictions)
#' @keywords internal
.train_match_gams <- function(team_mdl_df, train_filter = NULL, nthreads = 4L) {
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
    "ti(psr_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)" = list(var = "psr_diff", k = 4)
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
    "+ s(abs(epr_diff), bs = \"ts\", k = 5)",
    "+ s(abs(epr_recv_diff), bs = \"ts\", k = 5)",
    "+ s(abs(epr_disp_diff), bs = \"ts\", k = 5)",
    "+ s(abs(epr_spoil_diff), bs = \"ts\", k = 5)",
    "+ s(abs(epr_hitout_diff), bs = \"ts\", k = 5)",
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
    drop.unused.levels = FALSE
  )
  team_mdl_df$gam_pred_tot_xscore <- predict(afl_total_xpoints_mdl, newdata = team_mdl_df, type = "response")

  # Model 2: xScore differential
  cli::cli_progress_step("Training xScore diff model")
  gam_df$gam_pred_tot_xscore <- team_mdl_df$gam_pred_tot_xscore[train_mask]
  m2_base <- paste(
    "xscore_diff ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ ti(epr_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
    "+ s(gam_pred_tot_xscore, bs = \"ts\", k = 5)",
    "+ s(epr_diff, bs = \"ts\", k = 5)",
    "+ s(epr_recv_diff, bs = \"ts\", k = 5)",
    "+ s(epr_disp_diff, bs = \"ts\", k = 5)",
    "+ s(epr_spoil_diff, bs = \"ts\", k = 5)",
    "+ s(epr_hitout_diff, bs = \"ts\", k = 5)",
    "+ s(torp_diff, bs = \"ts\", k = 5)",
    "+ ti(torp_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
    "+ s(log_dist_diff, bs = \"ts\", k = 5) + s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m2_optional <- c("s(psr_diff, bs = \"ts\", k = 5)",
                    "ti(psr_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
                    "s(osr_diff, bs = \"ts\", k = 5)", "s(dsr_diff, bs = \"ts\", k = 5)")
  m2_formula <- stats::as.formula(.add_optional(m2_base, m2_optional))

  afl_xscore_diff_mdl <- mgcv::bam(
    m2_formula,
    data = gam_df, weights = gam_df$weightz,
    family = gaussian(), nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  team_mdl_df$gam_pred_xscore_diff <- predict(afl_xscore_diff_mdl, newdata = team_mdl_df, type = "response")

  # Model 3: Conversion differential
  cli::cli_progress_step("Training conversion model")
  gam_df$gam_pred_xscore_diff <- team_mdl_df$gam_pred_xscore_diff[train_mask]
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
    "+ ti(epr_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
    "+ s(epr_diff, bs = \"ts\", k = 5)",
    "+ s(epr_recv_diff, bs = \"ts\", k = 5)",
    "+ s(epr_disp_diff, bs = \"ts\", k = 5)",
    "+ s(epr_spoil_diff, bs = \"ts\", k = 5)",
    "+ s(epr_hitout_diff, bs = \"ts\", k = 5)",
    "+ s(torp_diff, bs = \"ts\", k = 5)",
    "+ ti(torp_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
    "+ s(gam_pred_tot_xscore, bs = \"ts\", k = 5)",
    "+ s(gam_pred_xscore_diff, bs = \"ts\", k = 5)",
    "+ s(venue_fac, bs = \"re\")",
    "+ s(log_dist_diff, bs = \"ts\", k = 5) + s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m3_optional <- c("s(psr_diff, bs = \"ts\", k = 5)",
                    "ti(psr_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
                    "s(osr_diff, bs = \"ts\", k = 5)", "s(dsr_diff, bs = \"ts\", k = 5)")
  m3_formula <- stats::as.formula(.add_optional(m3_base, m3_optional))

  afl_conv_mdl <- mgcv::bam(
    m3_formula,
    data = gam_df, weights = gam_df$shot_weightz,
    family = gaussian(), nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  team_mdl_df$gam_pred_conv_diff <- predict(afl_conv_mdl, newdata = team_mdl_df, type = "response")

  # Model 4: Score differential
  cli::cli_progress_step("Training score diff model")
  gam_df$gam_pred_conv_diff <- team_mdl_df$gam_pred_conv_diff[train_mask]
  m4_base <- paste(
    "score_diff ~",
    "s(team_type_fac, bs = \"re\")",
    "+ s(team_name.x, bs = \"re\") + s(team_name.y, bs = \"re\")",
    "+ s(team_name_season.x, bs = \"re\") + s(team_name_season.y, bs = \"re\")",
    "+ ti(epr_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
    "+ ti(gam_pred_xscore_diff, gam_pred_conv_diff, bs = \"ts\", k = 5)",
    "+ ti(gam_pred_tot_xscore, gam_pred_conv_diff, bs = \"ts\", k = 5)",
    "+ s(gam_pred_xscore_diff)",
    "+ s(epr_diff, bs = \"ts\", k = 5)",
    "+ s(epr_recv_diff, bs = \"ts\", k = 5)",
    "+ s(epr_disp_diff, bs = \"ts\", k = 5)",
    "+ s(epr_spoil_diff, bs = \"ts\", k = 5)",
    "+ s(epr_hitout_diff, bs = \"ts\", k = 5)",
    "+ s(torp_diff, bs = \"ts\", k = 5)",
    "+ ti(torp_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
    "+ s(log_dist_diff, bs = \"ts\", k = 5) + s(familiarity_diff, bs = \"ts\", k = 5)",
    "+ s(days_rest_diff_fac, bs = \"re\")"
  )
  m4_optional <- c("s(psr_diff, bs = \"ts\", k = 5)",
                    "ti(psr_diff, gam_pred_tot_xscore, bs = c(\"ts\", \"ts\"), k = 4)",
                    "s(osr_diff, bs = \"ts\", k = 5)", "s(dsr_diff, bs = \"ts\", k = 5)")
  m4_formula <- stats::as.formula(.add_optional(m4_base, m4_optional))

  afl_score_mdl <- mgcv::bam(
    m4_formula,
    data = gam_df, weights = gam_df$weightz,
    family = "gaussian", nthreads = nthreads, select = TRUE, discrete = TRUE,
    drop.unused.levels = FALSE
  )
  team_mdl_df$gam_pred_score_diff <- predict(afl_score_mdl, newdata = team_mdl_df, type = "response")

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
    drop.unused.levels = FALSE
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


# .train_match_xgb ----

#' Train the 5-model sequential XGBoost pipeline
#'
#' Mirrors the GAM pipeline structure: total xPoints -> xScore diff -> conv diff
#' -> score diff -> win probability, each step feeding the next.
#'
#' @param team_mdl_df Complete model dataset (with GAM predictions already added)
#' @param train_filter Logical vector indicating training rows (NULL = all completed matches)
#' @return List with $models (named list of 5 XGBoost models) and $data (team_mdl_df
#'   with xgb_pred_score_diff and xgb_pred_win columns added)
#' @keywords internal
.train_match_xgb <- function(team_mdl_df, train_filter = NULL) {
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

  base_cols <- c(
    "team_type_fac",
    "game_year_decimal.x", "game_prop_through_year.x",
    "game_prop_through_month.x", "game_prop_through_day.x",
    "epr_diff", "epr_recv_diff", "epr_disp_diff",
    "epr_spoil_diff", "epr_hitout_diff",
    "torp_diff", "psr_diff", osr_dsr_cols,
    "log_dist_diff",
    "familiarity_diff",
    "days_rest_diff_fac"
  )

  reg_params <- list(
    objective = "reg:squarederror", eval_metric = "rmse",
    tree_method = "hist", eta = 0.05, subsample = 0.7,
    colsample_bytree = 0.8, max_depth = 3, min_child_weight = 15
  )
  cls_params <- list(
    objective = "binary:logistic", eval_metric = "logloss",
    tree_method = "hist", eta = 0.05, subsample = 0.7,
    colsample_bytree = 0.8, max_depth = 3, min_child_weight = 15
  )

  # Season-grouped CV folds
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
    list(model = model, preds = predict(model, dtrain),
         best_n = best_n, cv_score = cv_score)
  }

  # Helper: predict on full dataset
  predict_all <- function(model, df, feature_cols) {
    mat <- stats::model.matrix(~ . - 1, data = df[, feature_cols, drop = FALSE])
    predict(model, xgboost::xgb.DMatrix(data = mat))
  }

  # Step 1: total xPoints
  s1 <- train_step(xgb_df, xgb_df$total_xpoints_adj, xgb_df$weightz, base_cols, reg_params, "total_xpoints")
  xgb_df$xgb_pred_tot_xscore <- s1$preds
  team_mdl_df$xgb_pred_tot_xscore <- predict_all(s1$model, team_mdl_df, base_cols)

  # Step 2: xScore diff
  s2_cols <- c(base_cols, "xgb_pred_tot_xscore")
  s2 <- train_step(xgb_df, xgb_df$xscore_diff, xgb_df$weightz, s2_cols, reg_params, "xscore_diff")
  xgb_df$xgb_pred_xscore_diff <- s2$preds
  team_mdl_df$xgb_pred_xscore_diff <- predict_all(s2$model, team_mdl_df, s2_cols)

  # Step 3: conv diff
  s3_cols <- c(base_cols, "xgb_pred_tot_xscore", "xgb_pred_xscore_diff")
  s3 <- train_step(xgb_df, xgb_df$shot_conv_diff, xgb_df$shot_weightz, s3_cols, reg_params, "conv_diff")
  xgb_df$xgb_pred_conv_diff <- s3$preds
  team_mdl_df$xgb_pred_conv_diff <- predict_all(s3$model, team_mdl_df, s3_cols)

  # Step 4: score diff
  s4_cols <- c(base_cols, "xgb_pred_xscore_diff", "xgb_pred_conv_diff", "xgb_pred_tot_xscore")
  s4 <- train_step(xgb_df, xgb_df$score_diff, xgb_df$weightz, s4_cols, reg_params, "score_diff")
  xgb_df$xgb_pred_score_diff <- s4$preds
  team_mdl_df$xgb_pred_score_diff <- predict_all(s4$model, team_mdl_df, s4_cols)

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
  list(models = models, steps = steps, data = team_mdl_df)
}
