# Has the matchup table's duplicated pipeline already drifted from production?
# ============================================================================
#
# `matchup_table.R`'s `.freeze_match_state()` re-implements the load ->
# feature -> injury-overlay -> train sequence that `run_predictions_pipeline()`
# runs, by its own admission (see that file's header). It feeds finals pricing
# to inthegame-blog. Nothing has ever checked that the two agree.
#
# Since 2026-08-11 the production half is callable on its own as
# `build_prediction_state()`, so both can be built in ONE process and compared
# directly. That is what this script does. It answers the question that gates
# migrating `.freeze_match_state()` onto the shared seam:
#
#   * identical  -> the migration is a pure de-duplication, safe to do
#   * different  -> the blog has been priced off a different model than the
#                   published tips, and THAT is the finding, not the refactor
#
# `refresh_results = FALSE` is deliberate and matches current behaviour:
# `.freeze_match_state()` does not refresh results from the AFL API either, so
# passing TRUE would introduce a difference rather than measure one.
#
# Runtime is dominated by training the GAM + XGBoost chain TWICE (~10-20 min
# each). Run it detached:
#   powershell.exe -Command 'Rscript "data-raw/05-validation/compare_matchup_state_to_pipeline.R"'

suppressMessages(devtools::load_all("."))

SEASON <- get_afl_season()
WEEK   <- get_afl_week(type = "next")

cat("\n=========================================================\n")
cat("matchup-table state vs production state\n")
cat("season:", SEASON, " week:", WEEK, "\n")
cat("=========================================================\n\n")

t0 <- Sys.time()
fs <- .freeze_match_state(season = SEASON, week = WEEK)
t1 <- Sys.time()
cat("\n.freeze_match_state() took",
    round(as.numeric(difftime(t1, t0, units = "mins")), 1), "min\n\n")

ps <- tryCatch(
  build_prediction_state(week = WEEK, season = SEASON, refresh_results = FALSE),
  error = function(e) {
    cat("build_prediction_state() ERRORED:", conditionMessage(e), "\n")
    NULL
  }
)
t2 <- Sys.time()
cat("\nbuild_prediction_state() took",
    round(as.numeric(difftime(t2, t1, units = "mins")), 1), "min\n\n")

if (is.null(ps)) {
  cat("VERDICT: could not compare -- production state did not build.\n")
  quit(status = 1)
}

a <- fs$team_mdl_df
b <- ps$team_mdl_df

cat("--- shape ---\n")
cat("freeze  :", nrow(a), "rows x", ncol(a), "cols\n")
cat("pipeline:", nrow(b), "rows x", ncol(b), "cols\n")

only_a <- setdiff(names(a), names(b))
only_b <- setdiff(names(b), names(a))
cat("columns only in freeze  :", if (length(only_a)) paste(only_a, collapse = ", ") else "(none)", "\n")
cat("columns only in pipeline:", if (length(only_b)) paste(only_b, collapse = ", ") else "(none)", "\n\n")

# Align on a stable key before comparing cells -- row ORDER differing is not
# the same finding as row CONTENT differing, and conflating them would report
# a sort as a drift.
key <- intersect(c("match_id", "team_type"), names(a))
cat("aligning on:", paste(key, collapse = " + "), "\n")
if (length(key) == 0) {
  cat("!! no shared key -- comparing in existing row order, treat with caution\n")
} else {
  a <- a[do.call(order, a[key]), , drop = FALSE]
  b <- b[do.call(order, b[key]), , drop = FALSE]
}

shared <- intersect(names(a), names(b))
cat("comparing", length(shared), "shared columns\n\n")

if (nrow(a) != nrow(b)) {
  cat("!! ROW COUNTS DIFFER -- cell comparison skipped\n")
  cat("VERDICT: DIFFERENT\n")
  quit(status = 0)
}

cat("--- per-column differences ---\n")
diffs <- list()
for (nm in shared) {
  x <- a[[nm]]
  y <- b[[nm]]
  if (identical(x, y)) next
  n_diff <- sum(!(is.na(x) & is.na(y)) & (is.na(x) | is.na(y) | x != y), na.rm = TRUE)
  maxabs <- if (is.numeric(x) && is.numeric(y)) {
    max(abs(x - y), na.rm = TRUE)
  } else NA_real_
  diffs[[nm]] <- list(n = n_diff, maxabs = maxabs)
  cat(sprintf("  %-28s %6d rows differ   max|diff| = %s\n",
              nm, n_diff, if (is.na(maxabs)) "(non-numeric)" else format(maxabs, digits = 6)))
}

if (length(diffs) == 0) {
  cat("  (none -- every shared column identical)\n")
}

# The two paths apply the margin recalibration sidecar at DIFFERENT STAGES:
# build_prediction_state() applies it to team_mdl_df (match_model.R:892),
# .freeze_match_state() only loads it (matchup_table.R:235) and applies it
# later to the synthetic rows (matchup_table.R:596). So a difference in
# pred_score_diff alone is expected and is NOT drift -- but it has to be
# PROVEN to be exactly the calibration, not merely assumed, or this reports a
# stage difference as a model difference.
if ("pred_score_diff" %in% names(diffs)) {
  cat("\n--- is pred_score_diff explained entirely by the calibration stage? ---\n")
  recal <- apply_match_margin_calibration(a$pred_score_diff, fs$margin_calib)
  resid <- max(abs(recal - b$pred_score_diff), na.rm = TRUE)
  calib_b <- if (is.null(fs$margin_calib) || is.null(fs$margin_calib$b)) NA_real_ else fs$margin_calib$b
  cat("  calibration b        :", format(calib_b, digits = 6), "\n")
  cat("  max|freeze*calib - pipeline| =", format(resid, digits = 6), "\n")
  if (isTRUE(resid < 1e-8)) {
    cat("  -> FULLY EXPLAINED. Same model, same features; only the stage differs.\n")
    diffs[["pred_score_diff"]] <- NULL
    calibration_explains_all <- TRUE
  } else {
    cat("  -> NOT fully explained. Residual difference beyond the calibration.\n")
    calibration_explains_all <- FALSE
  }
} else {
  calibration_explains_all <- NA
}

cat("\n--- fitted GAM coefficients ---\n")
for (m in intersect(names(fs$gam_models), names(ps$gam_result$models))) {
  ca <- tryCatch(stats::coef(fs$gam_models[[m]]), error = function(e) NULL)
  cb <- tryCatch(stats::coef(ps$gam_result$models[[m]]), error = function(e) NULL)
  if (is.null(ca) || is.null(cb) || length(ca) != length(cb)) {
    cat(sprintf("  %-18s not comparable\n", m))
    next
  }
  cat(sprintf("  %-18s max|coef diff| = %s\n", m,
              format(max(abs(ca - cb), na.rm = TRUE), digits = 6)))
}

cat("\n--- xgb feature-column sets ---\n")
cat("freeze osr/dsr :", paste(fs$xgb_osr_dsr_cols, collapse = ", "), "\n")
cat("freeze weather :", paste(fs$xgb_weather_cols, collapse = ", "), "\n")

cat("\n=========================================================\n")
if (length(diffs) == 0 && length(only_a) == 0 && length(only_b) == 0) {
  cat("VERDICT: NO DRIFT.\n\n")
  cat("Same features, same fitted models. .freeze_match_state() has NOT\n")
  cat("drifted from production, so migrating it onto build_prediction_state()\n")
  cat("is a pure de-duplication.\n")
  if (isTRUE(calibration_explains_all)) {
    cat("\nBUT -- ONE MIGRATION HAZARD, and it would change served numbers:\n")
    cat("build_prediction_state() returns a team_mdl_df whose pred_score_diff\n")
    cat("is ALREADY calibrated (match_model.R:892). .predict_match_model()\n")
    cat("calibrates again (matchup_table.R:596). Swapping the builder in\n")
    cat("without removing that second call DOUBLE-APPLIES the sidecar, and\n")
    cat("nothing would fail -- the blog's finals pricing would just be wrong.\n")
  }
} else {
  cat("VERDICT: DIFFERENT, beyond the known calibration-stage difference.\n")
  cat("The blog's matchup table is priced off a different model state than\n")
  cat("the published predictions. Investigate the columns above BEFORE\n")
  cat("refactoring; the refactor would change served numbers and that must\n")
  cat("be a decision, not a side effect.\n")
}
cat("=========================================================\n")
