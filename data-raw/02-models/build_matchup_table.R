# Build and publish the 612-row directed matchup table (torp#108)
# =================================================================
# Runs after run_predictions_pipeline() in daily-ratings-predictions.yml.
# inthegame-blog reads the published file from R2 and uses it to price every
# finals tie: resolveFinal() (afl/season-sim.js) OVERRIDES its own strength
# formula whenever a table row is found, and the table covers all 306 ordered
# pairs in both venue contexts, so a row is ALWAYS found. That makes this file
# the sole input to the site's finals odds -- which is why it is built fresh
# every run and gated before it is allowed anywhere near a release.
#
# Until 2026-08-20 there was no build step at all. The file on R2 was a
# hand-made one-off from 2026-07-17 at round 19, five weeks stale in finals
# week, and nothing anywhere said so.
#
# THE GATE, and why it is this particular check.
# build_matchup_table() re-implements the model's predict path against
# fabricated rows. That path can silently diverge from production's: on
# 2026-08-20 its hand-built XGBoost design matrix was six columns short of
# what the models were trained on, and nothing failed -- an xgb.Booster in
# xgboost 3.x carries no feature_names, and predict() on a matrix with the
# wrong column count returns numbers rather than erroring. Every finals tie on
# the site was priced from the resulting blend for weeks.
#
# So before publishing, this scores the REAL upcoming-round rows through the
# same .predict_match_model() the fabricated rows go through, and compares
# against the pred_score_diff production already stored on them. Same rows,
# same state, same models: the two must agree. They differ by ~0.07 margin
# points when healthy and differed by 79.68 when broken.
#
# A failure here does NOT block the predictions that have already published --
# the workflow step is continue-on-error -- but it must be loud, because a
# quietly-skipped build leaves the site serving an ever-staler table with
# nothing to show for it.

suppressMessages(devtools::load_all(quiet = TRUE))

# Threshold is calibrated from measured behaviour, not guessed. Mutation-tested
# against 2026 R24 -- every one of these fires, and the healthy run sits two
# orders of magnitude below the smallest of them:
#
#   unmutated control                          max|d| =  0.0738   passes
#   XGBoost feature list drifts (the 2026-08 bug)        aborts inside
#                                              .xgb_predict before the gate
#   XGBoost models missing, GAM-only blend     max|d| =  8.5806   FIRES
#   margin calibration off by 15%              max|d| = 13.2849   FIRES
#   margin calibration silently halved         max|d| = 38.1175   FIRES
MAX_ABS_DIFF <- 1.0   # margin points
MIN_TEAMS    <- 18

season <- get_afl_season()
week   <- get_afl_week(type = "next")
cli::cli_h1("Matchup table: {season} R{week}")

# get_afl_week() downgrades a fixture-load failure to an empty frame and returns
# round 0 rather than erroring (R/utils.R). Building a table stamped for the
# wrong round is worse than not building one, because the blog reads that stamp
# to decide whether the table is current -- so refuse the implausible input
# rather than inherit it.
if (!is.finite(week) || week < 1) {
  cli::cli_abort(c(
    "Refusing to build for round {week}.",
    "i" = "get_afl_week() returns 0 when fixtures fail to load; a table stamped with the wrong round would be read as current by the site."
  ))
}

state <- torp:::.freeze_match_state(season = season, week = week)

# ---- Gate 1: the fabricated predict path still agrees with production -------
# Restricted to rows NOT YET PLAYED (win is NA), not just this round's rows.
# Every home-and-away round used to satisfy that for free -- the daily
# pipeline always ran before any game in the upcoming round had a result --
# but a finals round spans several days, and this script can run after some
# of the round's games are already decided. A played row is now a TRAINING
# row: torp:::.oof_predict_gam()'s season-grouped out-of-fold correction
# (2026-09) deliberately gives it a DIFFERENT gam_pred_score_diff than a
# plain predict() replay would produce, by design (it de-leaks the cascade).
# Comparing on played rows fails Gate 1 for a reason that has nothing to do
# with the drift this gate exists to catch -- confirmed 2026-09-04: two of
# four R26 rows were already-decided finals results and diverged by up to
# 16.3 margin points from a fresh replay, while the two genuinely-upcoming
# rows in the same round agreed to within 0.04.
real <- state$team_mdl_df[
  state$team_mdl_df$season.x == season & state$team_mdl_df$round_number.x == week &
    is.na(state$team_mdl_df$win),
]
if (nrow(real) == 0) {
  cli::cli_abort(c(
    "No {season} R{week} row(s) still awaiting a result -- cannot verify the predict path, refusing to publish.",
    "i" = "Either the round has no rows at all (pre-season / fixtures not published), or every game in it is already decided."
  ))
}
scored <- torp:::.predict_match_model(as.data.frame(real), state)
d <- abs(real$pred_score_diff - scored$pred_score_diff)
# Count the missing BEFORE taking a max that would drop them. na.rm = TRUE on a
# partially-NA vector reports the largest disagreement among the rows that
# happened to score, which reads as agreement while some rows never produced a
# comparable value at all -- a subset-failure variant of the very drift this
# gate is here to catch.
n_na_d <- sum(is.na(d))
if (n_na_d > 0) {
  cli::cli_abort(c(
    "Predict-path check FAILED: {n_na_d} of {length(d)} row(s) produced no comparable prediction.",
    "i" = "A partial NA means the replicate predict path broke for a subset of rows. Not publishing."
  ))
}
max_d <- max(d, na.rm = TRUE)
cli::cli_alert_info("Predict-path agreement on {nrow(real)} real row(s): max|d| = {round(max_d, 4)} margin points")
if (!is.finite(max_d) || max_d > MAX_ABS_DIFF) {
  cli::cli_abort(c(
    "Predict-path check FAILED: max|d| = {round(max_d, 4)} > {MAX_ABS_DIFF} margin points.",
    "x" = "build_matchup_table() disagrees with production on rows they both price.",
    "i" = "Most likely the XGBoost feature list in .predict_match_model() has drifted from .train_match_xgb(). Not publishing."
  ))
}

# ---- Build ------------------------------------------------------------------
tbl <- build_matchup_table(state = state)

# ---- Gate 2: shape and sanity ----------------------------------------------
n_teams  <- length(unique(c(tbl$home, tbl$away)))
expected <- n_teams * (n_teams - 1) * 2
n_na <- sum(is.na(tbl$p_home) | is.na(tbl$pred_margin) | is.na(tbl$pred_total))
cli::cli_alert_info("{nrow(tbl)} rows, {n_teams} teams, {n_na} NA row(s); margin range [{round(min(tbl$pred_margin), 1)}, {round(max(tbl$pred_margin), 1)}]")

if (n_teams < MIN_TEAMS)   cli::cli_abort("Only {n_teams} team(s) in the table, expected {MIN_TEAMS}. Not publishing.")
if (nrow(tbl) != expected) cli::cli_abort("Expected {expected} rows for {n_teams} teams, got {nrow(tbl)}. Not publishing.")
if (n_na > 0)              cli::cli_abort("{n_na} row(s) carry NA p_home/pred_margin/pred_total. Not publishing.")

# A table whose margins are all near zero is the signature of the predict path
# losing its discrimination -- the failure mode Gate 1 targets, caught again
# here in case a future break survives it.
if (stats::sd(tbl$pred_margin) < 5) {
  cli::cli_abort("pred_margin sd is {round(stats::sd(tbl$pred_margin), 2)}, implausibly flat for 18 AFL clubs. Not publishing.")
}

# ---- Publish ----------------------------------------------------------------
# TORP_MATCHUP_DRY_RUN=1 runs every gate and the full build, then stops short of
# the upload. This is how the gates get exercised against real data without
# promoting a vintage -- R2 is read at runtime by the site, so a release upload
# reaches production on the next blog-data run with no preview stage in between.
# Explicitly truthy, NOT nzchar(): nzchar("0") is TRUE, so a stray
# TORP_MATCHUP_DRY_RUN=0 -- the value someone would set meaning "off" -- would
# silently switch publishing off forever while the step stayed green. That is
# the exact failure mode this pipeline exists to prevent, and the escape hatch
# meant to test it safely would have reintroduced it.
if (tolower(Sys.getenv("TORP_MATCHUP_DRY_RUN")) %in% c("1", "true", "yes")) {
  # WARNING, not success: a dry run must not read like a normal publish to
  # anyone skimming the log or grepping it.
  cli::cli_alert_warning("DRY RUN: all gates passed, {nrow(tbl)} rows built, NOTHING UPLOADED")
  cat("::warning::Matchup table ran in DRY RUN mode -- nothing was published.
")
  quit(save = "no", status = 0)
}

save_to_release(
  as.data.frame(tbl),
  paste0("matchup_table_", season),
  "matchup-table-data",
  prev_rows_floor = 0.9
)
cli::cli_alert_success("Published matchup_table_{season}.parquet ({nrow(tbl)} rows, {season} R{week})")
