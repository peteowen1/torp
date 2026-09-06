# wpa_close_game_bias.R — WPA close-game exposure bias, calibrated vs uncalibrated
# torpverse/docs/plans/FABLE-RECAL-PLAN.md Step 6 (§2). Manual script, NOT wired to any pipeline
# (data-raw/rebuild_everything.R does not source this). Run from the torp
# package root:
#   Rscript data-raw/04-analysis/wpa_close_game_bias.R
#
# Purpose: WPA-in-TORP reinstatement (torp/CLAUDE.md TORP-composition note)
# was blocked on two prerequisites -- both now shipped (Steps 1-4 of this
# plan). This script is the promised follow-up measurement: does calibrated
# WPA still over-reward players with heavy close-game exposure, independent
# of their actual quality (torp_value)? If the close-game exposure effect
# shrinks materially under calibration, reinstatement becomes defensible.
# This script does NOT decide the blend weight -- it produces the evidence
# memo (FABLE-WPA-DECISION.md) for Pete to act on.
#
# Best run AFTER Step 5 has published wp_calibration.rds -- before that,
# get_wp_preds() has no sidecar to load and degrades to identity (D4), so
# the "calibrated" and "uncalibrated" variants below would be identical and
# this script would (correctly) report zero shrinkage.

suppressMessages({
  library(dplyr)
})
devtools::load_all()

# Also load dev torpmodels (sibling repo), not just dev torp. Root cause of a
# real incident (2026-07-22): with only `devtools::load_all()` for torp,
# get_wp_preds()'s calibration lookup goes through the *installed* library
# copy of torpmodels, which can be stale relative to what's actually
# published (e.g. predates load_torp_model() recognizing "wp_calibration" as
# a valid model name). load_model_with_fallback() then hits an error inside
# torpmodels::load_torp_model(), catches it, warns once, and returns NULL --
# so the "calibrated" arm silently degrades to identity and produces a
# bit-identical memo against the "uncalibrated" arm without any error.
# Mirrors torpmodels/data-raw/train_models.R's own load_all(torp) +
# load_all(torpmodels) pattern.
torpmodels_path <- "../torpmodels"
if (!dir.exists(torpmodels_path)) {
  cli::cli_abort(c(
    "Cannot find dev torpmodels at {.path {torpmodels_path}}.",
    "i" = "Run this script from the torp package root inside the torpverse sibling layout (torpverse/torp, torpverse/torpmodels)."
  ))
}
cli::cli_inform("Loading dev torpmodels from {torpmodels_path}...")
devtools::load_all(torpmodels_path)

SEASONS <- (torp::get_afl_season() - 3):(torp::get_afl_season() - 1)  # 3 most recent COMPLETED seasons
CLOSE_MARGIN <- 12          # matches torpverse/docs/reviews/FABLE-WP-EXPERIMENTS.md's Q4/close convention
MIN_GAMES <- 5              # drop tiny-sample player-seasons from the regression/leaderboard

cli::cli_h1("WPA close-game exposure bias -- torpverse/docs/plans/FABLE-RECAL-PLAN.md Step 6")
cli::cli_inform("Seasons: {paste(SEASONS, collapse = ', ')} | close margin: |{CLOSE_MARGIN}| | min games: {MIN_GAMES}")

# --- Script-local calibration bypass ---------------------------------------
# NOT a product-code change: get_wp_preds() (torp/R/add_variables.R, D4)
# reads the wp_calibration sidecar through torp's in-memory model cache
# (.torp_model_cache). Priming that cache with a NULL entry makes
# get_wp_preds() take its identity-fallback path for this session only;
# clearing the entry lets it load (and warn-once-cache) the real sidecar
# again. Nothing here persists past the R session.
set_wp_calibration_bypass <- function(bypass) {
  cache <- torp:::.torp_model_cache
  if (isTRUE(bypass)) {
    assign("wp_calibration", NULL, envir = cache)
  } else if (exists("wp_calibration", envir = cache)) {
    rm(list = "wp_calibration", envir = cache)
  }
}

# --- Build wp/wpa-annotated player-game WPA credit, once per calibration mode
# Mirrors the production WP feature-build convention used by
# torpmodels/data-raw/lib/train_lib.R's insample branch and
# torp/R/analyze_match.R's get_player_game_ratings(): clean_pbp() ->
# clean_model_data_epv() -> add_epv_vars() -> clean_model_data_wp() ->
# add_wp_vars(). create_wp_credit() (torp/R/wp_credit.R, untouched by this
# plan) then splits WPA between disposer/receiver per play and aggregates
# to one row per player per match.
build_wp_credit <- function(bypass_calibration, chains) {
  set_wp_calibration_bypass(bypass_calibration)
  label <- if (bypass_calibration) "UNCALIBRATED (bypassed)" else "CALIBRATED"
  cli::cli_inform("Building WP credit -- {label}...")

  pbp <- chains |>
    torp::clean_pbp() |>
    torp:::clean_model_data_epv() |>
    torp::add_epv_vars() |>
    torp:::clean_model_data_wp() |>
    torp::add_wp_vars()

  # wp is returned alongside the aggregated credit table so the caller can
  # hard-check that the two variants actually diverge (see sanity check
  # below) -- wp_credit alone (post-aggregation) obscures a bug where both
  # arms silently compute the same per-row wp.
  list(credit = torp::create_wp_credit(pbp), wp = pbp$wp)
}

cli::cli_inform("Loading chains for {paste(SEASONS, collapse = ', ')}...")
chains <- torp::load_chains(seasons = SEASONS, rounds = TRUE)
cli::cli_inform("chains rows: {nrow(chains)}")

build_uncal <- build_wp_credit(bypass_calibration = TRUE, chains = chains)
build_cal   <- build_wp_credit(bypass_calibration = FALSE, chains = chains)
set_wp_calibration_bypass(FALSE)  # leave the session in the normal (calibrated) state

credit_uncalibrated <- build_uncal$credit
credit_calibrated   <- build_cal$credit

cli::cli_inform("player-games: uncalibrated {nrow(credit_uncalibrated)}, calibrated {nrow(credit_calibrated)}")
if (nrow(credit_uncalibrated) != nrow(credit_calibrated)) {
  cli::cli_warn("Row-count mismatch between variants -- the calibration bypass may not have taken effect. Check torp:::.torp_model_cache state before trusting the results below.")
}

# --- Print the loaded calibration sidecar (CALIBRATED arm) -----------------
# Confirms which sidecar actually got applied -- form/a/b/c plus the
# temporal Q4/close slope gate this sidecar shipped with.
calib_sidecar <- torp:::load_model_with_fallback("wp_calibration")
if (is.null(calib_sidecar)) {
  cli::cli_abort(c(
    "wp_calibration sidecar failed to load for the CALIBRATED arm -- get_wp_preds() degraded to identity.",
    "i" = "Check that dev torpmodels (not a stale installed copy) is loaded and that the wp_calibration sidecar has actually been published (core-models release)."
  ))
}
cli::cli_h2("Loaded wp_calibration sidecar")
cli::cli_inform("form = {calib_sidecar$form %||% 'global_v1'} | a = {round(calib_sidecar$a, 4)} | b = {round(calib_sidecar$b, 4)} | c = {if (is.null(calib_sidecar$c)) NA else round(calib_sidecar$c, 4)}")
cli::cli_inform("slope_q4close_before = {round(calib_sidecar$slope_q4close_before, 4)} | slope_q4close_after = {round(calib_sidecar$slope_q4close_after, 4)}")
cli::cli_inform("slope_before (all rows) = {round(calib_sidecar$slope_before, 4)} | slope_after (all rows) = {round(calib_sidecar$slope_after, 4)}")

# --- Hard sanity check: the two arms must genuinely differ ------------------
# This is the exact bug class that produced a bit-identical uncalibrated vs
# calibrated memo on 2026-07-22 (root cause: stale installed torpmodels, see
# the load_all() comment above) -- never let that happen silently again.
if (length(build_uncal$wp) != length(build_cal$wp)) {
  cli::cli_abort("Uncalibrated and calibrated raw wp vectors have different lengths ({length(build_uncal$wp)} vs {length(build_cal$wp)}) -- cannot sanity-check divergence. Something upstream of get_wp_preds() differs between the two builds.")
}
wp_max_abs_diff <- max(abs(build_uncal$wp - build_cal$wp))
if (!is.finite(wp_max_abs_diff) || wp_max_abs_diff < 1e-12) {
  cli::cli_abort(c(
    "Uncalibrated and calibrated WP vectors are bit-identical (max abs diff = {wp_max_abs_diff}).",
    "i" = "The calibration bypass/apply mechanism did not take effect -- refusing to write a memo that would misreport zero shrinkage. Check torp:::.torp_model_cache, and confirm dev torpmodels (not an installed library copy) is loaded (see the load_all() comment near the top of this script)."
  ))
}
cli::cli_inform("Sanity check passed: max abs diff between uncalibrated and calibrated raw wp = {signif(wp_max_abs_diff, 6)}")

# --- Per-player-game close/margin flag --------------------------------------
# team_margin is signed from the credited player's own team's perspective
# (positive = their team won by that margin); close_game uses |team_margin|
# so it doesn't matter which side won.
results <- torp::load_results(SEASONS)
stopifnot(all(c("match_id", "home_team_name", "away_team_name", "home_score", "away_score") %in% names(results)))
results <- results |>
  dplyr::transmute(
    match_id, home_team_name, away_team_name,
    margin = as.numeric(home_score) - as.numeric(away_score)
  )

add_close_flag <- function(credit_dt) {
  out <- dplyr::as_tibble(credit_dt) |>
    dplyr::left_join(results, by = "match_id") |>
    dplyr::mutate(
      team_margin = dplyr::case_when(
        team == home_team_name ~ margin,
        team == away_team_name ~ -margin,
        TRUE ~ NA_real_
      ),
      close_game = abs(team_margin) <= CLOSE_MARGIN
    )
  n_unmatched <- sum(is.na(out$team_margin))
  if (n_unmatched > 0) {
    cli::cli_warn("{n_unmatched} of {nrow(out)} player-game rows could not be matched to a result/team (name mismatch?) -- excluded from close_game stats via NA")
  }
  out
}

credit_uncalibrated <- add_close_flag(credit_uncalibrated)
credit_calibrated   <- add_close_flag(credit_calibrated)

# --- torp_value covariate (independent of WP calibration -- EPV/PSV based,
# sourced from the release so it's identical across both variants) ---------
cli::cli_inform("Loading player_game_ratings for the torp_value covariate...")
pgr <- torp::load_player_game_ratings(seasons = SEASONS, rounds = TRUE)
stopifnot(all(c("player_id", "season", "torp_value") %in% names(pgr)))
torp_value_by_season <- pgr |>
  dplyr::group_by(player_id, season) |>
  dplyr::summarise(torp_value = mean(torp_value, na.rm = TRUE), .groups = "drop")

# --- Player-season aggregation ----------------------------------------------
player_season_summary <- function(credit_df) {
  credit_df |>
    dplyr::group_by(player_id, player_name, season) |>
    dplyr::summarise(
      n_games = dplyr::n(),
      season_wpa = sum(wp_credit, na.rm = TRUE),
      wpa_rate = season_wpa / n_games,
      close_share = mean(close_game, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(n_games >= MIN_GAMES) |>
    dplyr::left_join(torp_value_by_season, by = c("player_id", "season")) |>
    dplyr::filter(!is.na(torp_value), !is.na(close_share))
}

ps_uncal <- player_season_summary(credit_uncalibrated)
ps_cal   <- player_season_summary(credit_calibrated)
cli::cli_inform("player-seasons (>= {MIN_GAMES} games): uncalibrated {nrow(ps_uncal)}, calibrated {nrow(ps_cal)}")

# --- Report: correlation + "does close-game exposure predict WPA beyond
# quality?" regression coefficient, per variant --------------------------
report_bias <- function(ps, label) {
  cc <- stats::cor(ps$wpa_rate, ps$close_share, use = "complete.obs")
  fit <- stats::lm(wpa_rate ~ close_share + torp_value, data = ps)
  coef_close <- unname(stats::coef(fit)["close_share"])

  cli::cli_h2(label)
  cli::cli_inform("cor(wpa_rate, close_share) = {round(cc, 4)}")
  cli::cli_inform("lm(wpa_rate ~ close_share + torp_value)$close_share = {round(coef_close, 4)}")
  print(summary(fit))

  list(cor = cc, coef_close_share = coef_close, model = fit)
}

res_uncal <- report_bias(ps_uncal, "UNCALIBRATED")
res_cal   <- report_bias(ps_cal, "CALIBRATED")

shrink_pct <- 100 * (1 - abs(res_cal$coef_close_share) / abs(res_uncal$coef_close_share))
cli::cli_h2("Exposure-coefficient shrinkage under calibration")
cli::cli_inform("uncalibrated close_share coef: {round(res_uncal$coef_close_share, 4)}")
cli::cli_inform("calibrated   close_share coef: {round(res_cal$coef_close_share, 4)}")
cli::cli_inform("shrinkage: {round(shrink_pct, 1)}% (decision rule: reinstatement is defensible if this exceeds 50%, AND the calibrated leaderboard below isn't close-game-dominated)")

# --- Leaderboards side by side ----------------------------------------------
top20 <- function(ps) {
  ps |>
    dplyr::arrange(dplyr::desc(wpa_rate)) |>
    head(20) |>
    dplyr::select(player_name, season, n_games, season_wpa, wpa_rate, close_share, torp_value)
}

lb_uncal <- top20(ps_uncal)
lb_cal   <- top20(ps_cal)

cli::cli_h2("Top-20 WPA leaderboard -- UNCALIBRATED")
print(lb_uncal)
cli::cli_h2("Top-20 WPA leaderboard -- CALIBRATED")
print(lb_cal)

# "Dominated by close-game exposure": majority of the top-20 play more than
# half their season in close games. A blunt but simple, reproducible check.
cal_dominated <- mean(lb_cal$close_share > 0.5) > 0.5
cli::cli_inform("Calibrated top-20 dominated by close-game exposure: {if (cal_dominated) 'YES' else 'no'}")

# --- Decision memo (torpverse/docs/plans/FABLE-RECAL-PLAN.md Step 6.4) ---------------------------
# Does NOT pre-commit the blend weight -- produces the evidence for Pete.
decision_defensible <- isTRUE(shrink_pct > 50) && !cal_dominated

memo <- c(
  "# FABLE-WPA-DECISION.md -- WPA-in-TORP reinstatement evidence memo",
  "",
  sprintf("**Generated:** %s by `data-raw/04-analysis/wpa_close_game_bias.R`", format(Sys.Date())),
  sprintf("**Seasons analysed:** %s", paste(SEASONS, collapse = ", ")),
  "",
  sprintf("**Applied wp_calibration sidecar:** form = %s, a = %.4f, b = %.4f, c = %s (slope_q4close_before = %.4f, slope_q4close_after = %.4f)",
          if (is.null(calib_sidecar$form)) "global_v1" else calib_sidecar$form,
          calib_sidecar$a, calib_sidecar$b,
          if (is.null(calib_sidecar$c)) "NA" else sprintf("%.4f", calib_sidecar$c),
          calib_sidecar$slope_q4close_before, calib_sidecar$slope_q4close_after),
  sprintf("**Sanity check:** max abs diff between uncalibrated and calibrated raw wp = %.6g (must be >= 1e-12 or the script aborts before reaching this point)", wp_max_abs_diff),
  "",
  "## Decision rule (torpverse/docs/plans/FABLE-RECAL-PLAN.md Step 6.4)",
  "",
  "Reinstatement (as a blend component or continued parallel display -- Pete's call)",
  "becomes *defensible* if the close-game exposure coefficient shrinks materially",
  "(>50%) under calibration AND the calibrated leaderboard is not dominated by",
  "close-game exposure. This memo does NOT pre-commit the blend weight -- that is a",
  "separate, later decision commit, gated on this measurement.",
  "",
  "## Measured",
  "",
  sprintf("- Uncalibrated: cor(wpa_rate, close_share) = %.4f, lm coefficient on close_share = %.4f (n = %d player-seasons)",
          res_uncal$cor, res_uncal$coef_close_share, nrow(ps_uncal)),
  sprintf("- Calibrated:   cor(wpa_rate, close_share) = %.4f, lm coefficient on close_share = %.4f (n = %d player-seasons)",
          res_cal$cor, res_cal$coef_close_share, nrow(ps_cal)),
  sprintf("- Exposure-coefficient shrinkage under calibration: %.1f%%", shrink_pct),
  sprintf("- Calibrated top-20 leaderboard dominated by close-game exposure (>50%% of top-20 with close_share > 0.5): %s",
          if (cal_dominated) "YES" else "no"),
  "",
  sprintf("## Verdict: reinstatement is %s by this measurement",
          if (decision_defensible) "**DEFENSIBLE**" else "**NOT YET DEFENSIBLE**"),
  "",
  "Full leaderboards and regression summaries are console output only (not persisted",
  "here) -- re-run the script to reproduce, or capture its stdout alongside this file.",
  ""
)

memo_path <- file.path(getwd(), "FABLE-WPA-DECISION.md")  # run from torp repo root
writeLines(memo, memo_path)
cli::cli_alert_success("Wrote {memo_path}")
