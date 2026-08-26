# Publish the as-of xRAPM snapshots to the torpdata `team_rapm_asof-data`
# release -- one parquet per competition, one row per player per PLAYED round
# checkpoint.
#
# This is the production home for the rating behind the match model's
# `xrapm_diff` feature (AFL-DECAY-XRAPM-PLAN.md sec24/sec26). Before this
# existed the snapshot was a gitignored local artifact, which meant CI and any
# clean checkout silently got a flat-0 feature, and a stale file silently
# served frozen ratings.
#
# INCREMENTAL BY DESIGN. Every checkpoint goes through
# fit_team_rapm_asof_cached()/fit_team_spm_asof_cached(), whose disk cache
# (R/team_rapm_asof_cache.R) makes an already-computed checkpoint a near-instant
# read. So a daily run recomputes only genuinely-new rounds; a full cold build
# is ~25-37 min per comp, a warm no-new-rounds run is seconds. That is what
# makes this safe to hang off the daily ratings cadence.
#
# Usage:
#   Rscript data-raw/03-ratings/publish_team_rapm_asof.R            # both comps
#   Rscript data-raw/03-ratings/publish_team_rapm_asof.R AFLM       # one comp
#   TORP_XRAPM_DRY_RUN=1 Rscript data-raw/03-ratings/publish_team_rapm_asof.R
#
# TORP_XRAPM_DRY_RUN=1/true/yes runs every build and validation gate WITHOUT
# uploading -- use it to check a change before it reaches a public release.

devtools::load_all(".", quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
comps <- if (length(args) >= 1) args else c("AFLM", "AFLW")

dry_run <- tolower(Sys.getenv("TORP_XRAPM_DRY_RUN", "")) %in% c("1", "true", "yes")
if (dry_run) cli::cli_alert_info("DRY RUN -- building and validating, no upload.")

HALFLIFE_DAYS <- 730  # AFL-DECAY-XRAPM-PLAN.md sec18-19/sec24, Pete's pick.

# Minimum rows we expect in a real snapshot. A comp with fewer than this has
# almost certainly failed to build rather than legitimately shrunk, and must
# not overwrite a good published file.
MIN_SNAPSHOT_ROWS <- 500L

build_snapshot <- function(comp) {
  cli::cli_h2("Building {comp} as-of xRAPM snapshot (halflife={HALFLIFE_DAYS}d)")

  checkpoints <- .team_rapm_played_checkpoints(comp = comp)
  if (nrow(checkpoints) == 0) {
    cli::cli_alert_danger("No played checkpoints for comp {.val {comp}} -- skipping.")
    return(NULL)
  }
  data.table::setorder(checkpoints, season, round_number)
  cli::cli_inform("{nrow(checkpoints)} played checkpoint{?s} for {comp}.")

  t_start <- Sys.time()
  results <- vector("list", nrow(checkpoints))
  n_ok <- 0L
  n_skipped <- 0L

  for (i in seq_len(nrow(checkpoints))) {
    ref_date <- checkpoints$checkpoint_date[i]
    ckpt_season <- checkpoints$season[i]
    ckpt_round <- checkpoints$round_number[i]

    rapm_ratings <- tryCatch(
      fit_team_rapm_asof_cached(ref_date, comp = comp,
                                halflife_days = HALFLIFE_DAYS, nfolds = 10),
      error = function(e) {
        cli::cli_alert_danger("[{i}/{nrow(checkpoints)}] ref={as.character(ref_date)}: RAPM failed -- {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(rapm_ratings)) { n_skipped <- n_skipped + 1L; next }

    spm_asof <- tryCatch(
      fit_team_spm_asof_cached(ref_date, rapm_ratings, comp = comp),
      error = function(e) {
        cli::cli_alert_danger("[{i}/{nrow(checkpoints)}] ref={as.character(ref_date)}: SPM failed -- {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(spm_asof)) { n_skipped <- n_skipped + 1L; next }

    results[[i]] <- spm_asof[, .(
      player_id, ref_date = ref_date,
      season = ckpt_season, round_number = ckpt_round,
      team_rapm_shrunk,
      rapm = rapm_offense - rapm_defense,
      rapm_offense, rapm_defense,
      spm_offense, spm_defense,
      shrinkage_weight,
      n_games
    )]
    n_ok <- n_ok + 1L
  }

  elapsed_min <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 2)
  cli::cli_inform("{comp}: {n_ok} built, {n_skipped} skipped, {elapsed_min} min.")

  out <- data.table::rbindlist(results, fill = TRUE)
  if (nrow(out) == 0) return(NULL)
  attr(out, "elapsed_min") <- elapsed_min
  out
}

# Validate before publishing -- this writes to a PUBLIC release ----
validate_snapshot <- function(dt, comp) {
  required <- c("player_id", "season", "round_number", "team_rapm_shrunk")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    cli::cli_abort("{comp}: snapshot missing required column{?s}: {missing}. Refusing to publish.")
  }
  if (nrow(dt) < MIN_SNAPSHOT_ROWS) {
    cli::cli_abort("{comp}: only {nrow(dt)} rows (< {MIN_SNAPSHOT_ROWS}). Refusing to publish a likely-broken build.")
  }
  if (anyNA(dt$player_id)) {
    cli::cli_abort("{comp}: NA player_id present. Refusing to publish.")
  }
  if (all(is.na(dt$team_rapm_shrunk))) {
    cli::cli_abort("{comp}: team_rapm_shrunk is entirely NA -- the shipping column is empty. Refusing to publish.")
  }
  n_dup <- nrow(dt) - nrow(unique(dt, by = c("player_id", "season", "round_number")))
  if (n_dup > 0) {
    cli::cli_abort("{comp}: {n_dup} duplicate (player, season, round) row{?s} -- the join would inflate team sums. Refusing to publish.")
  }
  # Snapshots must never contain a round that has not been played.
  res <- data.table::as.data.table(load_results(TRUE, comp = comp))
  played <- unique(res[!is.na(home_score) & !is.na(away_score), .(season, round_number)])
  unplayed <- unique(dt[, .(season, round_number)])[!played, on = .(season, round_number)]
  if (nrow(unplayed) > 0) {
    cli::cli_abort(c(
      "{comp}: snapshot contains {nrow(unplayed)} checkpoint{?s} for rounds with no recorded score.",
      "x" = "Those rows churn daily until their date passes -- refusing to publish."
    ))
  }

  latest <- dt[which.max(season * 100L + round_number)]
  cli::cli_alert_success(
    "{comp} validated: {nrow(dt)} rows, {length(unique(dt$ref_date))} checkpoints, latest {latest$season} R{latest$round_number}."
  )
  invisible(TRUE)
}

for (comp in comps) {
  dt <- build_snapshot(comp)
  if (is.null(dt)) {
    cli::cli_alert_danger("{comp}: nothing built, skipping publish.")
    next
  }
  validate_snapshot(dt, comp)

  f_stem <- sprintf("career_team_rapm_asof_%s", comp)
  local_path <- file.path("data-raw", "03-ratings", paste0(f_stem, ".parquet"))
  arrow::write_parquet(dt, local_path)
  cli::cli_inform("Wrote local {local_path}.")

  if (dry_run) {
    cli::cli_alert_info("{comp}: DRY RUN -- skipping upload to {.val {TEAM_RAPM_ASOF_RELEASE_TAG}}.")
  } else {
    save_to_release(
      df = as.data.frame(dt),
      file_name = f_stem,
      release_tag = TEAM_RAPM_ASOF_RELEASE_TAG
    )
    cli::cli_alert_success("{comp}: published to {.val {TEAM_RAPM_ASOF_RELEASE_TAG}}.")
  }
}

cli::cli_alert_success("Done.")
