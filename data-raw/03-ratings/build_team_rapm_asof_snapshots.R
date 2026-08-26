# Build decay-weighted, as-of-date "career xRAPM/SPM" snapshots -- one row
# per player per checkpoint date. Ports panna's career_rapm.R/spm_asof.R
# mechanism to AFL; see docs/plans/AFL-DECAY-XRAPM-PLAN.md for the full
# design and docs/plans/AFL-DECAY-XRAPM-PLAN.md's "Stage E" write-up (this
# script's own section) for what was actually measured/run.
#
# Sandbox artifact: writes a LOCAL parquet only. Does NOT publish to a
# torpdata release -- that is a separate, later step once the three-way gate
# (AFL-DECAY-XRAPM-PLAN.md sec5) has actually run. Does not touch any
# production torp/torpmodels file.
#
# Usage:
#   Rscript data-raw/03-ratings/build_team_rapm_asof_snapshots.R AFLM 2025 2026
#   Rscript data-raw/03-ratings/build_team_rapm_asof_snapshots.R AFLW 2025 2026
# (season args are optional -- restrict to a checkpoint-date range for a
# scoped/partial run; omit both for every checkpoint the fixture calendar has.)

devtools::load_all(".", quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
comp <- if (length(args) >= 1) args[1] else "AFLM"
season_filter <- if (length(args) >= 2) as.integer(args[-1]) else NULL

HALFLIFE_DAYS <- 730  # AFL-DECAY-XRAPM-PLAN.md §18-19: swept properly on the
                      # validated margin~rating_diff method (round-level
                      # walk-forward, pooled 2023-2026) -- flat curve, R^2
                      # 0.2726-0.2769 across 180-100000d, no real interior
                      # optimum (the 0.0006 gap between 730 and any nearby
                      # point is noise). 730d (2yr) is Pete's pick among the
                      # statistically-tied values, made 2026-08-25.

cli::cli_h1("Building {comp} as-of RAPM/SPM snapshots (halflife={HALFLIFE_DAYS}d)")

# PLAYED rounds only. .team_rapm_checkpoint_dates() is pure fixture-calendar
# geometry and happily returns scheduled-but-unplayed rounds; building those is
# wrong twice over (a max(round) reader sees a phantom "current" round, and the
# phantom rows are unstable -- their `match_date <= ref_date` window grows daily
# until the date passes, so the same checkpoint yields a different rating each
# run). Same trap fixed in the stat-ratings pipeline.
checkpoints <- .team_rapm_played_checkpoints(comp = comp)
if (nrow(checkpoints) == 0) {
  cli::cli_abort("No PLAYED checkpoints for comp {.val {comp}} -- nothing to build.")
}
if (!is.null(season_filter)) {
  checkpoints <- checkpoints[season %in% season_filter]
}
data.table::setorder(checkpoints, season, round_number)
cli::cli_inform("{nrow(checkpoints)} checkpoint{?s} to build{if (!is.null(season_filter)) paste0(' (seasons ', paste(season_filter, collapse=','), ' only)') else ''}.")

t_start <- Sys.time()
results <- vector("list", nrow(checkpoints))
n_ok <- 0L
n_skipped <- 0L

for (i in seq_len(nrow(checkpoints))) {
  ref_date <- checkpoints$checkpoint_date[i]
  ckpt_season <- checkpoints$season[i]
  ckpt_round <- checkpoints$round_number[i]
  t0 <- Sys.time()

  rapm_ratings <- tryCatch(
    fit_team_rapm_asof_cached(ref_date, comp = comp, halflife_days = HALFLIFE_DAYS, nfolds = 10),
    error = function(e) {
      cli::cli_warn("[{i}/{nrow(checkpoints)}] ref={as.character(ref_date)}: build failed -- {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(rapm_ratings)) { n_skipped <- n_skipped + 1L; next }

  spm_asof <- fit_team_spm_asof_cached(ref_date, rapm_ratings, comp = comp)
  if (is.null(spm_asof)) { n_skipped <- n_skipped + 1L; next }

  # season/round_number carried so the match model can join this snapshot on
  # the SAME (player_id, season, round) key PSR already uses
  # (.build_team_ratings_df's rolling as-of join) rather than re-deriving a
  # date->round mapping at consumption time.
  #
  # team_rapm_shrunk is the SHIPPING value (AFL-DECAY-XRAPM-PLAN.md sec24,
  # Pete 2026-08-25) -- RAPM shrunk toward the decay-weighted SPM prior. It was
  # previously dropped here: the `rapm` column below is the RAW offense-defense
  # difference, a different rating. Both are now carried; consumers must pick
  # deliberately.
  out <- spm_asof[, .(
    player_id, ref_date = ref_date,
    season = ckpt_season, round_number = ckpt_round,
    team_rapm_shrunk,
    rapm = rapm_offense - rapm_defense,
    rapm_offense, rapm_defense,
    spm_offense, spm_defense,
    shrinkage_weight,
    total_minutes = n_games  # placeholder unit note below
  )]
  results[[i]] <- out
  n_ok <- n_ok + 1L
  elapsed <- round(as.numeric(Sys.time() - t0), 1)
  cli::cli_inform("[{i}/{nrow(checkpoints)}] ref={as.character(ref_date)} n_matches={attr(rapm_ratings, 'n_train_matches')} p={attr(rapm_ratings, 'p')} rows={nrow(out)} ({elapsed}s)")
}

total_elapsed <- round(as.numeric(Sys.time() - t_start) / 60, 1)
cli::cli_h2("{comp}: {n_ok} snapshot{?s} built, {n_skipped} skipped, {total_elapsed} min total")

out_dt <- data.table::rbindlist(results, fill = TRUE)
if (nrow(out_dt) == 0) {
  cli::cli_abort("No snapshots built for comp {.val {comp}} -- nothing to write.")
}

# NOTE: total_minutes here is actually n_games (a placeholder), NOT minutes --
# extract_team_rapm_ratings()/shrink_team_rapm() don't currently carry a
# TOG-minutes total through to the shrunk output. Flagged, not silently
# renamed to look correct; fix before this schema is treated as final for a
# torpdata release (AL-DECAY-XRAPM-PLAN.md storage schema, sec6 point 2).
out_path <- file.path("data-raw", "03-ratings", sprintf("career_team_rapm_asof_%s.parquet", comp))
arrow::write_parquet(out_dt, out_path)
cli::cli_inform("Wrote {out_path}: {nrow(out_dt)} rows, {length(unique(out_dt$ref_date))} snapshot dates.")
