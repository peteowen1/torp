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

HALFLIFE_DAYS <- 1095  # AFL-DECAY-XRAPM-PLAN.md Stage E: sweep was monotone
                       # to the grid boundary on both comps (longer always
                       # beat shorter, up to a 100000d/no-decay control) --
                       # not a clean interior optimum. 1095d (3yr) is a
                       # judgement call: within the "not worse than the
                       # boundary" region without degrading to no-decay,
                       # which would defeat the point of building this at
                       # all. Flagged for Pete's review, not a validated pick.

cli::cli_h1("Building {comp} as-of RAPM/SPM snapshots (halflife={HALFLIFE_DAYS}d)")

checkpoints <- .team_rapm_checkpoint_dates(comp = comp)
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
  t0 <- Sys.time()

  design <- tryCatch(
    build_team_rapm_asof(ref_date, comp = comp, halflife_days = HALFLIFE_DAYS),
    error = function(e) {
      cli::cli_warn("[{i}/{nrow(checkpoints)}] ref={as.character(ref_date)}: build failed -- {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(design)) { n_skipped <- n_skipped + 1L; next }

  fit <- fit_team_rapm_asof(design, nfolds = 10)
  rapm_ratings <- extract_team_rapm_ratings(design, fit)
  rapm_ratings <- rapm_ratings[rating_type == "individual"]

  spm_asof <- fit_team_spm_asof(ref_date, rapm_ratings, comp = comp)
  if (is.null(spm_asof)) { n_skipped <- n_skipped + 1L; next }

  out <- spm_asof[, .(
    player_id, ref_date = ref_date,
    rapm = rapm_offense - rapm_defense,
    rapm_offense, rapm_defense,
    spm_offense, spm_defense,
    total_minutes = n_games  # placeholder unit note below
  )]
  results[[i]] <- out
  n_ok <- n_ok + 1L
  elapsed <- round(as.numeric(Sys.time() - t0), 1)
  cli::cli_inform("[{i}/{nrow(checkpoints)}] ref={as.character(ref_date)} n_matches={design$n_train_matches} p={design$p} rows={nrow(out)} ({elapsed}s)")
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
