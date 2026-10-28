# What did the frozen PSR cost the 2026 match predictions? (torp#289)
#
# The released stat ratings took in no games after 2026 round 6 until they
# were rebuilt on 2026-10-28, and the match model reads them
# (R/match_model.R .build_rated_match_features()). This runs the honest
# round-by-round backfill (honest_backfill_predictions.R) for 2026 twice,
# differing ONLY in the stat ratings it reads:
#   ARM=fixed   the current release (rebuilt, v17)
#   ARM=frozen  the files as published before the rebuild, saved to
#               torpdata/data/raw/baseline_v16_2026-10-28/
# Everything else (models, training cutoffs, calibration, lineups) is the
# backfill's own, so the gap between arms is the PSR freeze alone.
#
# Chunked so it can run a few rounds at a time on a short-memory machine;
# each round is written as it finishes and skipped on a re-run.
# Usage (from the torp root, via PowerShell for arrow):
#   $env:ARM='frozen'; $env:ROUNDS='7:9'; Rscript data-raw/04-analysis/frozen_psr_cost.R
# Output: data-raw/outputs/frozen-psr-cost/<arm>/_profile/2026/round_<rr>.parquet

arm <- Sys.getenv("ARM")
rounds <- Sys.getenv("ROUNDS")
stopifnot(arm %in% c("fixed", "frozen"), nzchar(rounds))

source("data-raw/04-analysis/honest_backfill_predictions.R")  # defines hb_*, does not run

if (identical(arm, "frozen")) {
  frozen_dir <- "C:/dev/torpverse/torpdata/data/raw/baseline_v16_2026-10-28"
  frozen_loader <- function(seasons = get_afl_season(), use_disk_cache = FALSE, columns = NULL) {
    seasons <- validate_seasons(seasons)
    files <- file.path(frozen_dir, sprintf("player_stat_ratings_%d.parquet", seasons))
    stopifnot(all(file.exists(files)))
    out <- data.table::rbindlist(lapply(files, function(f) {
      d <- arrow::read_parquet(f)
      if (!is.null(columns)) d <- d[, intersect(columns, names(d)), drop = FALSE]
      d
    }), fill = TRUE)
    message("frozen arm: read ", length(files), " stat-rating files from ", frozen_dir)
    out
  }
  ns <- asNamespace("torp")
  unlockBinding("load_player_stat_ratings", ns)
  assign("load_player_stat_ratings", frozen_loader, envir = ns)
  lockBinding("load_player_stat_ratings", ns)
  # Anchor, read through the namespace (what the backfill's own calls resolve
  # to; a bare call from here would hit the attached copy): the frozen 2026
  # file stops growing after round 7.
  chk <- get("load_player_stat_ratings", envir = ns)(2026)
  g <- tapply(chk$n_games, chk$round, sum, na.rm = TRUE)
  stopifnot(g[["12"]] == g[["24"]])
} else {
  chk <- get("load_player_stat_ratings", envir = asNamespace("torp"))(2026)
  g <- tapply(chk$n_games, chk$round, sum, na.rm = TRUE)
  stopifnot(g[["24"]] > g[["12"]])   # anchor: the fixed file keeps growing
}
message(sprintf("arm %s: 2026 n_games at rounds 12 / 24 = %s / %s", arm, g[["12"]], g[["24"]]))

options(torp.local_data_dir = NA)
hb_run(hb_parse_args(c("--seasons=2026", paste0("--rounds=", rounds), "--accept-psr-coef-leak",
                       paste0("--out=data-raw/outputs/frozen-psr-cost/", arm))))
