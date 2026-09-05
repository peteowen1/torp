## AFLW decay-grain snapshot build (parallel, §19 pattern).
## One checkpoint per (season, round): ref_date = that round's first match - 1 day.
## Each checkpoint is an independent refit -> safe to parallelise; the §13 disk
## cache is keyed on (comp, ref_date, halflife_days) so workers never collide.
suppressMessages({
  library(data.table); library(parallel)
  devtools::load_all(quiet = TRUE)
})

args <- commandArgs(trailingOnly = TRUE)
HL <- if (length(args) >= 1) as.numeric(args[1]) else 730
NW <- if (length(args) >= 2) as.integer(args[2]) else 10L
OUT <- Sys.getenv("AFLW_DECAY_OUT", unset = tempdir())
cache_file <- file.path(OUT, sprintf("aflw_snap_hl%s.rds", HL))
if (file.exists(cache_file)) { cat(sprintf("halflife=%s: already built.\n", HL)); quit(status = 0) }

res <- as.data.table(load_results(TRUE, comp = "AFLW"))
fx  <- as.data.table(load_fixtures(all = TRUE, comp = "AFLW"))
fx  <- fx[!is.na(utc_start_time), .(match_id, match_date = as.Date(utc_start_time))]
mo  <- merge(unique(res[, .(match_id, season, round_number)]), fx, by = "match_id")
rounds <- mo[, .(ref_date = min(match_date) - 1L), by = .(season, round_number)]
setorder(rounds, season, round_number)
cat(sprintf("halflife=%s: %d round-checkpoints, %d workers\n", HL, nrow(rounds), NW))

t0 <- Sys.time()
cl <- makeCluster(NW); on.exit(stopCluster(cl), add = TRUE)
clusterEvalQ(cl, suppressMessages({
  library(data.table)
  devtools::load_all(quiet = TRUE)
}))

fit_one <- function(i, rounds_dt, halflife) {
  s <- rounds_dt$season[i]; r <- rounds_dt$round_number[i]; rd <- rounds_dt$ref_date[i]
  rr <- tryCatch(fit_team_rapm_asof_cached(rd, comp = "AFLW", halflife_days = halflife),
                 error = function(e) NULL)
  if (is.null(rr)) return(NULL)
  spm <- tryCatch(fit_team_spm_asof_cached(rd, rr, comp = "AFLW", halflife_days = halflife),
                  error = function(e) NULL)
  if (is.null(spm)) return(NULL)
  spm[, `:=`(season = s, round_number = r,
             spm_pure_net = spm_offense - spm_defense,
             rapm_raw_net = rapm_offense - rapm_defense)]
  spm[, .(season, round_number, player_id, team_rapm_shrunk, rapm_raw_net, spm_pure_net, shrinkage_weight)]
}

out <- parLapply(cl, seq_len(nrow(rounds)), fit_one, rounds_dt = rounds, halflife = HL)
n_ok <- sum(!vapply(out, is.null, logical(1)))
cat(sprintf("halflife=%s: %.1f min -- %d/%d checkpoints produced ratings\n",
            HL, as.numeric(Sys.time() - t0, units = "mins"), n_ok, nrow(rounds)))
snap <- rbindlist(out, fill = TRUE)
snap[, player_id := as.character(player_id)]
saveRDS(snap, cache_file)
cat(sprintf("Saved %s (%d rows, mean shrinkage_weight=%.3f)\n",
            cache_file, nrow(snap), mean(snap$shrinkage_weight, na.rm = TRUE)))
