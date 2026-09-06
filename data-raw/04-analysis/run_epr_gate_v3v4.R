#!/usr/bin/env Rscript
# Fast EPR gate: v3 (production) against v4 (Net Points), plus face validity
# ==============================================================================
# The decision gate for the engine switch (docs/HOW-WE-WORK.md section 2 and
# the rule there: a change worth making loses on nothing and wins on more than
# one). Both arms are built by THIS code on THIS data, one season at a time,
# because the whole-history load killed Rscript on 2026-08-18. v4's difficulty
# models are fitted leak-safe: each season on the one before (2021 on 2022).
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/run_epr_gate_v3v4.R"'
#
# Takes about an hour. Frames and rating tables are cached in data-raw/outputs
# as v3v4_pgd_{v3,v4}.parquet / v3v4_rt_{v3,v4}.parquet; delete to rebuild.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
SEASONS <- 2021:get_afl_season()
sink(file.path(OUT_DIR, "epr_gate_v3v4.txt"), split = TRUE)
cat("=== fast EPR gate: v3 production vs v4 Net Points ===\nrun at", format(Sys.time()), "\n")
t0 <- Sys.time()
set_const <- function(...) { vals <- list(...); for (nm in names(vals)) assignInNamespace(nm, vals[[nm]], ns = "torp") }

build_pgd <- function(engine) {
  f <- file.path(OUT_DIR, paste0("v3v4_pgd_", engine, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing cached {engine} frame"); d <- as.data.table(read_parquet(f)); setattr(d, "epv_engine", engine); return(d) }
  parts <- lapply(SEASONS, function(s) {
    cli::cli_h2("{engine}: season {s}")
    pbp <- load_pbp(s, rounds = TRUE); chains <- load_chains(s, rounds = TRUE)
    pstats <- load_player_stats(s); teams <- load_teams(s)
    terms <- if (identical(engine, "v4")) np_difficulty_terms_for_season(s, pbp, chains, available = SEASONS) else NULL
    d <- as.data.table(create_player_game_data(pbp, pstats, teams, chains = chains,
                                               epv_engine = engine, difficulty_terms = terms))
    rm(pbp, chains, pstats, teams, terms); invisible(gc(verbose = FALSE))
    d
  })
  d <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  setattr(d, "epv_engine", engine)
  write_parquet(d, f)
  d
}

shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)), error = function(e) NULL)
res <- as.data.table(load_results(TRUE))

build_ratings <- function(pgd, engine) {
  f <- file.path(OUT_DIR, paste0("v3v4_rt_", engine, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing cached {engine} ratings"); return(as.data.table(read_parquet(f))) }
  # the EPR constants read EPV_ENGINE: v4 is in margin points, so its prior
  # rates carry no per-channel scale
  set_const(EPV_ENGINE = engine)
  if (identical(engine, "v4")) {
    set_const(EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7, EPR_PRIOR_RATE_SPOIL = -0.3)
  }
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", engine)
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) out <- calculate_torp(out, psr_df)
  out <- as.data.table(out)
  write_parquet(out, f)
  out
}

pgd3 <- build_pgd("v3"); cat("v3 frame:", nrow(pgd3), "rows at", format(Sys.time()), "\n")
pgd4 <- build_pgd("v4"); cat("v4 frame:", nrow(pgd4), "rows at", format(Sys.time()), "\n")
rt3 <- build_ratings(pgd3, "v3"); cat("v3 ratings:", nrow(rt3), "rows\n")
rt4 <- build_ratings(pgd4, "v4"); cat("v4 ratings:", nrow(rt4), "rows\n")
set_const(EPV_ENGINE = "v3")

cat("\n==== FACE VALIDITY (top 40, stability, appears-from-nowhere) ====\n")
# face_validity() wants ONE row per player: the latest rated round of each
# engine, not the round-by-round table (that merge is cartesian)
snap <- function(rt) {
  rt <- as.data.table(rt)[!is.na(epr)]
  last <- rt[season == max(season), max(round)]
  rt[season == max(season) & round == last]
}
fv <- tryCatch(face_validity(snap(rt3), snap(rt4)),
               error = function(e) { cat("face_validity failed:", conditionMessage(e), "\n"); NULL })
if (!is.null(fv)) print(fv)

cat("\n==== FAST EPR GATE (lineup rating -> team points, club and season fixed effects) ====\n")
g3 <- bm_epr_gate(pgd3, rt3, res, "v3 production")
g4 <- bm_epr_gate(pgd4, rt4, res, "v4 net points")
print(g3); print(g4); compare_epr_gates(g3, g4)

cat("\n==== BENCHMARK SUITE (calibrated) ====\n")
b3 <- tryCatch(benchmark_rating(pgd3, "v3", results = res), error = function(e) { cat("suite v3 failed:", conditionMessage(e), "\n"); NULL })
b4 <- tryCatch(benchmark_rating(pgd4, "v4", results = res), error = function(e) { cat("suite v4 failed:", conditionMessage(e), "\n"); NULL })
if (!is.null(b3) && !is.null(b4)) { print(b3); print(b4); compare_benchmarks(b3, b4) }

saveRDS(list(g3 = g3, g4 = g4, b3 = b3, b4 = b4), file.path(OUT_DIR, "epr_gate_v3v4.rds"))
cat("\ndone in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "min at", format(Sys.time()), "\n")
sink()
