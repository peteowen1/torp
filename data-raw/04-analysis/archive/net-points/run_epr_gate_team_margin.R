#!/usr/bin/env Rscript
# Fast EPR gate: v4 as shipped against the team-sums-to-its-own-margin convention
# ==============================================================================
# The convention is ESPN Net Points': each team's players sum to that team's own
# margin, +63 and -63, rather than only the difference between the teams being
# pinned. Pete asked for it twice and was right that it is what the metric we are
# named after does; the scoping note is docs/plans/NET-POINTS-TEAM-SUM-CONVENTION.md.
#
# It has been judged on repeatability (worse: 0.591 shipped against 0.567) and on
# team dependence (better: 21% against 11%, and 14% with the mirror charge). It
# has never faced the gate that actually chose v4, which is what this runs.
#
# NOT a reconciliation tweak. Forcing team totals through .np_reconcile() needs a
# residual larger than the value it corrects -- that is the measured failure of
# level = "half_margin" -- so every row is RE-ALLOCATED instead: credit to the
# side that gained it, blame to the side that conceded it, keeping the shares the
# ledger already computes. Where a side has no named recipient on a row, which is
# most rows because a retained disposal names nobody on defence, that side's
# amount goes to a team pool spread by time on ground.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/run_epr_gate_team_margin.R"'
#
# About an hour: the shipped arm is cached, the convention arm rebuilds the
# ledger with payments season by season.
suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
SEASONS <- 2021:get_afl_season()
MIRROR_SHARE <- as.numeric(Sys.getenv("TM_MIRROR_SHARE", "0"))
# The configuration that beat the shipped ledger on repeatability (0.606 against
# 0.591): a flat split per row between the named player and his side, with the
# side pool spread by defensive acts rather than minutes. Pete's design.
NAMED_SHARE <- as.numeric(Sys.getenv("TM_NAMED_SHARE", "0.5"))
POOL_BY     <- Sys.getenv("TM_POOL_BY", "dacts")
sink(file.path(OUT_DIR, "epr_gate_team_margin.txt"), split = TRUE)
cat("=== fast EPR gate: v4 shipped vs team-sums-to-its-own-margin ===\n")
cat("run at", format(Sys.time()), "| mirror share", MIRROR_SHARE, "\n")
t0 <- Sys.time()
set_const <- function(...) {
  vals <- list(...); for (nm in names(vals)) assignInNamespace(nm, vals[[nm]], ns = "torp")
}

# The transform now lives in the engine (.np_team_margin), behind
# NP_TEAM_MARGIN_CONVENTION, with an invariant that aborts if named + pool ever
# stops equalling what a side was charged. The gate used to carry its own copy;
# that copy is what produced a arm identical to the shipped one, because it
# patched a column the rating layer never reads.
#
# Arm: doubling each side and keeping the ledger's own shares (named share NA),
# pool by defensive acts. Six seasons, five pairs: repeatability 0.5716 against
# the shipped 0.591, team dependence 23% against 11%, and the tightest position
# spread of anything tested at 2.12. It is the only convention variant with no
# known mechanical defect -- the half-split repeats better (0.6107) but puts
# rucks at -3.49 through a naming artefact.

build_pgd <- function(tag) {
  f <- file.path(OUT_DIR, paste0("tm_pgd_", tag, ".parquet"))
  if (tag == "v4" && file.exists(file.path(OUT_DIR, "v3v4_pgd_v4.parquet"))) {
    cli::cli_alert_info("Reusing the shipped v4 frame")
    d <- as.data.table(read_parquet(file.path(OUT_DIR, "v3v4_pgd_v4.parquet")))
    setattr(d, "epv_engine", "v4"); return(d)
  }
  if (file.exists(f)) {
    cli::cli_alert_info("Reusing cached {tag} frame")
    d <- as.data.table(read_parquet(f)); setattr(d, "epv_engine", "v4"); return(d)
  }
  assignInNamespace("NP_TEAM_MARGIN_CONVENTION", TRUE, ns = "torp")
  assignInNamespace("NP_TEAM_MARGIN_NAMED_SHARE", NA_real_, ns = "torp")
  assignInNamespace("NP_TEAM_MARGIN_POOL_BY", "dacts", ns = "torp")
  on.exit(assignInNamespace("NP_TEAM_MARGIN_CONVENTION", FALSE, ns = "torp"), add = TRUE)
  parts <- lapply(SEASONS, function(s) {
    cli::cli_h2("convention: season {s}")
    pbp <- load_pbp(s, rounds = TRUE); chains <- load_chains(s, rounds = TRUE)
    pstats <- load_player_stats(s); teams <- load_teams(s)
    terms <- np_difficulty_terms_for_season(s, pbp, chains, available = SEASONS)
    d <- as.data.table(create_player_game_data(pbp, pstats, teams, chains = chains,
                                               epv_engine = "v4", difficulty_terms = terms))
    rm(pbp, chains, pstats, teams, terms); invisible(gc(verbose = FALSE))
    d
  })
  d <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  setattr(d, "epv_engine", "v4")
  write_parquet(d, f)
  d
}

shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res_all <- as.data.table(load_results(TRUE))

build_ratings <- function(pgd, tag) {
  cached <- if (tag == "v4") file.path(OUT_DIR, "v3v4_rt_v4.parquet") else
                             file.path(OUT_DIR, paste0("tm_rt_", tag, ".parquet"))
  if (file.exists(cached)) {
    cli::cli_alert_info("Reusing cached {tag} ratings")
    return(as.data.table(read_parquet(cached)))
  }
  set_const(EPV_ENGINE = "v4", EPR_PRIOR_RATE_RECV = -0.7,
            EPR_PRIOR_RATE_DISP = -0.7, EPR_PRIOR_RATE_SPOIL = -0.3)
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", "v4")
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) {
    out <- calculate_torp(out, psr_df)
  }
  out <- as.data.table(out); write_parquet(out, cached); out
}

pgd_a <- build_pgd("v4");  cat("shipped frame:", nrow(pgd_a), "rows\n")
pgd_b <- build_pgd("tm");  cat("convention frame:", nrow(pgd_b), "rows\n")

cmp <- merge(pgd_a[, .(match_id = as.character(match_id), player_id = as.character(player_id), a = epv)],
             pgd_b[, .(match_id = as.character(match_id), player_id = as.character(player_id), b = epv)],
             by = c("match_id", "player_id"))
cat(sprintf("ARMS GUARD: %d shared player-games, mean |diff| %.4f, identical %.1f%%\n",
            nrow(cmp), mean(abs(cmp$a - cmp$b)), 100 * mean(abs(cmp$a - cmp$b) < 1e-9)))
stopifnot(mean(abs(cmp$a - cmp$b)) > 1e-6)

rt_a <- build_ratings(pgd_a, "v4"); rt_b <- build_ratings(pgd_b, "tm")

cat("\n==== FACE VALIDITY ====\n")
snap <- function(rt) {
  rt <- as.data.table(rt)[!is.na(epr)]
  last <- rt[season == max(season), max(round)]
  rt[season == max(season) & round == last]
}
fv <- tryCatch(face_validity(snap(rt_a), snap(rt_b)),
               error = function(e) { cat("face_validity failed:", conditionMessage(e), "\n"); NULL })
if (!is.null(fv)) print(fv)

cat("\n==== FAST EPR GATE (lineup rating -> team points, club and season FE) ====\n")
ga <- bm_epr_gate(pgd_a, rt_a, res_all, "v4 shipped")
gb <- bm_epr_gate(pgd_b, rt_b, res_all, "team sums to its own margin")
print(ga); print(gb); compare_epr_gates(ga, gb)

cat("\n==== BENCHMARK SUITE (calibrated) ====\n")
ba <- tryCatch(benchmark_rating(pgd_a, "v4", results = res_all), error = function(e) NULL)
bb <- tryCatch(benchmark_rating(pgd_b, "v4", results = res_all), error = function(e) NULL)
if (!is.null(ba) && !is.null(bb)) { print(ba); print(bb); compare_benchmarks(ba, bb) }

saveRDS(list(ga = ga, gb = gb, ba = ba, bb = bb),
        file.path(OUT_DIR, "epr_gate_team_margin.rds"))
cat("\ndone in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "min\n")
sink()
