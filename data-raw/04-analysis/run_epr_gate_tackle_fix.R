#!/usr/bin/env Rscript
# Fast EPR gate: v4 as shipped against v4 with NP_TURNOVER_ON_ALL_ACTS
# ==============================================================================
# The defect being fixed: the turnover split fired only on kicks and handballs,
# so any other act that lost the ball paid the whole swing as blame on the actor
# and credited the opponent who won it nothing. 15.9 rows a match, 27.1 points of
# swing, none of it credited, and per event they are worth 1.72 against 0.54 for
# the disposal turnovers that do pay -- they are the tackled-in-possession cases.
#
# Same bar as the gate that chose v4 (docs/HOW-WE-WORK.md section 2): a change
# worth making loses on nothing and wins on more than one.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/run_epr_gate_tackle_fix.R"'
#
# The shipped arm reuses the cached v3v4_pgd_v4 / v3v4_rt_v4 files; only the
# fixed arm is built, one season at a time, difficulty models fitted leak-safe.
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
sink(file.path(OUT_DIR, "epr_gate_tackle_fix.txt"), split = TRUE)
cat("=== fast EPR gate: v4 shipped vs v4 + credit the ball winner ===\n")
cat("run at", format(Sys.time()), "\n")
t0 <- Sys.time()
set_const <- function(...) {
  vals <- list(...)
  for (nm in names(vals)) assignInNamespace(nm, vals[[nm]], ns = "torp")
}

build_pgd <- function(tag, turnover_all) {
  f <- file.path(OUT_DIR, paste0("tf_pgd_", tag, ".parquet"))
  if (tag == "v4" && file.exists(file.path(OUT_DIR, "v3v4_pgd_v4.parquet"))) {
    cli::cli_alert_info("Reusing the shipped v4 frame")
    d <- as.data.table(read_parquet(file.path(OUT_DIR, "v3v4_pgd_v4.parquet")))
    setattr(d, "epv_engine", "v4"); return(d)
  }
  if (file.exists(f)) {
    cli::cli_alert_info("Reusing cached {tag} frame")
    d <- as.data.table(read_parquet(f)); setattr(d, "epv_engine", "v4"); return(d)
  }
  set_const(NP_TURNOVER_ON_ALL_ACTS = turnover_all)
  stopifnot(identical(torp:::NP_TURNOVER_ON_ALL_ACTS, turnover_all))
  parts <- lapply(SEASONS, function(s) {
    cli::cli_h2("{tag}: season {s}")
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
res <- as.data.table(load_results(TRUE))

build_ratings <- function(pgd, tag) {
  cached <- if (tag == "v4") file.path(OUT_DIR, "v3v4_rt_v4.parquet") else
                             file.path(OUT_DIR, paste0("tf_rt_", tag, ".parquet"))
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
  out <- as.data.table(out)
  write_parquet(out, cached)
  out
}

pgd_a <- build_pgd("v4", FALSE)
cat("shipped frame:", nrow(pgd_a), "rows at", format(Sys.time()), "\n")
pgd_b <- build_pgd("fix", TRUE)
cat("fixed frame:  ", nrow(pgd_b), "rows at", format(Sys.time()), "\n")

# the arms must not be the same frame: this fix moves value, so if the two
# agree everywhere the flag never reached the build
cmp <- merge(pgd_a[, .(match_id, player_id, a = epv)],
             pgd_b[, .(match_id, player_id, b = epv)], by = c("match_id", "player_id"))
cat(sprintf("ARMS GUARD: %d shared player-games, mean |diff| %.4f, identical rows %.1f%%\n",
            nrow(cmp), mean(abs(cmp$a - cmp$b)),
            100 * mean(abs(cmp$a - cmp$b) < 1e-9)))
stopifnot(mean(abs(cmp$a - cmp$b)) > 1e-6)

rt_a <- build_ratings(pgd_a, "v4");  cat("shipped ratings:", nrow(rt_a), "rows\n")
rt_b <- build_ratings(pgd_b, "fix"); cat("fixed ratings:  ", nrow(rt_b), "rows\n")

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
ga <- bm_epr_gate(pgd_a, rt_a, res, "v4 shipped")
gb <- bm_epr_gate(pgd_b, rt_b, res, "v4 + ball winner credited")
print(ga); print(gb); compare_epr_gates(ga, gb)

cat("\n==== BENCHMARK SUITE (calibrated) ====\n")
ba <- tryCatch(benchmark_rating(pgd_a, "v4", results = res),
               error = function(e) { cat("suite shipped failed:", conditionMessage(e), "\n"); NULL })
bb <- tryCatch(benchmark_rating(pgd_b, "v4", results = res),
               error = function(e) { cat("suite fixed failed:", conditionMessage(e), "\n"); NULL })
if (!is.null(ba) && !is.null(bb)) { print(ba); print(bb); compare_benchmarks(ba, bb) }

saveRDS(list(ga = ga, gb = gb, ba = ba, bb = bb),
        file.path(OUT_DIR, "epr_gate_tackle_fix.rds"))
cat("\ndone in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "min at",
    format(Sys.time()), "\n")
sink()
