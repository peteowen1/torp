#!/usr/bin/env Rscript
# Fast EPR gate: v4 as shipped against Free Against blame + chains tackle credit
# ==============================================================================
# Plan, rules and the judging order: torpverse docs/plans/NP-FREE-AGAINST-AND-TACKLES.md.
# The arm turns on NP_FREE_AGAINST_BLAME = TRUE, NP_TACKLE_POOL_SHARE = 0.5 and
# NP_TACKLE_STOPPAGE_SHARE = 0.5; everything else is production.
#
# Judged on leak-free prediction first (Pete, 2026-11-25): each season's ratings
# are built only from earlier matches and scored out of sample; both arms share
# each season's leak-safe difficulty terms, so only the flags differ.
# Repeatability (face validity, suite) is reported beside it, not as the decider.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/run_epr_gate_free_against_tackles.R"'
#
# Cached per season in data-raw/outputs as fat_pgd_{off,on}_<s>.parquet; a
# killed run resumes from the last finished season.
suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torpmodels", quiet = TRUE)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
SEASONS <- 2021:get_afl_season()
sink(file.path(OUT_DIR, "epr_gate_free_against_tackles.txt"), split = TRUE)
cat("=== fast EPR gate: v4 shipped vs Free Against + chains tackles ===\n")
cat("run at", format(Sys.time()), "\n")
t0 <- Sys.time()
set_arm <- function(on) {
  assignInNamespace("NP_FREE_AGAINST_BLAME", on, ns = "torp")
  assignInNamespace("NP_TACKLE_POOL_SHARE", if (on) 0.5 else 0, ns = "torp")
  assignInNamespace("NP_TACKLE_STOPPAGE_SHARE", if (on) 0.5 else 0, ns = "torp")
  stopifnot(identical(torp:::NP_FREE_AGAINST_BLAME, on))
}
# Historical gate (2026-11-25), run when these flags defaulted OFF. From v18
# they default ON (and NP_NAMED_LOSER_SHARE = 1), so the old "production is
# off" precondition no longer holds; set_arm() sets both arms explicitly, but
# a re-run's baseline also carries named-loser blame, unlike the recorded run.
stopifnot(identical(torp:::EPV_ENGINE, "v4"))

for (s in SEASONS) {
  f_off <- file.path(OUT_DIR, sprintf("fat_pgd_off_%d.parquet", s))
  f_on  <- file.path(OUT_DIR, sprintf("fat_pgd_on_%d.parquet", s))
  if (file.exists(f_off) && file.exists(f_on)) { cli::cli_alert_info("season {s}: cached"); next }
  cli::cli_h2("season {s}")
  pbp <- load_pbp(s, rounds = TRUE); chains <- load_chains(s, rounds = TRUE)
  pstats <- load_player_stats(s); teams <- load_teams(s)
  terms <- np_difficulty_terms_for_season(s, pbp, chains, available = SEASONS)
  set_arm(FALSE)
  d0 <- as.data.table(create_player_game_data(pbp, pstats, teams, chains = chains,
                                              epv_engine = "v4", difficulty_terms = terms))
  write_parquet(d0, f_off)
  set_arm(TRUE)
  d1 <- as.data.table(create_player_game_data(pbp, pstats, teams, chains = chains,
                                              epv_engine = "v4", difficulty_terms = terms))
  set_arm(FALSE)
  write_parquet(d1, f_on)
  cat(sprintf("season %d: %d / %d rows; %.1f min so far\n", s, nrow(d0), nrow(d1),
              as.numeric(Sys.time() - t0, units = "mins")))
  rm(pbp, chains, pstats, teams, terms, d0, d1); invisible(gc(verbose = FALSE))
}
read_arm <- function(tag) {
  d <- rbindlist(lapply(SEASONS, function(s)
    as.data.table(read_parquet(file.path(OUT_DIR, sprintf("fat_pgd_%s_%d.parquet", tag, s))))),
    use.names = TRUE, fill = TRUE)
  setattr(d, "epv_engine", "v4"); d
}
pgd_a <- read_arm("off");  cat("shipped frame:", nrow(pgd_a), "rows\n")
pgd_b <- read_arm("on");   cat("arm frame:    ", nrow(pgd_b), "rows\n")

# The arms must differ (the flag reached the build) and only in the pool channel.
cmp <- merge(pgd_a[, .(match_id, player_id, a = epv, a_own = epv_disp, a_won = epv_recv)],
             pgd_b[, .(match_id, player_id, b = epv, b_own = epv_disp, b_won = epv_recv)],
             by = c("match_id", "player_id"))
cat(sprintf("ARMS GUARD: %d shared player-games, mean |diff| epv %.4f; own %.2e; won %.2e\n",
            nrow(cmp), mean(abs(cmp$a - cmp$b)), mean(abs(cmp$a_own - cmp$b_own)),
            mean(abs(cmp$a_won - cmp$b_won))))
stopifnot(nrow(cmp) == nrow(pgd_a), mean(abs(cmp$a - cmp$b)) > 1e-6)

shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res <- as.data.table(load_results(TRUE))

build_ratings <- function(pgd, tag) {
  cached <- file.path(OUT_DIR, paste0("fat_rt_", tag, ".parquet"))
  if (file.exists(cached)) {
    cli::cli_alert_info("Reusing cached {tag} ratings"); return(as.data.table(read_parquet(cached)))
  }
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", "v4")
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else .afl_last_round(s)
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) out <- calculate_torp(out, psr_df)
  out <- as.data.table(out)
  write_parquet(out, cached)
  out
}
rt_a <- build_ratings(pgd_a, "off");  cat("shipped ratings:", nrow(rt_a), "rows\n")
rt_b <- build_ratings(pgd_b, "on");   cat("arm ratings:    ", nrow(rt_b), "rows\n")

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
gb <- bm_epr_gate(pgd_b, rt_b, res, "v4 + Free Against + tackles")
print(ga); print(gb); compare_epr_gates(ga, gb)

cat("\n==== BENCHMARK SUITE (calibrated) ====\n")
ba <- tryCatch(benchmark_rating(pgd_a, "v4", results = res),
               error = function(e) { cat("suite shipped failed:", conditionMessage(e), "\n"); NULL })
bb <- tryCatch(benchmark_rating(pgd_b, "v4", results = res),
               error = function(e) { cat("suite arm failed:", conditionMessage(e), "\n"); NULL })
if (!is.null(ba) && !is.null(bb)) { print(ba); print(bb); compare_benchmarks(ba, bb) }

saveRDS(list(ga = ga, gb = gb, ba = ba, bb = bb, fv = fv),
        file.path(OUT_DIR, "epr_gate_free_against_tackles.rds"))
cat("\ndone in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "min at",
    format(Sys.time()), "\n")
sink()
