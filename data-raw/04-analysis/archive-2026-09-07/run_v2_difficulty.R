# The difficulty split and the calibration, applied to v2 production.
#
# WHY THIS IS THE INTERESTING ARM. v3 costs +0.367 dMAE against production and
# the reason is now understood: v2's `epv_disp` carries goals, behinds, shots
# and metres gained, v3 dropped them for a chain-native description, and a
# rating built from scoring stats predicts future scoring by construction.
#
# But nothing about the difficulty split requires that trade. It changes how the
# CHAIN credit is divided between disposer and receiver; it does not touch the
# box terms. So v2 + difficulty is v2's predictive box signal PLUS v3's better
# chain attribution -- potentially both, rather than either.
#
# The calibration is engine-agnostic too: it scales each channel by its own
# margin coefficient and needs nothing from v3.
#
# THE DOUBLE-COUNT THAT HAD TO BE FIXED FIRST. Under v3 the difficulty split
# excludes aerial contests because they have their own surprise term. Under v2
# the equivalent is `contest_epv`, a 3-way split on contested kicks added into
# epv_recv -- and v2 additionally cuts .disp_scale to 1/3 on those rows for the
# same reason. Without excluding them the split pays that swing twice. The
# exclusion did not exist until now because the split had only ever run under
# v3, and it would have inflated every number below without erroring.
#
# THREE ARMS:
#   1  v2 production           exactly what is live
#   2  v2 + difficulty         the split, box terms untouched
#   3  v2 + difficulty + cal   and the raw-layer channel calibration
#
# ~12 min: one player-game build plus two rating builds. The benchmark suite and
# the EPR gate then run in seconds.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")
source("C:/dev/torpverse/torp/data-raw/04-analysis/cache_guard.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "v2_difficulty_run.txt"), split = TRUE)
cat("=== difficulty split + calibration on v2 ===\nrun at", format(Sys.time()), "\n")
cat("code fingerprint:", code_fingerprint(), "\n")

set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")
with_const <- function(l, expr) {
  old <- lapply(names(l), function(nm) get(nm, envir = asNamespace("torp")))
  names(old) <- names(l); set_const(l); on.exit(set_const(old), add = TRUE); force(expr)
}

pbp <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams <- load_teams(TRUE); chains <- load_chains(TRUE)
res <- as.data.table(load_results(TRUE))
shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)

V2_CONST <- list(EPV_ENGINE = "v2", EPV3_CHANNELS = 3L,
                 EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
                 EPV3_STOP_ZERO_SUM = FALSE,
                 EPV_STANDARDISE_CHANNELS = c("recv", "disp", "spoil"),
                 EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
                 EPR_PRIOR_RATE_RECV = -0.7 * 0.919, EPR_PRIOR_RATE_DISP = -0.7 * 0.919,
                 EPR_PRIOR_RATE_SPOIL = -0.3 * 0.919, EPR_PRIOR_RATE_HITOUT = -0.3 * 0.919,
                 EPR_PRIOR_GAMES_RECV = 3, EPR_PRIOR_GAMES_DISP = 3,
                 EPR_PRIOR_GAMES_SPOIL = 3, EPR_PRIOR_GAMES_HITOUT = 3)

pgd_v2 <- cached_frame("v2v3_pgd_v2", function() {
  with_const(V2_CONST,
    as.data.table(create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v2")))
}, on_stale = "rebuild")
pgd_v2d <- cached_frame("v2diff_pgd", function() {
  with_const(c(V2_CONST, list(EPV_DIFFICULTY_SPLIT = TRUE)),
    as.data.table(create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v2")))
}, on_stale = "rebuild")
pgd_v2 <- as.data.table(pgd_v2); pgd_v2d <- as.data.table(pgd_v2d)

# Guard: the arms must differ, and they must differ in the CHAIN channels only.
# The box terms are untouched by the split, so a change in `goals` or `kicks`
# would mean something leaked.
cat(sprintf("\narms differ: mean|d epv_disp| %.4f | mean|d epv_recv| %.4f\n",
            mean(abs(pgd_v2$epv_disp - pgd_v2d$epv_disp), na.rm = TRUE),
            mean(abs(pgd_v2$epv_recv - pgd_v2d$epv_recv), na.rm = TRUE)))
if (mean(abs(pgd_v2$epv_disp - pgd_v2d$epv_disp), na.rm = TRUE) < 1e-9)
  stop("arms identical -- the difficulty flag did not take under v2")
for (bc in intersect(c("goals", "kicks", "disposals", "marks"), names(pgd_v2))) {
  d <- mean(abs(pgd_v2[[bc]] - pgd_v2d[[bc]]), na.rm = TRUE)
  if (d > 1e-9) cat(sprintf("  !! box column %s changed by %.4f -- it should not have\n", bc, d))
}

cat("\n########## BENCHMARK SUITE ##########\n")
a  <- benchmark_rating(pgd_v2,  "v2",            results = res)
b  <- benchmark_rating(pgd_v2d, "v2+difficulty", results = res)
ac <- benchmark_rating(pgd_v2,  "v2",            results = res, calibrate = TRUE)
bc <- benchmark_rating(pgd_v2d, "v2+difficulty", results = res, calibrate = TRUE)
print(a); print(b); print(ac); print(bc)
cat("\n--- uncalibrated ---\n");            compare_benchmarks(a, b)
cat("\n--- calibrated (what would ship) ---\n"); compare_benchmarks(ac, bc)
cat("\n--- what calibration alone does to v2 ---\n"); compare_benchmarks(a, ac)

cat("\n########## EPR GATE ##########\n")
build_ratings <- function(pgd, tag) {
  f <- file.path(OUT_DIR, paste0("v2diff_rt_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing ratings {tag}")
    return(as.data.table(read_parquet(f))) }
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", "v2")
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) out <- calculate_torp(out, psr_df)
  out <- as.data.table(out); write_parquet(out, f); out
}
rt_v2  <- with_const(V2_CONST, build_ratings(pgd_v2,  "v2"))
rt_v2d <- with_const(V2_CONST, build_ratings(pgd_v2d, "v2diff"))
g1 <- bm_epr_gate(pgd_v2,  rt_v2,  res, "v2")
g2 <- bm_epr_gate(pgd_v2d, rt_v2d, res, "v2+difficulty")
print(g1); print(g2); compare_epr_gates(g1, g2)

cat("\n########## HOW TO READ IT ##########\n")
cat("If v2+difficulty holds the EPR gate's OOS MAE while improving conservation\n")
cat("and separation, it is the best of both -- v2's box signal with a better\n")
cat("chain attribution -- and it needs no engine flip at all.\n")
cat("If OOS MAE degrades the way v3's did, the split itself costs prediction and\n")
cat("the chain-native rebuild was not what was costing it.\n")

saveRDS(list(bench = list(a, b, ac, bc), gate = list(g1, g2)),
        file.path(OUT_DIR, "v2_difficulty_run.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
