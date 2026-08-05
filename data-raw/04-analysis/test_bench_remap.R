# Does remapping the bench slot put Gawn back above Darcy?
#
# Pass/fail is stated before the run, so it cannot be reinterpreted after:
#
#   PASS  Gawn's epv_hitout_adj exceeds Darcy's, because Gawn's raw hitout value
#         is nearly double (4.981 against 2.585) and nothing about that changed.
#   FAIL  Darcy still leads, meaning INT was not the cause and the diagnosis in
#         docs/reviews/INT-CENTRING-BUG-2026-08-06.md is wrong.
#
# Also checks what it does to everyone else, because a fix that corrects one
# case and breaks the leaderboard is not a fix.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "bench_remap_test.txt"), split = TRUE)
cat("=== Bench-role remap: does Gawn come back? ===\nrun at", format(Sys.time()), "\n")

set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")
with_const <- function(l, expr) {
  old <- lapply(names(l), function(nm) get(nm, envir = asNamespace("torp")))
  names(old) <- names(l); set_const(l); on.exit(set_const(old), add = TRUE); force(expr)
}
V2 <- list(EPV_ENGINE = "v2", EPV3_STOP_ZERO_SUM = FALSE,
           EPV_STANDARDISE_CHANNELS = c("recv", "disp", "spoil"),
           EPV_DIFFICULTY_SPLIT = FALSE, EPV_PER_CHANNEL_POINTS_SCALE = FALSE,
           EPV_POINTS_SCALE = 0.919)

pbp <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams <- load_teams(TRUE); chains <- load_chains(TRUE); res <- load_results(TRUE)

before <- as.data.table(read_parquet(file.path(OUT_DIR, "v2v3_pgd_v2.parquet")))
f <- file.path(OUT_DIR, "v2_benchremap_pgd.parquet")
after <- if (file.exists(f)) {
  cli::cli_alert_info("Reusing remapped frame"); as.data.table(read_parquet(f))
} else {
  d <- with_const(c(V2, list(ROLE_REMAP_BENCH = TRUE)),
    as.data.table(create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v2")))
  write_parquet(d, f); d
}

S <- max(before$season, na.rm = TRUE)
WHO <- c("Sean Darcy", "Max Gawn")
cat("\n########## THE TEST ##########\n")
cmp <- merge(
  before[season == S & player_name %chin% WHO,
         .(before_adj = round(mean(epv_hitout_adj, na.rm = TRUE), 3),
           raw = round(mean(epv_hitout, na.rm = TRUE), 3)), by = player_name],
  after[season == S & player_name %chin% WHO,
        .(after_adj = round(mean(epv_hitout_adj, na.rm = TRUE), 3)), by = player_name],
  by = "player_name")
print(cmp)
gw <- cmp[player_name == "Max Gawn"]; sd_ <- cmp[player_name == "Sean Darcy"]
cat(sprintf("\n  raw hitout value:        Gawn %.3f vs Darcy %.3f  (Gawn %.1fx)\n",
            gw$raw, sd_$raw, gw$raw / sd_$raw))
cat(sprintf("  epv_hitout_adj BEFORE:   Gawn %.3f vs Darcy %.3f  -> %s\n",
            gw$before_adj, sd_$before_adj,
            ifelse(gw$before_adj > sd_$before_adj, "Gawn", "DARCY")))
cat(sprintf("  epv_hitout_adj AFTER:    Gawn %.3f vs Darcy %.3f  -> %s\n",
            gw$after_adj, sd_$after_adj,
            ifelse(gw$after_adj > sd_$after_adj, "Gawn", "DARCY")))
cat("\n  VERDICT: ", if (gw$after_adj > sd_$after_adj) "PASS" else "FAIL", "\n")

cat("\n########## WHAT IT DID TO EVERYONE ELSE ##########\n")
k <- c("player_name", "match_id")
ch <- c("epv_recv_adj", "epv_disp_adj", "epv_spoil_adj", "epv_hitout_adj")
m <- merge(before[, c(k, ch), with = FALSE], after[, c(k, ch), with = FALSE],
           by = k, suffixes = c("_b", "_a"))
cat("mean |change| per player-game:\n")
print(data.table(channel = ch,
                 mean_abs_change = round(vapply(ch, function(c)
                   mean(abs(m[[paste0(c, "_b")]] - m[[paste0(c, "_a")]]), na.rm = TRUE), 0), 4)))

cat("\ntop 15 rucks by hitout channel AFTER the remap:\n")
r <- after[season == S, .(gm = .N, adj = round(mean(epv_hitout_adj, na.rm = TRUE), 3),
                          raw = round(mean(epv_hitout, na.rm = TRUE), 2),
                          tog = round(mean(time_on_ground_percentage, na.rm = TRUE), 0)),
           by = player_name][gm >= 6]
setorder(r, -adj); print(r[1:15], nrows = 20)

cat("\n########## BENCHMARK PANEL, before vs after ##########\n")
a <- benchmark_rating(before, "before", results = res, calibrate = TRUE)
b <- benchmark_rating(after,  "after",  results = res, calibrate = TRUE)
compare_benchmarks(a, b)

saveRDS(list(cmp = cmp, rucks = r), file.path(OUT_DIR, "bench_remap_test.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
