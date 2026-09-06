# Scheme C: centre the hitout channel on ruck involvement, not position.
#
# PASS/FAIL STATED BEFORE THE RUN.
#   PASS  Grundy > Gawn > Cox on epv_hitout_adj, matching raw output where
#         Grundy 5.554 > Gawn 4.981 > Cox 2.138. And the ruck leaderboard is
#         ordered roughly by raw production rather than by TOG.
#   FAIL  a part-timer still tops it, meaning the cell was not the whole story.
#
# Predicted from the standalone arithmetic: Grundy 2.668, Gawn 1.994, Cox 0.103.
# If the pipeline reproduces those, the implementation matches the design; if it
# does not, the wiring is wrong even should the ordering look right.
#
# Two arms: bench remap alone (already measured) against remap + scheme C.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "scheme_c_test.txt"), split = TRUE)
cat("=== Scheme C: hitout centred on ruck involvement ===\nrun at", format(Sys.time()), "\n")

set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")
with_const <- function(l, expr) {
  old <- lapply(names(l), function(nm) get(nm, envir = asNamespace("torp")))
  names(old) <- names(l); set_const(l); on.exit(set_const(old), add = TRUE); force(expr)
}
V2 <- list(EPV_ENGINE = "v2", EPV3_STOP_ZERO_SUM = FALSE,
           EPV_STANDARDISE_CHANNELS = c("recv", "disp", "spoil"),
           EPV_DIFFICULTY_SPLIT = FALSE, EPV_PER_CHANNEL_POINTS_SCALE = FALSE,
           EPV_POINTS_SCALE = 0.919, ROLE_REMAP_BENCH = TRUE)

pbp <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams <- load_teams(TRUE); chains <- load_chains(TRUE); res <- load_results(TRUE)

before <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_benchremap_pgd.parquet")))
f <- file.path(OUT_DIR, "v2_schemec_pgd.parquet")
after <- if (file.exists(f)) {
  cli::cli_alert_info("Reusing scheme C frame"); as.data.table(read_parquet(f))
} else {
  d <- with_const(c(V2, list(EPV_HITOUT_CENTRE_ON_RUCK = TRUE)),
    as.data.table(create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v2")))
  write_parquet(d, f); d
}
S <- max(before$season, na.rm = TRUE)

cat("\n########## THE TEST ##########\n")
WHO <- c("Brodie Grundy", "Max Gawn", "Mason Cox")
g <- function(x, lbl) x[season == S & player_name %chin% WHO,
  .(raw = round(mean(epv_hitout, na.rm = TRUE), 3),
    adj = round(mean(epv_hitout_adj, na.rm = TRUE), 3)), by = player_name][, arm := lbl][]
t <- merge(g(before, "b")[, .(player_name, raw, before = adj)],
           g(after, "a")[, .(player_name, after = adj)], by = "player_name")
setorder(t, -raw)
t[, predicted := c(2.668, 1.994, 0.103)[match(player_name, c("Brodie Grundy", "Max Gawn", "Mason Cox"))]]
print(t)
ok_order <- t$after[1] > t$after[2] && t$after[2] > t$after[3]
ok_match <- max(abs(t$after - t$predicted), na.rm = TRUE) < 0.25
cat(sprintf("\n  ordering Grundy > Gawn > Cox: %s\n", ifelse(ok_order, "PASS", "FAIL")))
cat(sprintf("  matches the standalone prediction (within 0.25): %s (worst %.3f)\n",
            ifelse(ok_match, "PASS", "FAIL"), max(abs(t$after - t$predicted), na.rm = TRUE)))

cat("\n########## THE RUCK LEADERBOARD ##########\n")
lb <- function(x, lbl) {
  r <- x[season == S, .(gm = .N, tog = round(mean(time_on_ground_percentage, na.rm = TRUE), 0),
                        raw = round(mean(epv_hitout, na.rm = TRUE), 2),
                        adj = round(mean(epv_hitout_adj, na.rm = TRUE), 3)),
         by = player_name][gm >= 6]
  setorder(r, -adj); cat("\n--", lbl, "--\n"); print(r[1:12], nrows = 14)
  r
}
rb <- lb(before, "bench remap only"); ra <- lb(after, "+ scheme C")
cat(sprintf("\ncor(adj, raw) among rucks: before %.3f | after %.3f  (higher = ranked by production)\n",
            cor(rb$adj, rb$raw), cor(ra$adj, ra$raw)))
cat(sprintf("cor(adj, TOG) among rucks: before %.3f | after %.3f  (nearer 0 = less a minutes artefact)\n",
            cor(rb$adj, rb$tog), cor(ra$adj, ra$tog)))

cat("\n########## THE PLAYERS SCHEME B WOULD HAVE MISSED ##########\n")
NON <- c("Rory Lobb", "Mark Blicavs", "Toby Murray", "Sam De Koning", "Jake Riccardi")
cmp <- merge(before[season == S & player_name %chin% NON,
                    .(rc = round(mean(ruck_contests, na.rm = TRUE), 1),
                      before = round(mean(epv_hitout_adj, na.rm = TRUE), 3)), by = player_name],
             after[season == S & player_name %chin% NON,
                   .(after = round(mean(epv_hitout_adj, na.rm = TRUE), 3)), by = player_name],
             by = "player_name")
setorder(cmp, -rc); print(cmp)
cat("\nThese are listed as defenders, forwards and midfielders but ruck 16-29\n")
cat("times a game. They should FALL: previously centred against non-rucks, now\n")
cat("against rucks, where they are below average.\n")

cat("\n########## BENCHMARK PANEL ##########\n")
compare_benchmarks(benchmark_rating(before, "remap only", results = res, calibrate = TRUE),
                   benchmark_rating(after, "+ scheme C", results = res, calibrate = TRUE))

saveRDS(list(test = t, before = rb, after = ra), file.path(OUT_DIR, "scheme_c_test.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
