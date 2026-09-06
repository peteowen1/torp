# Does the 16-slot mirrored key beat the 21-slot one, on all four channels?
#
# ROLE_USE_LINEUP_GROUP merges the left/right pairs -- BPL+BPR -> BP,
# HBFL+HBFR -> HBF, WL+WR -> W, HFFL+HFFR -> HFF, FPL+FPR -> FP -- taking 21
# cells to 16 and roughly DOUBLING the size of every merged cell.
#
# WHY IT MIGHT HELP, AND ONLY FOR ONE CHANNEL. Every cell mean is estimated from
# data, so a small cell gives a noisy mean, and a noisy mean adds noise to every
# player judged against it. spoil is the thinnest channel -- an aggregate of
# eight box stats, several with negative weights that partly cancel -- and it is
# the one that got WORSE under the bench remap, cor(adj, tog) going +0.025 ->
# +0.197. Bigger cells are the obvious cheap lever.
#
# STATED BEFORE THE RUN:
#   PASS  spoil's cor(adj, tog) moves toward zero from +0.197, and none of the
#         other three degrades materially.
#   FAIL  spoil does not improve, or the others pay for it -- in which case the
#         21-slot key stays and spoil needs a different answer.
#
# Note a left/right merge is only valid if the two sides are actually
# equivalent, so that is checked rather than assumed.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "lineup_group_test.txt"), split = TRUE)
cat("=== 16-slot mirrored key vs 21-slot ===\nrun at", format(Sys.time()), "\n")

set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")
with_const <- function(l, expr) {
  old <- lapply(names(l), function(nm) get(nm, envir = asNamespace("torp")))
  names(old) <- names(l); set_const(l); on.exit(set_const(old), add = TRUE); force(expr)
}
BEST <- list(EPV_ENGINE = "v2", EPV3_STOP_ZERO_SUM = FALSE,
             EPV_STANDARDISE_CHANNELS = c("recv", "disp", "spoil"),
             EPV_DIFFICULTY_SPLIT = FALSE, EPV_PER_CHANNEL_POINTS_SCALE = FALSE,
             EPV_POINTS_SCALE = 0.919, ROLE_REMAP_BENCH = TRUE,
             EPV_HITOUT_CENTRE_ON_RUCK = TRUE, EPV_RUCK_BLEND_WIDTH = 10)

pbp <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams <- load_teams(TRUE); chains <- load_chains(TRUE); res <- load_results(TRUE)

slots21 <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_blend_pgd.parquet")))
f <- file.path(OUT_DIR, "v2_lineupgroup_pgd.parquet")
slots16 <- if (file.exists(f)) {
  cli::cli_alert_info("Reusing 16-slot frame"); as.data.table(read_parquet(f))
} else {
  d <- with_const(c(BEST, list(ROLE_USE_LINEUP_GROUP = TRUE)),
    as.data.table(create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v2")))
  write_parquet(d, f); d
}
cat("\nraw channels identical across arms:",
    all(vapply(c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout"),
               function(c) isTRUE(all.equal(slots21[[c]], slots16[[c]])), logical(1))), "\n")

cat("\n########## IS THE LEFT/RIGHT MERGE EVEN VALID? ##########\n")
cat("A merge assumes the two sides are equivalent. If BPL and BPR produce\n")
cat("different amounts, merging them makes the cell mean fit neither.\n\n")
d <- copy(slots21)
d[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
pairs <- list(c("BPL", "BPR"), c("HBFL", "HBFR"), c("WL", "WR"),
              c("HFFL", "HFFR"), c("FPL", "FPR"))
cat(sprintf("%-12s %10s %10s %8s\n", "pair", "left", "right", "gap"))
for (p in pairs) {
  l <- d[lineup_position == p[1], weighted.mean(epv_recv / tog_safe, tog_safe, na.rm = TRUE)]
  r <- d[lineup_position == p[2], weighted.mean(epv_recv / tog_safe, tog_safe, na.rm = TRUE)]
  cat(sprintf("%-12s %10.3f %10.3f %8.3f\n", paste(p, collapse = "/"), l, r, abs(l - r)))
}
cat("\n(recv per-80, the widest channel. Small gaps mean the merge is safe.)\n")

cat("\n########## THE ADJUSTMENT LAYER, ALL FOUR CHANNELS ##########\n")
a21 <- .bm_adj(slots21)[, arm := "21 slots"][]
a16 <- .bm_adj(slots16)[, arm := "16 slots"][]
both <- rbind(a21, a16)
cat("\ncor(adj, tog) -- the unambiguous one; nearer zero is better:\n")
print(dcast(both, channel ~ arm, value.var = "cor_adj_tog"))
cat("\ncor(adj, raw):\n")
print(dcast(both, channel ~ arm, value.var = "cor_adj_raw"))
cat("\ntop-5 overlap with the raw leaders:\n")
print(dcast(both, channel ~ arm, value.var = "top5_overlap"))

cat("\n########## VERDICT ##########\n")
s21 <- a21[channel == "spoil"]$cor_adj_tog; s16 <- a16[channel == "spoil"]$cor_adj_tog
better <- abs(s16) < abs(s21)
others <- merge(a21[channel != "spoil", .(channel, t21 = cor_adj_tog)],
                a16[channel != "spoil", .(channel, t16 = cor_adj_tog)], by = "channel")
others[, degraded := abs(t16) > abs(t21) + 0.05]
cat(sprintf("  spoil cor(adj,tog): %+.3f -> %+.3f  (%s)\n", s21, s16,
            ifelse(better, "improved", "not improved")))
print(others)
ok <- better && !any(others$degraded)
cat("\n  VERDICT: ", if (ok) "PASS -- 16 slots is the better key"
    else "FAIL -- keep 21 slots; spoil needs a different answer", "\n")

saveRDS(both, file.path(OUT_DIR, "lineup_group_test.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
