# The recv channel: is cor(adj, raw) = 0.500 a defect or the design working?
#
# READ THIS BEFORE THE NUMBERS. For the hitout channel a low cor(adj, raw) was
# damning, because hitout is ruck-EXCLUSIVE: everyone who scores in it is doing
# the same job, so reordering them meant the cells were wrong.
#
# recv is the opposite. EVERY position scores in it and they score very
# differently -- a key forward's raw reception value is not comparable to a
# midfielder's. The entire purpose of the positional adjustment is to reorder
# them onto "relative to your own position". **So a low cor(adj, raw) and a low
# top-5 overlap are what SUCCESS looks like here**, and reading them as a bug
# would be applying the hitout diagnosis by analogy -- the exact mistake made
# twice today.
#
# The real question is therefore not "did it reorder" but "did it reorder
# SENSIBLY", which is three checks:
#
#   1. RECONSTRUCTION -- does the formula reproduce the pipeline? If not,
#      nothing else in this file means anything. Run first, deliberately, after
#      the spoil retraction: a new measurement is not evidence until it has
#      been checked against a case with a known answer.
#   2. CENTRING -- is the weighted mean adj ~0 within every cell? That is what
#      centring guarantees; a cell far from zero means it did not take.
#   3. WHO MOVES -- are the risers players who beat their own position, or is
#      one cell systematically inflated the way INT was?

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "recv_channel.txt"), split = TRUE)
cat("=== The recv channel ===\nrun at", format(Sys.time()), "\n")

d <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_schemec_pgd.parquet")))
S <- max(d$season, na.rm = TRUE)
d[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
d[, p80 := epv_recv / tog_safe]
mk <- function(x) {
  sl <- torp:::.remap_bench_role(as.character(x$lineup_position), x$player_id,
                                 x$season, x$position_group)
  if (isTRUE(ROLE_USE_LINEUP_GROUP)) torp:::.collapse_lineup_group(sl) else sl
}
d[, rk := mk(d)]
cat("recv IS standardised:", "recv" %in% EPV_STANDARDISE_CHANNELS,
    "-> adj = (per80 - cell_mean) / cell_sd * pooled_sd * TOG\n")

cat("\n########## 1. RECONSTRUCTION CHECK (run first, on purpose) ##########\n")
cells <- d[, .(n = .N, m = weighted.mean(p80, tog_safe, na.rm = TRUE),
               s = .wtd_sd(p80, tog_safe)), by = .(cell = rk)]
pooled <- .wtd_sd(d$p80, d$tog_safe)
# Reconstruct PER GAME, then average -- not average-then-reconstruct.
# .position_adjust() multiplies the centred value by that game's TOG, so the
# result is nonlinear in (p80, tog) and mean-then-formula is not
# formula-then-mean. On hitout the two agreed to 0.05 because the magnitudes
# are small; on recv, where per-80 runs 11-18 instead of ~5, the same
# discrepancy blows out to 3.4 and the check correctly refused the file.
g <- merge(d[season == S, .(player_name, cell = rk, tog_safe, p80,
                            actual_g = epv_recv_adj)],
           cells, by = "cell")
g[, recon_g := (p80 - m) / s * pooled * tog_safe]
chk <- g[, .(cell = cell[1], tog = mean(tog_safe), p80 = mean(p80),
             recon = mean(recon_g, na.rm = TRUE),
             actual = mean(actual_g, na.rm = TRUE), gm = .N),
         by = player_name][gm >= 10]
err <- chk[, max(abs(recon - actual), na.rm = TRUE)]
cat(sprintf("  pooled weighted sd %.4f | worst |recon - actual| over %d players: %.4f\n",
            pooled, nrow(chk), err))
if (err > 0.25) {
  cat("  !! RECONSTRUCTION DOES NOT MATCH. Nothing below is trustworthy --\n")
  cat("     the model of the pipeline is wrong, so stop and fix that first.\n")
} else cat("  reconstruction matches; the rest of this file can be read.\n")
print(head(chk[order(-actual), .(player_name, cell, tog = round(tog, 2),
       p80 = round(p80, 2), recon = round(recon, 3), actual = round(actual, 3))], 4))

cat("\n########## 2. DID THE CENTRING TAKE? ##########\n")
cat("weighted mean adj within each cell -- should be ~0 by construction:\n")
z <- d[, .(n = .N, wmean_adj = round(weighted.mean(epv_recv_adj, tog_safe, na.rm = TRUE), 4)),
       by = .(cell = rk)][order(-abs(wmean_adj))]
print(z[1:8])
cat("\nand by LISTED position (step 2 centres on this, step 1 does not):\n")
print(d[!is.na(position_group), .(n = .N,
        wmean_adj = round(weighted.mean(epv_recv_adj, tog_safe, na.rm = TRUE), 3)),
        by = position_group][order(-wmean_adj)])

cat("\n########## 3. WHO MOVES, AND IS IT SENSIBLE? ##########\n")
pl <- d[season == S, .(gm = .N, pos = position_group[1], cell = rk[1],
                       tog = round(mean(tog_safe), 2),
                       raw = round(mean(epv_recv, na.rm = TRUE), 2),
                       adj = round(mean(epv_recv_adj, na.rm = TRUE), 3)),
        by = player_name][gm >= 8]
setorder(pl, -raw); cat("\ntop 8 by RAW reception value:\n")
print(pl[1:8, .(player_name, pos, cell, tog, raw, adj)])
setorder(pl, -adj); cat("\ntop 8 by ADJUSTED:\n")
print(pl[1:8, .(player_name, pos, cell, tog, raw, adj)])
cat("\ncell mix of the adjusted top 20:\n")
print(pl[1:20, .N, by = cell][order(-N)])
cat("\nIf the adjusted top is spread across cells, the reordering is doing its\n")
cat("job. If it is dominated by one cell, that cell is inflated.\n")

cat(sprintf("\ncor(raw, adj) %.3f | cor(adj, tog) %.3f | cor(raw, tog) %.3f\n",
            cor(pl$raw, pl$adj), cor(pl$adj, pl$tog), cor(pl$raw, pl$tog)))

cat("\n########## VERDICT INPUT ##########\n")
cat("recv reorders heavily BY DESIGN -- every position scores in it and they\n")
cat("score differently. Low cor(adj, raw) is expected. The channel is only\n")
cat("broken if section 2 shows a cell far from zero, or section 3 shows one\n")
cat("cell owning the adjusted top.\n")

saveRDS(list(chk = chk, cells = z, players = pl), file.path(OUT_DIR, "recv_channel.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
