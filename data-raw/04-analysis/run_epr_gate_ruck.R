# The ruck repricing on the fast causal gate.
#
# WHY THIS RUN EXISTS. The production match gate says dMAE +0.2194 with the CI
# sitting on zero, and the in-sample check (diagnose_ruck_gate_cost.R) says the
# repriced channel is BETTER on every measure -- correlation with margin 0.089 ->
# 0.169, incremental R-squared over the other three channels 0.00184 -> 0.00394,
# t on the coefficient 1.83 -> 2.69. Those two do not agree, and a ship decision
# taken while they disagree is a guess.
#
# The production gate feeds five GAMs and an XGBoost epr_diff, psr_diff,
# torp_diff, elo_diff, xelo_diff, weather and venue, then blends. It reweights
# whatever changes, which is why five separate rating changes have gated neutral
# there. This gate strips it to the rating alone, so a rating change has nowhere
# to hide and the same true effect lands as a much bigger measured one against
# the same noise.
#
# NOTE ON THE PGD ARGUMENT. The two arms have DIFFERENT player-game frames --
# the ruck weights are applied when the channel is built, not during the rating
# build -- so unlike run_epr_gate_v2cal.R each arm passes its own. Passing one
# shared frame here would silently score both ratings against the same lineups
# and could only understate the difference.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "epr_gate_ruck.txt"), split = TRUE)
cat("=== Ruck repricing on the fast causal gate ===\nrun at", format(Sys.time()), "\n")
cat("\nshipped : hitout +0.0510  advantage +0.1748  attendance +0.0232\n")
cat("repriced: hitout +0.0615  advantage +0.1748  attendance -0.0232\n")
cat("(attendance sign flipped; win weight set so break-even sits at the league\n")
cat(" average win rate of 37.7%. Advantage weight unchanged between arms.)\n")

res <- load_results(TRUE)
rd <- function(f) as.data.table(read_parquet(file.path(OUT_DIR, f)))
pgd_a <- rd("ruck_pgd_shipped.parquet");  rt_a <- rd("ruck_rt_shipped.parquet")
pgd_b <- rd("ruck_pgd_repriced.parquet"); rt_b <- rd("ruck_rt_repriced.parquet")

cat(sprintf("\nframes: shipped %s rows, repriced %s rows\n",
            format(nrow(pgd_a), big.mark = ","), format(nrow(pgd_b), big.mark = ",")))
k <- c("match_id", "player_id")
mm <- merge(pgd_a[, c(k, "epv_hitout"), with = FALSE],
            pgd_b[, c(k, "epv_hitout"), with = FALSE], by = k, suffixes = c("_a", "_b"))
dd <- mean(abs(mm$epv_hitout_a - mm$epv_hitout_b), na.rm = TRUE)
cat(sprintf("ARMS GUARD -- mean|diff| in raw epv_hitout: %.5f\n", dd))
if (dd < 1e-9) { cat("!! IDENTICAL -- aborting\n"); sink(); quit(status = 1) }

g1 <- bm_epr_gate(pgd_a, rt_a, res, "shipped")
g2 <- bm_epr_gate(pgd_b, rt_b, res, "repriced")
print(g1); print(g2); compare_epr_gates(g1, g2)

cat("\nREAD, and decide from this rather than from the production gate.\n")
cat("OOS mean MAE is the guardrail: a RISE means the rating got worse at\n")
cat("predicting team points, undiluted by the other features. The within-team\n")
cat("coefficient and its t are the causal reads -- does the rating track WHO IS\n")
cat("PLAYING rather than which club they play for.\n")
cat("\nIf this gate agrees with the production one (repriced worse), the in-sample\n")
cat("gain was a fit artefact and the repricing should not ship. If it disagrees\n")
cat("(repriced same or better), the production result is dilution plus noise on\n")
cat("397 matches, and the in-sample gain is the better evidence.\n")

saveRDS(list(shipped = g1, repriced = g2), file.path(OUT_DIR, "epr_gate_ruck.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
