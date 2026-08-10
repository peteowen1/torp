# Why did the ruck hitout ratings bunch up after the repricing?
#
# Pete's observation, and it is correct: the top-10 rucks' `epr_hitout` used to
# run 1.13 down to 0.48 and now runs 0.48 down to 0.11. They are closer
# together, which means the channel discriminates between rucks LESS than it did.
#
# Two weights changed at once, so which one did it is not guessable:
#   hta   0.1748 -> 0.1015   the DISCRIMINATING term (rucks differ a lot here)
#   rc   +0.0232 -> -0.0232  the LEVEL term (rucks are all similar here)
#
# A constant shift cannot change a spread, so the naive answer is "the hta
# halving did it". But `ruck_contests` is not constant across rucks -- a number
# one ruck attends far more than a backup -- and its sign flip turns volume from
# a reward into a penalty, which compresses the top end specifically. Both are
# live hypotheses and this separates them by building the channel four ways.
#
# Runs on the saved player-game frame, so it is arithmetic and costs nothing.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "hitout_spread.txt"), split = TRUE)
cat("=== Why did the ruck hitout ratings narrow? ===\nrun at", format(Sys.time()), "\n")

d <- as.data.table(read_parquet(file.path(OUT_DIR, "ruck_pgd_shipped.parquet")))
need <- c("hitouts", "hitouts_to_advantage", "ruck_contests")
if (!all(need %in% names(d))) stop("missing box columns: ",
                                   paste(setdiff(need, names(d)), collapse = ", "))
for (cc in need) d[[cc]] <- fcoalesce(as.numeric(d[[cc]]), 0)
rk <- d[ruck_contests >= 10]
cat(sprintf("\nruck-games (>=10 contests): %s of %s player-games\n",
            format(nrow(rk), big.mark = ","), format(nrow(d), big.mark = ",")))

cat("\n########## 1. HOW MUCH DO RUCKS DIFFER ON EACH INPUT? ##########\n")
cat("A term can only create spread if its INPUT varies between rucks.\n\n")
inp <- rbindlist(lapply(need, function(v) data.table(
  input = v, mean = round(mean(rk[[v]]), 2), sd = round(sd(rk[[v]]), 2),
  cv = round(sd(rk[[v]]) / mean(rk[[v]]), 3))))
print(inp)
cat("\ncv = sd/mean. A LOW cv means every ruck looks the same on that input, so\n")
cat("the weight on it moves the level and not the ordering.\n")

cat("\n########## 2. BUILD THE CHANNEL FOUR WAYS ##########\n")
mk <- function(w_h, w_a, w_c) rk$hitouts * w_h + rk$hitouts_to_advantage * w_a +
                              rk$ruck_contests * w_c
arms <- list(
  "shipped (0.0510 / 0.1748 / +0.0232)" = mk(0.0510, 0.1748,  0.0232),
  "only hta repriced  (-> 0.1015)"      = mk(0.0510, 0.1015,  0.0232),
  "only rc flipped    (-> -0.0232)"     = mk(0.0510, 0.1748, -0.0232),
  "both (what is being gated)"          = mk(0.0510, 0.1015, -0.0232))
sd0 <- sd(arms[[1]])
res <- rbindlist(lapply(names(arms), function(nm) {
  v <- arms[[nm]]
  data.table(arm = nm, mean = round(mean(v), 3), sd = round(sd(v), 3),
             sd_vs_shipped = round(sd(v) / sd0, 3),
             cor_with_shipped = round(cor(v, arms[[1]]), 4))
}))
print(res)
cat("\nsd_vs_shipped is the answer. Whichever single change drops it most is the\n")
cat("cause; if neither alone does, they compress jointly.\n")

cat("\n########## 3. WHERE THE SPREAD COMES FROM, PER ARM ##########\n")
cat("Variance share of each term inside the channel. A term with a big share is\n")
cat("what the channel is actually ranking rucks on.\n\n")
share <- function(w_h, w_a, w_c) {
  parts <- cbind(h = rk$hitouts * w_h, hta = rk$hitouts_to_advantage * w_a,
                 rc = rk$ruck_contests * w_c)
  tot <- rowSums(parts)
  round(100 * apply(parts, 2, function(p) cov(p, tot)) / var(tot), 1)
}
sh <- rbind(shipped = share(0.0510, 0.1748, 0.0232),
            repriced = share(0.0510, 0.1015, -0.0232))
print(sh)
cat("\n(Shares are covariance-with-total over total variance, so they sum to 100\n")
cat("and a term that moves AGAINST the total reads negative.)\n")

cat("\n########## 4. DOES THE ORDERING SURVIVE? ##########\n")
cat("Compression only matters if it also reorders. Separating the two:\n\n")
pl <- rk[, .(g = .N,
             ship = mean(mk(0.0510, 0.1748,  0.0232)[.I]),
             repr = mean(mk(0.0510, 0.1015, -0.0232)[.I])),
         by = .(player_id, player_name)][g >= 8]
cat(sprintf("  rucks with >=8 games : %d\n", nrow(pl)))
cat(sprintf("  Spearman on ruck order: %.4f\n",
            cor(pl$ship, pl$repr, method = "spearman")))
cat(sprintf("  sd across rucks       : %.3f -> %.3f  (%.2fx)\n",
            sd(pl$ship), sd(pl$repr), sd(pl$repr) / sd(pl$ship)))
setorder(pl, -ship)
print(head(pl[, .(player_name, g, shipped = round(ship, 3),
                  repriced = round(repr, 3))], 12))

cat("\n########## 5. WHAT IT MEANS ##########\n")
cat("If the ordering holds (high Spearman) and only the scale shrank, the channel\n")
cat("still knows who the better rucks are -- it just says it more quietly, and a\n")
cat("points-scale decision can restore the volume. If the ordering ALSO moved,\n")
cat("the repricing changed who is good, which is a different claim needing its\n")
cat("own evidence.\n")

saveRDS(list(inputs = inp, arms = res, shares = sh, players = pl),
        file.path(OUT_DIR, "hitout_spread.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
