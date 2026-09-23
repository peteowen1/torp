# The 100m+ aerial contests I claimed did not exist (issue #210)
# =============================================================================
# I wrote "NONE over 100m" in NEWS.md and the commit message, and used it as the
# evidence that the aerial-contest model is clean.
#
# It is false, and the way it went wrong is worth recording. np_contest_model_leak_check.R
# printed `pct_over_100m` ROUNDED TO ONE DECIMAL. 16 rows in 50,050 is 0.032%,
# which prints as "0.0", and I read a rounded percentage as an absolute zero --
# while the `max` column in the very same table read 143.3m and said otherwise.
#
# The house rule this breaks is already written down: never quote a derived
# statistic without the raw count behind it. A percentage cannot express "none";
# only a count can.
#
# This script asks the question the way it should have been asked the first time:
# with sum(), not mean(), and with the offending rows printed so their shape can
# be seen rather than inferred.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_over100m_check.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
cst <- as.data.table(build_aerial_contests(ch, pbp))

say("aerial contests: ", format(nrow(cst), big.mark = ","))
say("\n=== COUNTS, not percentages ===")
for (thr in c(60, 80, 100, 120, 140)) {
  n <- sum(cst$kick_len > thr)
  say("  kick_len > ", formatC(thr, width = 3), "m : ", formatC(n, width = 6),
      "   (", format(round(100 * n / nrow(cst), 4), nsmall = 4), "%)")
}
say("  max kick_len      : ", round(max(cst$kick_len), 1), "m")

big <- cst[kick_len > 100]
say("\n=== the ", nrow(big), " contests over 100m ===")
if (nrow(big) > 0) {
  say("If these are the same leak, they should be one-sided: the resolving row")
  say("belongs to the OTHER team (def_win) and is a reception, not a duel.")
  print(big[, .(n = .N), by = .(out_desc, def_win)][order(-n)])
  say("\n  def_win TRUE : ", big[def_win == TRUE, .N], " of ", nrow(big))
  say("  reception outcomes (Uncontested Mark / Mark On Lead): ",
      big[out_desc %chin% c("Uncontested Mark", "Mark On Lead"), .N], " of ", nrow(big))
  say("\nfirst rows, so the geometry is visible rather than described:")
  print(head(big[, .(match_id, kick_do, kick_x, kick_y, out_x, out_y,
                     kick_len = round(kick_len, 1), out_desc, def_win)], 8))
}

say("\n=== does removing them change the verdict? ===")
say("The 'clean' conclusion rests on the bulk being physically plausible.")
say("Recomputed with the 100m+ rows excluded:")
ok <- cst[kick_len <= 100]
say("  n          : ", format(nrow(ok), big.mark = ","))
say("  median     : ", round(median(ok$kick_len), 1), "m")
say("  p95        : ", round(quantile(ok$kick_len, .95), 1), "m")
say("  >60m count : ", sum(ok$kick_len > 60), " (",
    round(100 * mean(ok$kick_len > 60), 1), "%)")
say("\nand the share of the whole table those rows represent: ",
    format(round(100 * nrow(big) / nrow(cst), 4), nsmall = 4), "%")
say("\nThe verdict stands or falls on MATERIALITY, which is now stated as a count")
say("and a share rather than as a false absolute.")
