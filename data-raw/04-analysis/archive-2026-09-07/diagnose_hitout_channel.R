# Why is Mason Cox a top-100 player? Anatomy of the hitout channel.
#
# THE FORMULA (player_credit.R:644, v2):
#
#   epv_hitout = hitouts              * EPV_HITOUT_WT        (0.0510)
#              + hitouts_to_advantage * EPV_HITOUT_ADV_WT    (0.1748)
#              + ruck_contests        * EPV_RUCK_CONTEST_WT  (0.0232)
#
# All three at once: winning the tap, winning it to a teammate, AND simply
# ATTENDING the contest. There is no debit for losing one. A ruck who attends 90
# contests and wins 20 still banks 90 x 0.0232 for turning up.
#
# The per-channel calibration then multiplies the whole channel by 4.03, so the
# attendance term is amplified fourfold along with everything else -- which is
# why the biggest risers were six rucks.
#
# This prints the component breakdown per ruck so the attendance share is
# visible rather than inferred, and lines our ranking up against two external
# lists Pete supplied as SENSE CHECKS -- explicitly not targets, both flawed,
# and they disagree with each other sharply (Champion Data has Grundy 2nd among
# rucks; Stats Insider has him 82nd overall). Where all three of ours, theirs
# and theirs disagree, ours is the one to doubt first.
#
# ~1 min.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "hitout_diagnosis.txt"), split = TRUE)
cat("=== Anatomy of the hitout channel ===\nrun at", format(Sys.time()), "\n")
cat(sprintf("\nweights: hitout %.4f | to advantage %.4f | contest ATTENDED %.4f | loss debit %s\n",
            EPV_HITOUT_WT, EPV_HITOUT_ADV_WT, EPV_RUCK_CONTEST_WT, "none under v2"))

pgd <- as.data.table(read_parquet(file.path(OUT_DIR, "v2v3_pgd_v2.parquet")))
s <- max(pgd$season, na.rm = TRUE)
cur <- pgd[season == s]

r <- cur[, .(gm = .N,
             rc = mean(ruck_contests, na.rm = TRUE),
             ho = mean(hitouts, na.rm = TRUE),
             hta = mean(hitouts_to_advantage, na.rm = TRUE),
             epv_hitout = mean(epv_hitout, na.rm = TRUE)),
         by = .(player_name, position_group)][gm >= 8 & rc > 5]
r[, `:=`(win_pct = round(100 * ho / rc, 1),
         adv_pct = round(100 * hta / pmax(ho, 1e-9), 1),
         from_attend = rc * EPV_RUCK_CONTEST_WT,
         from_win = ho * EPV_HITOUT_WT,
         from_adv = hta * EPV_HITOUT_ADV_WT)]
r[, attend_share := round(100 * from_attend / (from_attend + from_win + from_adv), 1)]
setorder(r, -epv_hitout)

cat("\n=== TOP 15 BY HITOUT CHANNEL, and where it comes from ===\n")
cat("(per game. attend%% = share of the channel that is pure attendance)\n\n")
print(r[1:15, .(player = player_name, gm, contests = round(rc, 1), hitouts = round(ho, 1),
                to_adv = round(hta, 1), `win%` = win_pct, `adv%` = adv_pct,
                channel = round(epv_hitout, 2), `attend%` = attend_share)], nrows = 20)

cat("\n=== THE SPECIFIC QUESTION: Mason Cox ===\n")
for (nm in c("Mason Cox", "Max Gawn", "Brodie Grundy", "Sean Darcy", "Tristan Xerri")) {
  x <- r[player_name == nm]
  if (nrow(x) == 0) { cat(sprintf("  %-16s not in the qualified set\n", nm)); next }
  cat(sprintf("  %-16s %4.1f contests, %4.1f hitouts (%4.1f%%), %4.1f to adv | channel %.2f = %.2f attend + %.2f win + %.2f adv\n",
              nm, x$rc, x$ho, x$win_pct, x$hta, x$epv_hitout,
              x$from_attend, x$from_win, x$from_adv))
}

cat("\n=== SENSE CHECK against two external lists (NOT targets) ===\n")
cat("Champion Data player rating, best rucks 2026:\n")
cd <- c("Max Gawn", "Brodie Grundy", "Toby Nankervis", "Luke Jackson", "Tristan Xerri",
        "Tim English", "Rowan Marshall", "Sam Draper", "Kieren Briggs", "Lachlan McAndrew")
si <- c("Max Gawn", "Tristan Xerri", "Luke Jackson", "Rowan Marshall", "Jarrod Witts",
        "Tom De Koning", "Toby Nankervis", "Tim English", "Brodie Grundy",
        "Darcy Cameron", "Oscar McInerney")
cat("  ", paste(cd, collapse = ", "), "\n")
cat("Stats Insider, best rucks 2026:\n  ", paste(si, collapse = ", "), "\n")
cat("\nThe two disagree sharply -- Champion Data ranks Grundy 2nd among rucks,\n")
cat("Stats Insider ranks him 82nd overall. Neither is truth. But a ruck that\n")
cat("neither list has anywhere near the top is one OURS should not have there.\n")

r[, our_rank := frank(-epv_hitout)]
cat("\nour hitout-channel rank for the names on those lists:\n")
ref <- unique(c(cd, si))
chk <- r[player_name %chin% ref, .(player = player_name, our_rank = as.integer(our_rank),
                                   channel = round(epv_hitout, 2), `win%` = win_pct)]
setorder(chk, our_rank); print(chk, nrows = 20)
miss <- setdiff(ref, r$player_name)
if (length(miss)) cat("  not in our qualified set:", paste(miss, collapse = ", "), "\n")

cat("\nand the names WE rank top 5 that neither list mentions:\n")
ours <- r[1:5, player_name]
cat("  ", paste(setdiff(ours, ref), collapse = ", "), "\n")

saveRDS(r, file.path(OUT_DIR, "hitout_diagnosis.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
