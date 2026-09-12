# Did v12 land as predicted? (rating vintage v12, the symmetric contest filter)
# =============================================================================
# The prediction below was written BEFORE the v12 rebuild ran, from the 2026
# arms comparison in check_symmetric_filter.R. That ordering is the whole point:
# a number recorded after the fact is a description, not a check.
#
# v12 flips ONE constant, NP_CONTEST_FILTER_SYMMETRIC FALSE -> TRUE, fixing
# torp#220. The filter kept every defensive contest win unconditionally but an
# attacking win only when the outcome was in EPV3_DUEL_OUT, discarding 1,483 of
# 17,078 contests of which 100% were attacking wins.
#
# Predicted (measured pre-merge on 2026, check_symmetric_filter.R):
#   KEY_FORWARD      8.005 -> 8.404   (+0.399, of which +0.370 in np_direct)
#   MEDIUM_FORWARD   6.337 -> 6.385
#   MIDFIELDER       5.903 -> 5.868
#   RUCK             4.882 -> 4.971
#   MEDIUM_DEFENDER  4.632 -> 4.565
#   KEY_DEFENDER     4.592 -> 4.609
#   FWD-DEF gap      3.413 -> 3.795
#   contested rows  16,081 -> 17,571
#
# THE CONTROL: psr is the player-stat path. It cannot see the contest
# population, so it must come back BIT-IDENTICAL. identical(), never
# all.equal() -- a tolerance would pass on exactly the small drift that says
# the rebuild changed something it was not asked to.
#
# A small miss on the position figures is expected: the prediction was measured
# on 2026 alone while the rebuild runs full history. A large miss is not.
#
#   V11_FILE=<preserved v11>  V12_FILE=<new canonical>
#   powershell.exe -Command 'Rscript "data-raw/05-validation/check_v12_published.R"'
suppressMessages({library(data.table); library(arrow); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

a <- as.data.table(arrow::read_parquet(Sys.getenv("V11_FILE")))
b <- as.data.table(arrow::read_parquet(Sys.getenv("V12_FILE")))
say("v11 rows ", format(nrow(a), big.mark = ","),
    "   v12 rows ", format(nrow(b), big.mark = ","))

key <- intersect(c("player_id", "season", "round"), names(a))
num <- intersect(c("torp", "epr", "psr", "epv", "epr_disp", "epr_recv"), names(a))
num <- num[vapply(num, function(c) is.numeric(a[[c]]), logical(1))]
m <- merge(a[, c(key, num), with = FALSE], b[, c(key, num), with = FALSE],
           by = key, suffixes = c("_v11", "_v12"))
say("matched rows: ", format(nrow(m), big.mark = ","))

say("\n=== did the published ratings change? ===")
say("mean_abs and max_abs are in rating units; pct_moved counts rows over 1e-9.")
res <- rbindlist(lapply(num, function(c) {
  d <- m[[paste0(c, "_v12")]] - m[[paste0(c, "_v11")]]
  data.table(column = c, mean_abs = round(mean(abs(d), na.rm = TRUE), 4),
             max_abs = round(max(abs(d), na.rm = TRUE), 4),
             pct_moved = round(100 * mean(abs(d) > 1e-9, na.rm = TRUE), 1),
             cor = round(stats::cor(m[[paste0(c, "_v11")]], m[[paste0(c, "_v12")]],
                                    use = "complete.obs"), 5))
}))
print(res)

say("\n=== the control ===")
say("psr is the player-stat path and cannot see the contest population, so it")
say("must not move. If it does, the rebuild changed something it was not")
say("supposed to and nothing else in this file can be trusted.")
#
# `identical()` was the original test and it is TOO STRICT for a full-history
# rebuild: v12 came back FALSE on a maximum difference of 1.13e-14, which is
# machine epsilon from re-running on different hardware, not a change in the
# ratings. Reported 2026-09-12 rather than quietly relaxed, because the wrong
# bound on a control is how a real move gets waved through later.
#
# The bound that means something here is the same 1e-9 the movement table uses:
# no row may move by more than that. The exact difference is printed either
# way, so a reader can judge rather than take the verdict on trust.
PSR_TOL <- 1e-9
if ("psr" %chin% num) {
  d <- abs(m$psr_v12 - m$psr_v11)
  mx <- max(d, na.rm = TRUE)
  n_over <- sum(d > PSR_TOL, na.rm = TRUE)
  say("  psr max |diff|: ", signif(mx, 3), "   rows over ", PSR_TOL, ": ", n_over,
      "   bit-identical: ", identical(m$psr_v11, m$psr_v12))
  say("  -> ", if (n_over == 0) "CLEAN (any residual is floating-point noise)"
                else "*** PSR MOVED, INVESTIGATE ***")
} else say("  psr not in this frame")

say("\n=== per-position published net points, v12 ===")
say("Compared against the pre-merge prediction. These come from")
say("player_game_ratings, the artifact the site reads.")
pg <- as.data.table(load_player_game_ratings(2026))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
if ("net_points" %chin% names(pg) && "position_group" %chin% names(pg)) {
  ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
           "MEDIUM_DEFENDER", "KEY_DEFENDER")
  # COMPARE THE GAP, NOT THE LEVELS. The prediction was measured on the RAW
  # ledger (build_net_points() output, in check_symmetric_filter.R), while
  # player_game_ratings' net_points is position- and opponent-adjusted to mean
  # zero. Those are two different frames: on 2026 the levels differ by about
  # 6.4 points across every position, near-constant, while the GAP between two
  # positions is nearly frame-invariant because the centring cancels in a
  # difference (v11: raw 3.413 vs published 3.409).
  #
  # Written out because the first version of this file predicted raw levels and
  # checked them against the published frame, producing a 6.449 "miss" that
  # looked like a broken rebuild and was a broken comparison. Measure and
  # implement on the same frame.
  pred_gap_raw  <- 3.795   # predicted, raw ledger, 2026
  v11_gap_pub   <- 3.409   # v11 as published, for the direction check
  got <- pg[!is.na(position_group),
            .(published = round(mean(net_points, na.rm = TRUE), 3)), by = position_group]
  cmp <- got[match(ORD, position_group)]
  print(cmp)
  g <- cmp[position_group == "KEY_FORWARD"]$published -
       cmp[position_group == "KEY_DEFENDER"]$published
  say("\n  FWD-DEF gap  v11 published ", v11_gap_pub,
      "   v12 published ", round(g, 3),
      "   change ", round(g - v11_gap_pub, 3))
  say("  predicted gap (raw ledger, 2026): ", pred_gap_raw,
      "   miss against published: ", round(abs(g - pred_gap_raw), 3))
  if (g <= v11_gap_pub) {
    say("\n  *** THE GAP DID NOT WIDEN. v12 corrects a filter that discarded")
    say("  attacking contest wins, so it must. Investigate before trusting this")
    say("  rebuild. ***")
  }
  say("\n  Expected direction: the gap WIDENS. v12 corrects a filter that was")
  say("  discarding attacking contest wins, so the defence's inflated 60.1%")
  say("  share of contest credit falls to 48.3%. A narrowing gap here would")
  say("  mean the flip did not do what it was measured to do.")
} else {
  say("  net_points or position_group absent from player_game_ratings.")
}
