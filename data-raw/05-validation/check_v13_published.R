# Did v13 land as predicted? (rating vintage v13, three contest outcomes)
# =============================================================================
# Written BEFORE the rebuild, from check_three_way_on_v12.R measured against the
# SHIPPED v12 baseline. That baseline matters: the same model measured against
# v11 read +0.235 on the gap because v11 still had the asymmetric filter, which
# put the filter fix inside the three-way arm and left it out of the baseline.
# Against v12 it reads -0.147. Same model, corrected comparison.
#
# v13 flips two constants together, which is deliberate -- they are one design:
#   EPV3_CONTEST_OUTCOMES      "two"  -> "three"
#   EPV3_CONTEST_EXCLUDE_SHOTS FALSE  -> TRUE
#
# Predicted, raw ledger, 2026 (check_three_way_on_v12.R):
#   KEY_FORWARD      8.404 -> 7.891    MEDIUM_FORWARD  6.385 -> 6.164
#   MIDFIELDER       5.868 -> 5.730    RUCK            4.971 -> 4.809
#   MEDIUM_DEFENDER  4.565 -> 4.424    KEY_DEFENDER    4.609 -> 4.243
#   FWD-DEF gap      3.795 -> 3.648    (-0.147)
#   contested rows  17,571 -> 16,922
#
# COMPARE THE GAP, NOT THE LEVELS. The prediction is on the raw ledger;
# player_game_ratings' net_points is position- and opponent-adjusted to mean
# zero, so the levels differ by ~6.4 across every position while the gap is
# nearly frame-invariant. v12 checked this: raw 3.795 against published 3.752.
# The first version of the v12 check compared levels and reported a 6.449 "miss"
# that was a broken comparison, not a broken rebuild.
#
# THE CONTROL: psr cannot see the contest model, so it must not move. Bounded at
# 1e-9 rather than identical() -- v12 came back non-identical on 1.13e-14, which
# is machine epsilon from a full-history rebuild, not a change.
#
#   V12_FILE=<preserved v12>  V13_FILE=<new canonical>
#   powershell.exe -Command 'Rscript "data-raw/05-validation/check_v13_published.R"'
suppressMessages({library(data.table); library(arrow); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

a <- as.data.table(arrow::read_parquet(Sys.getenv("V12_FILE")))
b <- as.data.table(arrow::read_parquet(Sys.getenv("V13_FILE")))
say("v12 rows ", format(nrow(a), big.mark = ","),
    "   v13 rows ", format(nrow(b), big.mark = ","))

key <- intersect(c("player_id", "season", "round"), names(a))
num <- intersect(c("torp", "epr", "psr", "epv", "epr_disp", "epr_recv"), names(a))
num <- num[vapply(num, function(c) is.numeric(a[[c]]), logical(1))]
m <- merge(a[, c(key, num), with = FALSE], b[, c(key, num), with = FALSE],
           by = key, suffixes = c("_v12", "_v13"))
say("matched rows: ", format(nrow(m), big.mark = ","))

say("\n=== did the published ratings change? ===")
say("mean_abs and max_abs are in rating units; pct_moved counts rows over 1e-9.")
res <- rbindlist(lapply(num, function(c) {
  d <- m[[paste0(c, "_v13")]] - m[[paste0(c, "_v12")]]
  data.table(column = c, mean_abs = round(mean(abs(d), na.rm = TRUE), 4),
             max_abs = round(max(abs(d), na.rm = TRUE), 4),
             pct_moved = round(100 * mean(abs(d) > 1e-9, na.rm = TRUE), 1),
             cor = round(stats::cor(m[[paste0(c, "_v12")]], m[[paste0(c, "_v13")]],
                                    use = "complete.obs"), 5))
}))
print(res)

say("\n=== the control ===")
say("psr is the player-stat path and cannot see the contest model, so it must")
say("not move. If it does, the rebuild changed something it was not supposed to")
say("and nothing else here can be trusted.")
PSR_TOL <- 1e-9
if ("psr" %chin% num) {
  d <- abs(m$psr_v13 - m$psr_v12); mx <- max(d, na.rm = TRUE)
  n_over <- sum(d > PSR_TOL, na.rm = TRUE)
  say("  psr max |diff|: ", signif(mx, 3), "   rows over ", PSR_TOL, ": ", n_over)
  say("  -> ", if (n_over == 0) "CLEAN (any residual is floating-point noise)"
                else "*** PSR MOVED, INVESTIGATE ***")
} else say("  psr not in this frame")

say("\n=== per-position published net points, v13 ===")
pg <- as.data.table(load_player_game_ratings(2026))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
if ("net_points" %chin% names(pg) && "position_group" %chin% names(pg)) {
  ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
           "MEDIUM_DEFENDER", "KEY_DEFENDER")
  pred_gap_raw <- 3.648   # predicted, raw ledger, 2026
  v12_gap_pub  <- 3.752   # v12 as published, for the direction check
  got <- pg[!is.na(position_group),
            .(published = round(mean(net_points, na.rm = TRUE), 3)), by = position_group]
  cmp <- got[match(ORD, position_group)]
  print(cmp)
  g <- cmp[position_group == "KEY_FORWARD"]$published -
       cmp[position_group == "KEY_DEFENDER"]$published
  say("\n  FWD-DEF gap  v12 published ", v12_gap_pub,
      "   v13 published ", round(g, 3),
      "   change ", round(g - v12_gap_pub, 3))
  say("  predicted gap (raw ledger, 2026): ", pred_gap_raw,
      "   miss against published: ", round(abs(g - pred_gap_raw), 3))
  if (g >= v12_gap_pub) {
    say("\n  *** THE GAP DID NOT NARROW. Unlike v12, this vintage is expected to")
    say("  REDUCE it: pricing an intercept mark at its true -0.720 instead of")
    say("  +0.270 costs key forwards more than key defenders. A widening gap")
    say("  means the flip did not do what it was measured to do. ***")
  }
} else {
  say("  net_points or position_group absent from player_game_ratings.")
}
