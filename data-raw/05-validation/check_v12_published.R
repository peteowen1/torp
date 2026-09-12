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
say("must be bit-identical. If it moved, the rebuild changed something it was")
say("not supposed to and nothing else in this file can be trusted.")
if ("psr" %chin% num) {
  ident <- identical(m$psr_v11, m$psr_v12)
  p <- res[column == "psr"]
  say("  psr identical(): ", ident, "   pct_moved ", p$pct_moved, "%   max_abs ", p$max_abs,
      "  -> ", if (ident) "CLEAN" else "*** PSR MOVED, INVESTIGATE ***")
} else say("  psr not in this frame")

say("\n=== per-position published net points, v12 ===")
say("Compared against the pre-merge prediction. These come from")
say("player_game_ratings, the artifact the site reads.")
pg <- as.data.table(load_player_game_ratings(2026))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
if ("net_points" %chin% names(pg) && "position_group" %chin% names(pg)) {
  ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
           "MEDIUM_DEFENDER", "KEY_DEFENDER")
  pred <- data.table(position_group = ORD,
                     predicted = c(8.404, 6.385, 5.868, 4.971, 4.565, 4.609))
  got <- pg[!is.na(position_group),
            .(published = round(mean(net_points, na.rm = TRUE), 3)), by = position_group]
  cmp <- merge(pred, got, by = "position_group", all.x = TRUE)[match(ORD, position_group)]
  cmp[, diff := round(published - predicted, 3)]
  print(cmp)
  g <- function(col) cmp[position_group == "KEY_FORWARD"][[col]] -
                     cmp[position_group == "KEY_DEFENDER"][[col]]
  say("\n  FWD-DEF gap  predicted ", round(g("predicted"), 3),
      "   published ", round(g("published"), 3))
  say("  Largest per-position miss: ", round(max(abs(cmp$diff), na.rm = TRUE), 3))
  say("\n  Expected direction: the gap WIDENS. v12 corrects a filter that was")
  say("  discarding attacking contest wins, so the defence's inflated 60.1%")
  say("  share of contest credit falls to 48.3%. A narrowing gap here would")
  say("  mean the flip did not do what it was measured to do.")
} else {
  say("  net_points or position_group absent from player_game_ratings.")
}
