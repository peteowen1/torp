# Did v10 land as predicted? (rating vintage v10)
# =============================================================================
# v10 flipped two constants together and the release notes predicted a specific
# per-position move. A prediction that is never checked against the published
# artifact is just a hope, and this session has already produced several
# confident numbers that were measured on the wrong frame or the wrong layer.
#
# Predicted (measured pre-merge, np_v10_combined_effect.R):
#   KEY_FORWARD      2.042 -> 2.032
#   MEDIUM_FORWARD   0.900 -> 0.762
#   MIDFIELDER       0.352 -> 0.331
#   RUCK             0.439 -> 0.618
#   MEDIUM_DEFENDER -1.319 -> -1.294
#   KEY_DEFENDER    -2.432 -> -2.235
#   FWD-DEF gap      4.474 -> 4.267
#
# Compares the PRESERVED v9 parquet against the new canonical, both read from
# the release rather than recomputed, so this checks what was actually
# published and not what the code would produce if re-run.
#
#   V9_FILE=<preserved v9>  V10_FILE=<new canonical>
#   powershell.exe -Command 'Rscript "data-raw/05-validation/check_v10_published.R"'
suppressMessages({library(data.table); library(arrow); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

a <- as.data.table(arrow::read_parquet(Sys.getenv("V9_FILE")))
b <- as.data.table(arrow::read_parquet(Sys.getenv("V10_FILE")))
say("v9 rows ", format(nrow(a), big.mark = ","), "   v10 rows ", format(nrow(b), big.mark = ","))

key <- intersect(c("player_id", "season", "round"), names(a))
num <- intersect(c("torp", "epr", "psr", "epv", "epr_disp", "epr_recv"), names(a))
num <- num[vapply(num, function(c) is.numeric(a[[c]]), logical(1))]
m <- merge(a[, c(key, num), with = FALSE], b[, c(key, num), with = FALSE],
           by = key, suffixes = c("_v9", "_v10"))
say("matched rows: ", format(nrow(m), big.mark = ","))

say("\n=== did the published ratings actually change? ===")
say("mean_abs and max_abs are in rating units; pct_moved counts rows over 1e-9.")
res <- rbindlist(lapply(num, function(c) {
  d <- m[[paste0(c, "_v10")]] - m[[paste0(c, "_v9")]]
  data.table(column = c, mean_abs = round(mean(abs(d), na.rm = TRUE), 4),
             max_abs = round(max(abs(d), na.rm = TRUE), 4),
             pct_moved = round(100 * mean(abs(d) > 1e-9, na.rm = TRUE), 1),
             cor = round(stats::cor(m[[paste0(c, "_v9")]], m[[paste0(c, "_v10")]],
                                    use = "complete.obs"), 5))
}))
print(res)

say("\n=== the control ===")
say("psr is the player-stat path and is independent of the contest population")
say("and the error-blame rule, so it must be bit-identical. If it moved, the")
say("rebuild changed something it was not supposed to.")
if ("psr" %in% num) {
  p <- res[column == "psr"]
  say("  psr pct_moved ", p$pct_moved, "%   max_abs ", p$max_abs,
      "  -> ", if (p$pct_moved == 0) "CLEAN" else "*** PSR MOVED, INVESTIGATE ***")
} else say("  psr not in this frame")

say("\n=== per-position published net points, v10 ===")
say("Compared against the pre-merge prediction. The published figures come from")
say("player_game_ratings, which is the artifact the site reads.")
pg <- as.data.table(load_player_game_ratings(2026))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
if ("net_points" %chin% names(pg) && "position_group" %chin% names(pg)) {
  ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
           "MEDIUM_DEFENDER", "KEY_DEFENDER")
  pred <- data.table(position_group = ORD,
                     predicted = c(2.032, 0.762, 0.331, 0.618, -1.294, -2.235))
  got <- pg[!is.na(position_group), .(published = round(mean(net_points, na.rm = TRUE), 3)),
            by = position_group]
  cmp <- merge(pred, got, by = "position_group", all.x = TRUE)
  cmp <- cmp[match(ORD, position_group)]
  cmp[, diff := round(published - predicted, 3)]
  print(cmp)
  g <- function(col) cmp[position_group == "KEY_FORWARD"][[col]] -
                     cmp[position_group == "KEY_DEFENDER"][[col]]
  say("\n  FWD-DEF gap  predicted ", round(g("predicted"), 3),
      "   published ", round(g("published"), 3))
  say("\n  Largest per-position miss: ", round(max(abs(cmp$diff), na.rm = TRUE), 3))
  say("  A small miss is expected -- the prediction was measured on the 2026")
  say("  frame alone while the rebuild runs full history, and a scheduled run")
  say("  republished the ratings mid-sequence. A LARGE miss is not.")
} else {
  say("  net_points or position_group absent from player_game_ratings --")
  say("  the published frame may not carry them yet on this vintage.")
}
