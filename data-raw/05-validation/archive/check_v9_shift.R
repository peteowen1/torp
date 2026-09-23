# Did the v9 rebuild actually change the published ratings? (torp#210)
# =============================================================================
# The fix was predicted to move published net points by mean |delta| 0.396 a
# game. That figure was measured on .np_team_margin() output per player-game,
# which is NOT what torp_ratings.parquet holds -- that carries season EPR/PSR/
# TORP ratings. So this does not re-test the 0.396; it tests the claim that
# actually matters now:
#
#   the published canonical is genuinely v9 logic, not a re-upload of v8
#
# A rebuild that silently no-opped would leave the two files equal, and the
# byte sizes differing (16,375,436 vs 16,360,683) is suggestive but not proof --
# a parquet can differ in bytes while carrying identical values, and an
# unchanged file would be the failure mode worth catching.
#
# Compares the preserved v8 against the new canonical on their shared key.
#
#   powershell.exe -Command 'Rscript "data-raw/05-validation/check_v9_shift.R"'
suppressMessages({library(data.table); library(arrow)})
say <- function(...) cat(..., "\n", sep = "")

old <- as.data.table(arrow::read_parquet(Sys.getenv("V8_FILE")))
new <- as.data.table(arrow::read_parquet(Sys.getenv("V9_FILE")))
say("v8 rows ", format(nrow(old), big.mark = ","),
    "   v9 rows ", format(nrow(new), big.mark = ","))
say("v8 cols ", ncol(old), "   v9 cols ", ncol(new))

key <- intersect(c("player_id", "season", "round"), names(old))
say("joining on: ", paste(key, collapse = ", "))
num <- intersect(c("torp", "epr", "psr", "epv", "epr_disp", "epr_recv"), names(old))
num <- num[vapply(num, function(c) is.numeric(old[[c]]), logical(1))]
say("numeric columns compared: ", paste(num, collapse = ", "))

m <- merge(old[, c(key, num), with = FALSE],
           new[, c(key, num), with = FALSE],
           by = key, suffixes = c("_v8", "_v9"))
say("matched rows: ", format(nrow(m), big.mark = ","))

say("\n=== per-column change (0 everywhere would mean the rebuild no-opped) ===")
say("mean_abs and max_abs are in rating units; pct_moved counts rows changing")
say("by more than 1e-9.")
res <- rbindlist(lapply(num, function(c) {
  a <- m[[paste0(c, "_v8")]]; b <- m[[paste0(c, "_v9")]]
  d <- b - a
  data.table(column = c,
             mean_abs = round(mean(abs(d), na.rm = TRUE), 4),
             max_abs = round(max(abs(d), na.rm = TRUE), 4),
             pct_moved = round(100 * mean(abs(d) > 1e-9, na.rm = TRUE), 1),
             cor = round(stats::cor(a, b, use = "complete.obs"), 5))
}))
print(res)

say("\n=== VERDICT ===")
moved <- res[pct_moved > 0, .N]
if (moved == 0) {
  say("  *** NO COLUMN MOVED. The rebuild did not apply the new logic. ***")
  say("  Investigate before trusting the published v9.")
} else {
  say("  ", moved, " of ", nrow(res), " columns moved -- the rebuild applied v9 logic.")
  say("  Correlations near 1 are EXPECTED and reassuring: this was a difficulty")
  say("  re-weighting, not a new rating system, so ranks should largely survive.")
}
