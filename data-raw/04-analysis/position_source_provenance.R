# O1 follow-up (plan §7.14) — Pete's two challenges, 2026-07-27.
#
# (1) "Aren't the 20-way lineup mapping and the 6-tier position coming from
#     different datasets -- one is how the team says they line up each week and
#     can change weekly, the other is Champion Data's label. Does THAT change
#     weekly or is it set for the season?"
#
#     This matters because §7.13 retracted my CHB flag on the strength of the
#     clubs' "listed position". If that label is a SEASON-LEVEL PLAYER attribute
#     rather than a weekly ROLE observation, then listed_modal per lineup code
#     measures "what kind of player gets named at CHB", not "what kind of post
#     CHB is" -- and it is near-circular with height. The retraction would then
#     rest on weak evidence.
#
# (2) "I think CHB and FB are both key defenders."
#
#     Tested here on the only basis that is neither a label nor a body
#     measurement: what players named at each code actually DO. If CHB's
#     statistical profile sits with FB, Pete is right and §7.13's retraction is
#     itself retracted.
suppressMessages({library(arrow); library(data.table)})

D  <- "C:/dev/torpverse/torpdata/data/"
SF <- 2021:2026
DS <- 2021:2025
rd <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(D, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names = TRUE, fill = TRUE)

# ============ (1) provenance: which sources vary, and how often? ============
cat("=== (1) DO THE THREE POSITION SOURCES VARY WITHIN A SEASON? ===\n\n")

det <- rd("player_details_%d.parquet", DS)[, .(player_id, season, listed = position)]
cat(sprintf("player_details rows: %s | unique player-seasons: %s\n",
            format(nrow(det), big.mark=","),
            format(uniqueN(det[, .(player_id, season)]), big.mark=",")))
cat("  -> one row per player-season, so `position` is a SEASON-LEVEL label.\n")
ds <- det[, .(n_distinct = uniqueN(listed)), by = .(player_id, season)]
cat(sprintf("  within-season variation: %.1f%% of player-seasons have >1 value\n",
            100*mean(ds$n_distinct > 1)))
dc <- det[, .(n_distinct = uniqueN(listed), n_seasons = uniqueN(season)), by = player_id]
cat(sprintf("  across seasons: %.1f%% of players with 2+ seasons ever change label\n\n",
            100*mean(dc[n_seasons >= 2, n_distinct > 1])))

pg <- rd("player_game_%d.parquet", SF)[, .(player_id, season, round = as.numeric(round),
        position_group, lineup_position)]
pg <- pg[!is.na(lineup_position) & !lineup_position %in% c("INT","EMERG","SUB")]

lpv <- pg[, .(n = uniqueN(lineup_position)), by = .(player_id, season)]
pgv <- pg[, .(n = uniqueN(position_group)),  by = .(player_id, season)]
cat(sprintf("lineup_position (team sheet, 20-way): %.1f%% of player-seasons have >1 value (mean %.2f)\n",
            100*mean(lpv$n > 1), mean(lpv$n)))
cat(sprintf("position_group  (PBP-derived, 6-way): %.1f%% of player-seasons have >1 value (mean %.2f)\n",
            100*mean(pgv$n > 1), mean(pgv$n)))

cat("\nVERDICT on (1):\n")
cat("  lineup_position = weekly team-sheet ROLE, varies within a season.\n")
cat("  position_group  = PBP-derived, also varies week to week.\n")
cat("  player_details.position = SEASON-LEVEL PLAYER LABEL, one per player-season.\n")
cat("  So §7.13's 'listed by club' column describes the PLAYER, not the POST --\n")
cat("  weaker evidence about a lineup code than I treated it as.\n")

# ============ (2) what do players at each code actually DO? =================
cat("\n\n=== (2) STATISTICAL PROFILE OF EACH LINEUP CODE ===\n")
cat("Neither a label nor a body measurement -- what actually happens on field.\n\n")

sr <- rd("player_stat_ratings_%d.parquet", SF); sr[, round := as.numeric(round)]
# discriminating stats: the things a key defensive post does vs a rebounding one
WANT <- c("intercept_marks","intercepts","one_percenters","spoils","rebound50s",
          "contest_def_one_on_ones","disposals","uncontested_possessions",
          "inside50s","marks_inside50","goals","score_involvements")
cols <- paste0(WANT, "_raw")
cols <- cols[cols %in% names(sr)]
cat("stats used:", paste(sub("_raw$","",cols), collapse=", "), "\n\n")

m <- merge(pg, sr[, c("player_id","season","round", cols), with = FALSE],
           by = c("player_id","season","round"))
prof <- m[, lapply(.SD, mean, na.rm = TRUE), by = lineup_position, .SDcols = cols]
setnames(prof, cols, sub("_raw$", "", cols))

sc <- copy(prof); nm <- setdiff(names(sc), "lineup_position")
for (cc in nm) sc[[cc]] <- as.numeric(scale(sc[[cc]]))   # z across the 18 codes

cat("--- per-code means (raw scale) ---\n")
print(prof[order(-get(sub("_raw$","",cols[1])))], nrows = 30)

cat("\n--- how far is each defensive code from FB, in profile space? ---\n")
defc <- c("FB","CHB","BPL","BPR","HBFL","HBFR")
sub_ <- sc[lineup_position %in% defc]
mat <- as.matrix(sub_[, ..nm]); rownames(mat) <- sub_$lineup_position
fb <- mat["FB", ]
dist_fb <- sort(sqrt(rowSums((mat - matrix(fb, nrow(mat), ncol(mat), byrow=TRUE))^2)))
for (i in seq_along(dist_fb))
  cat(sprintf("  %-5s  euclidean distance from FB = %.2f\n", names(dist_fb)[i], dist_fb[i]))

cat("\n--- hierarchical clustering of ALL 18 codes on profile ---\n")
mm <- as.matrix(sc[, ..nm]); rownames(mm) <- sc$lineup_position
hc <- hclust(dist(mm), method = "ward.D2")
print(hc$labels[hc$order])
cat("\ncut into 6 clusters:\n")
k6 <- cutree(hc, 6)
for (g in sort(unique(k6))) cat(sprintf("  cluster %d: %s\n", g,
                                 paste(names(k6)[k6 == g], collapse = ", ")))
cat("\ncut into 9 clusters:\n")
k9 <- cutree(hc, 9)
for (g in sort(unique(k9))) cat(sprintf("  cluster %d: %s\n", g,
                                 paste(names(k9)[k9 == g], collapse = ", ")))
