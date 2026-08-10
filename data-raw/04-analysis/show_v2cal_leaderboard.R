# Top 20 under the calibrated v2, with the sub-category split.
#
# Shows the latest available rating for each player under both arms, so the
# question "what did the calibration actually do to the leaderboard" has an
# answer rather than an inference. Production ranks are carried alongside.
#
# The channel names are v2's, and they mean what they say here (unlike v3, where
# epr_spoil holds aerial-contest value and epr_hitout holds stoppage value --
# those aliases do not apply to v2).
#
# ~1 min, cached frames only.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "v2cal_leaderboard.txt"), split = TRUE)
cat("=== Top 20 under calibrated v2 ===\nrun at", format(Sys.time()), "\n")

CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
latest <- function(f, label) {
  d <- as.data.table(read_parquet(file.path(OUT_DIR, f)))
  s <- max(d$season, na.rm = TRUE)
  d <- d[season == s]
  d <- d[, .SD[which.max(round)], by = player_id]
  keep <- c("player_id", "player_name", "position_group", "round",
            intersect(c("epr", "psr", "torp", CH), names(d)))
  d <- d[, keep, with = FALSE]
  setnames(d, setdiff(keep, c("player_id", "player_name", "position_group", "round")),
           paste0(setdiff(keep, c("player_id", "player_name", "position_group", "round")), "_", label))
  d[]
}
g <- latest("v2cal_rt_global.parquet", "prod")
p <- latest("v2cal_rt_percal.parquet", "cal")
cat("season", max(as.data.table(read_parquet(file.path(OUT_DIR, "v2cal_rt_percal.parquet")))$season,
                  na.rm = TRUE), "| players", nrow(p), "\n")

m <- merge(p, g[, .(player_id, epr_prod, torp_prod)], by = "player_id")
m <- m[is.finite(epr_cal)]
m[, `:=`(rk_cal = frank(-epr_cal), rk_prod = frank(-epr_prod))]
# `move` on the SOURCE table, not only inside the display copy -- the movers
# block sorts on it and died there.
m[, move := as.integer(rk_prod - rk_cal)]
setorder(m, rk_cal)

cat("\n=== TOP 20 by calibrated EPR, with sub-categories ===\n")
cat("(recv = receiving, disp = disposal, spoil = spoil/tackle, hitout = ruck)\n\n")
top <- m[1:20, .(rk = as.integer(rk_cal),
                 player = player_name,
                 pos = substr(position_group, 1, 14),
                 epr = round(epr_cal, 2),
                 recv = round(epr_recv_cal, 2),
                 disp = round(epr_disp_cal, 2),
                 spoil = round(epr_spoil_cal, 2),
                 hitout = round(epr_hitout_cal, 2),
                 was = as.integer(rk_prod),
                 move = as.integer(rk_prod - rk_cal))]
print(top, nrows = 25)

cat("\n=== WHAT THE CHANNELS CONTRIBUTE, top 20 vs everyone ===\n")
shr <- function(d, lbl) data.table(
  group = lbl,
  recv = round(mean(d$epr_recv_cal, na.rm = TRUE), 2),
  disp = round(mean(d$epr_disp_cal, na.rm = TRUE), 2),
  spoil = round(mean(d$epr_spoil_cal, na.rm = TRUE), 2),
  hitout = round(mean(d$epr_hitout_cal, na.rm = TRUE), 2))
print(rbind(shr(m[1:20], "top 20"), shr(m, "all players")))

cat("\n=== POSITION MIX ===\n")
pm <- merge(m[1:40, .N, by = position_group],
            m[1:20, .N, by = position_group], by = "position_group",
            all.x = TRUE, suffixes = c("_top40", "_top20"))
setnames(pm, c("N_top40", "N_top20"), c("in_top40", "in_top20"))
pm[is.na(in_top20), in_top20 := 0L]
print(pm[order(-in_top40)])

cat("\n=== BIGGEST MOVERS (production rank -> calibrated rank) ===\n")
mv <- m[rk_prod <= 100 | rk_cal <= 100]
setorder(mv, -move)
cat("\nrisen most:\n")
print(mv[1:8, .(player = player_name, pos = substr(position_group, 1, 14),
                was = as.integer(rk_prod), now = as.integer(rk_cal),
                spoil = round(epr_spoil_cal, 2), hitout = round(epr_hitout_cal, 2))])
setorder(mv, move)
cat("\nfallen most:\n")
print(mv[1:8, .(player = player_name, pos = substr(position_group, 1, 14),
                was = as.integer(rk_prod), now = as.integer(rk_cal),
                disp = round(epr_disp_cal, 2), recv = round(epr_recv_cal, 2))])

cat(sprintf("\nSpearman between the two leaderboards: %.4f | mean |rank change| %.1f of %d\n",
            cor(m$rk_cal, m$rk_prod, method = "spearman"),
            mean(abs(m$move)), nrow(m)))
cat("\nExpect risers to be contest- and ruck-heavy: the fitted scale lifts spoil\n")
cat("~3x and hitout ~4x, and fallers to be disposal-volume players, since disp\n")
cat("is scaled DOWN to ~0.50.\n")

saveRDS(m, file.path(OUT_DIR, "v2cal_leaderboard.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
