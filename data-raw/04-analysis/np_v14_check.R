# v14 check: the shipped pool split, on the real code path, two seasons
# =============================================================================
# NP_POOL_DACTS_SHARE 0 vs 0.5 (NP_BOOK_CONCEDING_SIDE on in both), through
# .np_team_margin() itself rather than np_pool_dacts_ab.R's post-hoc re-spread.
#   1. 2026 must reproduce the post-hoc key forward / key defender gap (0.797).
#   2. Season-to-season repeatability, 2025 -> 2026, per player net points per
#      game, both arms. panna's version of this rule cost 0.658 -> 0.525.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_v14_check.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")
MIN_GMS <- 8
source("data-raw/04-analysis/np_season_build.R")
shipped <- NP_POOL_DACTS_SHARE

arm <- function(b, k) {
  x <- as.data.table(torp:::.np_team_margin(b$np, b$pbp, b$ps, b$res, dacts_share = k))
  x[, .(match_id = as.character(match_id), player_id = as.character(player_id), net_points)]
}
per_player <- function(season) {
  b <- np_season_build(season)
  a <- merge(arm(b, 0)[, .(match_id, player_id, old = net_points)],
             arm(b, shipped)[, .(match_id, player_id, new = net_points)], by = c("match_id", "player_id"))
  s <- a[, .(gms = .N, old = mean(old), new = mean(new)), by = player_id][gms >= MIN_GMS]
  pg <- as.data.table(load_player_game_ratings(season))
  pg[, player_id := as.character(player_id)]
  pos <- pg[, .N, by = .(player_id, pos = position_group)][order(player_id, -N, pos)][, .SD[1], by = player_id]
  s <- merge(s, pos[, .(player_id, pos)], by = "player_id", all.x = TRUE)
  g <- s[, .(old = mean(old), new = mean(new)), by = pos]
  say(season, ": ", uniqueN(a$match_id), " matches, ", nrow(s), " players >= ", MIN_GMS, " games | KF - KD gap ",
      round(g[pos == "KEY_FORWARD", old] - g[pos == "KEY_DEFENDER", old], 3), " -> ",
      round(g[pos == "KEY_FORWARD", new] - g[pos == "KEY_DEFENDER", new], 3))
  s[, season := season]
}
s25 <- per_player(2025); s26 <- per_player(2026)
stopifnot(nrow(s25) > 300, nrow(s26) > 300)

y <- merge(s25[, .(player_id, old25 = old, new25 = new)], s26[, .(player_id, old26 = old, new26 = new)],
           by = "player_id")
say("\nRepeatability, players >= ", MIN_GMS, " games in both seasons (n = ", nrow(y), "):")
say("   time on ground only (k = 0):   r = ", round(cor(y$old25, y$old26), 3))
say("   shipped (k = ", shipped, "):          r = ", round(cor(y$new25, y$new26), 3))

# Position is itself repeatable (a key forward is a key forward next year), so a
# rule that paid by position scores well on the line above for that reason
# alone. Within-position repeatability -- each season centred on its position
# mean -- asks whether the PLAYER signal survived.
y <- merge(y, s26[, .(player_id, pos)], by = "player_id")
for (v in c("old25", "new25", "old26", "new26")) y[, (v) := get(v) - mean(get(v)), by = pos]
say("Within position (each season centred on its position mean):")
say("   time on ground only: r = ", round(cor(y$old25, y$old26), 3),
    "   shipped: r = ", round(cor(y$new25, y$new26), 3))
