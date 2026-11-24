# torp#296: does EPV double-count the chain-boundary repeat rows?
# The AFL feed lists the event at every chain boundary twice: last row of one
# chain, first row of the next (same action, player, team, second). Flag rule
# copied from torpdata/scripts/build_afl_chains.R (`repeat_of_prev`).
#
# Anchors, set before running:
#   B1  ~5.5% of chains rows are repeats (torpdata's comment; agent: 5.55% on 2024)
#   B2  original and repeat have the same player and team in every pair
#   B3  collapsing (repeat's delta_epv folded into its original, repeat
#       dropped) leaves each match's delta_epv sum unchanged
#   B4  both builds still sum to each team's margin
# Agent result on 2024 to compare against: player-season totals mean |change|
# 0.12, max 0.74, correlation 0.99998.
#
# Usage: powershell.exe -Command 'Rscript data-raw/04-analysis/measure_repeat_rows_296.R'
#   env SEASON (default 2026), OUT_DIR (default tempdir()).
suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
options(torp.local_data_dir = NA)
s <- as.integer(Sys.getenv("SEASON", "2026"))
out_dir <- Sys.getenv("OUT_DIR", tempdir())
say <- function(...) cat(sprintf(...), "\n", sep = "")
# Match what is published today (v17: in-sample difficulty fit), so the size
# is the size of this change alone.
ns <- asNamespace("torp")
if (exists("NP_DIFFICULTY_FIT", envir = ns)) {
  unlockBinding("NP_DIFFICULTY_FIT", ns); assign("NP_DIFFICULTY_FIT", "in_sample", envir = ns)
}

pbp <- as.data.table(load_pbp(s, rounds = TRUE))
ch <- as.data.table(load_chains(s, rounds = TRUE))
pstats <- load_player_stats(s); teams <- load_teams(s)
say("%d: %d pbp rows, %d chains rows, %d matches", s, nrow(pbp), nrow(ch), uniqueN(pbp$match_id))

same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
setorder(ch, match_id, display_order)
ch[, repeat_of_prev := chain_number != shift(chain_number) &
     same(description, shift(description)) & period == shift(period) &
     same(period_seconds, shift(period_seconds)) &
     same(team_id, shift(team_id)) & same(player_id, shift(player_id)), by = match_id]
ch[is.na(repeat_of_prev), repeat_of_prev := FALSE]
say("B1 repeats: %d of %d chains rows (%.2f%%)", sum(ch$repeat_of_prev), nrow(ch),
    100 * mean(ch$repeat_of_prev))
print(ch[repeat_of_prev == TRUE, .N, by = description][order(-N)])

# map to pbp rows; pair each repeat with the pbp row before it
pbp[, display_order := as.integer(display_order)]
ch[, display_order := as.integer(display_order)]
pbp <- merge(pbp, ch[, .(match_id, display_order, repeat_of_prev)],
             by = c("match_id", "display_order"), all.x = TRUE, sort = FALSE)
pbp[is.na(repeat_of_prev), repeat_of_prev := FALSE]
setorder(pbp, match_id, display_order)
pbp[, `:=`(prev_player = shift(player_id), prev_team = shift(team_id),
           prev_desc = shift(description), prev_delta = shift(delta_epv),
           prev_xp = shift(exp_pts), prev_do = shift(display_order)), by = match_id]
rp <- pbp[repeat_of_prev == TRUE]
say("repeats that are pbp rows: %d; previous pbp row is the original (same do-1 desc) in %d",
    nrow(rp), sum(rp$prev_desc == rp$description, na.rm = TRUE))
say("B2 same player %d / same team %d of %d pairs", sum(same(rp$player_id, rp$prev_player)),
    sum(same(rp$team_id, rp$prev_team)), nrow(rp))
say("exp_pts: original vs repeat, mean |diff| %.3f, share identical %.3f",
    mean(abs(rp$exp_pts - rp$prev_xp), na.rm = TRUE), mean(abs(rp$exp_pts - rp$prev_xp) < 1e-9, na.rm = TRUE))
say("delta_epv: repeat rows mean |d| %.3f, sum %.1f; their originals mean |d| %.3f; other rows mean |d| %.3f",
    mean(abs(rp$delta_epv), na.rm = TRUE), sum(rp$delta_epv, na.rm = TRUE),
    mean(abs(rp$prev_delta), na.rm = TRUE),
    mean(abs(pbp[repeat_of_prev == FALSE, delta_epv]), na.rm = TRUE))

# three traced pairs: original, repeat, and the row after
set.seed(296)
show <- rp[!is.na(delta_epv)][sample(.N, 3)]
for (i in seq_len(nrow(show))) {
  w <- pbp[match_id == show$match_id[i] & display_order >= show$prev_do[i] - 1 &
             display_order <= show$display_order[i] + 1,
           .(display_order, chain_number, period, period_seconds, team = team_id,
             player = player_name, description, exp_pts = round(exp_pts, 3),
             delta_epv = round(delta_epv, 3), repeat_of_prev)]
  say("\ntrace %d: %s", i, show$match_id[i]); print(w)
}

# collapse: fold each repeat's delta into its original, drop the repeat
col <- copy(pbp)
col[, fold := shift(fifelse(repeat_of_prev & !is.na(delta_epv), delta_epv, 0), type = "lead", fill = 0),
    by = match_id]
col[, delta_epv := delta_epv + fold]
col <- col[repeat_of_prev == FALSE]
chk <- merge(pbp[, .(a = sum(delta_epv, na.rm = TRUE)), by = match_id],
             col[, .(b = sum(delta_epv, na.rm = TRUE)), by = match_id], by = "match_id")
say("\nB3 max |match delta_epv sum change| = %.3g over %d matches", max(abs(chk$a - chk$b)), nrow(chk))

drop_cols <- c("repeat_of_prev", "prev_player", "prev_team", "prev_desc", "prev_delta",
               "prev_xp", "prev_do", "fold")
full <- as.data.table(create_player_game_data(pbp[, !drop_cols[1:7], with = FALSE], pstats, teams, chains = ch[, !"repeat_of_prev"]))
coll <- as.data.table(create_player_game_data(col[, !drop_cols, with = FALSE], pstats, teams, chains = ch[, !"repeat_of_prev"]))

res <- .np_match_results(pbp, NULL)
r <- rbind(res[, .(match_id, team = home_team_name, margin = home_score - away_score)],
           res[, .(match_id, team = away_team_name, margin = away_score - home_score)])
for (nm in c("full", "coll")) {
  j <- merge(get(nm)[, .(np = sum(net_points)), by = .(match_id, team)], r, by = c("match_id", "team"))
  say("B4 %s: %d team-matches, max |sum - margin| %.3g", nm, nrow(j), max(abs(j$np - j$margin)))
}
d <- merge(full[, .(player_id, match_id, player_name, np_full = net_points)],
           coll[, .(player_id, match_id, np_coll = net_points)], by = c("player_id", "match_id"))
say("player-matches %d: mean |chg| %.3f, max %.2f, cor %.6f", nrow(d),
    mean(abs(d$np_coll - d$np_full)), max(abs(d$np_coll - d$np_full)), cor(d$np_full, d$np_coll))
sea <- d[, .(games = .N, full = sum(np_full), coll = sum(np_coll)), by = .(player_id, player_name)]
say("player-season totals, %d players: mean |chg| %.3f, max %.2f, cor %.6f, spearman %.6f, sd of totals %.1f",
    nrow(sea), mean(abs(sea$coll - sea$full)), max(abs(sea$coll - sea$full)),
    cor(sea$full, sea$coll), cor(sea$full, sea$coll, method = "spearman"), sd(sea$full))
say("largest movers:")
print(sea[order(-abs(coll - full))][1:6, .(player_name, games, full = round(full, 2), coll = round(coll, 2))])
say("top 5 by total:")
print(sea[order(-full)][1:5, .(player_name, games, full = round(full, 2), coll = round(coll, 2))])
fwrite(sea, file.path(out_dir, sprintf("np296_season_%d.csv", s)))
