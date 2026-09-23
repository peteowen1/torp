# Do players keep more of their gains than their losses, and who benefits? (2026-09-23)
# =============================================================================
# Football found shooters kept 90% of a gain and 30% of a loss, which paid them
# +612.7 goals a season against -23.7 of real finishing (panna PR #266). This
# asks the same of AFL: on each player's OWN actions, the share of the row he
# keeps when the row gained vs when it lost. "Excused" = (share of gains - share
# of losses) x his losses: what team-mates carry of his losses beyond an even
# share. Per game, by position, 2026. Read-only.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_asymmetry_check.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")
source("data-raw/04-analysis/np_season_build.R")
b <- np_season_build(2026)
fin <- torp:::.np_team_margin(b$np, b$pbp, b$ps, b$res)
paid <- as.data.table(attr(fin, "np_team_margin_payments"))
prow <- as.data.table(attr(fin, "np_team_margin_pool_rows"))
say("pool rows cols: ", paste(names(prow), collapse = ", "))
for (d in list(paid, prow)) d[, match_id := as.character(match_id)]
stopifnot("display_order" %in% names(prow))
# the acting player and team on each row
act <- unique(as.data.table(b$pbp)[!is.na(player_id), .(match_id = as.character(match_id), display_order,
                                                        actor = as.character(player_id), act_team = team)],
              by = c("match_id", "display_order"))
side <- rbind(paid[, .(match_id, display_order, team, v = paid)],
              prow[, .(match_id, display_order, team, v = pool)])
v <- merge(side[, .(v = sum(v)), by = .(match_id, display_order, team)], act,
           by = c("match_id", "display_order"))[team == act_team]
own <- merge(v, paid[, .(kept = sum(paid)), by = .(match_id, display_order, player_id = as.character(player_id))],
             by.x = c("match_id", "display_order", "actor"), by.y = c("match_id", "display_order", "player_id"),
             all.x = TRUE)
own[is.na(kept), kept := 0]
say("rows with an acting player on the acting side: ", format(nrow(own), big.mark = ","),
    " | matched to the actor's pay: ", round(100 * mean(own$kept != 0), 1), "%")
pg <- as.data.table(load_player_game_ratings(2026))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
pos <- pg[, .N, by = .(player_id, pos = position_group)][order(player_id, -N)][, .SD[1], by = player_id]
gms <- pg[, .(gms = uniqueN(match_id)), by = player_id]
pl <- own[, .(gain = sum(v[v > 0]), loss = sum(v[v < 0]),
              kept_gain = sum(kept[v > 0]), kept_loss = sum(kept[v < 0])), by = .(player_id = actor)]
pl <- merge(merge(pl, pos[, .(player_id, pos)], by = "player_id"), gms, by = "player_id")[gms >= 5]
lg <- pl[, .(sg = sum(kept_gain) / sum(gain), sl = sum(kept_loss) / sum(loss))]
say("\nAFL 2026, players with 5+ games: ", nrow(pl), " | league share kept of own gains ",
    round(lg$sg, 3), ", of own losses ", round(lg$sl, 3))
out <- pl[, .(players = .N, gain_pg = sum(gain) / sum(gms), loss_pg = sum(loss) / sum(gms),
              kept_of_gains = sum(kept_gain) / sum(gain), kept_of_losses = sum(kept_loss) / sum(loss)), by = pos]
out[, excused_pg := (kept_of_gains - kept_of_losses) * -loss_pg]
print(out[order(-excused_pg)][, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
pl[, excused := (kept_gain / gain - kept_loss / loss) * -loss / gms]
say("\nmost excused per game (players):")
nm <- unique(as.data.table(b$pbp)[, .(player_id = as.character(player_id), player_name)], by = "player_id")
print(merge(pl, nm, by = "player_id")[order(-excused)][1:8, .(player_name, pos, gms,
      loss_pg = round(loss / gms, 2), kept_gain = round(kept_gain / gain, 2), kept_loss = round(kept_loss / loss, 2), excused = round(excused, 2))])
