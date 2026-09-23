# Football's pool rule on AFL: credit by defensive acts, blame evenly
# =============================================================================
# panna splits its defensive pool's CREDIT half 50% flat + 50% by each player's
# share of defensive acts, and its blame half flat. torp splits one netted pool
# by time on ground. Now that every row books both sides
# (NP_BOOK_CONCEDING_SIDE), the pool rows carry their sign, so the two halves
# can be separated. This measures what panna's rule would do on 2026 WITHOUT
# changing the package: it re-spreads the pool rows post hoc.
#
# One axis: the pool's split. Named payments and the anchor are unchanged. Every
# team still sums to its margin by construction (the pool's team total is not
# touched, only who gets it), and that is asserted.
#
# Question asked 2026-09-23: does it close the key forward (+1.6) / key
# defender (-1.8) gap? Prior, from the five-group split: the whole pool accounts
# for only +0.39 of a 3.47 gap, so it cannot close it; the question is how much.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_pool_dacts_ab.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")
SEASON <- 2026; MIN_GMS <- 8; DACTS_SHARE <- 0.5

source("data-raw/04-analysis/np_season_build.R")
b <- np_season_build(SEASON)
fin <- torp:::.np_team_margin(b$np, b$pbp, b$ps, b$res)
pr <- as.data.table(attr(fin, "np_team_margin_pool_rows"))
parts <- as.data.table(attr(fin, "np_team_margin_parts"))
fin <- as.data.table(fin)
fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
pr[, match_id := as.character(match_id)]

# DEFENCE entries: the pool row belongs to the side that did not have the ball.
act <- unique(b$pbp[!is.na(team), .(match_id, display_order, acting = as.character(team))])
pr <- merge(pr, act, by = c("match_id", "display_order"), all.x = TRUE)
pr[, def_credit := !is.na(acting) & team != acting & pool > 0]
tp <- pr[, .(dcred = sum(pool[def_credit]), other = sum(pool[!def_credit])), by = .(match_id, team)]
say("pool rows ", nrow(pr), " | defensive credit ", round(sum(tp$dcred)), " pts | everything else ",
    round(sum(tp$other)), " pts (season)")

ps <- b$ps
dz <- function(x) pmax(dplyr::coalesce(as.numeric(x), 0), 0)
lu <- ps[, .(match_id = as.character(match_id), player_id = as.character(player_id),
             tog = pmax(time_on_ground_percentage, 1) / 100,
             dacts = dz(tackles) + dz(intercepts) + dz(one_percenters))]
lu <- merge(lu, unique(fin[, .(match_id, player_id, team)]), by = c("match_id", "player_id"))
lu[, `:=`(w_tog = tog / sum(tog), w_d = if (sum(dacts) > 0) dacts / sum(dacts) else tog / sum(tog)),
   by = .(match_id, team)]
lu <- merge(lu, tp, by = c("match_id", "team"), all.x = TRUE)
lu[is.na(dcred), dcred := 0][is.na(other), other := 0]
# old: everything by time on ground. new: defensive credit half flat-by-tog,
# half by defensive acts; blame and everything else by time on ground.
lu[, move := dcred * DACTS_SHARE * (w_d - w_tog)]
stopifnot(abs(sum(lu$move)) < 1e-6)                                 # pure reallocation
chk <- lu[, .(m = sum(move)), by = .(match_id, team)]
stopifnot(max(abs(chk$m)) < 1e-9)                                   # every team still sums

a <- merge(fin[, .(match_id, player_id, net = net_points)], lu[, .(match_id, player_id, move)],
           by = c("match_id", "player_id"), all.x = TRUE)
a[is.na(move), move := 0][, net_new := net + move]
pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
tmf <- pg[, .N, by = .(player_id, pos = position_group)][order(player_id, -N, pos)][, .SD[1], by = player_id]
s <- a[, .(gms = .N, old = mean(net), new = mean(net_new), move = mean(move)), by = player_id][gms >= MIN_GMS]
s <- merge(s, tmf[, .(player_id, pos)], by = "player_id", all.x = TRUE)
say("\nplayers >= ", MIN_GMS, " games: ", nrow(s), "; per player-game r(old,new) = ",
    round(cor(a$net, a$net_new), 4), ", season rank r = ", round(cor(s$old, s$new, method = "spearman"), 4))
say("\nmean per game by position (old -> new, move):")
print(s[, .(n = .N, old = round(mean(old), 3), new = round(mean(new), 3), move = round(mean(move), 3),
            sd_move = round(sd(move), 3)), by = pos][order(old)])
g <- s[, .(v = mean(old), w = mean(new)), by = pos]
say("\nkey forward - key defender gap: ", round(g[pos == "KEY_FORWARD", v] - g[pos == "KEY_DEFENDER", v], 3),
    " -> ", round(g[pos == "KEY_FORWARD", w] - g[pos == "KEY_DEFENDER", w], 3))
nm <- unique(b$pbp[!is.na(player_id), .(player_id = as.character(player_id), player_name)])
s <- merge(s, nm, by = "player_id", all.x = TRUE)
say("\nbiggest movers:")
print(s[order(-abs(move))][1:10, .(player_name, pos, gms, old = round(old, 2), new = round(new, 2), move = round(move, 2))])
