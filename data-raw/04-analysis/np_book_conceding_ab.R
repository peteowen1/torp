# Book the conceding side of every row: A/B on the 2026 season
# =============================================================================
# NP_BOOK_CONCEDING_SIDE (agreed with Pete 2026-09-23). One axis changes:
# .np_team_margin(book_conceding = FALSE) vs TRUE, on the SAME build_net_points()
# output, in one pass. Checks fixed before any result was looked at:
#   1. OFF reproduces the published step exactly (identical()).
#   2. Every team still sums to its own margin (asserted inside the function).
#   3. The anchor's share of absolute value falls from ~36% toward panna's level
#      (under 10%).
#   4. Player totals barely move, because the new pool and the old anchor are
#      both shared by time on ground: per player-game r > 0.95, season rank
#      correlation > 0.98. A big move means a bug, not a finding.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_book_conceding_ab.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")
SEASON <- 2026
MIN_GMS <- 8

source("data-raw/04-analysis/np_season_build.R")
.c <- np_season_build(SEASON)
pbp <- .c$pbp; ps <- .c$ps; res <- .c$res; np <- .c$np
say("season build (key ", .c$key, "): ", nrow(np), " player-matches, ",
    uniqueN(np$match_id), " matches")
stopifnot(nrow(np) > 5000, uniqueN(np$match_id) > 150)

off <- torp:::.np_team_margin(np, pbp, ps, res, book_conceding = FALSE)
on  <- torp:::.np_team_margin(np, pbp, ps, res, book_conceding = TRUE)

# 1. the limiting case. First run (2026-09-23, OFF then the default): OFF was
# identical() to the published step. Now the two arms must still agree exactly.
say("\n1. ON and OFF net_points identical: ",
    identical(as.data.table(off)$net_points, as.data.table(on)$net_points),
    "  (max |diff| ", signif(max(abs(as.data.table(off)$net_points - as.data.table(on)$net_points)), 3), ")")

part <- function(x) as.data.table(attr(x, "np_team_margin_parts"))
po <- part(off); pn <- part(on)
share_anchor <- function(p) sum(abs(p$recon)) / (sum(abs(p$named)) + sum(abs(p$share)) + sum(abs(p$recon)))
say("\n3. Anchor share of absolute value (player-match parts, same definition both arms):")
say("   off ", round(100 * share_anchor(po), 1), "%   on ", round(100 * share_anchor(pn), 1), "%")
say("   pool share: off ", round(100 * sum(abs(po$share)) / (sum(abs(po$named)) + sum(abs(po$share)) + sum(abs(po$recon))), 1),
    "%   on ", round(100 * sum(abs(pn$share)) / (sum(abs(pn$named)) + sum(abs(pn$share)) + sum(abs(pn$recon))), 1), "%")

a <- merge(as.data.table(off)[, .(match_id, player_id = as.character(player_id), off = net_points)],
           as.data.table(on)[, .(match_id, player_id = as.character(player_id), on = net_points)],
           by = c("match_id", "player_id"))
stopifnot(nrow(a) == nrow(off))
say("\n4. Player totals:  n = ", nrow(a), " player-matches")
say("   per player-game r = ", round(cor(a$off, a$on), 4),
    "   mean |change| = ", round(mean(abs(a$on - a$off)), 3), " pts   worst ", round(max(abs(a$on - a$off)), 2))
s <- a[, .(gms = .N, off = mean(off), on = mean(on)), by = player_id][gms >= MIN_GMS]
say("   season per-game (", nrow(s), " players, >= ", MIN_GMS, " games): r = ", round(cor(s$off, s$on), 4),
    ", rank r = ", round(cor(s$off, s$on, method = "spearman"), 4))
nm <- unique(pbp[!is.na(player_id), .(player_id = as.character(player_id), player_name)])
s <- merge(s, nm, by = "player_id", all.x = TRUE)
s[, move := on - off]
say("   biggest movers (pts/game):")
print(s[order(-abs(move))][1:10, .(player_name, gms, off = round(off, 2), on = round(on, 2), move = round(move, 2))])

# position: does the move land on one group (it should be small everywhere)
pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
a <- merge(a, unique(pg[, .(match_id, player_id, position_group)]), by = c("match_id", "player_id"), all.x = TRUE)
say("\n   mean change by position (pts/game; + = better off under booking):")
print(a[, .(n = .N, mean_move = round(mean(on - off), 3)), by = position_group][order(mean_move)])

say("\nverdict vs the pre-set checks: anchor under 10%? ", share_anchor(pn) < 0.10,
    " | player-game r > 0.95? ", cor(a$off, a$on) > 0.95,
    " | season rank r > 0.98? ", cor(s$off, s$on, method = "spearman") > 0.98)
