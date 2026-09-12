# Does NP_TEAM_MARGIN_POOL_BY move the position balance? (defender program)
# =============================================================================
# The decomposition (np_position_balance_levers.R) found the forward-defender
# gap is 4.273 a game and that 3.543 of it is EARNED -- key forwards' own acts
# are worth +4.079 and key defenders' -1.972, which is marking near goal versus
# rebounding out of defensive 50. That part is football, not allocation, and no
# re-split touches it.
#
# The one ALLOCATED lever of any size is np_team, the offence pool: 0.814 of the
# gap, 19%. Key defenders are charged -6.433 against forwards' -5.619, and per
# 100 TOG that is -643 against -562. The reason is this constant:
#
#   lu[, w := if (identical(NP_TEAM_MARGIN_POOL_BY, "tog")) tog else pmax(dacts, 0.5)]
#
# The pool is spread by DEFENSIVE ACTS, so the side's offence pool is charged in
# proportion to how much defending a player did. Defenders do the most, so they
# carry the most. That is worth questioning on its own terms.
#
# HOW IT WAS CHOSEN, and why this is not a re-litigation: "dacts" was picked on
# REPEATABILITY -- 0.606 against tog's 0.598, per its own docstring. Position
# balance was never the criterion. So this sweep asks a question the original
# choice did not, and a 0.008 repeatability edge is a thin reason to keep a
# split that charges defenders most if it costs real balance.
#
# Two arms, everything else identical. Measured, not shipped.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_pool_by_position_sweep.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
meta <- unique(pg[, .(match_id, player_id, position_group, tog)])
ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
         "MEDIUM_DEFENDER", "KEY_DEFENDER")

run <- function(by) {
  assignInNamespace("NP_TEAM_MARGIN_POOL_BY", by, ns = "torp")
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = tm,
                         return_payments = TRUE)
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  d <- merge(fin[, .(match_id, player_id, net_points, np_team)], meta,
             by = c("match_id", "player_id"), all.x = TRUE)
  d[!is.na(position_group)]
}

say("=== arm: dacts (shipped) ===")
a <- run("dacts")
say("=== arm: tog ===")
b <- run("tog")
assignInNamespace("NP_TEAM_MARGIN_POOL_BY", "dacts", ns = "torp")

sm <- function(d, nm) {
  x <- d[, .(net = round(mean(net_points), 3), pool = round(mean(np_team), 3)),
         by = position_group]
  setnames(x, c("net", "pool"), paste0(c("net_", "pool_"), nm))
}
t <- merge(sm(a, "dacts"), sm(b, "tog"), by = "position_group")
t <- t[match(ORD, position_group)]
t[, net_change := round(net_tog - net_dacts, 3)]
say("\n=== published net points a game, by position ===")
say("net_* is the published figure; pool_* is that position's np_team share.")
print(t)

g <- function(col) t[position_group == "KEY_FORWARD"][[col]] -
                   t[position_group == "KEY_DEFENDER"][[col]]
g0 <- g("net_dacts"); g1 <- g("net_tog")
say("\nKEY_FORWARD - KEY_DEFENDER gap: ", round(g0, 3), " -> ", round(g1, 3),
    "   (", if (g1 < g0) "NARROWER" else "WIDER", " by ", round(abs(g1 - g0), 3), ")")

say("\n=== how much does any individual move? ===")
m <- merge(a[, .(match_id, player_id, x = net_points)],
           b[, .(match_id, player_id, y = net_points)], by = c("match_id", "player_id"))
m[, d := y - x]
say("  player-games     : ", format(nrow(m), big.mark = ","))
say("  moved            : ", format(m[abs(d) > 1e-9, .N], big.mark = ","))
say("  mean |move|      : ", round(mean(abs(m$d)), 3))
say("  max  |move|      : ", round(max(abs(m$d)), 3))
say("  moving > 1 point : ", format(m[abs(d) > 1, .N], big.mark = ","))

say("\n=== READ IT AGAINST WHAT IS AVAILABLE ===")
say("  the whole gap                    4.273")
say("  the EARNED part (untouchable)    3.543")
say("  the ALLOCATED part (all pools)   0.731")
say("  this lever delivers              ", round(abs(g1 - g0), 3))
say("\nA lever that moves less than the allocated part is not failing -- the")
say("allocated part is the ceiling. What matters is whether it buys enough to")
say("justify losing the 0.606 -> 0.598 repeatability edge that chose dacts.")
