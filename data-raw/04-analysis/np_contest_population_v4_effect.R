# What does the duel-evidence population do to published ratings? (#209)
# =============================================================================
# EPV3_CONTEST_POPULATION = "evidence" is Pete's rule, decided from real chain
# sequences on 2026-09-11: a contest is a kick where chains logged a target, or
# the outcome is self-evidently a duel (contested/pack mark, spoil, fumbled or
# dropped mark), plus a contest that drew a free kick. Plain receptions drop.
#
# It cuts the population 50,050 -> 17,493 and takes the worst calibration error
# in the 0.8-0.99 band from 13.6 points to 2.8.
#
# UNLIKE THE OTHER TWO CHANGES TODAY, THIS ONE IS NOT INERT UNDER v4. The
# contest path is reached from .np_difficulty_terms(), which OVERWRITES p_hat
# and splits the surprise into contest and ground terms on every contested row
# (epv_net_points.R:879). Change which rows are contested and you change the
# ledger. So it needs the position measurement before anyone considers flipping
# the default, and a vintage bump plus a full-history rebuild if it ever ships.
#
# The number that matters is the forward-defender gap, currently 4.474 a game
# after the #210 leak fix. For scale: that fix WIDENED it by 0.511, and the
# simulated named-loser change narrows it by 0.378.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_population_v4_effect.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
pg  <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
pos <- unique(pg[, .(match_id, player_id, position_group)])
ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
         "MEDIUM_DEFENDER", "KEY_DEFENDER")

run <- function(population) {
  assignInNamespace("EPV3_CONTEST_POPULATION", population, ns = "torp")
  tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
  say("  population ", population, ": ", format(nrow(tm), big.mark = ","),
      " terms, ", format(tm[contested == TRUE, .N], big.mark = ","), " contested (",
      round(100 * mean(tm$contested), 1), "%)")
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = tm,
                         return_payments = TRUE)
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  merge(fin[, .(match_id, player_id, net_points)], pos,
        by = c("match_id", "player_id"), all.x = TRUE)
}

say("=== baseline: population \"all\" (shipped) ===")
a <- run("all")
say("\n=== Pete's rule: population \"evidence\" ===")
b <- run("evidence")
assignInNamespace("EPV3_CONTEST_POPULATION", "all", ns = "torp")

sm <- function(d) d[!is.na(position_group),
                    .(mean_np = round(mean(net_points), 3)), by = position_group]
A <- sm(a); B <- sm(b)
cmp <- merge(A, B, by = "position_group", suffixes = c("_all", "_evidence"))
cmp[, change := round(mean_np_evidence - mean_np_all, 3)]
say("\n=== published net points a game, by position ===")
print(cmp[match(ORD, position_group)])

g <- function(x, col) x[position_group == "KEY_FORWARD"][[col]] -
                      x[position_group == "KEY_DEFENDER"][[col]]
g0 <- g(cmp, "mean_np_all"); g1 <- g(cmp, "mean_np_evidence")
say("\nKEY_FORWARD - KEY_DEFENDER gap: ", round(g0, 3), " -> ", round(g1, 3),
    "   (", if (g1 < g0) "NARROWER" else "WIDER", " by ", round(abs(g1 - g0), 3), ")")

say("\n=== how much does any individual move? ===")
m <- merge(a[, .(match_id, player_id, np_all = net_points)],
           b[, .(match_id, player_id, np_ev = net_points)],
           by = c("match_id", "player_id"))
m[, d := np_ev - np_all]
say("  player-games      : ", format(nrow(m), big.mark = ","))
say("  moved at all      : ", format(m[abs(d) > 1e-9, .N], big.mark = ","))
say("  mean |move|       : ", round(mean(abs(m$d)), 3))
say("  max  |move|       : ", round(max(abs(m$d)), 3))
say("  moving > 1 point  : ", format(m[abs(d) > 1, .N], big.mark = ","))

say("\n=== READ IT AGAINST THE OTHERS ===")
say("  #210 leak fix       WIDENED the gap by 0.511 (3.963 -> 4.474)")
say("  named-loser (sim)   narrows by 0.378")
say("  this change         ", if (g1 < g0) "narrows" else "widens", " by ", round(abs(g1 - g0), 3))
say("\nAnd the calibration win is the point regardless of the gap: 13.6 -> 2.8")
say("points of overconfidence in the 0.8-0.99 band. A better-specified model is")
say("worth having even if it moves no position average.")
