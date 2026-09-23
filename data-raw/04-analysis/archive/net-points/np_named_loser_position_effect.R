# What would charging the BEATEN TARGET do, by position? (#209, defender thread)
# =============================================================================
# Under v4, on a contest the defence wins:
#   the DEFENDER wins it and IS named and paid (role = contest_winner)
#   the beaten target is NOT named -- the KICKER wears the debit via own_hm
#
# Measured by position on the 5,349 defensive wins that carry a named target:
#   beaten target   56.7% KEY_FORWARD, 24.4% MEDIUM_FORWARD
#   winner          53.3% KEY_DEFENDER, 30.2% MEDIUM_DEFENDER
#
# So wiring loser_pid into v4 moves debit FROM THE KICKER TO THE BEATEN KEY
# FORWARD. That is a forward-blame change, not a defender fix -- the opposite of
# how I first described it, and Pete rightly asked to see the effect before any
# code ships.
#
# THIS SCRIPT CHANGES NO CODE. It simulates the transfer on the payment table:
# take each defensive-win contest with a named target, move `share` of the
# kicker's own_hm to that target, and re-aggregate per player-game. That is
# arithmetically what the wired version would do, without touching the engine.
#
# The question it answers is narrow and is the only one worth answering first:
#   does this narrow the FORWARD-DEFENDER gap, and by how much?
# Current gap after the #210 leak fix is 4.474 a game (KEY_FORWARD minus
# KEY_DEFENDER). If the answer is "barely", the feature is a correctness nicety
# and should be judged as one, not as part of the defender program.
#
# CAVEAT STATED UP FRONT: a simulation on the payment table is NOT the same as
# the wired engine. The team-margin rescale in .np_team_margin() renormalises
# each side to its own margin, so moving value between two players on the SAME
# team is preserved, but the rescale can still redistribute second-order. Treat
# the numbers as a good estimate of direction and rough size, not as the exact
# published effect.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_named_loser_position_effect.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))

# the named beaten target, with the i50 marker on (it is what makes the coverage)
assignInNamespace("EPV3_TARGET_FROM_I50", TRUE, ns = "torp")
cst <- as.data.table(build_aerial_contests(ch, pbp))
assignInNamespace("EPV3_TARGET_FROM_I50", FALSE, ns = "torp")
losers <- cst[def_win == TRUE & !is.na(target_pid),
              .(match_id = as.character(match_id), display_order = kick_do,
                loser_pid = as.character(target_pid))]
say("defensive wins with a named beaten target: ", format(nrow(losers), big.mark = ","))

np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                       stoppages = "allocate", difficulty_terms = tm,
                       return_payments = TRUE)
pay <- as.data.table(attr(np, "np_payments"))
pay[, match_id := as.character(match_id)]

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
pos <- unique(pg[, .(match_id, player_id, position_group)])
ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
         "MEDIUM_DEFENDER", "KEY_DEFENDER")

# Baseline published numbers, through the real rescale.
base <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
base[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
# `home_away` comes too, and it is NOT optional. The payment table's `hm` is in
# the HOME-MARGIN frame; `net_points` is in each player's OWN frame. Adding one
# to the other without flipping gives every away player the wrong sign, and the
# first run of this script did exactly that -- key forwards came out GAINING
# 0.025 from being charged a debit, which is the tell. Frame bugs hide on the
# home team because the transform there is the identity.
base <- merge(base[, .(match_id, player_id, home_away, net_points)], pos,
              by = c("match_id", "player_id"), all.x = TRUE)
base[, sgn := fifelse(home_away == "Home", 1, -1)]

say("\n=== BASELINE: published net points a game, by position ===")
b <- base[!is.na(position_group), .(n = .N, mean_np = round(mean(net_points), 3)),
          by = position_group]
print(b[match(ORD, position_group)])
gap0 <- b[position_group == "KEY_FORWARD", mean_np] -
        b[position_group == "KEY_DEFENDER", mean_np]
say("  KEY_FORWARD - KEY_DEFENDER gap: ", round(gap0, 3))

say("\n=== SIMULATED: move `share` of the kicker's debit to the beaten target ===")
say("actor payments on those contests are the kicker's own_hm.")
act <- pay[role == "actor"]
hit <- merge(act, losers, by = c("match_id", "display_order"))
say("  actor payments on named-loser contests: ", format(nrow(hit), big.mark = ","))
say("  total |value| on them                 : ", round(sum(abs(hit$hm)), 1), " points")

for (share in c(0.5, 1.0)) {
  moved <- hit[, .(match_id, from = player_id, to = loser_pid, team,
                   amt = hm * share)]
  # subtract from the kicker, add to the beaten target
  delta <- rbindlist(list(
    moved[, .(match_id, player_id = from, d = -amt)],
    moved[, .(match_id, player_id = to,   d =  amt)]))
  delta <- delta[, .(d = sum(d)), by = .(match_id, player_id)]
  sim <- merge(base, delta, by = c("match_id", "player_id"), all.x = TRUE)
  sim[is.na(d), d := 0]
  # flip the home-frame transfer into each player's own frame before adding
  sim[, np_new := net_points + d * sgn]
  s <- sim[!is.na(position_group), .(mean_np = round(mean(np_new), 3),
                                     change = round(mean(np_new - net_points), 3)),
           by = position_group]
  say("\n--- share = ", share, " ---")
  print(s[match(ORD, position_group)])
  g <- s[position_group == "KEY_FORWARD", mean_np] -
       s[position_group == "KEY_DEFENDER", mean_np]
  say("  KEY_FORWARD - KEY_DEFENDER gap: ", round(gap0, 3), " -> ", round(g, 3),
      "   (", if (g < gap0) "NARROWER" else "WIDER", " by ", round(abs(g - gap0), 3), ")")
}

say("\n=== READ IT AGAINST WHAT ELSE IS ON THE TABLE ===")
say("The #210 leak fix moved key defenders -2.107 -> -2.432 and WIDENED this gap")
say("3.963 -> 4.474. The defender program's shipped win (position-variance")
say("standardisation) moved KD spread 1.40 -> 1.60. If the number above is small")
say("next to those, this is a correctness nicety and should be judged as one.")
