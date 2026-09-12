# How big a lever is NP_CONTEST_WINNER_SHARE, before sweeping it?
# =============================================================================
# The constant routes a ceded contest surprise between the NAMED winner and the
# defending team's pool: the winner takes `rho`, the pool takes `1 - rho`.
# Keyed by outcome -- 0.80 on a mark, 0.50 on a spoil -- and its own docstring
# says those were "chosen as the defaults for the year-over-year test, not as
# results", so it is genuinely untuned.
#
# SIZE IT BEFORE SWEEPING IT. A grid of full rebuilds is hours; the derivative
# is one run plus arithmetic, and the effect-sizing rule says a lever worth a
# hundredth of the target is a correctness note, not a lever.
#
# The arithmetic. For a position P, raising rho by d moves
#     + d * (contest value P wins as the NAMED winner)
#     - d * (P's share of the pool that value would otherwise have gone to)
# so the net derivative is (named credit) - (pool share of the same money), and
# the position gap moves by the difference between two positions' derivatives.
# That is exact for the ledger, not an approximation, because rho enters
# linearly and nothing else depends on it.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_winner_share_lever.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
         "MEDIUM_DEFENDER", "KEY_DEFENDER")

pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
say("RATING_VINTAGE ", RATING_VINTAGE, " | EPV3_CONTEST_OUTCOMES ",
    EPV3_CONTEST_OUTCOMES, " | winner share: mark ",
    NP_CONTEST_WINNER_SHARE[["Contested Mark"]], ", spoil ",
    NP_CONTEST_WINNER_SHARE[["Spoil"]])

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
pos <- pg[!is.na(position_group), .N, by = .(player_id, position_group)]
setorder(pos, player_id, -N, position_group)
pos <- pos[, .SD[1], by = player_id][, .(player_id, position_group)]
gcount <- pg[!is.na(position_group), .(player_games = .N), by = position_group]

tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
np <- as.data.table(build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                                     stoppages = "allocate", difficulty_terms = tm))
np[, player_id := as.character(player_id)]
np <- merge(np, pos, by = "player_id", all.x = TRUE)

# USE THE FINAL LEDGER COLUMNS, not the payments table. Two reasons, both found
# by a first version of this script returning NaN:
#
#   1. Pool rows carry player_id = NA in the payments ledger -- they are spread
#      to players later by .np_defensive_pool() -- so the pool side cannot be
#      attributed there at all, and the division by an empty total gave NaN.
#   2. `role == "contest_winner"` covers BOTH a same-team winner (win_hm, which
#      rho does NOT touch) and a defensive cession (cede_c_hm * rho, which it
#      does). Scaling the combined figure by rho would scale money the constant
#      has no effect on.
#
# np_contest_won is exactly `sum(cede_c_hm * rho)` (epv_net_points.R:1382) and
# np_defensive is the pool, both per player. Those are the two sides.
say("\n=== how much value does this constant actually route? ===")
say("Expected points across ", SEASON, ". np_contest_won is the rho-scaled")
say("share paid to a NAMED defensive contest winner; np_defensive is the pool")
say("the rest of it falls into.")
say("  np_contest_won : ", format(round(sum(abs(np$np_contest_won), na.rm = TRUE), 0), big.mark = ","))
say("  np_defensive   : ", format(round(sum(abs(np$np_defensive), na.rm = TRUE), 0), big.mark = ","))

say("\n=== the derivative: points a game per +0.10 of winner share ===")
say("rho enters linearly, so d(np_contest_won)/drho = np_contest_won / rho and")
say("the same money leaves the pool. d_named is what a position gains, d_pool")
say("what it gives up, net is the sum. Positive means that position benefits")
say("from a HIGHER winner share. Uses the spoil share (", NP_CONTEST_WINNER_SHARE[["Spoil"]],
    ") as the")
say("scale, since spoils are the bulk of defensive contest wins.")
RHO <- NP_CONTEST_WINNER_SHARE[["Spoil"]]
agg <- np[!is.na(position_group),
          .(cw = sum(np_contest_won, na.rm = TRUE),
            dp = sum(np_defensive, na.rm = TRUE)), by = position_group]
agg <- merge(agg, gcount, by = "position_group")
# The pool money that came FROM contest cessions is (1 - rho)/rho times the
# named side; a position shares it in proportion to its slice of np_defensive.
pool_from_contest <- sum(abs(agg$cw)) * (1 - RHO) / RHO
agg[, share_of_pool := abs(dp) / sum(abs(dp))]
agg[, `:=`(d_named = 0.10 * cw / RHO / player_games,
           d_pool  = -0.10 * share_of_pool * pool_from_contest / (1 - RHO) / player_games)]
agg[, net := round(d_named + d_pool, 4)]
agg[, `:=`(d_named = round(d_named, 4), d_pool = round(d_pool, 4))]
setorder(agg, -net)
print(agg[, .(position_group, player_games, d_named, d_pool, net)])
d <- agg

kf <- d[position_group == "KEY_FORWARD"]$net
kd <- d[position_group == "KEY_DEFENDER"]$net
say("\n  gap moves by ", round(kf - kd, 4), " points a game per 0.10 of share")
say("  Against a v13 gap of about 3.648, taking the share from 0.50 to 0.80")
say("  on spoils (three steps) would move it about ",
    round(3 * (kf - kd), 3), ".")
say("\nIf that is under about 0.05 the constant is a correctness note rather")
say("than a lever, and a sweep is not worth the rebuilds.")
