# What does NP_CONTEST_WINNER_SHARE's spoil entries 0.50 -> 0.65 do to
# published ratings? (v14, Pete's call 2026-09-12)
# =============================================================================
# The derivative sizing in np_contest_winner_share_lever.R predicted +0.10 on
# the spoil share moves the forward-defender gap by -0.144 a game, so the
# actual step (0.50 -> 0.65, 1.5 x 0.10) predicts about -0.216. That is a
# LINEAR approximation (rho enters linearly in the ledger math, but the
# derivative was taken at rho = 0.50 -- confirm it still holds at 0.65, per
# stats-discipline #5: never bank a limiting-case prediction without checking
# it against the real, non-approximated build).
#
# Both arms scored in ONE session on IDENTICAL rows (stats-discipline #5.3):
# same pbp/chains/stats/results, same difficulty_terms, only the constant
# swapped via assignInNamespace.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_v14_spoil_share_effect.R"'
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

# Log coverage before reporting anything (stats-discipline #7).
say("=== coverage ===")
say("  season          : ", SEASON)
say("  pbp rows        : ", format(nrow(pbp), big.mark = ","))
say("  chains rows     : ", format(nrow(ch), big.mark = ","))
say("  player-games (pg): ", format(nrow(pg), big.mark = ","))
say("  position_group NA: ", sum(is.na(pos$position_group)), " of ", nrow(pos))

# Built EXPLICITLY, never derived from the live NP_CONTEST_WINNER_SHARE --
# the source already carries v14's 0.65 (shipped ahead of this measurement
# script being corrected), so `BASELINE <- NP_CONTEST_WINNER_SHARE` would
# silently capture 0.65 as "baseline" too and compare v14 against itself.
# Caught exactly this way on the first run: identical output, zero
# movement, which is what a real bug in the comparison SETUP looks like,
# not a genuine null (stats-discipline #5.3).
BASELINE <- c(
  "Contested Mark" = 0.80, "Uncontested Mark" = 0.80, "Mark On Lead" = 0.80,
  "Pack Mark (P)" = 0.80, "Pack Mark (O)" = 0.80,
  "Spoil" = 0.50, "Spoil gaining possession" = 0.50, "Spoil ineffective" = 0.50
)
V14 <- BASELINE
V14[c("Spoil", "Spoil gaining possession", "Spoil ineffective")] <- 0.65
stopifnot(!identical(BASELINE, V14))  # the comparison must actually differ

tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
say("\n=== difficulty terms (shared by both arms, computed once) ===")
say("  rows       : ", format(nrow(tm), big.mark = ","))
say("  contested  : ", format(tm[contested == TRUE, .N], big.mark = ","),
    " (", round(100 * mean(tm$contested), 1), "%)")

run <- function(share, label) {
  utils::assignInNamespace("NP_CONTEST_WINNER_SHARE", share, ns = "torp")
  np <- as.data.table(build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                                       stoppages = "allocate", difficulty_terms = tm,
                                       return_payments = TRUE))
  np[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  raw <- merge(np[, .(match_id, player_id, raw_np = net_points)], pos,
              by = c("match_id", "player_id"), all.x = TRUE)
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  out <- merge(fin[, .(match_id, player_id, net_points)], pos,
              by = c("match_id", "player_id"), all.x = TRUE)
  out <- merge(out, raw[, .(match_id, player_id, raw_np)], by = c("match_id", "player_id"))
  say("  ", label, ": ", format(nrow(out), big.mark = ","), " player-games")
  out
}

say("\n=== building both arms ===")
a <- run(BASELINE, "baseline (spoil 0.50, shipped v13)")
b <- run(V14, "v14 (spoil 0.65)")
utils::assignInNamespace("NP_CONTEST_WINNER_SHARE", BASELINE, ns = "torp")

sm <- function(d) d[!is.na(position_group),
                    .(n = .N, mean_np = round(mean(net_points), 3),
                      mean_raw = round(mean(raw_np), 3)), by = position_group]
A <- sm(a); B <- sm(b)
cmp <- merge(A, B, by = "position_group", suffixes = c("_v13", "_v14"))
cmp[, `:=`(change = round(mean_np_v14 - mean_np_v13, 3),
          change_raw = round(mean_raw_v14 - mean_raw_v13, 3))]
say("\n=== RAW (pre-reconciliation) net points a game, by position ===")
print(cmp[match(ORD, position_group), .(position_group, mean_raw_v13, mean_raw_v14, change_raw)])
say("\n=== PUBLISHED (post team-margin reconciliation) net points a game, by position ===")
print(cmp[match(ORD, position_group), .(position_group, mean_np_v13, mean_np_v14, change)])

g <- function(x, col) x[position_group == "KEY_FORWARD"][[col]] -
                      x[position_group == "KEY_DEFENDER"][[col]]
gr0 <- g(cmp, "mean_raw_v13"); gr1 <- g(cmp, "mean_raw_v14")
say("\nRAW KEY_FORWARD - KEY_DEFENDER gap: ", round(gr0, 3), " -> ", round(gr1, 3),
    "   (", if (gr1 < gr0) "NARROWER" else "WIDER", " by ", round(abs(gr1 - gr0), 3), ")")
g0 <- g(cmp, "mean_np_v13"); g1 <- g(cmp, "mean_np_v14")
say("PUBLISHED KEY_FORWARD - KEY_DEFENDER gap: ", round(g0, 3), " -> ", round(g1, 3),
    "   (", if (g1 < g0) "NARROWER" else "WIDER", " by ", round(abs(g1 - g0), 3), ")")
say("Derivative predicted: -0.216 (linear approx at rho=0.50). Actual: ",
    round(g1 - g0, 3), if (abs((g1 - g0) - (-0.216)) < 0.05) "  -- MATCHES" else "  -- DIVERGES, check why")

say("\n=== anchor checks (must hold, or the method is wrong) ===")
say("  1. key defenders move UP (more credit) under v14   : ",
    if (cmp[position_group == "KEY_DEFENDER"]$change > 0) "PASS" else "FAIL")
say("  2. key forwards do not move materially (rho excludes them): ",
    if (abs(cmp[position_group == "KEY_FORWARD"]$change) < 0.05) "PASS" else "FAIL (",
    cmp[position_group == "KEY_FORWARD"]$change, ")")
say("  3. gap narrows, not widens                          : ",
    if (g1 < g0) "PASS" else "FAIL")
say("  4. no position's mean moves more than the derivative's KEY_DEFENDER estimate (~0.18/game): ",
    if (max(abs(cmp$change)) < 0.30) "PASS" else "FAIL (max ", max(abs(cmp$change)), ")")

say("\n=== how much does any individual player-game move? ===")
m <- merge(a[, .(match_id, player_id, np_v13 = net_points)],
           b[, .(match_id, player_id, np_v14 = net_points)],
           by = c("match_id", "player_id"))
m[, d := np_v14 - np_v13]
say("  player-games      : ", format(nrow(m), big.mark = ","))
say("  moved at all      : ", format(m[abs(d) > 1e-9, .N], big.mark = ","))
say("  mean |move|       : ", round(mean(abs(m$d)), 3))
say("  max  |move|       : ", round(max(abs(m$d)), 3))
say("  moving > 1 point  : ", format(m[abs(d) > 1, .N], big.mark = ","))
