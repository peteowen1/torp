# Where does the forward-defender gap actually come from? (defender program)
# =============================================================================
# Pete: "find areas we can help balance positions out so defenders benefit."
#
# Everything in the defender program so far has been a guess about WHICH channel
# is short-changing defenders, then a measurement that half-confirms it. This
# does the decomposition first: build_net_points() returns EIGHT named columns
# that sum to net_points exactly, so the gap between a key forward and a key
# defender is fully accounted for by those eight. Print them per position and
# the answer is arithmetic, not a hypothesis.
#
#   np_direct         his own acts -- disposals, marks, gathers
#   np_defensive_won  turnovers he personally won back
#   np_contest_won    aerial contests he won
#   np_stoppage       ruck work and first possession
#   np_ceded          what he gave up on his own losses
#   np_defensive      his share of the team's DEFENSIVE pool
#   np_team           his share of the team's OFFENCE pool
#   np_residual       the team-margin reconciliation
#
# The two pool columns are the ones worth watching, because they are the only
# ones NOT earned by a named act -- they are shares of a team total, and how
# they are split is a free design choice rather than a measurement. If the gap
# lives there, it is adjustable without inventing new credit.
#
# Also computed: what each position would need for the gap to close, and whether
# a pool-split change of plausible size could deliver it. A lever that cannot
# move the number enough is not a lever.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_position_balance_levers.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))

np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                       stoppages = "allocate", difficulty_terms = tm)
np <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
np[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
meta <- unique(pg[, .(match_id, player_id, position_group, tog = time_on_ground_pct)])
d <- merge(np, meta, by = c("match_id", "player_id"), all.x = TRUE)
d <- d[!is.na(position_group)]

PARTS <- c("np_direct", "np_defensive_won", "np_contest_won", "np_stoppage",
           "np_ceded", "np_defensive", "np_team", "np_residual")
PARTS <- intersect(PARTS, names(d))
ORD <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "RUCK",
         "MEDIUM_DEFENDER", "KEY_DEFENDER")

say("=== the eight columns, mean per game by position ===")
say("These sum to net_points exactly, so the gap is fully explained here.")
t <- d[, c(.(n = .N), lapply(.SD, function(v) round(mean(v), 3))),
       by = position_group, .SDcols = c(PARTS, "net_points")]
print(t[match(ORD, position_group)])

say("\n=== the gap, column by column (KEY_FORWARD minus KEY_DEFENDER) ===")
say("Positive = the forward gets more of that column. These add to the total gap.")
kf <- t[position_group == "KEY_FORWARD"]; kd <- t[position_group == "KEY_DEFENDER"]
g <- data.table(column = c(PARTS, "net_points"),
                key_fwd = unlist(kf[, c(PARTS, "net_points"), with = FALSE]),
                key_def = unlist(kd[, c(PARTS, "net_points"), with = FALSE]))
g[, gap := round(key_fwd - key_def, 3)]
g[, share_of_total := round(100 * gap / g[column == "net_points", gap], 1)]
print(g[order(-abs(gap))])

say("\n=== which of these are EARNED vs ALLOCATED? ===")
say("Earned columns come from a named act and can only be changed by changing")
say("what the act is worth. Pool columns are a share of a team total -- how they")
say("split is a design choice, so they are the adjustable ones.")
say("  EARNED    : np_direct, np_defensive_won, np_contest_won, np_stoppage, np_ceded")
say("  ALLOCATED : np_defensive (defensive pool), np_team (offence pool), np_residual")
alloc <- c("np_defensive", "np_team", "np_residual")
ga <- g[column %chin% intersect(alloc, PARTS), sum(gap)]
ge <- g[column %chin% setdiff(PARTS, alloc), sum(gap)]
say("\n  gap carried by ALLOCATED columns : ", round(ga, 3))
say("  gap carried by EARNED columns     : ", round(ge, 3))
say("  total                              : ", round(ga + ge, 3))

say("\n=== how are the two pools currently split? ===")
say("If a pool is split by time on ground or by disposals, a defender's share is")
say("set by something he does not control. Correlation of each pool column with")
say("TOG and with his own-act volume:")
d[, own_vol := abs(np_direct)]
for (v in intersect(alloc, PARTS)) {
  say("  ", formatC(v, width = -14),
      " vs TOG r=", round(cor(d[[v]], d$tog, use = "complete.obs"), 3),
      "   vs own-act volume r=", round(cor(d[[v]], d$own_vol, use = "complete.obs"), 3))
}

say("\n=== per-position pool shares, normalised by time on ground ===")
say("A fair split would give every position a similar pool per unit of TOG.")
p <- d[, lapply(.SD, function(v) round(mean(v / pmax(tog, 1) * 100), 3)),
       by = position_group, .SDcols = intersect(alloc, PARTS)]
print(p[match(ORD, position_group)])

say("\n=== LEVERS: what would it take to close the gap? ===")
gap_total <- g[column == "net_points", gap]
say("Current KEY_FORWARD - KEY_DEFENDER gap: ", round(gap_total, 3), " a game")
for (v in intersect(alloc, PARTS)) {
  kfv <- kf[[v]]; kdv <- kd[[v]]
  say("\n  ", v, ": KF ", round(kfv, 3), "  KD ", round(kdv, 3),
      "  (gap ", round(kfv - kdv, 3), ")")
  if (abs(kfv - kdv) > 0.01) {
    say("    equalising this column alone would close ",
        round(100 * (kfv - kdv) / gap_total, 1), "% of the gap")
  } else {
    say("    already near-equal -- not a lever")
  }
}
say("\nA lever is only useful if the column is BOTH large enough and allocated")
say("rather than earned. Anything in the earned list needs a rules change, not")
say("a re-split, and this session has shown how easily those move the wrong way.")
