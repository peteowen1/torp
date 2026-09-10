# Are the CROSS-TEAM share constants inert under the team-margin convention?
# =============================================================================
# .np_team_margin() rescales each SIDE of every row up to that row's full
# value (own * target / side_sum). Any constant whose only job is to decide
# how a row's value splits BETWEEN the two teams is therefore divided straight
# back out -- the rescale forces both sides to the full value regardless.
#
# Algebra says this hits:
#   NP_STOPPAGE_LOSER_SHARE  (winner side vs loser side)
#   NP_BLAME_SHARE           (disposer's team vs the defence's team)
# and NOT the within-team ones (NP_OFFENCE_POOL_SHARE,
# NP_UNCONTESTED_RECEIVER_SHARE, NP_BALL_WINNER_SHARE_BY_ACT), because the
# rescale preserves ratios WITHIN a side.
#
# Algebra is not evidence. This measures it: same data, same code, different
# constant, compare PUBLISHED net_points player-for-player.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_share_inertness_check.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- fread("data-raw/outputs/np_difficulty_terms_2025_2026.csv")
tm[, match_id := as.character(match_id)]
tm  <- tm[substr(match_id, 5, 8) == as.character(SEASON)]

run_arm <- function(label, blame_share, stoppage_loser_share) {
  say("\n--- arm: ", label, " (blame_share=", blame_share,
      ", stoppage_loser_share=", stoppage_loser_share, ") ---")
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = tm,
                         return_payments = TRUE,
                         blame_share = blame_share,
                         stoppage_loser_share = stoppage_loser_share)
  pre <- as.data.table(np)[, .(match_id = as.character(match_id),
                               player_id = as.character(player_id),
                               pre = net_points)]
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin <- fin[, .(match_id = as.character(match_id),
                 player_id = as.character(player_id), published = net_points)]
  merge(pre, fin, by = c("match_id", "player_id"))
}

base   <- run_arm("SHIPPED",              0.30, 0.50)
blame  <- run_arm("blame_share 0.30->0.70",  0.70, 0.50)
stopp  <- run_arm("stoppage_loser 0.50->0.20", 0.30, 0.20)

cmp <- function(a, b, what) {
  m <- merge(a, b, by = c("match_id", "player_id"), suffixes = c("_a", "_b"))
  say("\n=== ", what, " ===")
  say("  n player-matches compared: ", format(nrow(m), big.mark = ","))
  say("  RAW ledger  (build_net_points alone): max |diff| = ",
      signif(max(abs(m$pre_a - m$pre_b)), 4),
      " | mean |diff| = ", signif(mean(abs(m$pre_a - m$pre_b)), 4))
  say("  PUBLISHED   (after .np_team_margin): max |diff| = ",
      signif(max(abs(m$published_a - m$published_b)), 4),
      " | mean |diff| = ", signif(mean(abs(m$published_a - m$published_b)), 4))
  n_moved <- m[abs(published_a - published_b) > 1e-6, .N]
  say("  players whose PUBLISHED number moved at all (>1e-6): ",
      format(n_moved, big.mark = ","), " of ", format(nrow(m), big.mark = ","),
      " (", round(100 * n_moved / nrow(m), 2), "%)")
}

cmp(base, blame, "NP_BLAME_SHARE 0.30 vs 0.70")
cmp(base, stopp, "NP_STOPPAGE_LOSER_SHARE 0.50 vs 0.20")
