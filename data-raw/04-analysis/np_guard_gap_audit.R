# Complete the audit: which NP_ share constants are rating-defining but NOT
# registered in .rating_defining_constants()?
# =============================================================================
# Established so far, by measurement on 2026 (9,794 player-matches):
#   NP_STOPPAGE_LOSER_SHARE  INERT (0% of players move)   -- correctly unguarded
#   NP_STOPPAGE_SPLIT        LIVE  (100% move, max 2.83)  -- NOT guarded  <-- gap
# Two more are unregistered and untested: NP_OFFENCE_POOL_SHARE and
# NP_CONTEST_WINNER_SHARE. Both look like within-side splits, which survive
# .np_team_margin()'s rescale -- but that reasoning already failed once today
# (it predicted NP_BLAME_SHARE inert; it isn't), so measure, don't assume.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_guard_gap_audit.R"'
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

run_arm <- function(offence_pool_share = NP_OFFENCE_POOL_SHARE) {
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = tm,
                         return_payments = TRUE,
                         offence_pool_share = offence_pool_share)
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin[, .(match_id = as.character(match_id),
          player_id = as.character(player_id), published = net_points)]
}

report <- function(a, b, what) {
  m <- merge(a, b, by = c("match_id", "player_id"), suffixes = c("_a", "_b"))
  n_moved <- m[abs(published_a - published_b) > 1e-6, .N]
  say("\n=== ", what, " ===")
  say("  PUBLISHED max |diff| = ", signif(max(abs(m$published_a - m$published_b)), 4),
      " | mean |diff| = ", signif(mean(abs(m$published_a - m$published_b)), 4))
  say("  players moved: ", format(n_moved, big.mark = ","), " of ",
      format(nrow(m), big.mark = ","), " (", round(100 * n_moved / nrow(m), 2), "%)  -> ",
      if (n_moved == 0) "INERT" else "LIVE (rating-defining)")
}

say("Baseline arm...")
base <- run_arm()

say("NP_OFFENCE_POOL_SHARE 0.10 -> 0.40 ...")
opool <- run_arm(offence_pool_share = 0.40)
report(base, opool, "NP_OFFENCE_POOL_SHARE 0.10 vs 0.40")

say("\nNP_CONTEST_WINNER_SHARE (marks 0.80 -> 0.40) ...")
alt <- NP_CONTEST_WINNER_SHARE
alt[grepl("Mark", names(alt))] <- 0.40
assignInNamespace("NP_CONTEST_WINNER_SHARE", alt, ns = "torp")
cws <- run_arm()
report(base, cws, "NP_CONTEST_WINNER_SHARE marks 0.80 vs 0.40")
