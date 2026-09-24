# Does NP_STOPPAGE_SPLIT move PUBLISHED numbers, and is it guarded?
# =============================================================================
# NP_STOPPAGE_LOSER_SHARE is inert (measured: 0 of 9,794 players move) and is
# correctly absent from .rating_defining_constants(). NP_STOPPAGE_SPLIT is
# ALSO absent from that list -- but it controls the WITHIN-side ruck/player/
# pool split, and within-side ratios survive .np_team_margin()'s rescale, so
# it should move published numbers.
#
# It was changed on 2026-09-10 (ground: 0.20/0.50 -> 0.35/0.35). If it moves
# published output and is unguarded, that change went out without the vintage
# bump check_vintage_alignment() exists to force.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_stoppage_split_guard_check.R"'
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

say("Shipped NP_STOPPAGE_SPLIT:")
print(NP_STOPPAGE_SPLIT)

run_arm <- function(label) {
  say("\n--- arm: ", label, " ---")
  np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                         stoppages = "allocate", difficulty_terms = tm,
                         return_payments = TRUE)
  pre <- as.data.table(np)[, .(match_id = as.character(match_id),
                               player_id = as.character(player_id), pre = net_points)]
  fin <- as.data.table(torp:::.np_team_margin(np, pbp, ps, res))
  fin <- fin[, .(match_id = as.character(match_id),
                 player_id = as.character(player_id), published = net_points)]
  merge(pre, fin, by = c("match_id", "player_id"))
}

base <- run_arm("SHIPPED (ground 0.35/0.35/0.30)")

# The PREVIOUS shipped value, before the 2026-09-10 correction.
alt <- list(hitout   = c(ruck = 0.50, player = 0.30, pool = 0.20),
            ruck_own = c(ruck = 0.00, player = 0.80, pool = 0.20),
            ground   = c(ruck = 0.20, player = 0.50, pool = 0.30))
assignInNamespace("NP_STOPPAGE_SPLIT", alt, ns = "torp")
say("\nOverridden to the PRE-2026-09-10 value (ground 0.20/0.50/0.30).")
old <- run_arm("PRE-2026-09-10 (ground 0.20/0.50/0.30)")

m <- merge(base, old, by = c("match_id", "player_id"), suffixes = c("_new", "_old"))
say("\n=== NP_STOPPAGE_SPLIT$ground 0.35/0.35 (shipped) vs 0.20/0.50 (previous) ===")
say("  n player-matches: ", format(nrow(m), big.mark = ","))
say("  RAW ledger:  max |diff| = ", signif(max(abs(m$pre_new - m$pre_old)), 4),
    " | mean |diff| = ", signif(mean(abs(m$pre_new - m$pre_old)), 4))
say("  PUBLISHED:   max |diff| = ", signif(max(abs(m$published_new - m$published_old)), 4),
    " | mean |diff| = ", signif(mean(abs(m$published_new - m$published_old)), 4))
n_moved <- m[abs(published_new - published_old) > 1e-6, .N]
say("  players whose PUBLISHED number moved (>1e-6): ",
    format(n_moved, big.mark = ","), " of ", format(nrow(m), big.mark = ","),
    " (", round(100 * n_moved / nrow(m), 2), "%)")

say("\n=== Is it in the vintage guard? ===")
rd <- names(torp:::.rating_defining_constants())
for (k in c("NP_STOPPAGE_SPLIT", "NP_STOPPAGE_LOSER_SHARE", "NP_BLAME_SHARE",
            "NP_OFFENCE_POOL_SHARE", "NP_UNCONTESTED_RECEIVER_SHARE",
            "NP_BALL_WINNER_SHARE_BY_ACT", "NP_CONTEST_WINNER_SHARE")) {
  say("  ", k, ": ", if (k %in% rd) "REGISTERED" else "NOT registered")
}
