# Split np_team into its three real pieces, on real 2026 rows
# =============================================================================
# Pete's question: TEAM PRESSURE (np_defensive) is one clean pool, but TEAM
# OFFENCE (np_team) bundles the genuine NP_OFFENCE_POOL_SHARE slice with
# whatever .np_team_margin() adds on top to force each team's players to sum
# to that team's own margin (epv_net_points.R:2352-2560). This pulls the two
# apart by calling build_net_points() BEFORE the margin convention (np_team
# is then pure offence-pool) and .np_team_margin() AFTER, so the delta between
# them is the reconciliation term in isolation.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_team_pool_3way.R"'
#
# Env: NP3_SEASON (default 2026).
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
say <- function(...) cat(..., "\n", sep = "")

SEASON <- as.integer(Sys.getenv("NP3_SEASON", "2026"))
S <- "data-raw/outputs"

pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- fread(file.path(S, "np_difficulty_terms_2025_2026.csv"))
tm[, match_id := as.character(match_id)]
tm  <- tm[substr(match_id, 5, 8) == as.character(SEASON)]

say("Building the PRE-margin ledger (np_team is pure NP_OFFENCE_POOL_SHARE here)...")
np_pre <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                           stoppages = "allocate", difficulty_terms = tm,
                           return_payments = TRUE)
np_pre <- as.data.table(np_pre)

say("Applying .np_team_margin() -- the production step -- to get the POST ledger...")
np_post <- torp:::.np_team_margin(np_pre, pbp, ps, res)
np_post <- as.data.table(np_post)

# Sanity: np_defensive must be untouched by the margin convention.
chk <- merge(np_pre[, .(match_id, player_id, team, pre_def = np_defensive)],
             np_post[, .(match_id, player_id, post_def = np_defensive)],
             by = c("match_id", "player_id"))
gap_def <- max(abs(chk$pre_def - chk$post_def))
say("Sanity check -- max |np_defensive pre - post|: ", signif(gap_def, 6),
    " (should be 0: the margin convention must not touch it)")

three <- merge(
  np_pre[, .(match_id, player_id, team, np_defensive,
             np_team_offence = np_team, net_points_pre = net_points)],
  np_post[, .(match_id, player_id, np_team_post = np_team, net_points = net_points)],
  by = c("match_id", "player_id"))
three[, np_margin_recon := np_team_post - np_team_offence]

# Verify the recon term really is the whole difference the margin convention
# made: net_points_pre + recon should equal the final published net_points,
# since .np_team_margin() only ever changes np_team.
gap_sum <- max(abs(three$net_points_pre + three$np_margin_recon - three$net_points))
say("Sanity check -- net_points_pre + np_margin_recon vs final net_points,",
    " max gap: ", signif(gap_sum, 6), " (should be ~0)")

# Bring in position and player name.
pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
nm <- unique(pg[, .(match_id, player_id, player_name, position_group, tog)])
three[, player_id := as.character(player_id)]
three <- merge(three, nm, by = c("match_id", "player_id"), all.x = TRUE)

say("\n=== League average per game, by position_group ===")
say("(np_defensive = TEAM PRESSURE, unaffected by the margin convention;",
    " np_team_offence = the pure NP_OFFENCE_POOL_SHARE slice;",
    " np_margin_recon = the team-margin correction, currently hidden inside TEAM OFFENCE)")
by_pos <- three[!is.na(position_group), .(
  n_player_games = .N,
  np_defensive = round(mean(np_defensive), 3),
  np_team_offence = round(mean(np_team_offence), 3),
  np_margin_recon = round(mean(np_margin_recon), 3),
  net_points = round(mean(net_points), 3)
), by = position_group][order(-net_points)]
print(by_pos)

say("\n=== Named check: Harris Andrews (key defender) vs a comparable-minutes forward ===")
who <- three[player_name %in% c("Harris Andrews"),
             .(gms = .N,
               np_defensive = round(mean(np_defensive), 3),
               np_team_offence = round(mean(np_team_offence), 3),
               np_margin_recon = round(mean(np_margin_recon), 3),
               net_points = round(mean(net_points), 3)),
             by = .(player_name, position_group)]
print(who)

fwrite(three, file.path(S, "np_team_pool_3way_2026.csv"))
say("\nWrote np_team_pool_3way_2026.csv: ", nrow(three), " player-match rows")
