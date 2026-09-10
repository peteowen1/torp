# Trace the per-row rescale factor (target/side_sum) by role and position
# =============================================================================
# np_team_pool_3way.R found the defender gap lives almost entirely in
# `named` (the row-level rescale of each player's own individually-credited
# value), not in the pool-weighting constant. This traces WHY: for every
# payment row, `.np_team_margin()` computes a per-(row,side) rescale factor
# `rf = target/side_sum` and multiplies every named player's own share of
# that row by it (`scaled = own * rf`). If defenders' typical rows (contest
# wins, turnovers forced, stoppage wins) get a systematically different `rf`
# from forwards' typical rows (disposals, receives), OR get guarded out of
# the named calculation entirely (side_sum too small relative to the row
# value) more often, that is the direct mechanism.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_rescale_factor_trace.R"'
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

say("Building the ledger with payments...")
np_pre <- as.data.table(build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                                         stoppages = "allocate", difficulty_terms = tm,
                                         return_payments = TRUE))

pay <- as.data.table(attr(np_pre, "np_payments"))
say("Payment ledger roles present: ", paste(sort(unique(pay$role)), collapse = ", "))

pay[, match_id := as.character(match_id)]
ha <- unique(pbp[, .(match_id = as.character(match_id), team, home_away)])
pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)
pay[, own := hm * data.table::fifelse(home_away == "Home", 1, -1)]
if (!"doubled" %in% names(pay)) pay[, doubled := FALSE]
pay[, v := sum(hm[doubled == FALSE]), by = .(match_id, display_order)]
pay <- pay[abs(v) > 1e-12]
pay[, gain_home := v > 0]
pay[, side := data.table::fifelse((home_away == "Home") == gain_home, "gain", "concede")]
pay[, side_sum := sum(own), by = .(match_id, display_order, side)]
pay[, target := data.table::fifelse(side == "gain", abs(v), -abs(v))]
pay[, guarded_out := abs(side_sum) <= 0.05 * abs(v)]
pay[, rf := data.table::fifelse(guarded_out, NA_real_, target / side_sum)]
pay[, scaled := own * rf]

# Only rows where a specific player is named (pool rows have player_id = NA)
named_rows <- pay[!is.na(player_id)]
named_rows[, role := as.character(role)]
named_rows[, bucket := data.table::fcase(
  role %in% c("actor", "receiver"), "offence (actor/receiver)",
  role %in% c("ball_winner", "contest_winner", "stoppage_ruck", "stoppage_first_poss"), "defence/stoppage win",
  default = paste0("other: ", role[1])
), by = role]

say("\n=== Rows per role, and how often they get guarded OUT of the named rescale ===")
by_role <- named_rows[, .(
  n_rows = .N,
  total_own = round(sum(own), 1),
  pct_guarded_out = round(100 * mean(guarded_out), 1),
  mean_rf = round(mean(rf, na.rm = TRUE), 3),
  median_rf = round(median(rf, na.rm = TRUE), 3)
), by = .(role, bucket)][order(-n_rows)]
print(by_role)

say("\n=== Same, collapsed to offence vs defence/stoppage buckets ===")
by_bucket <- named_rows[, .(
  n_rows = .N, total_own = round(sum(own), 1),
  pct_guarded_out = round(100 * mean(guarded_out), 1),
  mean_rf = round(mean(rf, na.rm = TRUE), 3),
  median_rf = round(median(rf, na.rm = TRUE), 3)
), by = bucket]
print(by_bucket)

# --- now by POSITION: what rf does each position's credited value actually
# experience, weighted by how much `own` value sits on each row (so a
# position that gets more value from low-rf rows shows it here) -----------
pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
nm <- unique(pg[, .(match_id, player_id, position_group)])
named_rows[, player_id := as.character(player_id)]
nr <- merge(named_rows, nm, by = c("match_id", "player_id"), all.x = TRUE)

say("\n=== Value-weighted rescale factor by position (weighted by |own|; NA rf excluded) ===")
by_pos <- nr[!is.na(position_group) & !is.na(rf), .(
  n_rows = .N,
  total_own = round(sum(own), 1),
  weighted_rf = round(sum(rf * abs(own)) / sum(abs(own)), 4),
  simple_mean_rf = round(mean(rf), 4)
), by = position_group][order(-total_own)]
print(by_pos)

say("\n=== Share of each position's OWN credit that gets guarded OUT (falls to pool instead of named) ===")
by_pos_guard <- nr[!is.na(position_group), .(
  n_rows = .N,
  own_total = round(sum(own), 1),
  own_guarded_out = round(sum(own[guarded_out == TRUE]), 1),
  pct_own_guarded_out = round(100 * sum(own[guarded_out == TRUE]) / sum(own), 1)
), by = position_group][order(-own_total)]
print(by_pos_guard)

# --- close the loop: is the whole "named" gap just the raw Stage-1 credit
# gap, carried through by a near-uniform rf? Per player-game raw `own`
# (Player Credit: actor+receiver+ball_winner+contest_winner+stoppage rows,
# BEFORE any margin-convention rescale at all) by position. -----------------
say("\n=== Per player-game raw Player Credit (own, pre-rescale) by position ===")
own_by_pg <- nr[!is.na(position_group), .(own = sum(own)), by = .(match_id, player_id, position_group)]
say("(this is the sum of `own` across all his named rows in that game -- exactly what feeds `named`",
    " before the ~1.05-1.1x rescale is applied)")
by_pos_final <- own_by_pg[, .(n_player_games = .N, own_per_game = round(mean(own), 3)),
                          by = position_group][order(-own_per_game)]
print(by_pos_final)
