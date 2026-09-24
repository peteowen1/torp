# The two tables, done properly this time -- both verified to actually sum
# to net_points before being printed.
# =============================================================================
# Table 1 was broken because it was missing a column: the player's own
# individually-named credit (his disposals, receives, turnovers won, contests
# won, stoppages) was never shown, so "margin reconciliation" was silently
# doing double duty. Fixed by adding that column explicitly, using
# PRODUCTION's own component columns (not the payment-ledger recompute used
# for Table 2 -- these are two different, non-overlapping views).
#
# Table 2 was already correct (named+share+recon = net_points, verified
# earlier to 1e-15); reprinted here unchanged for comparison.
#
# Both use the SHIPPED production settings (spread="matchup", pool_by="dacts").
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_two_tables_fixed.R"'
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
ha <- unique(pbp[, .(match_id, team, home_away)])
mg <- as.data.table(res)[, .(match_id = as.character(match_id), margin = home_score - away_score)]

say("Building the ledger (shipped settings: spread='matchup')...")
np_pre <- as.data.table(build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                                         stoppages = "allocate", difficulty_terms = tm,
                                         return_payments = TRUE, spread = "matchup"))

# --- Table 2's mechanism (shipped: pool_by = "dacts") -----------------------
pay <- as.data.table(attr(np_pre, "np_payments"))
pay[, match_id := as.character(match_id)]
pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)
pay[, own := hm * data.table::fifelse(home_away == "Home", 1, -1)]
if (!"doubled" %in% names(pay)) pay[, doubled := FALSE]
pay[, v := sum(hm[doubled == FALSE]), by = .(match_id, display_order)]
pay <- pay[abs(v) > 1e-12]
pay[, gain_home := v > 0]
pay[, side := data.table::fifelse((home_away == "Home") == gain_home, "gain", "concede")]
pay[, side_sum := sum(own), by = .(match_id, display_order, side)]
pay[, target := data.table::fifelse(side == "gain", abs(v), -abs(v))]
pay[, scaled := data.table::fifelse(abs(side_sum) > 0.05 * abs(v), own * target / side_sum, NA_real_)]
namd <- pay[!is.na(scaled) & !is.na(player_id), .(named = sum(scaled)),
            by = .(match_id, team, player_id = as.character(player_id))]
prow <- pay[, .(target = target[1], got_named = sum(scaled[!is.na(player_id)], na.rm = TRUE)),
            by = .(match_id, display_order, team)]
prow[, pool_row := target - got_named]
poolT2 <- prow[, .(pool = sum(pool_row)), by = .(match_id, team)]

dz <- function(x) pmax(dplyr::coalesce(as.numeric(x), 0), 0)
lu <- ps[, .(match_id = as.character(match_id), player_id = as.character(player_id),
            tog = pmax(time_on_ground_percentage, 1) / 100,
            dacts = dz(tackles) + dz(intercepts) + dz(one_percenters))]
lu <- merge(lu, unique(np_pre[, .(match_id = as.character(match_id), player_id = as.character(player_id), team)]),
            by = c("match_id", "player_id"))
lu[, w := pmax(dacts, 0.5)]
ros <- merge(lu, poolT2, by = c("match_id", "team"), all.x = TRUE)[is.na(pool), pool := 0]
ros[, share := pool * w / sum(w), by = .(match_id, team)]

outT2 <- merge(ros[, .(match_id, team, player_id, tog, share)], namd,
               by = c("match_id", "team", "player_id"), all = TRUE)
outT2[is.na(named), named := 0][is.na(share), share := 0][, val := named + share]
if (anyNA(outT2$tog)) outT2[is.na(tog), tog := 0.75]
chk <- merge(merge(outT2[, .(tot = sum(val)), by = .(match_id, team)], ha, by = c("match_id", "team")),
             mg, by = "match_id")
chk[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
outT2 <- merge(outT2, chk[, .(match_id, team, short = want - tot)], by = c("match_id", "team"))
outT2[, recon := short * tog / sum(tog), by = .(match_id, team)]
outT2[, val := val + recon]

np <- copy(np_pre)
np[, match_id := as.character(match_id)]; np[, player_id := as.character(player_id)]
np <- merge(np, outT2[, .(match_id, player_id, named, share, recon, .new = val)],
            by = c("match_id", "player_id"), all.x = TRUE)
np[is.na(.new), `:=`(.new = 0, named = 0, share = 0, recon = 0)]
np[, margin_correction := .new - net_points]     # what .np_team_margin() actually adds
np[, individual_credit := np_direct + np_ceded + np_defensive_won + np_contest_won + np_stoppage]
np[, net_points_final := .new]

# --- verify BOTH decompositions actually sum to net_points before printing --
gap_t2 <- max(abs(np$named + np$share + np$recon - np$net_points_final))
gap_t1 <- max(abs(np$individual_credit + np$np_defensive + np$np_team + np$margin_correction +
                   np$np_residual - np$net_points_final))
say("Table 2 sum check (named+share+recon vs net_points): max gap ", signif(gap_t2, 6), " -- should be ~0")
say("Table 1 sum check (individual_credit+TEAM_PRESSURE+TEAM_OFFENCE_orig+MARGIN_CORRECTION+residual): max gap ",
    signif(gap_t1, 6), " -- should be ~0")

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
nm <- unique(pg[, .(match_id, player_id, position_group)])
np <- merge(np, nm, by = c("match_id", "player_id"), all.x = TRUE)

order_lvls <- c("RUCK", "MIDFIELDER", "MEDIUM_FORWARD", "KEY_FORWARD", "MEDIUM_DEFENDER", "KEY_DEFENDER")

t1 <- np[!is.na(position_group), .(
  INDIVIDUAL_CREDIT = round(mean(individual_credit), 3),
  TEAM_PRESSURE = round(mean(np_defensive), 3),
  TEAM_OFFENCE_original = round(mean(np_team), 3),
  MARGIN_CORRECTION = round(mean(margin_correction), 3),
  RESIDUAL = round(mean(np_residual), 3),
  NET_POINTS = round(mean(net_points_final), 3)
), by = position_group][match(order_lvls, position_group)]

t2 <- np[!is.na(position_group), .(
  np_team_recon_player = round(mean(named), 3),
  np_team_recon_pool = round(mean(share), 3),
  np_team_recon_final = round(mean(recon), 3),
  NET_POINTS = round(mean(net_points_final), 3)
), by = position_group][match(order_lvls, position_group)]

say("\n=== TABLE 1 (fixed) -- production's own components, all 6 sum exactly ===")
print(t1)
say("\n=== TABLE 2 -- the payment-ledger recompute, all 3 sum exactly ===")
print(t2)
