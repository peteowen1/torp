# Raw Player Credit AND the TOG-weighted margin residual, mean + SD, by position
# =============================================================================
# Closing the loop on today's investigation: how much of the defender gap is
# in the raw, event-driven Player Credit (which chains/PBP data structurally
# can't see -- a good defender's value shows up as the ball NOT coming his
# way) versus the size and spread of the team-level pooling/reconciliation
# that's meant to compensate for exactly that blind spot.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_credit_and_recon_variance.R"'
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

say("Building the ledger...")
np_pre <- as.data.table(build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                                         stoppages = "allocate", difficulty_terms = tm,
                                         return_payments = TRUE))

pay <- as.data.table(attr(np_pre, "np_payments"))
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
pay[, scaled := data.table::fifelse(abs(side_sum) > 0.05 * abs(v), own * target / side_sum, NA_real_)]

# Raw Player Credit per player-game: sum of `own` on rows where he's named.
raw_credit <- pay[!is.na(player_id), .(own = sum(own)), by = .(match_id, player_id)]
raw_credit[, player_id := as.character(player_id)]

# --- the pool + the two-stage reconciliation, exactly as production runs it
namd <- pay[!is.na(scaled) & !is.na(player_id), .(named = sum(scaled)),
            by = .(match_id, team, player_id = as.character(player_id))]
prow <- pay[, .(target = target[1], got_named = sum(scaled[!is.na(player_id)], na.rm = TRUE)),
            by = .(match_id, display_order, team)]
prow[, pool_row := target - got_named]
pool <- prow[, .(pool = sum(pool_row)), by = .(match_id, team)]

dz <- function(x) pmax(dplyr::coalesce(as.numeric(x), 0), 0)
lu <- ps[, .(match_id = as.character(match_id), player_id = as.character(player_id),
            tog = pmax(time_on_ground_percentage, 1) / 100,
            dacts = dz(tackles) + dz(intercepts) + dz(one_percenters))]
lu <- merge(lu, unique(np_pre[, .(match_id = as.character(match_id), player_id = as.character(player_id), team)]),
            by = c("match_id", "player_id"))
lu[, w := pmax(dacts, 0.5)]
ros <- merge(lu, pool, by = c("match_id", "team"), all.x = TRUE)[is.na(pool), pool := 0]
ros[, share := pool * w / sum(w), by = .(match_id, team)]

out <- merge(ros[, .(match_id, team, player_id, tog, share)], namd,
             by = c("match_id", "team", "player_id"), all = TRUE)
out[is.na(named), named := 0][is.na(share), share := 0][, val := named + share]
if (anyNA(out$tog)) out[is.na(tog), tog := 0.75]

mg <- as.data.table(res)[, .(match_id = as.character(match_id), margin = home_score - away_score)]
chk <- merge(merge(out[, .(tot = sum(val)), by = .(match_id, team)], ha, by = c("match_id", "team")),
             mg, by = "match_id")
chk[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
out <- merge(out, chk[, .(match_id, team, short = want - tot)], by = c("match_id", "team"))
out[, recon := short * tog / sum(tog), by = .(match_id, team)]

# --- join position and build the final table --------------------------------
pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
nm <- unique(pg[, .(match_id, player_id, position_group)])

both <- merge(out[, .(match_id, player_id, recon)], raw_credit, by = c("match_id", "player_id"), all = TRUE)
both[is.na(own), own := 0][is.na(recon), recon := 0]
both <- merge(both, nm, by = c("match_id", "player_id"), all.x = TRUE)

final <- both[!is.na(position_group), .(
  n_player_games = .N,
  raw_credit_mean = round(mean(own), 3),
  raw_credit_sd = round(sd(own), 3),
  recon_tog_mean = round(mean(recon), 3),
  recon_tog_sd = round(sd(recon), 3)
), by = position_group]

order_lvls <- c("KEY_FORWARD", "MEDIUM_FORWARD", "MIDFIELDER", "KEY_DEFENDER", "MEDIUM_DEFENDER", "RUCK")
final <- final[match(order_lvls, position_group)]
say("\n=== Raw Player Credit and the TOG-weighted margin residual, mean + SD, per player-game ===")
say("(raw_credit = event-driven, chains-visible value; recon = the final reconciliation",
    " step, spread flat by time on ground -- the one piece of the ledger that pays a",
    " defender for shape/positioning the play-by-play never records him touching)")
print(final)
