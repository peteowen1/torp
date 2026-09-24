# Pete's proposed redesign: pool the leftover FIRST (inverse-dacts), then
# rescale each player's own-plus-pool total proportionally to hit the margin,
# with a flat-by-TOG fallback only for degenerate teams. Compare against the
# shipped sequential-independent design (named/share computed in parallel,
# recon mops up whatever's left).
# =============================================================================
# This is a bigger change than the pool_by arms tested in np_pool_by_arms.R:
# it replaces the RESCALE MECHANISM itself (row-level rescale -> player-level
# proportional rescale of an already-pool-adjusted total), not just a weight.
# Read as a first look at whether the general approach helps, not a decision.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_sequential_redesign.R"'
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

say("Building the PRE-margin ledger...")
np_pre <- as.data.table(build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                                         stoppages = "allocate", difficulty_terms = tm,
                                         return_payments = TRUE))
ha <- unique(pbp[, .(match_id, team, home_away)])
mg <- as.data.table(res)[, .(match_id = as.character(match_id), margin = home_score - away_score)]

# --- shipped design (for comparison): parallel named/share, recon last -----
np_team_margin_shipped <- function(np, pbp_data, player_stats, res, pool_by = "dacts") {
  pay <- as.data.table(attr(np, "np_payments"))
  np <- as.data.table(np)
  p <- as.data.table(pbp_data)
  pay[, match_id := as.character(match_id)]
  hax <- unique(p[, .(match_id = as.character(match_id), team, home_away)])
  pay <- merge(pay, hax, by = c("match_id", "team"), all.x = TRUE)
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
  pool <- prow[, .(pool = sum(pool_row)), by = .(match_id, team)]

  ps2 <- as.data.table(player_stats)
  dz <- function(x) pmax(dplyr::coalesce(as.numeric(x), 0), 0)
  lu <- ps2[, .(match_id = as.character(match_id), player_id = as.character(player_id),
               tog = pmax(time_on_ground_percentage, 1) / 100,
               dacts = dz(tackles) + dz(intercepts) + dz(one_percenters))]
  lu <- merge(lu, unique(np[, .(match_id = as.character(match_id), player_id = as.character(player_id), team)]),
              by = c("match_id", "player_id"))
  lu[, w := switch(pool_by, tog = tog, dacts = pmax(dacts, 0.5), inv_dacts = 1 / pmax(dacts, 0.5),
                   stop("unknown pool_by"))]
  ros <- merge(lu, pool, by = c("match_id", "team"), all.x = TRUE)[is.na(pool), pool := 0]
  ros[, share := pool * w / sum(w), by = .(match_id, team)]
  out <- merge(ros[, .(match_id, team, player_id, tog, share)], namd,
               by = c("match_id", "team", "player_id"), all = TRUE)
  out[is.na(named), named := 0][is.na(share), share := 0][, val := named + share]
  if (anyNA(out$tog)) out[is.na(tog), tog := 0.75]

  mg2 <- as.data.table(res)[, .(match_id = as.character(match_id), margin = home_score - away_score)]
  chk <- merge(merge(out[, .(tot = sum(val)), by = .(match_id, team)], hax, by = c("match_id", "team")),
               mg2, by = "match_id")
  chk[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
  out <- merge(out, chk[, .(match_id, team, short = want - tot)], by = c("match_id", "team"))
  out[, recon := short * tog / sum(tog), by = .(match_id, team)]
  out[, val := val + recon]

  np[, match_id := as.character(match_id)]; np[, player_id := as.character(player_id)]
  np <- merge(np, out[, .(match_id, player_id, .new = val)], by = c("match_id", "player_id"), all.x = TRUE)
  np[is.na(.new), .new := 0]
  np[, net_points := .new]
  np[]
}

# --- Pete's sequential design: pool first, then a straight proportional
# rescale of (own total + pool share), TOG-flat only where the rescale would
# blow up. No independent per-row rescale of "named" at all.
np_sequential <- function(np, pbp_data, player_stats, res, pool_by = "inv_dacts") {
  pay <- as.data.table(attr(np, "np_payments"))
  np <- as.data.table(np)
  p <- as.data.table(pbp_data)
  pay[, match_id := as.character(match_id)]
  hax <- unique(p[, .(match_id = as.character(match_id), team, home_away)])
  pay <- merge(pay, hax, by = c("match_id", "team"), all.x = TRUE)
  pay[, own := hm * data.table::fifelse(home_away == "Home", 1, -1)]
  if (!"doubled" %in% names(pay)) pay[, doubled := FALSE]
  pay[, v := sum(hm[doubled == FALSE]), by = .(match_id, display_order)]
  pay <- pay[abs(v) > 1e-12]
  pay[, gain_home := v > 0]
  pay[, side := data.table::fifelse((home_away == "Home") == gain_home, "gain", "concede")]
  pay[, side_sum := sum(own), by = .(match_id, display_order, side)]
  pay[, target := data.table::fifelse(side == "gain", abs(v), -abs(v))]
  pay[, scaled := data.table::fifelse(abs(side_sum) > 0.05 * abs(v), own * target / side_sum, NA_real_)]
  prow <- pay[, .(target = target[1], got_named = sum(scaled[!is.na(player_id)], na.rm = TRUE)),
              by = .(match_id, display_order, team)]
  prow[, pool_row := target - got_named]
  pool <- prow[, .(pool = sum(pool_row)), by = .(match_id, team)]

  ps2 <- as.data.table(player_stats)
  dz <- function(x) pmax(dplyr::coalesce(as.numeric(x), 0), 0)
  lu <- ps2[, .(match_id = as.character(match_id), player_id = as.character(player_id),
               tog = pmax(time_on_ground_percentage, 1) / 100,
               dacts = dz(tackles) + dz(intercepts) + dz(one_percenters))]
  lu <- merge(lu, unique(np[, .(match_id = as.character(match_id), player_id = as.character(player_id), team)]),
              by = c("match_id", "player_id"))
  lu[, w := switch(pool_by, tog = tog, dacts = pmax(dacts, 0.5), inv_dacts = 1 / pmax(dacts, 0.5),
                   stop("unknown pool_by"))]
  ros <- merge(lu, pool, by = c("match_id", "team"), all.x = TRUE)[is.na(pool), pool := 0]
  ros[, share := pool * w / sum(w), by = .(match_id, team)]

  base <- merge(ros[, .(match_id, team, player_id, tog, share)],
               np[, .(match_id = as.character(match_id), player_id = as.character(player_id),
                     net_points_pre = net_points)],
               by = c("match_id", "player_id"), all.x = TRUE)
  base[is.na(net_points_pre), net_points_pre := 0]
  base[, base_val := net_points_pre + share]

  mg2 <- as.data.table(res)[, .(match_id = as.character(match_id), margin = home_score - away_score)]
  chk <- merge(base[, .(base_sum = sum(base_val)), by = .(match_id, team)], hax, by = c("match_id", "team"))
  chk <- merge(chk, mg2, by = "match_id")
  chk[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
  base <- merge(base, chk[, .(match_id, team, base_sum, want)], by = c("match_id", "team"))
  base[, safe := abs(base_sum) > 0.05 * abs(want) & abs(want) > 1e-9]

  base[safe == TRUE, final_val := base_val * want / base_sum]
  base[safe == FALSE, final_val := want * tog / sum(tog), by = .(match_id, team)]

  n_unsafe <- data.table::uniqueN(base[safe == FALSE, .(match_id, team)])
  n_total  <- data.table::uniqueN(base[, .(match_id, team)])
  say("  team-matches needing the flat-TOG fallback: ", n_unsafe, " of ", n_total)

  np[, match_id := as.character(match_id)]; np[, player_id := as.character(player_id)]
  np <- merge(np, base[, .(match_id, player_id, share, final_val)],
              by = c("match_id", "player_id"), all.x = TRUE)
  np[is.na(final_val), final_val := net_points]
  np[, net_points := final_val]
  np[]
}

say("\n=== Shipped design (dacts) ===")
shipped <- np_team_margin_shipped(copy(np_pre), pbp, ps, res, pool_by = "dacts")
fin <- merge(shipped[, .(tot = sum(net_points)), by = .(match_id, team)], ha, by = c("match_id", "team"))
fin <- merge(fin, mg, by = "match_id"); fin[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
say("  conservation gap: ", signif(max(abs(fin$tot - fin$want)), 4))

say("\n=== Sequential redesign (inv_dacts pool, then proportional rescale) ===")
seq_iv <- np_sequential(copy(np_pre), pbp, ps, res, pool_by = "inv_dacts")
fin2 <- merge(seq_iv[, .(tot = sum(net_points)), by = .(match_id, team)], ha, by = c("match_id", "team"))
fin2 <- merge(fin2, mg, by = "match_id"); fin2[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
say("  conservation gap: ", signif(max(abs(fin2$tot - fin2$want)), 4))

say("\n=== Sequential redesign (dacts pool -- same as shipped weight, to isolate the mechanism change) ===")
seq_dc <- np_sequential(copy(np_pre), pbp, ps, res, pool_by = "dacts")

pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
nm <- unique(pg[, .(match_id, player_id, position_group)])

by_pos <- function(dt, label) {
  dt <- merge(dt, nm, by = c("match_id", "player_id"), all.x = TRUE)
  dt[!is.na(position_group), .(net_points = round(mean(net_points), 3)), by = position_group][, arm := label]
}

allarm <- rbindlist(list(
  by_pos(shipped, "shipped (dacts)"),
  by_pos(seq_dc,  "sequential, dacts pool"),
  by_pos(seq_iv,  "sequential, inv_dacts pool")
))
say("\n=== Net points/game by position, across designs ===")
wide <- dcast(allarm, position_group ~ arm, value.var = "net_points")
setcolorder(wide, c("position_group", "shipped (dacts)", "sequential, dacts pool", "sequential, inv_dacts pool"))
order_lvls <- c("RUCK", "MIDFIELDER", "MEDIUM_FORWARD", "KEY_FORWARD", "MEDIUM_DEFENDER", "KEY_DEFENDER")
wide <- wide[match(order_lvls, position_group)]
print(wide)
