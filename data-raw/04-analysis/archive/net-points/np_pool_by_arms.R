# Which piece of the margin correction actually drives the defender gap, and
# does re-weighting NP_TEAM_MARGIN_POOL_BY fix it?
# =============================================================================
# Follow-up to np_team_pool_3way.R. That script showed the "margin
# reconciliation" component (currently hidden inside np_team / TEAM OFFENCE)
# runs far more negative for key defenders (-7.69/game) than rucks (-4.71).
# But that combined term is actually THREE things inside .np_team_margin()
# (epv_net_points.R:2352-2560):
#   named  -- each player's own individually-credited rows, rescaled to fit
#             the margin. NOT affected by NP_TEAM_MARGIN_POOL_BY.
#   share  -- his cut of the unnamed leftover on every row, spread by
#             NP_TEAM_MARGIN_POOL_BY ("dacts" shipped, or "tog"). This is the
#             only piece the constant controls.
#   recon  -- the final small gap-filler, always spread by TOG.
# This pulls all three apart by position, then re-runs the pool-spread step
# under three arms: shipped "dacts", "tog", and a new "inv_dacts" (defenders
# get LESS of the negative pool instead of more) to see whether the lever
# Pete wants actually closes the gap.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_pool_by_arms.R"'
#
# Env: NP3_SEASON (default 2026). Nothing here is shipped -- read-only
# measurement, no source file touched.
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

# --- a local copy of .np_team_margin() with `pool_by` as a real argument,
# instead of reading the NP_TEAM_MARGIN_POOL_BY constant. Everything else is
# byte-identical to epv_net_points.R:2352-2560. Read-only experiment; the
# shipped function and constant are untouched.
np_team_margin_arm <- function(np, pbp_data, player_stats, res, pool_by = "dacts") {
  pay <- attr(np, "np_payments")
  pay <- as.data.table(pay)
  np <- as.data.table(np)
  p <- as.data.table(pbp_data)
  pay[, match_id := as.character(match_id)]
  ha <- unique(p[, .(match_id = as.character(match_id), team, home_away)])
  pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)
  pay[, own := hm * data.table::fifelse(home_away == "Home", 1, -1)]
  if (!"doubled" %in% names(pay)) pay[, doubled := FALSE]
  pay[, v := sum(hm[doubled == FALSE]), by = .(match_id, display_order)]
  pay <- pay[abs(v) > 1e-12]
  pay[, gain_home := v > 0]
  pay[, side := data.table::fifelse((home_away == "Home") == gain_home, "gain", "concede")]
  pay[, side_sum := sum(own), by = .(match_id, display_order, side)]
  pay[, target := data.table::fifelse(side == "gain", abs(v), -abs(v))]
  pay[, scaled := data.table::fifelse(abs(side_sum) > 0.05 * abs(v),
                                      own * target / side_sum, NA_real_)]
  ns <- NP_TEAM_MARGIN_NAMED_SHARE
  pay[, named_tot := sum(scaled[!is.na(player_id)], na.rm = TRUE),
      by = .(match_id, display_order, team)]
  namd <- pay[!is.na(scaled) & !is.na(player_id), .(named = sum(scaled)),
              by = .(match_id, team, player_id = as.character(player_id))]
  prow <- pay[, .(target = target[1],
                  got_named = sum(scaled[!is.na(player_id)], na.rm = TRUE)),
              by = .(match_id, display_order, team)]
  prow[, pool_row := target - got_named]
  pool <- prow[, .(pool = sum(pool_row)), by = .(match_id, team)]

  ps2 <- as.data.table(player_stats)
  dz <- function(x) pmax(dplyr::coalesce(as.numeric(x), 0), 0)
  lu <- ps2[, .(match_id = as.character(match_id), player_id = as.character(player_id),
               tog = pmax(time_on_ground_percentage, 1) / 100,
               dacts = dz(tackles) + dz(intercepts) + dz(one_percenters))]
  lu <- merge(lu, unique(np[, .(match_id = as.character(match_id),
                                player_id = as.character(player_id), team)]),
              by = c("match_id", "player_id"))
  lu[, w := switch(pool_by,
                   "tog" = tog,
                   "dacts" = pmax(dacts, 0.5),
                   "inv_dacts" = 1 / pmax(dacts, 0.5),
                   stop("unknown pool_by"))]

  ros <- merge(lu, pool, by = c("match_id", "team"), all.x = TRUE)[is.na(pool), pool := 0]
  ros[, share := pool * w / sum(w), by = .(match_id, team)]
  out <- merge(ros[, .(match_id, team, player_id, tog, share)], namd,
               by = c("match_id", "team", "player_id"), all = TRUE)
  out[is.na(named), named := 0][is.na(share), share := 0][, val := named + share]
  if (anyNA(out$tog)) out[is.na(tog), tog := 0.75]

  mg <- as.data.table(res)[, .(match_id = as.character(match_id), margin = home_score - away_score)]
  chk <- merge(merge(out[, .(tot = sum(val)), by = .(match_id, team)], ha,
                     by = c("match_id", "team")), mg, by = "match_id")
  chk[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
  out <- merge(out, chk[, .(match_id, team, short = want - tot)], by = c("match_id", "team"))
  out[, recon := short * tog / sum(tog), by = .(match_id, team)]
  out[, val := val + recon]

  np[, match_id := as.character(match_id)]
  np[, player_id := as.character(player_id)]
  np <- merge(np, out[, .(match_id, player_id, named, share, recon, .new = val)],
              by = c("match_id", "player_id"), all.x = TRUE)
  np[is.na(.new), `:=`(.new = 0, named = 0, share = 0, recon = 0)]
  np[, np_team := np_team + (.new - net_points)]
  np[, net_points := .new]
  np[]
}

arms <- c("dacts", "tog", "inv_dacts")
pg <- as.data.table(load_player_game_ratings(SEASON))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
nm <- unique(pg[, .(match_id, player_id, player_name, position_group)])

results_by_arm <- list()
for (arm in arms) {
  say("\n=== Arm: pool_by = '", arm, "' ===")
  post <- np_team_margin_arm(copy(np_pre), pbp, ps, res, pool_by = arm)

  # Conservation must still hold under every arm -- structural, not tied to
  # the weight choice.
  ha <- unique(pbp[, .(match_id, team, home_away)])
  fin <- merge(post[, .(tot = sum(net_points)), by = .(match_id, team)], ha, by = c("match_id", "team"))
  mg <- as.data.table(res)[, .(match_id = as.character(match_id), margin = home_score - away_score)]
  fin <- merge(fin, mg, by = "match_id")
  fin[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
  gap <- max(abs(fin$tot - fin$want))
  say("  conservation check (should be ~0): ", signif(gap, 4))

  post <- merge(post, nm, by = c("match_id", "player_id"), all.x = TRUE)
  by_pos <- post[!is.na(position_group), .(
    named = round(mean(named), 3), share = round(mean(share), 3),
    recon = round(mean(recon), 3), net_points = round(mean(net_points), 3)
  ), by = position_group][order(-net_points)]
  print(by_pos)
  by_pos[, arm := arm]
  results_by_arm[[arm]] <- by_pos
}

say("\n=== Side by side: net points/game by position, across arms ===")
allarm <- rbindlist(results_by_arm)
wide <- dcast(allarm, position_group ~ arm, value.var = "net_points")
print(wide[order(-dacts)])

say("\n=== Key defender vs ruck gap, across arms (net points/game) ===")
gapchk <- allarm[position_group %in% c("KEY_DEFENDER", "RUCK", "MEDIUM_DEFENDER"),
                 .(position_group, arm, net_points)]
print(dcast(gapchk, position_group ~ arm, value.var = "net_points"))
