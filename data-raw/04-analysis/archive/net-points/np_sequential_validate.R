# Face validity + year-over-year repeatability for the sequential redesign
# =============================================================================
# np_sequential_redesign.R found that switching the margin-reconciliation
# mechanism (pool first, then proportionally rescale own+pool, instead of an
# independent per-row rescale) closes almost the whole defender/forward gap in
# POSITION AVERAGES -- but every position converged toward zero, which is
# exactly the kind of summary statistic that can hide deleted signal. This
# runs the two checks every rating change here goes through before it's
# treated as real: named-player face validity and year-over-year
# repeatability, comparing shipped vs the sequential (dacts pool) design.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_sequential_validate.R"'
#
# Env: NP3_SEASONS (default "2025,2026").
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
say <- function(...) cat(..., "\n", sep = "")

SEASONS <- as.integer(strsplit(Sys.getenv("NP3_SEASONS", "2025,2026"), ",")[[1]])
S <- "data-raw/outputs"
tm_all <- fread(file.path(S, "np_difficulty_terms_2025_2026.csv"))
tm_all[, match_id := as.character(match_id)]

# --- the two mechanisms, unchanged from np_sequential_redesign.R -----------
np_team_margin_shipped <- function(np, pbp_data, player_stats, res, pool_by = "dacts") {
  pay <- as.data.table(attr(np, "np_payments")); np <- as.data.table(np); p <- as.data.table(pbp_data)
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
  lu[, w := pmax(dacts, 0.5)]
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

np_sequential <- function(np, pbp_data, player_stats, res, pool_by = "dacts") {
  pay <- as.data.table(attr(np, "np_payments")); np <- as.data.table(np); p <- as.data.table(pbp_data)
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
  lu[, w := pmax(dacts, 0.5)]
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
  np[, match_id := as.character(match_id)]; np[, player_id := as.character(player_id)]
  np <- merge(np, base[, .(match_id, player_id, final_val)], by = c("match_id", "player_id"), all.x = TRUE)
  np[is.na(final_val), final_val := net_points]
  np[, net_points := final_val]
  np[]
}

# --- build both designs, both seasons ---------------------------------------
per_season <- list()
for (sn in SEASONS) {
  say("\n=== Season ", sn, " ===")
  pbp <- as.data.table(load_pbp(sn)); pbp[, match_id := as.character(match_id)]
  ch  <- as.data.table(load_chains(sn))
  ps  <- as.data.table(load_player_stats(sn, refresh = TRUE))
  res <- as.data.table(load_results(sn))
  tm  <- tm_all[substr(match_id, 5, 8) == as.character(sn)]

  np_pre <- as.data.table(build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                                           stoppages = "allocate", difficulty_terms = tm,
                                           return_payments = TRUE))
  shipped <- np_team_margin_shipped(copy(np_pre), pbp, ps, res)[, .(match_id, player_id, net_points_shipped = net_points)]
  seq_dc  <- np_sequential(copy(np_pre), pbp, ps, res)[, .(match_id, player_id, net_points_seq = net_points)]
  both <- merge(shipped, seq_dc, by = c("match_id", "player_id"))
  both[, season := sn]
  per_season[[as.character(sn)]] <- both
}

pm <- rbindlist(per_season)

pg <- as.data.table(load_player_game_ratings(TRUE))
pg[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
nm <- unique(pg[, .(match_id, player_id, player_name, position_group)])
pm <- merge(pm, nm, by = c("match_id", "player_id"), all.x = TRUE)

# --- 1. Face validity: named players against their own position's average --
say("\n\n===================== FACE VALIDITY (2026 only) =====================")
season_pm <- pm[season == max(SEASONS)]
pos_avg <- season_pm[!is.na(position_group), .(pos_avg_shipped = mean(net_points_shipped),
                                                pos_avg_seq = mean(net_points_seq)),
                     by = position_group]

check_players <- c("Harris Andrews", "Charlie Curnow", "Aaron Naughton", "Nick Daicos", "Max Gawn")
pp <- season_pm[player_name %in% check_players & !is.na(position_group),
                .(gms = .N, shipped = round(mean(net_points_shipped), 3),
                  seq = round(mean(net_points_seq), 3)),
                by = .(player_name, position_group)]
pp <- merge(pp, pos_avg, by = "position_group")
pp[, `:=`(shipped_vs_pos = round(shipped - pos_avg_shipped, 3),
          seq_vs_pos = round(seq - pos_avg_seq, 3))]
print(pp[, .(player_name, position_group, gms, shipped, shipped_vs_pos, seq, seq_vs_pos)])

say("\n--- Top 10 by net_points, shipped vs sequential (2026, min 10 games) ---")
elig <- season_pm[, .N, by = player_id][N >= 10, player_id]
tot <- season_pm[player_id %in% elig, .(player_name = player_name[1], position_group = position_group[1],
                                        shipped = round(mean(net_points_shipped), 3),
                                        seq = round(mean(net_points_seq), 3)),
                 by = player_id]
say("Shipped top 10:")
print(tot[order(-shipped)][1:10, .(player_name, position_group, shipped, seq)])
say("Sequential top 10:")
print(tot[order(-seq)][1:10, .(player_name, position_group, shipped, seq)])

# --- 2. Year-over-year repeatability -----------------------------------------
say("\n\n===================== YEAR-OVER-YEAR REPEATABILITY =====================")
if (length(SEASONS) >= 2) {
  s1 <- min(SEASONS); s2 <- max(SEASONS)
  a <- pm[season == s1, .(player_id, position_group,
                          m1_shipped = net_points_shipped, m1_seq = net_points_seq)]
  a <- a[, .(position_group = position_group[1], gms1 = .N,
            m1_shipped = mean(m1_shipped), m1_seq = mean(m1_seq)), by = player_id]
  b <- pm[season == s2, .(player_id, m2_shipped = net_points_shipped, m2_seq = net_points_seq)]
  b <- b[, .(gms2 = .N, m2_shipped = mean(m2_shipped), m2_seq = mean(m2_seq)), by = player_id]
  yy <- merge(a, b, by = "player_id")
  yy <- yy[gms1 >= 10 & gms2 >= 10]
  say("n player-year pairs (>=10 games both seasons): ", nrow(yy))
  say("Overall year-over-year r -- shipped: ", round(cor(yy$m1_shipped, yy$m2_shipped), 4),
      " | sequential: ", round(cor(yy$m1_seq, yy$m2_seq), 4))

  say("\nBy broad line:")
  yy[, line := data.table::fcase(
    position_group %in% c("KEY_DEFENDER", "MEDIUM_DEFENDER"), "Defence",
    position_group %in% c("KEY_FORWARD", "MEDIUM_FORWARD"), "Forward",
    position_group == "MIDFIELDER", "Midfield",
    position_group == "RUCK", "Ruck",
    default = "Other")]
  byline <- yy[line != "Other", .(n = .N,
                                  r_shipped = round(cor(m1_shipped, m2_shipped), 3),
                                  r_seq = round(cor(m1_seq, m2_seq), 3)), by = line]
  print(byline)
} else {
  say("Need >=2 seasons for repeatability -- set NP3_SEASONS.")
}
