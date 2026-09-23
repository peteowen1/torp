# Sweep the ESPN team-sum convention: how much of the defensive charge should go
# to the beaten player's positional mirror, and how should the rest be spread?
#
# Scored on the criterion the whole v4 design used (D17): year-over-year
# repeatability of a player's per-game value, WITHIN position, so no arm can win
# by re-levelling positions. Team dependence is reported as a secondary read,
# never as the criterion: good players cluster at good clubs, so a low figure is
# not automatically a better metric, and a pure-noise metric would score zero.
suppressMessages({library(data.table); library(arrow); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
S <- "data-raw/outputs"
SEASONS <- c(2025L, 2026L)
MIN_G <- 10L
say <- function(...) cat(..., "\n", sep = "")
t0 <- Sys.time()

terms_all <- fread(file.path(S, "np_difficulty_terms_2025_2026.csv"))
terms_all[, match_id := as.character(match_id)]

# one build of the base ledger per season, reused by every arm
base <- list()
for (s in SEASONS) {
  pbp <- as.data.table(load_pbp(s)); ch <- as.data.table(load_chains(s))
  ps  <- as.data.table(load_player_stats(s, refresh = TRUE)); res <- as.data.table(load_results(s))
  tm  <- terms_all[substr(match_id, 5, 8) == as.character(s)]
  np  <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                          stoppages = "allocate", difficulty_terms = tm, return_payments = TRUE)
  pay <- as.data.table(attr(np, "np_payments")); pay[, match_id := as.character(match_id)]
  ha  <- unique(pbp[, .(match_id = as.character(match_id), team, home_away)])
  tid <- unique(pbp[, .(team_id = as.character(team_id), team)])
  dz <- function(x) pmax(dplyr::coalesce(as.numeric(x), 0), 0)
  tg <- ps[, .(match_id = as.character(match_id), player_id = as.character(player_id),
               team_id = as.character(team_id), tog = pmax(time_on_ground_percentage, 1) / 100,
               dacts = dz(tackles) + dz(intercepts) + dz(one_percenters))]
  tg <- merge(tg, tid, by = "team_id")[, team_id := NULL]
  base[[as.character(s)]] <- list(pay = pay, ha = ha, tog = tg,
    res = res[!is.na(home_score), .(match_id = as.character(match_id),
                                    margin = home_score - away_score)])
  rm(pbp, ch, ps, np); gc()
  say("built ", s, " (", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min)")
}

lu <- as.data.table(read_parquet(file.path(S, "v3v4_pgd_v4.parquet"),
        col_select = c("match_id","player_id","lineup_position","team","season",
                       "position_group","player_name")))
lu <- lu[season %in% SEASONS]
lu[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
info  <- lu[, .(name = player_name[1], pos = position_group[1]), by = player_id]
slots <- lu[!is.na(lineup_position) & lineup_position != "EMERG",
            .(match_id, team, player_id, slot = lineup_position)]

arm <- function(s, mirror_share, rest) {
  b <- base[[as.character(s)]]; pay <- copy(b$pay); ha <- b$ha
  pay <- merge(pay, ha, by = c("match_id","team"), all.x = TRUE)
  pay[, own := hm * fifelse(home_away == "Home", 1, -1)]
  pay[, v := sum(hm), by = .(match_id, display_order)]
  pay <- pay[abs(v) > 1e-12]
  pay[, gain_home := v > 0]
  pay[, side := fifelse((home_away == "Home") == gain_home, "gain", "concede")]
  pay[, side_sum := sum(own), by = .(match_id, display_order, side)]
  pay[, target := fifelse(side == "gain", abs(v), -abs(v))]
  # guard the rescale: a side whose payments nearly cancel would need a huge
  # multiplier, which would invent value. Those go to the pool instead.
  pay[, scaled := fifelse(abs(side_sum) > 0.05 * abs(v), own * target / side_sum, NA_real_)]

  mir <- NULL
  if (mirror_share > 0) {
    g <- pay[side == "gain" & !is.na(scaled) & !is.na(player_id) & scaled > 0]
    g <- g[order(-scaled), .SD[1], by = .(match_id, display_order)]
    g[, player_id := as.character(player_id)]
    g <- merge(g[, .(match_id, display_order, v, gain_team = team, gainer = player_id)],
               slots[, .(match_id, player_id, slot)],
               by.x = c("match_id","gainer"), by.y = c("match_id","player_id"), all.x = TRUE)
    g <- g[!is.na(slot)]
    g[, mirror := suppressWarnings(.np_mirror_of(slot))]
    g <- merge(g, unique(ha)[, .(match_id, opp_team = team)], by = "match_id",
               allow.cartesian = TRUE)
    g <- g[opp_team != gain_team]
    mir <- merge(g, slots, by.x = c("match_id","opp_team","mirror"),
                 by.y = c("match_id","team","slot"), allow.cartesian = TRUE)
    if (nrow(mir) > 0) {
      mir[, n_slot := .N, by = .(match_id, display_order)]
      mir[, charge := -abs(v) * mirror_share / n_slot]
    } else mir <- NULL
  }

  rows  <- unique(pay[, .(match_id, display_order, v, gain_home)])
  sides <- rbind(copy(rows)[, side := "gain"], copy(rows)[, side := "concede"])
  sides <- merge(sides, ha, by = "match_id", allow.cartesian = TRUE)
  sides <- sides[((home_away == "Home") == gain_home) == (side == "gain")]
  sides[, target := fifelse(side == "gain", abs(v), -abs(v))]
  cov <- pay[!is.na(scaled), .(got = sum(scaled)), by = .(match_id, display_order, side)]
  sides <- merge(sides, cov, by = c("match_id","display_order","side"), all.x = TRUE)
  if (is.null(mir)) {
    sides[, mir_got := 0]
  } else {
    mc <- mir[, .(mir_got = sum(charge)), by = .(match_id, display_order)]
    sides <- merge(sides, mc, by = c("match_id","display_order"), all.x = TRUE)
    sides[is.na(mir_got), mir_got := 0]
  }
  sides[side == "concede" & is.na(got), got := 0]
  pr <- sides[is.na(got) | side == "concede",
              .(match_id, team, scaled = target - fifelse(side == "concede", got + mir_got, 0))]
  pr <- pr[abs(scaled) > 1e-12]

  alloc <- rbind(
    pay[!is.na(scaled), .(match_id, team, player_id = as.character(player_id), scaled)],
    if (is.null(mir)) NULL else mir[, .(match_id, team = opp_team,
                                        player_id = as.character(player_id), scaled = charge)],
    pr[, .(match_id, team, player_id = NA_character_, scaled)])

  tg <- copy(b$tog)
  tg[, w := if (identical(rest, "tog")) tog else pmax(dacts, 0.5)]
  pool  <- alloc[is.na(player_id), .(pool = sum(scaled)), by = .(match_id, team)]
  named <- alloc[!is.na(player_id), .(named = sum(scaled)), by = .(match_id, team, player_id)]
  ros <- merge(tg, pool, by = c("match_id","team"), all.x = TRUE)[is.na(pool), pool := 0]
  ros[, share := pool * w / sum(w), by = .(match_id, team)]
  out <- merge(ros[, .(match_id, team, player_id, tog, share)], named,
               by = c("match_id","team","player_id"), all = TRUE)
  out[is.na(named), named := 0][is.na(share), share := 0][, val := named + share]
  chk <- merge(merge(out[, .(tot = sum(val)), by = .(match_id, team)], ha,
                     by = c("match_id","team")), b$res, by = "match_id")
  chk[, want := margin * fifelse(home_away == "Home", 1, -1)]
  out <- merge(out, chk[, .(match_id, team, short = want - tot)], by = c("match_id","team"))
  out[, val := val + short * tog / sum(tog), by = .(match_id, team)][, short := NULL]
  # the identity this whole exercise exists for
  fin <- merge(merge(out[, .(tot = sum(val)), by = .(match_id, team)], ha,
                     by = c("match_id","team")), b$res, by = "match_id")
  fin[, want := margin * fifelse(home_away == "Home", 1, -1)]
  stopifnot(max(abs(fin$tot - fin$want)) < 1e-8)
  out[, .(match_id, team, player_id, val)]
}

fz <- function(r, n) { z <- atanh(r); w <- n - 3; tanh(sum(z * w) / sum(w)) }

score <- function(label, ms, rest) {
  a <- rbindlist(lapply(SEASONS, function(s) arm(s, ms, rest)[, season := s]))
  p <- a[, .(g = .N, v = mean(val)), by = .(player_id, season, team)]
  p <- merge(p, info, by = "player_id")[g >= MIN_G]
  w <- dcast(p, player_id + pos ~ season, value.var = "v")
  setnames(w, c("2025","2026"), c("y1","y2"), skip_absent = TRUE)
  w <- w[!is.na(y1) & !is.na(y2)]
  by_pos <- w[, .(n = .N, r = if (.N > 8) cor(y1, y2) else NA_real_), by = pos][!is.na(r)]
  cur <- p[season == 2026]
  data.table(label, mirror = ms, rest, n = nrow(w),
             r_within = round(fz(by_pos$r, by_pos$n), 4),
             r_all    = round(cor(w$y1, w$y2), 4),
             team_pct = round(100 * summary(lm(cur$v ~ factor(cur$team)))$r.squared),
             sd_def   = round(sd(cur[pos %like% "DEFENDER"]$v), 2),
             sd_mid   = round(sd(cur[pos == "MIDFIELDER"]$v), 2))
}

grid <- rbind(
  data.table(ms = c(0, 0.25, 0.4, 0.5, 0.6, 0.75, 1.0), rest = "tog"),
  data.table(ms = c(0, 0.5), rest = "dacts"))

out <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
  r <- score(sprintf("mirror %.2f / %s", grid$ms[i], grid$rest[i]), grid$ms[i], grid$rest[i])
  say(sprintf("  %-22s r_within %.4f | r_all %.4f | team %2d%% | sd_def %.2f | sd_mid %.2f",
              r$label, r$r_within, r$r_all, r$team_pct, r$sd_def, r$sd_mid))
  r
}))
say("")
print(out[order(-r_within)])
fwrite(out, file.path(S, "espn_mirror_sweep.csv"))
say("\ndone in ", round(as.numeric(Sys.time() - t0, units = "mins"), 1), " min")
