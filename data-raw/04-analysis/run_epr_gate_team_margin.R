#!/usr/bin/env Rscript
# Fast EPR gate: v4 as shipped against the team-sums-to-its-own-margin convention
# ==============================================================================
# The convention is ESPN Net Points': each team's players sum to that team's own
# margin, +63 and -63, rather than only the difference between the teams being
# pinned. Pete asked for it twice and was right that it is what the metric we are
# named after does; the scoping note is docs/plans/NET-POINTS-TEAM-SUM-CONVENTION.md.
#
# It has been judged on repeatability (worse: 0.591 shipped against 0.567) and on
# team dependence (better: 21% against 11%, and 14% with the mirror charge). It
# has never faced the gate that actually chose v4, which is what this runs.
#
# NOT a reconciliation tweak. Forcing team totals through .np_reconcile() needs a
# residual larger than the value it corrects -- that is the measured failure of
# level = "half_margin" -- so every row is RE-ALLOCATED instead: credit to the
# side that gained it, blame to the side that conceded it, keeping the shares the
# ledger already computes. Where a side has no named recipient on a row, which is
# most rows because a retained disposal names nobody on defence, that side's
# amount goes to a team pool spread by time on ground.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/run_epr_gate_team_margin.R"'
#
# About an hour: the shipped arm is cached, the convention arm rebuilds the
# ledger with payments season by season.
suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_suite.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
SEASONS <- 2021:get_afl_season()
MIRROR_SHARE <- as.numeric(Sys.getenv("TM_MIRROR_SHARE", "0"))
# The configuration that beat the shipped ledger on repeatability (0.606 against
# 0.591): a flat split per row between the named player and his side, with the
# side pool spread by defensive acts rather than minutes. Pete's design.
NAMED_SHARE <- as.numeric(Sys.getenv("TM_NAMED_SHARE", "0.5"))
POOL_BY     <- Sys.getenv("TM_POOL_BY", "dacts")
sink(file.path(OUT_DIR, "epr_gate_team_margin.txt"), split = TRUE)
cat("=== fast EPR gate: v4 shipped vs team-sums-to-its-own-margin ===\n")
cat("run at", format(Sys.time()), "| mirror share", MIRROR_SHARE, "\n")
t0 <- Sys.time()
set_const <- function(...) {
  vals <- list(...); for (nm in names(vals)) assignInNamespace(nm, vals[[nm]], ns = "torp")
}

# ---- the convention, as a transform of one season's payments ----------------
team_margin_values <- function(pay, ha, tog, margins, slots, mirror_share,
                               named_share = NA_real_, pool_by = "tog") {
  pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)
  pay[, own := hm * fifelse(home_away == "Home", 1, -1)]
  pay[, v := sum(hm), by = .(match_id, display_order)]
  pay <- pay[abs(v) > 1e-12]
  pay[, gain_home := v > 0]
  pay[, side := fifelse((home_away == "Home") == gain_home, "gain", "concede")]
  pay[, side_sum := sum(own), by = .(match_id, display_order, side)]
  pay[, target := fifelse(side == "gain", abs(v), -abs(v))]
  # guard: a side whose payments nearly cancel would need a huge multiplier,
  # which would invent value. Those go to the pool instead.
  pay[, scaled := fifelse(abs(side_sum) > 0.05 * abs(v), own * target / side_sum, NA_real_)]

  mir <- NULL
  if (mirror_share > 0) {
    g <- pay[side == "gain" & !is.na(scaled) & !is.na(player_id) & scaled > 0]
    g <- g[order(-scaled), .SD[1], by = .(match_id, display_order)]
    g[, player_id := as.character(player_id)]
    g <- merge(g[, .(match_id, display_order, v, gain_team = team, gainer = player_id)],
               slots[, .(match_id, player_id, slot)],
               by.x = c("match_id", "gainer"), by.y = c("match_id", "player_id"), all.x = TRUE)
    g <- g[!is.na(slot)]
    g[, mirror := suppressWarnings(.np_mirror_of(slot))]
    g <- merge(g, unique(ha)[, .(match_id, opp_team = team)], by = "match_id",
               allow.cartesian = TRUE)[opp_team != gain_team]
    mir <- merge(g, slots, by.x = c("match_id", "opp_team", "mirror"),
                 by.y = c("match_id", "team", "slot"), allow.cartesian = TRUE)
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
  sides <- merge(sides, cov, by = c("match_id", "display_order", "side"), all.x = TRUE)
  if (is.null(mir)) {
    sides[, mir_got := 0]
  } else {
    mc <- mir[, .(mir_got = sum(charge)), by = .(match_id, display_order)]
    sides <- merge(sides, mc, by = c("match_id", "display_order"), all.x = TRUE)
    sides[is.na(mir_got), mir_got := 0]
  }
  sides[side == "concede" & is.na(got), got := 0]
  # carry display_order: the per-row split below groups on it, and building this
  # without it is what killed the first run of this gate
  pr <- sides[is.na(got) | side == "concede",
              .(match_id, display_order, team,
                scaled = target - fifelse(side == "concede", got + mir_got, 0))]
  pr <- pr[abs(scaled) > 1e-12]

  alloc <- rbind(
    pay[!is.na(scaled), .(match_id, display_order, team,
                          player_id = as.character(player_id), scaled)],
    if (is.null(mir)) NULL else mir[, .(match_id, display_order, team = opp_team,
                                        player_id = as.character(player_id), scaled = charge)],
    pr[, .(match_id, display_order, team, player_id = NA_character_, scaled)])

  # Flat split PER ROW: the named player takes named_share of that row's charge,
  # his side's pool the rest. Applying this per MATCH instead made every player's
  # number a rescaling of the final score and dropped repeatability to 0.04.
  if (!is.na(named_share)) {
    alloc[, row_tot := sum(scaled), by = .(match_id, display_order, team)]
    alloc[, named_tot := sum(scaled[!is.na(player_id)]), by = .(match_id, display_order, team)]
    named <- alloc[!is.na(player_id) & abs(named_tot) > 1e-12]
    named[, scaled := scaled * named_share * row_tot / named_tot]
    pools <- unique(alloc[, .(match_id, display_order, team, row_tot, named_tot)])
    pools[, scaled := fifelse(abs(named_tot) > 1e-12,
                              (1 - named_share) * row_tot, row_tot)]
    alloc <- rbind(named[, .(match_id, team, player_id, scaled)],
                   pools[, .(match_id, team, player_id = NA_character_, scaled)])
  } else {
    alloc <- alloc[, .(match_id, team, player_id, scaled)]
  }
  tog[, w := if (identical(pool_by, "tog")) tog else pmax(dacts, 0.5)]

  pool  <- alloc[is.na(player_id), .(pool = sum(scaled)), by = .(match_id, team)]
  named <- alloc[!is.na(player_id), .(named = sum(scaled)), by = .(match_id, team, player_id)]
  ros <- merge(tog, pool, by = c("match_id", "team"), all.x = TRUE)[is.na(pool), pool := 0]
  ros[, share := pool * w / sum(w), by = .(match_id, team)]
  out <- merge(ros[, .(match_id, team, player_id, tog, share)], named,
               by = c("match_id", "team", "player_id"), all = TRUE)
  out[is.na(named), named := 0][is.na(share), share := 0][, val := named + share]
  chk <- merge(merge(out[, .(tot = sum(val)), by = .(match_id, team)], ha,
                     by = c("match_id", "team")), margins, by = "match_id")
  chk[, want := margin * fifelse(home_away == "Home", 1, -1)]
  out <- merge(out, chk[, .(match_id, team, short = want - tot)], by = c("match_id", "team"))
  out[, val := val + short * tog / sum(tog), by = .(match_id, team)][, short := NULL]
  fin <- merge(merge(out[, .(tot = sum(val)), by = .(match_id, team)], ha,
                     by = c("match_id", "team")), margins, by = "match_id")
  fin[, want := margin * fifelse(home_away == "Home", 1, -1)]
  # the identity this whole exercise exists for
  stopifnot(max(abs(fin$tot - fin$want)) < 1e-8)
  out[, .(match_id, team, player_id, val)]
}

build_pgd <- function(tag) {
  f <- file.path(OUT_DIR, paste0("tm_pgd_", tag, ".parquet"))
  if (tag == "v4" && file.exists(file.path(OUT_DIR, "v3v4_pgd_v4.parquet"))) {
    cli::cli_alert_info("Reusing the shipped v4 frame")
    d <- as.data.table(read_parquet(file.path(OUT_DIR, "v3v4_pgd_v4.parquet")))
    setattr(d, "epv_engine", "v4"); return(d)
  }
  if (file.exists(f)) {
    cli::cli_alert_info("Reusing cached {tag} frame")
    d <- as.data.table(read_parquet(f)); setattr(d, "epv_engine", "v4"); return(d)
  }
  base <- as.data.table(read_parquet(file.path(OUT_DIR, "v3v4_pgd_v4.parquet")))
  base[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  slots_all <- base[!is.na(lineup_position) & lineup_position != "EMERG",
                    .(match_id, team, player_id, slot = lineup_position)]
  parts <- lapply(SEASONS, function(s) {
    cli::cli_h2("convention: season {s}")
    pbp <- as.data.table(load_pbp(s, rounds = TRUE))
    chains <- as.data.table(load_chains(s, rounds = TRUE))
    ps <- as.data.table(load_player_stats(s)); res <- as.data.table(load_results(s))
    terms <- np_difficulty_terms_for_season(s, pbp, chains, available = SEASONS)
    np <- build_net_points(pbp, ps, res, chains = chains, credit = "difficulty",
                           stoppages = "allocate", difficulty_terms = terms,
                           return_payments = TRUE)
    pay <- as.data.table(attr(np, "np_payments")); pay[, match_id := as.character(match_id)]
    ha <- unique(pbp[, .(match_id = as.character(match_id), team, home_away)])
    tid <- unique(pbp[, .(team_id = as.character(team_id), team)])
    dz <- function(x) pmax(dplyr::coalesce(as.numeric(x), 0), 0)
    tog <- ps[, .(match_id = as.character(match_id), player_id = as.character(player_id),
                  team_id = as.character(team_id),
                  tog = pmax(time_on_ground_percentage, 1) / 100,
                  dacts = dz(tackles) + dz(intercepts) + dz(one_percenters))]
    tog <- merge(tog, tid, by = "team_id")[, team_id := NULL]
    margins <- res[!is.na(home_score), .(match_id = as.character(match_id),
                                         margin = home_score - away_score)]
    tv <- team_margin_values(pay, ha, tog, margins,
                             slots_all[substr(match_id, 5, 8) == as.character(s)],
                             MIRROR_SHARE, NAMED_SHARE, POOL_BY)
    d <- base[season == s]
    d <- merge(d, tv[, .(match_id, player_id, new_val = val)],
               by = c("match_id", "player_id"), all.x = TRUE)
    d[is.na(new_val), new_val := 0]
    # keep the channel breakdown proportional: the convention changes the level
    # and who holds the value, not which channel earned it
    d[, .old := epv]
    d[, scale := fifelse(abs(.old) > 1e-9, new_val / .old, 0)]
    for (ch in c("epv_recv", "epv_disp", "epv_spoil")) {
      set(d, j = ch, value = d[[ch]] * d$scale)
    }
    d[, epv := epv_recv + epv_disp + epv_spoil]
    # a player whose old total was ~0 keeps his new value in the own-acts channel
    d[abs(.old) <= 1e-9, epv_disp := new_val][abs(.old) <= 1e-9, epv := new_val]
    d[, c(".old", "scale", "new_val") := NULL]
    rm(pbp, chains, ps, np, pay); invisible(gc(verbose = FALSE))
    d
  })
  d <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  setattr(d, "epv_engine", "v4")
  write_parquet(d, f)
  d
}

shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res_all <- as.data.table(load_results(TRUE))

build_ratings <- function(pgd, tag) {
  cached <- if (tag == "v4") file.path(OUT_DIR, "v3v4_rt_v4.parquet") else
                             file.path(OUT_DIR, paste0("tm_rt_", tag, ".parquet"))
  if (file.exists(cached)) {
    cli::cli_alert_info("Reusing cached {tag} ratings")
    return(as.data.table(read_parquet(cached)))
  }
  set_const(EPV_ENGINE = "v4", EPR_PRIOR_RATE_RECV = -0.7,
            EPR_PRIOR_RATE_DISP = -0.7, EPR_PRIOR_RATE_SPOIL = -0.3)
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", "v4")
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) {
    out <- calculate_torp(out, psr_df)
  }
  out <- as.data.table(out); write_parquet(out, cached); out
}

pgd_a <- build_pgd("v4");  cat("shipped frame:", nrow(pgd_a), "rows\n")
pgd_b <- build_pgd("tm");  cat("convention frame:", nrow(pgd_b), "rows\n")

cmp <- merge(pgd_a[, .(match_id = as.character(match_id), player_id = as.character(player_id), a = epv)],
             pgd_b[, .(match_id = as.character(match_id), player_id = as.character(player_id), b = epv)],
             by = c("match_id", "player_id"))
cat(sprintf("ARMS GUARD: %d shared player-games, mean |diff| %.4f, identical %.1f%%\n",
            nrow(cmp), mean(abs(cmp$a - cmp$b)), 100 * mean(abs(cmp$a - cmp$b) < 1e-9)))
stopifnot(mean(abs(cmp$a - cmp$b)) > 1e-6)

rt_a <- build_ratings(pgd_a, "v4"); rt_b <- build_ratings(pgd_b, "tm")

cat("\n==== FACE VALIDITY ====\n")
snap <- function(rt) {
  rt <- as.data.table(rt)[!is.na(epr)]
  last <- rt[season == max(season), max(round)]
  rt[season == max(season) & round == last]
}
fv <- tryCatch(face_validity(snap(rt_a), snap(rt_b)),
               error = function(e) { cat("face_validity failed:", conditionMessage(e), "\n"); NULL })
if (!is.null(fv)) print(fv)

cat("\n==== FAST EPR GATE (lineup rating -> team points, club and season FE) ====\n")
ga <- bm_epr_gate(pgd_a, rt_a, res_all, "v4 shipped")
gb <- bm_epr_gate(pgd_b, rt_b, res_all, "team sums to its own margin")
print(ga); print(gb); compare_epr_gates(ga, gb)

cat("\n==== BENCHMARK SUITE (calibrated) ====\n")
ba <- tryCatch(benchmark_rating(pgd_a, "v4", results = res_all), error = function(e) NULL)
bb <- tryCatch(benchmark_rating(pgd_b, "v4", results = res_all), error = function(e) NULL)
if (!is.null(ba) && !is.null(bb)) { print(ba); print(bb); compare_benchmarks(ba, bb) }

saveRDS(list(ga = ga, gb = gb, ba = ba, bb = bb),
        file.path(OUT_DIR, "epr_gate_team_margin.rds"))
cat("\ndone in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "min\n")
sink()
