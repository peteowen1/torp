# A fast EPR aggregator for the optimiser, and the fidelity proof it needs.
#
# calculate_epr_stats_batch() already accepts all 12 parameters, so there is no
# need to reimplement the AGGREGATION LOGIC -- only to avoid repeating the
# expensive part. The expensive part is the non-equi cross join of every
# player-game against every round, and that join does not depend on any
# parameter: only the exp(-days_diff/decay) weights and the sums built from them
# do. So the join is computed once and reused.
#
# THE RISK this creates is the one this repo keeps hitting: a harness that
# silently differs from production measures the wrong thing. So the fast path is
# not trusted, it is PROVEN -- at the production defaults it must reproduce
# calculate_epr_stats_batch() to floating-point tolerance, asserted, before the
# optimiser is allowed to use it.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_fastpath_fidelity.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

CH <- c("recv", "disp", "spoil", "hitout")

#' Build the parameter-independent join once
#'
#' Everything here is fixed across parameter settings: which past games count
#' toward which round, and how many days before it each was played.
epr_prepare <- function(pgd, round_info) {
  dt <- data.table::as.data.table(pgd)
  data.table::setkey(dt, match_id)
  ri <- data.table::as.data.table(round_info)
  all_mids <- unique(dt$match_id)
  dt[, match_idx := match(match_id, all_mids)]
  ri[, match_idx_max := vapply(match_ref, function(ref) sum(all_mids <= ref), integer(1))]
  cross <- dt[ri, on = .(match_idx <= match_idx_max), allow.cartesian = TRUE, nomatch = NULL]
  cross[, days_diff := as.numeric(as.Date(date_val) - as.Date(utc_start_time))]
  cross <- cross[days_diff >= 0]
  # Use the opponent-adjusted channels when present, exactly as production does.
  has_oadj <- all(paste0("epv_", CH, "_oadj") %in% names(cross))
  for (c in CH) {
    src <- if (has_oadj) paste0("epv_", c, "_oadj") else paste0("epv_", c, "_adj")
    cross[, (paste0(".x_", c)) := get(src)]
  }
  # THE BUG THE FIDELITY ASSERTION CAUGHT: production weights each past game by
  # TOG as well as by decay --
  #   wt_gms_c = sum(wt_c * tog_safe)
  #   c_sum    = sum(x_c * tog_safe * wt_c)
  # so a half-game counts half. Omitting tog_safe put the worst channel 0.997
  # away from production, which is enormous on an epr_disp whose sd is 1.07.
  cross[, tog_safe := pmax(
    data.table::fifelse(is.na(time_on_ground_percentage), 100,
                        time_on_ground_percentage) / 100, 0.1)]
  # Pre-multiply once; it is parameter-independent.
  for (c in CH) cross[, (paste0(".xt_", c)) := get(paste0(".x_", c)) * tog_safe]
  data.table::setattr(cross, "has_oadj", has_oadj)
  cross[]
}

#' Aggregate to EPR for one parameter setting
#'
#' All four channels in ONE by-group pass rather than four, and note the sums use
#' na.rm while wt_gms is taken over every row -- production's second subtlety,
#' since a game with a missing channel still contributes its weight to the
#' denominator.
epr_fast <- function(cross, p) {
  d <- vapply(CH, function(c) p[[paste0("decay_", c)]], numeric(1))
  agg <- cross[, {
    w_recv   <- exp(-days_diff / d[["recv"]])
    w_disp   <- exp(-days_diff / d[["disp"]])
    w_spoil  <- exp(-days_diff / d[["spoil"]])
    w_hitout <- exp(-days_diff / d[["hitout"]])
    tg <- tog_safe
    .(wt_recv   = sum(w_recv * tg,   na.rm = TRUE),
      wt_disp   = sum(w_disp * tg,   na.rm = TRUE),
      wt_spoil  = sum(w_spoil * tg,  na.rm = TRUE),
      wt_hitout = sum(w_hitout * tg, na.rm = TRUE),
      s_recv    = sum(.xt_recv   * w_recv,   na.rm = TRUE),
      s_disp    = sum(.xt_disp   * w_disp,   na.rm = TRUE),
      s_spoil   = sum(.xt_spoil  * w_spoil,  na.rm = TRUE),
      s_hitout  = sum(.xt_hitout * w_hitout, na.rm = TRUE))
  }, by = .(round_val, player_id)]
  for (c in CH) {
    agg[, (paste0("epr_", c)) := .bayesian_shrink(
      get(paste0("s_", c)), get(paste0("wt_", c)), p$loading,
      p[[paste0("prior_games_", c)]], p[[paste0("prior_rate_", c)]])]
  }
  agg[, c(paste0("wt_", CH), paste0("s_", CH)) := NULL]
  agg[]
}

default_params <- function() list(
  loading = EPR_LOADING_DEFAULT,
  decay_recv = EPR_DECAY_RECV, decay_disp = EPR_DECAY_DISP,
  decay_spoil = EPR_DECAY_SPOIL, decay_hitout = EPR_DECAY_HITOUT,
  prior_games_recv = EPR_PRIOR_GAMES_RECV, prior_games_disp = EPR_PRIOR_GAMES_DISP,
  prior_games_spoil = EPR_PRIOR_GAMES_SPOIL, prior_games_hitout = EPR_PRIOR_GAMES_HITOUT,
  prior_rate_recv = EPR_PRIOR_RATE_RECV, prior_rate_disp = EPR_PRIOR_RATE_DISP,
  prior_rate_spoil = EPR_PRIOR_RATE_SPOIL, prior_rate_hitout = EPR_PRIOR_RATE_HITOUT
)

# ---- Fidelity proof --------------------------------------------------------
say("=== Fast EPR path: fidelity against production ===")

pgd <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v3.parquet")))
pgd <- adjust_epv_for_opponents(pgd)
if (isTRUE(EPV_LEVEL_CENTRE)) pgd <- centre_epv_by_position(pgd)
say("pgd rows ", format(nrow(pgd), big.mark = ","))

fixtures <- load_fixtures(TRUE)
YEAR <- 2025L
rounds <- 1:24
fix_dt <- as.data.table(fixtures)
fix_dates <- fix_dt[season == YEAR & round_number %in% rounds,
                    .(date_val = lubridate::as_date(min(utc_start_time))),
                    by = .(round_val = round_number)]
round_info <- data.table(round_val = rounds,
                         match_ref = paste0("CD_M", YEAR, "014", sprintf("%02d", rounds)))
round_info <- round_info[fix_dates, on = "round_val", nomatch = NULL]
say("rounds resolved: ", nrow(round_info))

t0 <- Sys.time()
prod <- as.data.table(calculate_epr_stats_batch(pgd, round_info))
t_prod <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
say("production batch: ", round(t_prod, 1), "s, ", nrow(prod), " rows")

t0 <- Sys.time()
cross <- epr_prepare(pgd, round_info)
t_prep <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
say("fast path PREP (once): ", round(t_prep, 1), "s, cross rows ",
    format(nrow(cross), big.mark = ","))

t0 <- Sys.time()
fast <- epr_fast(cross, default_params())
t_fast <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
say("fast path AGGREGATE (per evaluation): ", round(t_fast, 2), "s")
say("speedup per evaluation: ", round(t_prod / t_fast, 1), "x")

say("")
say("--- ASSERTION: fast path == production at default parameters ---")
cmp <- merge(prod[, c("player_id", "round_val", paste0("epr_", CH)), with = FALSE],
             fast, by = c("player_id", "round_val"), suffixes = c("_p", "_f"))
say("matched rows: ", nrow(cmp), " of production's ", nrow(prod))
worst <- 0
for (c in CH) {
  a <- cmp[[paste0("epr_", c, "_p")]]; b <- cmp[[paste0("epr_", c, "_f")]]
  d <- max(abs(a - b), na.rm = TRUE)
  worst <- max(worst, d)
  say(sprintf("  epr_%-7s max |diff| %.3e", c, d))
}
say("worst across channels: ", signif(worst, 3))
if (!is.finite(worst) || worst > 1e-9 || nrow(cmp) != nrow(prod)) {
  say("")
  say("FAIL -- the fast path does NOT reproduce production. It must not be used.")
  close(con); quit(status = 1)
}
say("")
say("PASS: the fast path reproduces production to ", signif(worst, 3), ".")
say("It is safe for the optimiser inner loop.")

# ---- Does the parameter actually move the output? --------------------------
say("")
say("--- sanity: a changed parameter must change the output ---")
p2 <- default_params(); p2$decay_spoil <- 180
f2 <- epr_fast(cross, p2)
c2 <- merge(fast[, .(player_id, round_val, base = epr_spoil)],
            f2[, .(player_id, round_val, alt = epr_spoil)],
            by = c("player_id", "round_val"))
say("decay_spoil 523 -> 180: mean |diff| ",
    round(mean(abs(c2$base - c2$alt), na.rm = TRUE), 5),
    " | max ", round(max(abs(c2$base - c2$alt), na.rm = TRUE), 4))
stopifnot(mean(abs(c2$base - c2$alt), na.rm = TRUE) > 1e-9)
say("(non-zero confirms the knob is live -- a no-op knob would make every")
say(" optimiser result meaningless while looking perfectly well-behaved.)")

saveRDS(list(prepare = epr_prepare, fast = epr_fast, defaults = default_params),
        file.path(OUT_DIR, "epv3_fastpath_fns.rds"))
say("")
say("wrote fast-path functions to epv3_fastpath_fns.rds")
close(con)
cat("\nWrote ", OUT, "\n")
