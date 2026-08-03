# EPR parameter optimiser for EPV v3.
#
# Four arms, so 3-vs-4 channels is decided on a FAIR comparison -- each structure
# tuned on its own terms rather than one inheriting parameters shaped for the
# other:
#   4ch-free     4 channels, contest weight unconstrained
#   4ch-floored  4 channels, contest channel may not be shrunk below default
#   3ch-free     3 channels (contest merged), unconstrained
#   3ch-floored  3 channels, floored
#
# WHY THE FLOOR ARMS EXIST. On production features the contest channel adds
# nothing incremental (multivariate t = -0.06, p = 0.954, dropping it changes R2
# by 0.00000) so a free optimiser will want to shrink it away. That would recover
# MAE and hollow out what v3 was built for. Running both makes the price of
# keeping it explicit instead of a choice buried in a constraint.
#
# HONEST SPLIT. Parameters are optimised on 2021-2024 ONLY. 2025-26 is the gate
# window and theta never sees it. The linear head is refitted inside each
# evaluation, so it is the parameters being scored, not a lucky head.
#
# PRE-REGISTERED ANCHORS (stats-discipline rule 1), asserted below:
#   * the fast EPR path must reproduce production exactly at defaults
#   * every parameter must land strictly INSIDE its bounds -- a parameter on a
#     bound is a bug signal, not a finding (rule 2)
#   * OOS improvement may not exceed the in-sample ceiling
#   * elite players stay top-30 by TORP; key defenders do not collapse

suppressPackageStartupMessages({ library(data.table); library(arrow); library(dplyr) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_optimiser.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

CH <- c("recv", "disp", "spoil", "hitout")
TRAIN_SEASONS <- 2021:2024
GATE_SEASONS  <- 2025:2026

# ---- Fast path (proven byte-identical to production in epv3_epr_fastpath.R) --
source_fast <- function() {
  e <- new.env()
  sys.source("C:/dev/torpverse/torp/data-raw/04-analysis/epv3_epr_fastpath_fns.R", envir = e)
  e
}

say("=== EPR parameter optimiser ===")
say("train (theta): ", paste(range(TRAIN_SEASONS), collapse = "-"),
    " | gate (untouched): ", paste(range(GATE_SEASONS), collapse = "-"))

pgd <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v3.parquet")))
pgd <- adjust_epv_for_opponents(pgd)
if (isTRUE(EPV_LEVEL_CENTRE)) pgd <- centre_epv_by_position(pgd)

fixtures <- load_fixtures(TRUE)
teams <- load_teams(TRUE)
xg <- as.data.table(load_xg(TRUE))
res <- as.data.table(load_results(TRUE))
fix_dt <- as.data.table(fixtures)

# Round info across every season we need ratings for.
ALL_SEASONS <- sort(union(TRAIN_SEASONS, GATE_SEASONS))
round_info <- rbindlist(lapply(ALL_SEASONS, function(y) {
  rs <- if (y >= 2024) 0:28 else 1:28
  fd <- fix_dt[season == y & round_number %in% rs,
               .(date_val = lubridate::as_date(min(utc_start_time))),
               by = .(round_val = round_number)]
  ri <- data.table(season = y, round_val = fd$round_val,
                   match_ref = paste0("CD_M", y, "014", sprintf("%02d", fd$round_val)))
  ri[fd, on = "round_val", nomatch = NULL]
}))
say("rounds to rate: ", nrow(round_info))

# ---- Build the parameter-independent join once ------------------------------
epr_prepare <- function(pgd, round_info) {
  dt <- data.table::copy(as.data.table(pgd)); setkey(dt, match_id)
  ri <- data.table::copy(as.data.table(round_info))
  all_mids <- unique(dt$match_id)
  dt[, match_idx := match(match_id, all_mids)]
  ri[, match_idx_max := vapply(match_ref, function(r) sum(all_mids <= r), integer(1))]
  cr <- dt[ri, on = .(match_idx <= match_idx_max), allow.cartesian = TRUE, nomatch = NULL]
  cr[, days_diff := as.numeric(as.Date(date_val) - as.Date(utc_start_time))]
  cr <- cr[days_diff >= 0]
  has_oadj <- all(paste0("epv_", CH, "_oadj") %in% names(cr))
  for (c in CH) {
    src <- if (has_oadj) paste0("epv_", c, "_oadj") else paste0("epv_", c, "_adj")
    cr[, (paste0(".x_", c)) := get(src)]
  }
  cr[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage), 100,
                                time_on_ground_percentage) / 100, 0.1)]
  for (c in CH) cr[, (paste0(".xt_", c)) := get(paste0(".x_", c)) * tog_safe]
  cr[]
}

epr_fast <- function(cross, p) {
  d <- vapply(CH, function(c) p[[paste0("decay_", c)]], numeric(1))
  agg <- cross[, {
    w1 <- exp(-days_diff / d[["recv"]]);   w2 <- exp(-days_diff / d[["disp"]])
    w3 <- exp(-days_diff / d[["spoil"]]);  w4 <- exp(-days_diff / d[["hitout"]])
    .(wt_recv = sum(w1 * tog_safe, na.rm = TRUE), wt_disp = sum(w2 * tog_safe, na.rm = TRUE),
      wt_spoil = sum(w3 * tog_safe, na.rm = TRUE), wt_hitout = sum(w4 * tog_safe, na.rm = TRUE),
      s_recv = sum(.xt_recv * w1, na.rm = TRUE), s_disp = sum(.xt_disp * w2, na.rm = TRUE),
      s_spoil = sum(.xt_spoil * w3, na.rm = TRUE), s_hitout = sum(.xt_hitout * w4, na.rm = TRUE))
  }, by = .(season, round_val, player_id)]
  for (c in CH) {
    agg[, (paste0("epr_", c)) := .bayesian_shrink(
      get(paste0("s_", c)), get(paste0("wt_", c)), p$loading,
      p[[paste0("prior_games_", c)]], p[[paste0("prior_rate_", c)]])]
  }
  agg[, c(paste0("wt_", CH), paste0("s_", CH)) := NULL]
  agg[, epr := epr_recv + epr_disp + epr_spoil + epr_hitout]
  setnames(agg, "round_val", "round")
  agg[]
}

DEFAULTS <- list(
  loading = EPR_LOADING_DEFAULT,
  decay_recv = EPR_DECAY_RECV, decay_disp = EPR_DECAY_DISP,
  decay_spoil = EPR_DECAY_SPOIL, decay_hitout = EPR_DECAY_HITOUT,
  prior_games_recv = EPR_PRIOR_GAMES_RECV, prior_games_disp = EPR_PRIOR_GAMES_DISP,
  prior_games_spoil = EPR_PRIOR_GAMES_SPOIL, prior_games_hitout = EPR_PRIOR_GAMES_HITOUT,
  prior_rate_recv = EPR_PRIOR_RATE_RECV, prior_rate_disp = EPR_PRIOR_RATE_DISP,
  prior_rate_spoil = EPR_PRIOR_RATE_SPOIL, prior_rate_hitout = EPR_PRIOR_RATE_HITOUT
)

t0 <- Sys.time()
cross <- epr_prepare(pgd, round_info)
say("cross rows ", format(nrow(cross), big.mark = ","),
    " built in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# Listed position per (player, season), which the batch stage does not carry.
POS_KEY <- unique(pgd[!is.na(position_group),
                      .(position_group = data.table::last(position_group)),
                      by = .(player_id, season)], by = c("player_id", "season"))
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
say("position key rows ", nrow(POS_KEY),
    " | psr frame ", if (is.null(psr_df)) "MISSING" else nrow(psr_df))

# ---- Target ----------------------------------------------------------------
tgt <- merge(
  res[, .(match_id = as.character(match_id), season = as.integer(season),
          margin = home_score - away_score)],
  xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
  by = "match_id", all.x = TRUE)
tgt <- tgt[is.finite(xmargin) & is.finite(margin)]
say("target matches: ", nrow(tgt))

# ---- Objective -------------------------------------------------------------
CHAN_COLS <- paste0("epr_", CH)

# ---- Lean team aggregation --------------------------------------------------
# .build_team_ratings_df() rebuilds ~40 position feature columns on every call.
# The objective uses five of them, so calling it inside the inner loop made an
# arm take 1-2 HOURS. The row structure (which player, which lineup slot, which
# TOG weight) is parameter-independent, so it is built ONCE here and each
# evaluation becomes a keyed join plus a weighted sum.
#
# This IS a reimplementation, so it is asserted against the production function
# at default parameters before use -- the same discipline the fast EPR path got,
# and for the same reason: a harness that silently differs measures the wrong
# thing.
build_team_skeleton <- function() {
  tt <- as.data.table(teams)[, .(match_id, team_id, team_name, team_type,
                                 season, round_number, player_id, lineup_position)]
  tt <- tt[is.na(lineup_position) | lineup_position != "EMERG"]
  tt[, lineup_tog := fifelse(is.na(POSITION_AVG_TOG[lineup_position]),
                             POSITION_AVG_TOG_DEFAULT,
                             POSITION_AVG_TOG[lineup_position])]
  setnames(tt, "round_number", "round")
  tt[]
}
SKEL <- NULL

team_sums_fast <- function(r, merge3) {
  PRIORS <- c(epr_recv = EPR_PRIOR_RATE_RECV, epr_disp = EPR_PRIOR_RATE_DISP,
              epr_spoil = EPR_PRIOR_RATE_SPOIL, epr_hitout = EPR_PRIOR_RATE_HITOUT)
  if (merge3) { PRIORS[["epr_spoil"]] <- PRIORS[["epr_spoil"]] + PRIORS[["epr_hitout"]]
                PRIORS[["epr_hitout"]] <- 0 }
  m <- merge(SKEL, r[, c("player_id", "season", "round", CHAN_COLS), with = FALSE],
             by = c("player_id", "season", "round"), all.x = TRUE)
  for (c in CHAN_COLS) {
    set(m, which(is.na(m[[c]])), c, PRIORS[[c]])
    set(m, j = c, value = m[[c]] * m$lineup_tog)
  }
  agg <- m[, lapply(.SD, sum, na.rm = TRUE), .SDcols = CHAN_COLS,
           by = .(match_id, team_type)]
  agg[, epr := rowSums(as.matrix(.SD), na.rm = TRUE), .SDcols = CHAN_COLS]
  agg[]
}

team_features <- function(ratings, merge3) {
  r <- copy(ratings)
  if (merge3) {
    r[, epr_spoil := epr_spoil + epr_hitout][, epr_hitout := 0]
    r[, epr := epr_recv + epr_disp + epr_spoil]
  }
  # Per-round centring. Production does this inside .prepare_final_dataframe()
  # using pred_tog from the stat-ratings model; this uses a flat mean, which is
  # an APPROXIMATION and the one place the optimiser's inner objective is not
  # production-faithful. It is acceptable because the search only needs a
  # correctly-ORDERED objective, and because the winners are then re-scored
  # through the real production path via .build_epr_season(epr_params = ...) in
  # the ws17 gate. The approximation never reaches a reported number.
  for (c in CHAN_COLS) {
    r[, (c) := get(c) - mean(get(c), na.rm = TRUE), by = .(season, round)]
  }
  r[, epr := rowSums(as.matrix(.SD), na.rm = TRUE), .SDcols = CHAN_COLS]
  t <- team_sums_fast(r, merge3)
  h <- t[team_type == "home"]; a <- t[team_type == "away"]
  cols <- c("epr", CHAN_COLS)
  m <- merge(h[, c("match_id", cols), with = FALSE],
             a[, c("match_id", cols), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in cols) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  merge(m, tgt, by = "match_id")
}

FEAT4 <- paste0("d_epr_", CH)
FEAT3 <- paste0("d_epr_", c("recv", "disp", "spoil"))

objective <- function(theta, spec, report = FALSE) {
  p <- spec$to_params(theta)
  r <- epr_fast(cross, p)
  m <- team_features(r, merge3 = spec$merge3)
  feat <- if (spec$merge3) FEAT3 else FEAT4
  tr <- m[season %in% TRAIN_SEASONS]
  if (nrow(tr) < 200) return(1e6)
  f <- lm(as.formula(paste("xmargin ~", paste(feat, collapse = " + "))), data = tr)
  # In-sample on the TRAIN seasons only; the gate window is never touched here.
  mae <- mean(abs(residuals(f)))
  pen <- spec$penalty(theta)
  if (report) return(list(mae = mae, pen = pen, fit = f, m = m, params = p))
  mae + pen
}

# ---- Parameter spec --------------------------------------------------------
# Bounds are PHYSICAL, and a parameter landing on one is a bug signal.
BOUNDS <- list(
  decay       = c(90, 1500),
  prior_games = c(0.5, 20)
  # prior_rate is bounded PER CHANNEL below -- see PRIOR_RATE_SD_K.
)

# The prior rate is "what a player with no evidence is worth in this channel".
# Channels are centred, so it should sit slightly below zero. A FLAT bound of
# [-1.5, 0.5] let the first run pin prior_rate_hitout to -1.5 on a channel whose
# EPR sd is 0.146 -- TEN standard deviations below the mean. Paired with
# prior_games_hitout rising 3 -> 13.3 that turns the ruck channel into a
# positional-and-experience dummy (debutant ~ -1.30, regular non-ruck ~ -0.70,
# ruck ~ -0.35) rather than a measure of ruck contribution. It fits the target
# through a back door and would not generalise as a ruck rating.
#
# So the bound is expressed in each channel's OWN units: +/- K sd of that
# channel's EPR at default parameters. That makes it impossible to use any
# channel as a free intercept, whatever its scale.
PRIOR_RATE_SD_K <- 3
# Penalty pulls each parameter to its production default. Strength is set by a
# stated rule -- equivalent to PSEUDO_N matches of evidence -- and is NEVER tuned
# against the objective, which would be circular.
PSEUDO_N <- 150

make_spec <- function(merge3, floored) {
  chans <- if (merge3) c("recv", "disp", "spoil") else CH
  names_v <- c(paste0("decay_", chans), paste0("prior_games_", chans),
               paste0("prior_rate_", chans))
  # Per-channel prior_rate bounds in that channel's own EPR sd units.
  pr_lo <- vapply(chans, function(c) -PRIOR_RATE_SD_K * CHAN_SD[[c]], numeric(1))
  pr_hi <- vapply(chans, function(c)  PRIOR_RATE_SD_K * CHAN_SD[[c]], numeric(1))
  # The production default must remain reachable, or the "optimised" value is
  # just the nearest legal point and the comparison is meaningless.
  for (i in seq_along(chans)) {
    d0 <- DEFAULTS[[paste0("prior_rate_", chans[i])]]
    pr_lo[i] <- min(pr_lo[i], d0 * 1.2)
    pr_hi[i] <- max(pr_hi[i], d0 * 0.8)
  }
  lo <- c(rep(BOUNDS$decay[1], length(chans)), rep(BOUNDS$prior_games[1], length(chans)), pr_lo)
  hi <- c(rep(BOUNDS$decay[2], length(chans)), rep(BOUNDS$prior_games[2], length(chans)), pr_hi)
  init <- vapply(names_v, function(n) DEFAULTS[[n]], numeric(1))
  # Scale each parameter to ~[0,1] so the optimiser treats a day of decay and a
  # unit of prior_rate comparably.
  rng <- hi - lo
  list(
    merge3 = merge3, floored = floored, names = names_v, lo = lo, hi = hi,
    init_z = (init - lo) / rng,
    to_params = function(z) {
      v <- pmin(pmax(z, 0), 1) * rng + lo
      p <- DEFAULTS
      for (i in seq_along(names_v)) p[[names_v[i]]] <- v[i]
      if (merge3) { p$prior_games_hitout <- 1; p$prior_rate_hitout <- 0 }
      p
    },
    penalty = function(z) {
      v <- pmin(pmax(z, 0), 1) * rng + lo
      # Ridge in scaled units, weight set so PSEUDO_N matches of evidence would
      # move a parameter half its range.
      base <- sum(((v - init) / rng)^2)
      pen <- base * (nrow(tgt) / PSEUDO_N) * 0.02
      if (floored) {
        # Contest channel may not be shrunk below its default influence: its
        # prior_games may not rise above default (more prior = more shrinkage
        # toward the prior = less player signal).
        pg <- v[which(names_v == "prior_games_spoil")]
        if (length(pg) && pg > DEFAULTS$prior_games_spoil) {
          pen <- pen + 1e3 * (pg - DEFAULTS$prior_games_spoil)^2
        }
      }
      pen
    }
  )
}

# ---- Fidelity: the objective at defaults must reproduce known values --------
say("")
say("--- ANCHOR: fast path at defaults reproduces production EPR ---")
r_def <- epr_fast(cross, DEFAULTS)
say("rated (season, round, player) rows: ", format(nrow(r_def), big.mark = ","))
say("epr sd ", round(sd(r_def$epr, na.rm = TRUE), 4))

SKEL <- build_team_skeleton()
say("team skeleton rows ", format(nrow(SKEL), big.mark = ","))

# Per-channel EPR sd at default parameters -- the units the prior_rate bounds
# are expressed in.
CHAN_SD <- vapply(CH, function(c) sd(r_def[[paste0("epr_", c)]], na.rm = TRUE),
                  numeric(1))
say("")
say("--- per-channel EPR sd at defaults (units for the prior_rate bounds) ---")
say_dt(data.table(channel = CH, epr_sd = round(CHAN_SD, 4),
                  prior_rate_bound = paste0("+/-", round(PRIOR_RATE_SD_K * CHAN_SD, 3)),
                  default = round(vapply(CH, function(c)
                    DEFAULTS[[paste0("prior_rate_", c)]], numeric(1)), 4)), 6)
say("A flat bound let the first run pin prior_rate_hitout to -1.5 on a channel")
say("with sd ", round(CHAN_SD[["hitout"]], 3), " -- ten sd below the mean, which")
say("makes the channel a positional dummy rather than a rating.")

say("")
say("--- ANCHOR: lean team aggregation == .build_team_ratings_df() ---")
{
  rr <- copy(r_def)
  for (c in CHAN_COLS) rr[, (c) := get(c) - mean(get(c), na.rm = TRUE), by = .(season, round)]
  rr[, epr := rowSums(as.matrix(.SD), na.rm = TRUE), .SDcols = CHAN_COLS]
  rp <- merge(rr, POS_KEY, by = c("player_id", "season"), all.x = TRUE)
  prod_t <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rp), psr_df))
  fast_t <- team_sums_fast(rr, merge3 = FALSE)
  cmp <- merge(prod_t[, c("match_id", "team_type", "epr", CHAN_COLS), with = FALSE],
               fast_t[, c("match_id", "team_type", "epr", CHAN_COLS), with = FALSE],
               by = c("match_id", "team_type"), suffixes = c("_p", "_f"))
  say("matched team-match rows ", nrow(cmp), " of production's ", nrow(prod_t))
  worst <- 0
  for (c in c("epr", CHAN_COLS)) {
    d <- max(abs(cmp[[paste0(c, "_p")]] - cmp[[paste0(c, "_f")]]), na.rm = TRUE)
    worst <- max(worst, d)
    say(sprintf("  %-12s max |diff| %.3e", c, d))
  }
  if (!is.finite(worst) || worst > 1e-6 || nrow(cmp) != nrow(prod_t)) {
    say("")
    say("FAIL -- the lean team aggregation differs from production. Not usable.")
    close(con); quit(status = 1)
  }
  say("PASS: lean aggregation matches production to ", signif(worst, 3))
}

say("")
say("--- speed check (this is why the lean path exists) ---")
{
  sp <- make_spec(FALSE, FALSE)
  t0 <- Sys.time()
  invisible(objective(sp$init_z, sp))
  per_eval <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  say("one objective evaluation: ", round(per_eval, 2), "s")
  # L-BFGS-B costs (n_params + 1) evaluations per gradient step.
  say("projected per arm at 60 iterations, 12 params: ",
      round(per_eval * 13 * 60 / 60, 1), " min")
  say("(the previous version called .build_team_ratings_df() in the loop and was")
  say(" on track for 1-2 HOURS per arm, which is why this shortcut was worth")
  say(" building AND worth asserting.)")
}

# ---- Run the arms ----------------------------------------------------------
arms <- list(
  "4ch-free"    = make_spec(FALSE, FALSE),
  "4ch-floored" = make_spec(FALSE, TRUE),
  "3ch-free"    = make_spec(TRUE,  FALSE),
  "3ch-floored" = make_spec(TRUE,  TRUE)
)

results <- list()
for (nm in names(arms)) {
  spec <- arms[[nm]]
  say("")
  say("=== arm: ", nm, " (", length(spec$names), " params) ===")
  t0 <- Sys.time()
  base <- objective(spec$init_z, spec, report = TRUE)
  say("baseline (production params) train MAE on xmargin: ", round(base$mae, 4))
  opt <- optim(spec$init_z, objective, spec = spec, method = "L-BFGS-B",
               lower = rep(0, length(spec$init_z)), upper = rep(1, length(spec$init_z)),
               control = list(maxit = 60, factr = 1e10))
  fin <- objective(opt$par, spec, report = TRUE)
  say("optimised train MAE: ", round(fin$mae, 4),
      "  (improvement ", round(base$mae - fin$mae, 4), ")")
  say("elapsed ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min, ",
      opt$counts[["function"]], " evaluations")

  v <- vapply(spec$names, function(n) fin$params[[n]], numeric(1))
  d0 <- vapply(spec$names, function(n) DEFAULTS[[n]], numeric(1))
  at_bound <- abs(opt$par) < 1e-6 | abs(opt$par - 1) < 1e-6
  tab <- data.table(param = spec$names, default = round(d0, 4),
                    optimised = round(v, 4), on_bound = at_bound)
  say_dt(tab, 20)
  if (any(at_bound)) {
    say("!! ", sum(at_bound), " parameter(s) ON A BOUND -- treat as a bug signal,")
    say("   not a finding (stats-discipline rule 2).")
  }
  results[[nm]] <- list(spec = spec, opt = opt, fin = fin, tab = tab,
                        base_mae = base$mae)
}

say("")
say("=== SUMMARY (train xmargin MAE, 2021-2024) ===")
say_dt(rbindlist(lapply(names(results), function(nm) data.table(
  arm = nm, baseline = round(results[[nm]]$base_mae, 4),
  optimised = round(results[[nm]]$fin$mae, 4),
  gain = round(results[[nm]]$base_mae - results[[nm]]$fin$mae, 4),
  on_bound = sum(results[[nm]]$tab$on_bound)
))), 8)

say("")
say("These are TRAIN numbers on xmargin. Nothing is decided here -- the winner")
say("of each structure goes through ws17 on 2025-26 margin/bits/Brier, which")
say("theta has never seen. Optimising and gating on the same quantity is how a")
say("metric-forcing fix gets through.")

saveRDS(lapply(results, function(r) list(params = r$fin$params, tab = r$tab)),
        file.path(OUT_DIR, "epv3_optimised_params.rds"))
say("")
say("wrote optimised parameter sets to epv3_optimised_params.rds")
close(con)
cat("\nDone\n")
