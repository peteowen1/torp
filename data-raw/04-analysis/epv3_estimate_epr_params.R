# Estimate EPR decay and prior_games DIRECTLY from player data.
#
# The optimiser fits these against 1,241 match margins -- ~155 matches per
# parameter, with selection, opponent, venue and shot luck all sitting between
# the parameter and the evidence. That is what let a free prior_rate encode
# "this team is experienced" and look like a 1.02 MAE win.
#
# Both parameters are answerable off 56,576 player-games without mentioning a
# match, because both are properties of the CHANNEL's noise, not of match
# outcomes.
#
#   prior_games  is a variance ratio. The best estimate of a player's ability
#                after n games is (n*his_mean + k*league_mean)/(n + k), and
#                statistics fixes k = within-player variance / between-player
#                variance. Not a knob -- a measurement.
#
#   decay        is how fast players actually change. Correlate a player's
#                value against his own value L days later, for a range of L,
#                and fit r(L) = rho * exp(-L/decay). rho absorbs measurement
#                noise (which decorrelates instantly and would otherwise make
#                everything look fast); decay is read off the SHAPE, not the
#                level.
#
# ---------------------------------------------------------------------------
# PERFORMANCE REVIEW (done before writing, per Pete's instruction)
#
#   Bottleneck: the decay estimate. The obvious version pairs every game with
#   every other game for the same player -- O(games^2) per player, ~34M pairs
#   here, and QUADRATIC in history, so it degrades every season.
#   Fix: shift-based. Order each player's games by date; for lag k correlate
#   x[i] against x[i+k]. O(n) per lag x ~30 lags = ~1.7M ops, constant memory.
#
#   Three repo-specific hazards avoided:
#     * no get() inside dt[i, j] -- breaks the fast column path and has leaked
#       multi-GB RSS that gc() cannot see
#     * no as.data.table() on an already-valid data.table -- it deep-copies
#       regardless, it is not the no-op it looks like
#     * result vectors preallocated, not grown in a loop
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_estimated_epr_params.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

CH <- c("recv", "disp", "spoil", "hitout")
LBL <- c(recv = "recv", disp = "disp", spoil = "cont_aerial", hitout = "cont_stop")
MAX_LAG <- 30L

say("=== EPR decay and prior_games, estimated from player data ===")

pgd <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v3.parquet")))
pgd <- adjust_epv_for_opponents(pgd)
if (isTRUE(EPV_LEVEL_CENTRE)) pgd <- centre_epv_by_position(pgd)

# EPR consumes _oadj when present. Use the per-80 RATE, not the TOG-scaled
# value: epv_*_adj is (centred rate) * tog, so its within-player variance would
# be inflated by minutes played rather than by how noisy the measure is, and
# minutes are not what the shrinkage is protecting against.
sfx <- if (all(paste0("epv_", CH, "_oadj") %in% names(pgd))) "_oadj" else "_adj"
say("channel suffix in use: ", sfx)
pgd[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
for (c in CH) pgd[, (paste0("r_", c)) := get(paste0("epv_", c, sfx)) / tog_safe]

pgd[, .date := as.Date(utc_start_time)]
setorder(pgd, player_id, .date)
say("player-games ", format(nrow(pgd), big.mark = ","),
    " | players ", uniqueN(pgd$player_id),
    " | dates ", paste(range(pgd$.date), collapse = " to "))

# ---- 1. prior_games: the variance ratio ------------------------------------
say("")
say("=== 1. prior_games = within-player variance / between-player variance ===")
say("A high ratio means the measure bounces around game to game relative to how")
say("much players truly differ, so a small sample tells you little and the")
say("rating should lean on the league average for longer.")
say("")

rows <- vector("list", length(CH))
for (i in seq_along(CH)) {
  c <- CH[i]
  x <- pgd[[paste0("r_", c)]]
  pid <- pgd$player_id
  ok <- is.finite(x)
  d <- data.table(pid = pid[ok], x = x[ok])
  # Within-player: pooled variance about each player's own mean.
  agg <- d[, .(n = .N, m = mean(x), ss = sum((x - mean(x))^2)), by = pid]
  agg <- agg[n >= 2]
  s2_within <- sum(agg$ss) / sum(agg$n - 1)
  # Between-player: the spread of player means, MINUS the part that is just
  # sampling noise in those means. Var(mean_i) = tau^2 + s2_within/n_i, so
  # subtract the average of s2_within/n_i or tau^2 comes out inflated.
  var_of_means <- var(agg$m)
  tau2 <- var_of_means - s2_within * mean(1 / agg$n)
  rows[[i]] <- data.table(
    channel = LBL[[c]],
    n_players = nrow(agg),
    within_var = round(s2_within, 4),
    between_var_raw = round(var_of_means, 4),
    between_var = round(tau2, 4),
    prior_games = if (is.finite(tau2) && tau2 > 0) round(s2_within / tau2, 2) else NA_real_,
    production = EPR_PRIOR_GAMES_RECV
  )
}
pg_tab <- rbindlist(rows)
say_dt(pg_tab, 8)
say("")
say("production is 3.0 for every channel.")
if (any(!is.finite(pg_tab$prior_games))) {
  say("!! a non-finite estimate means between-player variance came out <= 0,")
  say("   i.e. the channel cannot distinguish players at all at this sample size.")
}

# ---- 2. decay: how fast a player's own value decorrelates -------------------
say("")
say("=== 2. decay: fit r(L) = rho * exp(-L / decay) ===")
say("Shift-based over ", MAX_LAG, " game lags. The pairwise version is O(n^2) in")
say("games per player and degrades every season; this is O(n) per lag.")
say("")

decay_rows <- vector("list", length(CH))
lag_detail <- vector("list", length(CH))
for (i in seq_along(CH)) {
  c <- CH[i]
  xcol <- paste0("r_", c)
  # Plain-vector extraction; no get() inside a data.table j-expression.
  d <- data.table(pid = pgd$player_id, dt = pgd$.date, x = pgd[[xcol]])
  d <- d[is.finite(x)]
  setorder(d, pid, dt)

  n_lag <- integer(MAX_LAG); r_lag <- numeric(MAX_LAG); day_lag <- numeric(MAX_LAG)
  for (k in seq_len(MAX_LAG)) {
    d[, `:=`(x2 = shift(x, k, type = "lead"),
             d2 = shift(dt, k, type = "lead"),
             p2 = shift(pid, k, type = "lead"))]
    sub <- d[!is.na(x2) & p2 == pid]
    n_lag[k] <- nrow(sub)
    r_lag[k] <- if (nrow(sub) > 50) cor(sub$x, sub$x2) else NA_real_
    day_lag[k] <- if (nrow(sub) > 50) mean(as.numeric(sub$d2 - sub$dt)) else NA_real_
  }
  d[, c("x2", "d2", "p2") := NULL]

  ld <- data.table(channel = LBL[[c]], lag_games = seq_len(MAX_LAG),
                   n_pairs = n_lag, r = round(r_lag, 4), days = round(day_lag, 1))
  lag_detail[[i]] <- ld

  # Linearise: log r = log rho - L/decay. Weight by pair count. Only positive r
  # is usable; a channel whose correlation reaches zero has no decay to fit.
  fit_d <- ld[is.finite(r) & r > 0.01 & is.finite(days)]
  if (nrow(fit_d) >= 5) {
    f <- lm(log(r) ~ days, data = fit_d, weights = n_pairs)
    slope <- coef(f)[["days"]]
    decay_est <- if (slope < 0) -1 / slope else NA_real_
    rho <- exp(coef(f)[["(Intercept)"]])
  } else {
    decay_est <- NA_real_; rho <- NA_real_
  }
  decay_rows[[i]] <- data.table(
    channel = LBL[[c]], n_lags_used = nrow(fit_d),
    rho_noise_ceiling = round(rho, 3),
    decay_days = round(decay_est, 0),
    half_life_days = round(decay_est * log(2), 0),
    production = c(recv = EPR_DECAY_RECV, disp = EPR_DECAY_DISP,
                   spoil = EPR_DECAY_SPOIL, hitout = EPR_DECAY_HITOUT)[[c]]
  )
}
dc_tab <- rbindlist(decay_rows)
say_dt(dc_tab, 8)
say("")
say("rho is the correlation a zero-lag pair would have -- i.e. the reliability")
say("ceiling set by measurement noise. A low rho means the channel is noisy")
say("game to game; it does NOT mean ability changes fast.")

say("")
say("--- the lag curves themselves (sanity: r should fall, monotonically-ish) ---")
for (i in seq_along(CH)) {
  say("")
  say("channel: ", LBL[[CH[i]]])
  say_dt(lag_detail[[i]][lag_games %in% c(1, 2, 3, 5, 8, 12, 16, 20, 25, 30)], 12)
}

# ---- Anchors ---------------------------------------------------------------
say("")
say("=== ANCHOR CHECKS (pre-registered) ===")
say("1. disposal should be the most RELIABLE channel (highest rho): high event")
say("   count per game, so less sampling noise.")
say("2. every decay must be positive and inside the optimiser's [90, 1500] range,")
say("   or the estimate is not usable as a drop-in.")
say("3. prior_games should be LARGER for low-count channels (contest, stoppage)")
say("   than for disposal -- fewer events per game means a noisier per-game read.")
say("")
best_rho <- dc_tab[which.max(rho_noise_ceiling)]$channel
say("highest rho: ", best_rho, if (best_rho == "disp") "  PASS" else "  <- CHECK THIS")
say("all decays positive and in [90, 1500]: ",
    all(is.finite(dc_tab$decay_days) & dc_tab$decay_days > 90 & dc_tab$decay_days < 1500))
pg_disp <- pg_tab[channel == "disp"]$prior_games
pg_cont <- pg_tab[channel == "cont_aerial"]$prior_games
say("prior_games contest (", pg_cont, ") > disposal (", pg_disp, "): ",
    isTRUE(pg_cont > pg_disp))

say("")
say("=== PROPOSED CONSTANTS ===")
for (i in seq_along(CH)) {
  c <- CH[i]
  say(sprintf("  EPR_DECAY_%-7s %6s  ->  %s",
              toupper(c), dc_tab$production[i], dc_tab$decay_days[i]))
}
for (i in seq_along(CH)) {
  say(sprintf("  EPR_PRIOR_GAMES_%-7s %4s  ->  %s",
              toupper(CH[i]), 3, pg_tab$prior_games[i]))
}
say("")
say("These are MEASUREMENTS, not a search result -- so they carry no overfitting")
say("risk and can be sanity-checked against football sense. They still have to")
say("clear ws19 on held-out match data before shipping.")

arrow::write_parquet(rbindlist(lag_detail), file.path(OUT_DIR, "epv3_lag_curves.parquet"))
saveRDS(list(prior_games = pg_tab, decay = dc_tab),
        file.path(OUT_DIR, "epv3_estimated_params.rds"))
close(con)
cat("\nWrote ", OUT, "\n")
