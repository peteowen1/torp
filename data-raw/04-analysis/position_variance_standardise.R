# position_variance_standardise.R -----------------------------------------
# The lever we walked past: make the EPV position adjustment RESCALE, not
# just recentre.
#
# create_player_game_data() step 7 (player_credit.R:299-302) does
#     adj = (p80 - weighted.mean(p80 | lineup_position)) * tog
# It subtracts a within-position mean and stops. The diagnosis noted this
# "subtracts a mean but never rescales, so it does nothing about the
# variance compression" -- and then every subsequent experiment tried to fix
# the compression somewhere else. This tests the obvious thing directly:
#     adj_std = (p80 - mean_pos) / sd_pos * S * tog
# where S is the pooled weighted SD across all positions, so the metric keeps
# its overall units and only the BETWEEN-position spread differences change.
#
# WHY THIS IS THE RIGHT SHAPE OF FIX
#   The defect (plan section 6.1 / Pete 2026-07-26) is under-DISPERSION, not
#   under-level -- position means cancel in the home-minus-away differencing,
#   so the 1.65x is purely a statement about spread. Standardising within
#   position is the mechanism that sets spread. It is one change at the layer
#   where the defect lives, not a post-hoc multiplier and not per-stat
#   curve-fitting.
#
# WHAT IT ASSUMES, STATED PLAINLY
#   Forcing equal within-position variance is a real assumption. It says every
#   position group should have the same spread of player value. That is what
#   the calibration gate implies, but it is not self-evidently true of
#   football, and it should not be waved through.
#
# Scored on the POOLED multi-season gate with bootstrap CIs -- section 6.11
# established that per-season KD/KF is too noisy to fit or judge against.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/position_variance_standardise.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")
CH <- c("recv", "disp", "spoil", "hitout")
DECAY <- c(recv = EPR_DECAY_RECV, disp = EPR_DECAY_DISP,
           spoil = EPR_DECAY_SPOIL, hitout = EPR_DECAY_HITOUT)
PGAMES <- c(recv = EPR_PRIOR_GAMES_RECV, disp = EPR_PRIOR_GAMES_DISP,
            spoil = EPR_PRIOR_GAMES_SPOIL, hitout = EPR_PRIOR_GAMES_HITOUT)
PRATE <- c(recv = EPR_PRIOR_RATE_RECV, disp = EPR_PRIOR_RATE_DISP,
           spoil = EPR_PRIOR_RATE_SPOIL, hitout = EPR_PRIOR_RATE_HITOUT)

pg <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
pg[, round := as.numeric(round)]
pg[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage), 100,
                              time_on_ground_percentage) / 100, 0.1)]

wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
wsd <- function(x, w) {
  m <- wmean(x, w)
  sqrt(sum(w * (x - m)^2, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE))
}

cat("=== within-position spread of per-80 EPV, by channel ===\n")
for (ch in CH) {
  raw <- paste0(ch, "_epv")
  if (!raw %in% names(pg)) { cat("missing", raw, "\n"); next }
  pg[, (paste0(".p80_", ch)) := get(raw) / tog_safe]
  pg[, (paste0(".m_", ch)) := wmean(get(paste0(".p80_", ch)), tog_safe), by = lineup_position]
  pg[, (paste0(".s_", ch)) := wsd(get(paste0(".p80_", ch)), tog_safe), by = lineup_position]
  S <- wsd(pg[[paste0(".p80_", ch)]], pg$tog_safe)
  pg[, (paste0("adjc_", ch)) := (get(paste0(".p80_", ch)) - get(paste0(".m_", ch))) * tog_safe]
  pg[, (paste0("adjs_", ch)) :=
       (get(paste0(".p80_", ch)) - get(paste0(".m_", ch))) /
       pmax(get(paste0(".s_", ch)), 1e-6) * S * tog_safe]
}
cat("\nper-80 SD by lineup_position (recv channel), showing the compression:\n")
print(head(pg[, .(sd_recv = round(wsd(.p80_recv, tog_safe), 2),
                  sd_disp = round(wsd(.p80_disp, tog_safe), 2)),
              by = lineup_position][order(sd_recv)], 8))

# ---- point-in-time aggregation for both arms ----------------------------
setorder(pg, player_id, utc_start_time)
pg[, .date := as.Date(utc_start_time)]
run_decay <- function(x, dates, lam) {
  n <- length(x); out <- numeric(n); s <- 0; prev <- dates[1]
  for (i in seq_len(n)) {
    s <- s * exp(-as.numeric(dates[i] - prev) / lam); prev <- dates[i]
    out[i] <- s; s <- s + x[i]
  }
  out
}
cat("\nAggregating (2 arms x 4 channels)...\n")
for (ch in CH) {
  lam <- DECAY[[ch]]
  pg[, (paste0(".den_", ch)) := run_decay(tog_safe, .date, lam), by = player_id]
  for (arm in c("adjc", "adjs")) {
    pg[, (paste0(".S_", arm, "_", ch)) :=
         run_decay(get(paste0(arm, "_", ch)) * tog_safe, .date, lam), by = player_id]
  }
  den <- pg[[paste0(".den_", ch)]] + PGAMES[[ch]]
  for (arm in c("adjc", "adjs")) {
    pg[, (paste0("epr_", arm, "_", ch)) :=
         (EPR_LOADING_DEFAULT * get(paste0(".S_", arm, "_", ch)) +
            PGAMES[[ch]] * PRATE[[ch]]) / den]
  }
}
pg[, epr_cur := rowSums(.SD), .SDcols = paste0("epr_adjc_", CH)]
pg[, epr_std := rowSums(.SD), .SDcols = paste0("epr_adjs_", CH)]

tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]
L <- merge(pg[position_group %in% BUCKETS,
              .(player_id, match_id, season, round, team_id, lineup_position,
                pos = position_group, tog_safe, epr_cur, epr_std)],
           tr[, .(player_id, season, round, epr_pub = epr, psr_pub = psr, torp_pub = torp)],
           by = c("player_id", "season", "round"))
L <- L[!is.na(torp_pub)]
cat(sprintf("\nreconstruction check: cor(my current-arm EPR, published EPR) = %.4f\n",
            cor(L$epr_cur, L$epr_pub, use = "complete.obs")))
cat("  (production also applies an opponent adjustment to EPV, so exact\n")
cat("   equality is not expected; a high correlation means the rebuild is sound.)\n")

# PSR is already centred by lineup_position in calculate_psr(); the same
# standardisation applied to it, as a third arm.
L[, psr_std := psr_pub / pmax(wsd(psr_pub, tog_safe), 1e-6) *
     wsd(L$psr_pub, L$tog_safe), by = lineup_position]

EW <- TORP_EPR_WEIGHT
L[, torp_cur := EW * epr_cur + (1 - EW) * psr_pub]
L[, torp_std := EW * epr_std + (1 - EW) * psr_pub]
L[, torp_std2 := EW * epr_std + (1 - EW) * psr_std]

res <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]

build <- function(col) {
  ag <- L[, .(s = sum(get(col) * tog_safe, na.rm = TRUE)), by = .(match_id, team_id, pos)]
  wd <- dcast(ag, match_id + team_id ~ pos, value.var = "s", fill = 0)
  bk <- intersect(BUCKETS, names(wd))
  h <- merge(res, wd, by.x = c("match_id", "home_team_id"), by.y = c("match_id", "team_id"))
  a <- merge(res[, .(match_id, away_team_id)], wd,
             by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
  setnames(a, bk, paste0("a_", bk))
  mm <- merge(h, a, by = "match_id")
  X <- vapply(bk, function(b) mm[[b]] - mm[[paste0("a_", b)]], numeric(nrow(mm)))
  colnames(X) <- bk
  list(X = X, y = mm$margin, bk = bk)
}

report <- function(col, label) {
  d <- build(col)
  fit <- lm(d$y ~ d$X)
  co <- coef(fit)[-1]; names(co) <- d$bk
  set.seed(9)
  # Index by NAME. dcast returns buckets alphabetically, so positional
  # indexing silently picks RUCK where KEY_FORWARD was intended.
  B <- replicate(1000, {
    i <- sample.int(length(d$y), length(d$y), replace = TRUE)
    c2 <- coef(lm(d$y[i] ~ d$X[i, , drop = FALSE]))[-1]
    names(c2) <- d$bk
    c(spread = max(c2) / min(c2),
      kdkf = unname(c2[["KEY_DEFENDER"]] / c2[["KEY_FORWARD"]]))
  })
  cat(sprintf("\n%s  (n = %d)\n", label, length(d$y)))
  print(round(co, 2))
  kd <- co[["KEY_DEFENDER"]]; kf <- co[["KEY_FORWARD"]]
  q <- quantile(B["kdkf", ], c(.025, .975), na.rm = TRUE)
  cat(sprintf("  spread %.2fx   KD/KF %.2f  95%% CI [%.2f, %.2f]\n",
              max(co)/min(co), kd/kf, q[1], q[2]))
  invisible(co)
}

cat("\n================ POOLED CALIBRATION ================\n")
report("torp_pub",  "published TORP (reference)")
report("torp_cur",  "arm 1: rebuilt, CURRENT adjustment (recentre only)")
report("torp_std",  "arm 2: EPV adjustment STANDARDISED within position")
report("torp_std2", "arm 3: EPV + PSR both standardised within position")

cat("\n=== effect on the leaderboard (end-of-season, >=12 games) ===\n")
last <- L[, .SD[.N], by = .(player_id, season)]
print(last[, .(n = .N,
               cur_sd = round(sd(torp_cur, na.rm = TRUE), 2),
               std_sd = round(sd(torp_std2, na.rm = TRUE), 2),
               cur_max = round(max(torp_cur, na.rm = TRUE), 2),
               std_max = round(max(torp_std2, na.rm = TRUE), 2)),
           by = pos][order(-std_max)])
