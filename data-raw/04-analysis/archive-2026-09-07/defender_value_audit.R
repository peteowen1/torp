# defender_value_audit.R --------------------------------------------------
# Positional calibration audit of EPR / PSR / TORP.
#
# Question: are defenders (especially key defenders) under-valued by the
# rating system relative to the margin they actually generate?
#
# Method: three tests, deliberately in this order because the first two
# disagree and the disagreement IS the finding.
#
#   A. Realized per-game value  -> margin, coefficients by position bucket.
#      CONFOUNDED by game state (a team under siege racks up defensive
#      volume). Reported only to show the size of that confound.
#   B. Pre-match smoothed RATING -> margin, coefficients by position bucket.
#      The real test. Coefficient == 1 means "one rating point in this unit
#      buys one margin point", i.e. calibrated.
#   C. Split-half reliability of per-game value by bucket. Bounds how much
#      of B's spread is attenuation-by-noise rather than genuine mis-scaling.
#
# Reads local torpdata parquet only; writes nothing. Run via PowerShell:
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/defender_value_audit.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow)
  library(data.table)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")

read_seasons <- function(stub) {
  rbindlist(lapply(SEASONS, function(s)
    as.data.table(read_parquet(file.path(DATA_DIR, sprintf("%s_%d.parquet", stub, s))))),
    use.names = TRUE, fill = TRUE)
}

pg  <- read_seasons("player_game_ratings")
res <- read_seasons("results")
tr  <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))

# team_id, not team name: results/ uses inconsistent short vs full club names
# across seasons and a name join silently drops ~85% of matches.
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]

pg <- pg[position_group %in% BUCKETS]
pg[, `:=`(pos = position_group, round = as.numeric(round))]
tr[, round := as.numeric(round)]

# Build home-minus-away team-sum differences for a set of value columns,
# split by position bucket. One row per match.
match_diffs <- function(dt, cols, weight = NULL) {
  agg <- if (is.null(weight)) {
    dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols, by = .(match_id, team_id, pos)]
  } else {
    dt[, lapply(.SD, function(v) sum(v * get(weight), na.rm = TRUE)),
       .SDcols = cols, by = .(match_id, team_id, pos)]
  }
  w <- dcast(agg, match_id + team_id ~ pos, value.var = cols, fill = 0)
  if (length(cols) == 1L) {
    bs <- setdiff(names(w), c("match_id", "team_id"))
    setnames(w, bs, paste0(cols, "_", bs))
  }
  m <- merge(res, w, by.x = c("match_id", "home_team_id"),
             by.y = c("match_id", "team_id"))
  a <- merge(res[, .(match_id, away_team_id)], w,
             by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
  vc <- setdiff(names(w), c("match_id", "team_id"))
  setnames(a, vc, paste0("a_", vc))
  m <- merge(m, a, by = "match_id")
  for (v in vc) m[[paste0("d_", v)]] <- m[[v]] - m[[paste0("a_", v)]]
  m[]
}

report <- function(m, prefix, label, terms = paste0("d_", prefix, "_", BUCKETS)) {
  terms <- terms[terms %in% names(m)]
  fit <- lm(as.formula(paste("margin ~", paste(terms, collapse = " + "))), data = m)
  s <- summary(fit)
  co <- as.data.table(s$coefficients, keep.rownames = "term")
  setnames(co, c("term", "est", "se", "t", "p"))
  co <- co[term != "(Intercept)"]
  co[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se,
            sd_unit = sapply(term, function(k) sd(m[[k]])))]
  co[, per_sd := est * sd_unit]
  cat(sprintf("\n--- %s | n = %d matches | R2 = %.3f\n", label, nrow(m), s$r.squared))
  print(co[order(-est), .(term,
                          est = round(est, 2), lo = round(lo, 2), hi = round(hi, 2),
                          sd_unit = round(sd_unit, 2), per_sd = round(per_sd, 2))])
  invisible(fit)
}

# --- A. realized per-game value (confounded; shown for contrast) ----------
cat("\n==========  A. REALIZED per-game value -> margin  ==========\n")
cat("Coefficients are dragged toward zero for defensive units by game state:\n")
cat("a team pinned in its own half accumulates spoils/rebounds while losing.\n")
mA <- match_diffs(pg, c("psv", "epv", "torp_value"))
report(mA, "torp_value", "TORP per-game value")
report(mA, "epv", "EPV per-game value")
report(mA, "psv", "PSV per-game value")

# --- B. pre-match rating (the real test) ---------------------------------
cat("\n\n==========  B. PRE-MATCH RATING -> margin  ==========\n")
cat("est = margin points bought per 1 rating point. 1.00 = calibrated,\n")
cat(">1 = that unit is UNDER-rated, <1 = over-rated.\n")
L <- merge(pg[, .(player_id, match_id, season, round, team_id, pos, tog)],
           tr[, .(player_id, season, round, epr, psr, torp,
                  recv_epr, disp_epr, spoil_epr, hitout_epr)],
           by = c("player_id", "season", "round"))
mB <- match_diffs(L, c("epr", "psr", "torp"), weight = "tog")
report(mB, "torp", "TORP rating")
report(mB, "epr", "EPR")
report(mB, "psr", "PSR")

cat("\n-- EPR component channels, own-bucket, for the defensive units --\n")
mC <- match_diffs(L, c("recv_epr", "disp_epr", "spoil_epr", "hitout_epr"), weight = "tog")
for (b in BUCKETS) {
  report(mC, NULL, paste("components:", b),
         terms = paste0("d_", c("recv_epr", "disp_epr", "spoil_epr", "hitout_epr"), "_", b))
}

# --- C. reliability: is B's spread real, or attenuation? -----------------
cat("\n\n==========  C. SPLIT-HALF RELIABILITY of per-game value  ==========\n")
cat("Low r would mean a bucket's value is mostly noise, which would\n")
cat("mechanically shrink its coefficient in B. High r for defenders rules\n")
cat("that explanation out.\n")
P <- pg[tog > 0.4]
setorder(P, player_id, season, round)
P[, half := seq_len(.N) %% 2, by = .(player_id, season)]
reliability <- function(col) {
  s <- P[, .(v = mean(get(col), na.rm = TRUE), n = .N),
         by = .(player_id, season, pos, half)][n >= 6]
  d <- dcast(s, player_id + season + pos ~ half, value.var = "v")
  setnames(d, c("0", "1"), c("a", "b"))
  d <- d[!is.na(a) & !is.na(b)]
  # Spearman-Brown up to full-season length
  d[, .(n_players = .N, r_half = cor(a, b)), by = pos
    ][, .(pos, n_players, r_full = round(2 * r_half / (1 + r_half), 3))]
}
rel <- Reduce(function(x, y) merge(x, y, by = c("pos", "n_players")),
              lapply(c("torp_value", "epv", "psv"), function(cl) {
                r <- reliability(cl); setnames(r, "r_full", paste0("r_", cl)); r
              }))
print(rel[order(-r_torp_value)])

# --- D. leaderboard representation ---------------------------------------
cat("\n\n==========  D. TOP-100 REPRESENTATION  ==========\n")
sea <- pg[tog > 0.5, .(g = .N,
                       torp = mean(torp_value, na.rm = TRUE),
                       psv = mean(psv, na.rm = TRUE),
                       epv = mean(epv, na.rm = TRUE)),
          by = .(season, player_id, player_name, pos)][g >= 10]
share <- function(col) {
  s <- sea[order(-get(col))][1:100][, .(v = .N / 100), by = pos]
  setnames(s, "v", paste0("top100_", col)); s
}
pop <- sea[, .(population = round(.N / nrow(sea), 3)), by = pos]
print(Reduce(function(x, y) merge(x, y, by = "pos", all.x = TRUE),
             list(pop, share("torp"), share("epv"), share("psv")))[order(-population)])

cat("\n\n==========  E. RATING CEILING BY BUCKET  ==========\n")
print(L[tog > 0.5, .(n = .N,
                     mean = round(mean(torp, na.rm = TRUE), 2),
                     sd = round(sd(torp, na.rm = TRUE), 2),
                     p90 = round(quantile(torp, 0.9, na.rm = TRUE), 2),
                     max = round(max(torp, na.rm = TRUE), 1)),
        by = pos][order(-mean)])
