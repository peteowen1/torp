# psr_calibration_walkforward.R -------------------------------------------
# Is the PSR half actually well calibrated across positions, or was that
# in-sample optimism?
#
# psr_role_aware_pricing.R found a freshly fitted, position-BLIND PSR prices
# key defenders almost perfectly (KD/KF 1.05, spread 1.28x) where the
# published frozen-2024 PSR measures 1.21 and EPR measures 2.22. But that fit
# saw most of the matches it was then calibrated on. This settles it.
#
# PROTOCOL
#   For each test season S: fit PSR betas on seasons < S only, score season S
#   players, accumulate. Then run the section 1.2 positional calibration on
#   the pooled OUT-OF-SAMPLE scores. Compare against the PUBLISHED psr from
#   torp_ratings on exactly the same matches, which is the real incumbent.
#
# The comparison is the point. If walk-forward PSR is calibrated and the
# published one is not, the fix is a vintage refresh (WS4 -- closed in July
# as MAE-null, but never scored on CALIBRATION, a different objective). If
# both are miscalibrated, my earlier 1.05 was in-sample optimism and the PSR
# side is not the answer either.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/psr_calibration_walkforward.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow); library(data.table); library(glmnet)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
TEST_SEASONS <- 2023:2026
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")

sr <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_stat_ratings_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
pg <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", s))))[
    , .(player_id, match_id, season, round = as.numeric(round), team_id,
        position_group, lineup_position)]),
  use.names = TRUE, fill = TRUE)
res <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, season = as.integer(substr(utc_start_time, 1, 4)),
             home_team_id, away_team_id, margin = home_score - away_score,
             match_date = as.Date(substr(utc_start_time, 1, 10)))]
tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]

sr[, round := as.numeric(round)]
adj <- grep("_adj_rating$", names(sr), value = TRUE)
for (a in adj) {
  r <- sub("_adj_rating$", "_rating", a)
  if (r %in% names(sr)) sr[, (r) := get(a)]
}
rating_cols <- setdiff(grep("_rating$", names(sr), value = TRUE), adj)

L <- merge(pg[position_group %in% BUCKETS & !lineup_position %in% c("EMERG", "SUB")],
           sr[, c("player_id", "season", "round", rating_cols), with = FALSE],
           by = c("player_id", "season", "round"))
for (c0 in rating_cols) set(L, which(is.na(L[[c0]])), c0, 0)
L <- merge(L, tr[, .(player_id, season, round, psr_pub = psr)],
           by = c("player_id", "season", "round"), all.x = TRUE)

glob <- L[, lapply(.SD, sum, na.rm = TRUE), .SDcols = rating_cols,
          by = .(match_id, team_id)]
side <- function(id_col, prefix) {
  x <- merge(res[, c("match_id", id_col), with = FALSE], glob,
             by.x = c("match_id", id_col), by.y = c("match_id", "team_id"))
  setnames(x, rating_cols, paste0(prefix, rating_cols))
  x[, (id_col) := NULL][]
}
M <- merge(res, side("home_team_id", "h_"), by = "match_id")
M <- merge(M, side("away_team_id", "a_"), by = "match_id")
feat <- c(paste0("h_", rating_cols), paste0("a_", rating_cols))
feat <- feat[feat %in% names(M)]
cat(sprintf("matches %d, features %d\n", nrow(M), length(feat)))

X_raw <- as.matrix(M[, ..feat])
anchor_of <- function(S) as.Date(sprintf("%d-12-31", S - 1))

# Walk-forward: fit on < S, score season S players.
L[, psr_wf := NA_real_]
for (S in TEST_SEASONS) {
  tr_i <- which(M$season < S)
  if (length(tr_i) < 200) next
  sds <- apply(X_raw[tr_i, ], 2, sd); sds[sds == 0 | is.na(sds)] <- 1
  Xs <- sweep(X_raw, 2, sds, "/")
  w <- exp(as.numeric(-(anchor_of(S) - M$match_date)) / MATCH_WEIGHT_DECAY_DAYS)
  w <- w / mean(w[tr_i])
  set.seed(5)
  cv <- cv.glmnet(Xs[tr_i, , drop = FALSE], M$margin[tr_i], alpha = 0,
                  weights = w[tr_i], nfolds = 10)
  b <- as.numeric(coef(cv, s = "lambda.min"))[-1]; names(b) <- feat
  sel <- which(L$season == S)
  contrib <- numeric(length(sel))
  for (s in rating_cols) {
    hn <- paste0("h_", s)
    if (!hn %in% names(b)) next
    contrib <- contrib + (b[[hn]] / sds[[hn]]) * L[[s]][sel]
  }
  set(L, sel, "psr_wf", contrib)
  cat(sprintf("  %d: fitted on %d matches, scored %d player-rows\n",
              S, length(tr_i), length(sel)))
}

# Third arm, and it is the one that makes the comparison honest: MY pipeline
# frozen at the 2024 vintage (fit once on < 2025, score every test season).
# Walk-forward vs published differs in vintage AND in feature construction
# (no top-22 filter), standardisation, alpha sweep and stat exclusions. If
# frozen-mine matches walk-forward-mine, vintage is NOT the driver and the
# gap is those other differences; if it matches published, vintage is.
{
  tr_i <- which(M$season < 2025)
  sds <- apply(X_raw[tr_i, ], 2, sd); sds[sds == 0 | is.na(sds)] <- 1
  Xs <- sweep(X_raw, 2, sds, "/")
  w <- exp(as.numeric(-(as.Date("2024-12-31") - M$match_date)) / MATCH_WEIGHT_DECAY_DAYS)
  w <- w / mean(w[tr_i])
  set.seed(5)
  cv <- cv.glmnet(Xs[tr_i, , drop = FALSE], M$margin[tr_i], alpha = 0,
                  weights = w[tr_i], nfolds = 10)
  b <- as.numeric(coef(cv, s = "lambda.min"))[-1]; names(b) <- feat
  contrib <- numeric(nrow(L))
  for (s in rating_cols) {
    hn <- paste0("h_", s)
    if (!hn %in% names(b)) next
    contrib <- contrib + (b[[hn]] / sds[[hn]]) * L[[s]]
  }
  L[, psr_frozen := contrib]
  cat(sprintf("  frozen-2024 arm: fitted on %d matches\n", length(tr_i)))
}

# Fourth arm: same frozen vintage and same features, but with production's
# ALPHA SWEEP instead of pure ridge. The shipped psr_coefficients.csv is full
# of exact zeros (kicks, handballs, disposals, marks, contested_possessions
# all 0), which is elastic-net selecting among heavily collinear team-sum
# stats -- the "collinearity instability" pannaverse flagged in FABLE-167.
# If sparsity is what breaks positional calibration, this arm reproduces the
# published behaviour from the ridge arm.
{
  tr_i <- which(M$season < 2025)
  sds <- apply(X_raw[tr_i, ], 2, sd); sds[sds == 0 | is.na(sds)] <- 1
  Xs <- sweep(X_raw, 2, sds, "/")
  w <- exp(as.numeric(-(as.Date("2024-12-31") - M$match_date)) / MATCH_WEIGHT_DECAY_DAYS)
  w <- w / mean(w[tr_i])
  best <- NULL
  for (al in c(0, 0.25, 0.5, 0.75, 1)) {
    set.seed(5)
    cvv <- cv.glmnet(Xs[tr_i, , drop = FALSE], M$margin[tr_i], alpha = al,
                     weights = w[tr_i], nfolds = 10)
    e <- min(cvv$cvm)
    if (is.null(best) || e < best$e) best <- list(e = e, cv = cvv, alpha = al)
  }
  b <- as.numeric(coef(best$cv, s = "lambda.min"))[-1]; names(b) <- feat
  cat(sprintf("  alpha-sweep arm: best alpha = %.2f, %d of %d betas exactly zero\n",
              best$alpha, sum(b == 0), length(b)))
  contrib <- numeric(nrow(L))
  for (s in rating_cols) {
    hn <- paste0("h_", s)
    if (!hn %in% names(b)) next
    contrib <- contrib + (b[[hn]] / sds[[hn]]) * L[[s]]
  }
  L[, psr_alpha := contrib]
}

calib <- function(dt, col, label) {
  d <- dt[!is.na(get(col))]
  ag <- d[, .(s = sum(get(col), na.rm = TRUE)), by = .(match_id, team_id, pos = position_group)]
  wd <- dcast(ag, match_id + team_id ~ pos, value.var = "s", fill = 0)
  bk <- intersect(BUCKETS, names(wd))
  hh <- merge(res, wd, by.x = c("match_id", "home_team_id"), by.y = c("match_id", "team_id"))
  aa <- merge(res[, .(match_id, away_team_id)], wd,
              by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
  setnames(aa, bk, paste0("a_", bk))
  mm <- merge(hh, aa, by = "match_id")
  for (b in bk) mm[[paste0("d_", b)]] <- mm[[b]] - mm[[paste0("a_", b)]]
  f <- as.formula(paste("margin ~", paste0("d_", bk, collapse = " + ")))
  fit <- lm(f, data = mm)
  co <- coef(fit)[-1]; names(co) <- bk
  cat(sprintf("\n%s   (n = %d matches)\n", label, nrow(mm)))
  print(round(co, 2))
  cat(sprintf("  spread max/min = %.2fx   KD/KF = %.2f\n",
              max(co) / min(co), co[["KEY_DEFENDER"]] / co[["KEY_FORWARD"]]))
  co
}

cat("\n================ OUT-OF-SAMPLE POSITIONAL CALIBRATION ================\n")
cat("Both measured on the SAME player-rows (test seasons only), so the only\n")
cat("difference is which PSR is being scored.\n")
Lt <- L[season %in% TEST_SEASONS & !is.na(psr_wf) & !is.na(psr_pub)]
cat(sprintf("\ncomparison rows: %s\n", format(nrow(Lt), big.mark = ",")))
c_wf  <- calib(Lt, "psr_wf",  "A: walk-forward refit, my pipeline")
c_frz <- calib(Lt, "psr_frozen", "B: FROZEN 2024 vintage, my pipeline (isolates vintage)")
c_alp <- calib(Lt, "psr_alpha", "D: frozen 2024 + ALPHA SWEEP, my pipeline (isolates sparsity)")
c_pub <- calib(Lt, "psr_pub", "C: PUBLISHED PSR (frozen 2024, production pipeline)")

cat("\n---- what the three arms isolate ----\n")
cat(sprintf("  A vs B : effect of VINTAGE alone      (spread %.2fx -> %.2fx)\n",
            max(c_frz)/min(c_frz), max(c_wf)/min(c_wf)))
cat(sprintf("  B vs C : effect of PIPELINE alone     (spread %.2fx -> %.2fx)\n",
            max(c_pub)/min(c_pub), max(c_frz)/min(c_frz)))
cat(sprintf("  D vs B : effect of SPARSITY alone     (spread %.2fx -> %.2fx)\n",
            max(c_alp)/min(c_alp), max(c_frz)/min(c_frz)))
cat(sprintf("  C vs D : residual pipeline difference (spread %.2fx -> %.2fx)\n",
            max(c_pub)/min(c_pub), max(c_alp)/min(c_alp)))

cat("\n---- verdict ----\n")
imp_spread <- (max(c_pub)/min(c_pub)) - (max(c_wf)/min(c_wf))
imp_ratio <- abs(c_pub[["KEY_DEFENDER"]]/c_pub[["KEY_FORWARD"]] - 1) -
             abs(c_wf[["KEY_DEFENDER"]]/c_wf[["KEY_FORWARD"]] - 1)
cat(sprintf("  spread  published %.2fx -> walk-forward %.2fx  (improvement %+.2f)\n",
            max(c_pub)/min(c_pub), max(c_wf)/min(c_wf), imp_spread))
cat(sprintf("  |KD/KF - 1|  published %.2f -> walk-forward %.2f  (improvement %+.2f)\n",
            abs(c_pub[["KEY_DEFENDER"]]/c_pub[["KEY_FORWARD"]] - 1),
            abs(c_wf[["KEY_DEFENDER"]]/c_wf[["KEY_FORWARD"]] - 1), imp_ratio))
cat("\nIf walk-forward is clearly better, a PSR vintage refresh is a real and\n")
cat("cheap calibration lever (WS4, closed in July on MAE but never scored on\n")
cat("calibration). If they are similar, the earlier 1.05 was in-sample\n")
cat("optimism and the PSR side is not the answer.\n")
