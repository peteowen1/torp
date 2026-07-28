# weight_optimisation.R ---------------------------------------------------
# Walk-forward optimisation of position- and channel-aware rating weights.
#
# THE STRUCTURAL POINT, read this before interpreting anything below
# ------------------------------------------------------------------
# Calibration cannot be an optimisation objective. The §1.2 gate regresses
# margin on per-bucket team-sum rating differences; if we are free to scale
# each bucket independently, setting bucket b's multiplier to its own fitted
# coefficient forces every coefficient to exactly 1.00. That is a
# reparameterisation, not an improvement -- it is free, and it changes no
# prediction. So here:
#
#     OBJECTIVE  = out-of-sample margin error (walk-forward, honest)
#     DIAGNOSTIC = calibration spread implied by the fitted weights
#
# It also follows that the optimal per-bucket weights ARE the regression
# coefficients. The open question is not what they are, it is whether they
# survive out of sample -- and whether a finer position x channel weighting
# beats them or just overfits.
#
# WHY THIS IS NOT SUBSUMED BY THE MATCH MODEL
# -------------------------------------------
# 2026-PSR-EPR-DIAGNOSIS §3 established that the match model already owns any
# reweighting of epr_diff vs psr_diff vs their four components, because it
# gets all of them as separate features. But it does NOT get position-split
# team sums: torp_diff is pooled over all 22 players before the model sees
# it. A position-aware weighting therefore produces a feature the match model
# cannot currently reconstruct, which is exactly why it is worth testing.
#
# MODELS COMPARED (all evaluated identically, walk-forward)
#   M0 pooled      -- current uniform weighting, one free global scale.
#   M1 bucket      -- one weight per position bucket (6 params).
#   M2 bucket x ch -- one weight per bucket x channel (30 params), ridge,
#                     shrunk toward M0 rather than toward zero.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/weight_optimisation.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow)
  library(data.table)
  library(glmnet)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")
CHANNELS <- c("recv_epr", "disp_epr", "spoil_epr", "hitout_epr", "psr")
# Uniform (shipped) contribution of each channel to torp.
UNIF <- c(recv_epr = TORP_EPR_WEIGHT, disp_epr = TORP_EPR_WEIGHT,
          spoil_epr = TORP_EPR_WEIGHT, hitout_epr = TORP_EPR_WEIGHT,
          psr = 1 - TORP_EPR_WEIGHT)

# ---- build the design ----------------------------------------------------
pg <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_ratings_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
res <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))

res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, season = as.integer(substr(utc_start_time, 1, 4)),
             home_team_id, away_team_id, margin = home_score - away_score)]
pg <- pg[position_group %in% BUCKETS]
pg[, round := as.numeric(round)]
tr[, round := as.numeric(round)]

L <- merge(pg[, .(player_id, match_id, season, round, team_id,
                  pos = position_group, tog)],
           tr[, .(player_id, season, round, recv_epr, disp_epr, spoil_epr,
                  hitout_epr, psr)],
           by = c("player_id", "season", "round"))

agg <- L[, lapply(.SD, function(v) sum(v * tog, na.rm = TRUE)),
         .SDcols = CHANNELS, by = .(match_id, team_id, pos)]
w <- dcast(agg, match_id + team_id ~ pos, value.var = CHANNELS, fill = 0)

m <- merge(res, w, by.x = c("match_id", "home_team_id"),
           by.y = c("match_id", "team_id"))
a <- merge(res[, .(match_id, away_team_id)], w,
           by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
vc <- setdiff(names(w), c("match_id", "team_id"))
setnames(a, vc, paste0("a_", vc))
m <- merge(m, a, by = "match_id")
for (v in vc) m[[paste0("d_", v)]] <- m[[v]] - m[[paste0("a_", v)]]

feat <- paste0("d_", vc)
feat <- feat[feat %in% names(m)]
# Pooled current-weighting feature: exactly today's torp_diff.
m[, d_pooled := 0]
for (ch in CHANNELS) {
  cols <- paste0("d_", ch, "_", BUCKETS)
  cols <- cols[cols %in% names(m)]
  m[, d_pooled := d_pooled + UNIF[[ch]] * rowSums(.SD), .SDcols = cols]
}
# Bucket-level sums under current channel weighting (for M1).
for (b in BUCKETS) {
  cols <- paste0("d_", CHANNELS, "_", b)
  cols <- cols[cols %in% names(m)]
  wts <- UNIF[sub(paste0("^d_|_", b, "$"), "", cols)]
  m[[paste0("bk_", b)]] <- as.matrix(m[, ..cols]) %*% UNIF[
    sapply(cols, function(cc) sub(paste0("^d_"), "", sub(paste0("_", b, "$"), "", cc)))]
}
bk <- paste0("bk_", BUCKETS)
cat(sprintf("design: %d matches, %d bucket-channel features\n", nrow(m), length(feat)))

# ---- walk-forward evaluation --------------------------------------------
test_seasons <- sort(unique(m$season))
test_seasons <- test_seasons[test_seasons >= min(test_seasons) + 2]

rmse <- function(e) sqrt(mean(e^2))
mae  <- function(e) mean(abs(e))

rows <- list()
weight_store <- list()
err_store <- list()
for (S in test_seasons) {
  tr_i <- m[season < S]; te_i <- m[season == S]
  if (nrow(tr_i) < 200 || nrow(te_i) < 20) next

  # M0 -- pooled, one free scale
  f0 <- lm(margin ~ d_pooled, data = tr_i)
  e0 <- te_i$margin - predict(f0, te_i)

  # M1 -- one weight per bucket
  f1 <- lm(as.formula(paste("margin ~", paste(bk, collapse = " + "))), data = tr_i)
  e1 <- te_i$margin - predict(f1, te_i)

  # M2 -- bucket x channel, ridge shrunk TOWARD M0 (not toward zero):
  # d_pooled is unpenalised, the 30 bucket-channel terms are penalised, so
  # lambda -> inf recovers M0 exactly.
  X_tr <- as.matrix(tr_i[, c("d_pooled", feat), with = FALSE])
  X_te <- as.matrix(te_i[, c("d_pooled", feat), with = FALSE])
  pf <- c(0, rep(1, length(feat)))
  cvf <- cv.glmnet(X_tr, tr_i$margin, alpha = 0, penalty.factor = pf, nfolds = 10)
  e2 <- te_i$margin - as.numeric(predict(cvf, X_te, s = "lambda.min"))

  rows[[as.character(S)]] <- data.table(
    season = S, n_test = nrow(te_i),
    M0_rmse = rmse(e0), M1_rmse = rmse(e1), M2_rmse = rmse(e2),
    M0_mae = mae(e0), M1_mae = mae(e1), M2_mae = mae(e2))
  err_store[[as.character(S)]] <- data.table(
    season = S, match_id = te_i$match_id, e0 = e0, e1 = e1, e2 = e2)
  weight_store[[as.character(S)]] <- data.table(
    season = S, bucket = BUCKETS, weight = coef(f1)[bk])
}
out <- rbindlist(rows)

cat("\n================ WALK-FORWARD OUT-OF-SAMPLE ================\n")
cat("Train on every prior season, test on the named season.\n\n")
print(out[, .(season, n_test,
              M0_rmse = round(M0_rmse, 2), M1_rmse = round(M1_rmse, 2),
              M2_rmse = round(M2_rmse, 2),
              M0_mae = round(M0_mae, 2), M1_mae = round(M1_mae, 2),
              M2_mae = round(M2_mae, 2))])

pooled <- out[, .(M0_rmse = sqrt(weighted.mean(M0_rmse^2, n_test)),
                  M1_rmse = sqrt(weighted.mean(M1_rmse^2, n_test)),
                  M2_rmse = sqrt(weighted.mean(M2_rmse^2, n_test)),
                  M0_mae = weighted.mean(M0_mae, n_test),
                  M1_mae = weighted.mean(M1_mae, n_test),
                  M2_mae = weighted.mean(M2_mae, n_test))]
cat("\n---- pooled across test seasons ----\n")
cat(sprintf("  M0 pooled (current)   RMSE %.2f   MAE %.2f\n", pooled$M0_rmse, pooled$M0_mae))
cat(sprintf("  M1 per-bucket         RMSE %.2f   MAE %.2f   (dMAE %+.2f)\n",
            pooled$M1_rmse, pooled$M1_mae, pooled$M1_mae - pooled$M0_mae))
cat(sprintf("  M2 bucket x channel   RMSE %.2f   MAE %.2f   (dMAE %+.2f)\n",
            pooled$M2_rmse, pooled$M2_mae, pooled$M2_mae - pooled$M0_mae))

cat("\n---- stability of the fitted per-bucket weights (M1) ----\n")
cat("If these swing wildly by training window, the 1.65x is not a\n")
cat("stable property and no reweighting should be shipped on it.\n\n")
ws <- rbindlist(weight_store)
print(dcast(ws, bucket ~ season, value.var = "weight")[
  , lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)])
cat("\nrelative to KEY_FORWARD:\n")
ws2 <- merge(ws, ws[bucket == "KEY_FORWARD", .(season, kf = weight)], by = "season")
ws2[, rel := weight / kf]
print(dcast(ws2, bucket ~ season, value.var = "rel")[
  , lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)])

cat("\n---- paired bootstrap on OOS MAE deltas (2000 reps, resampling matches) ----\n")
cat("  (negative = better than the comparison model)\n")
er <- rbindlist(err_store)
set.seed(7)
B <- 2000
n_er <- nrow(er)
a0 <- abs(er$e0); a1 <- abs(er$e1); a2 <- abs(er$e2)
d1 <- numeric(B); d2 <- numeric(B); d21 <- numeric(B)
for (i in seq_len(B)) {
  ix <- sample.int(n_er, n_er, replace = TRUE)
  d1[i]  <- mean(a1[ix]) - mean(a0[ix])
  d2[i]  <- mean(a2[ix]) - mean(a0[ix])
  d21[i] <- mean(a2[ix]) - mean(a1[ix])
}
ci <- function(v, lab) {
  cat(sprintf("  %-28s %+.3f  95%% CI [%+.3f, %+.3f]  P(better) %.2f\n",
              lab, mean(v), quantile(v, .025), quantile(v, .975), mean(v < 0)))
}
ci(d1,  "M1 - M0 (per-bucket)")
ci(d2,  "M2 - M0 (bucket x channel)")
ci(d21, "M2 - M1 (finer vs bucket)")

cat("\n---- calibration DIAGNOSTIC for the M1 solution ----\n")
cat("Reweighting by the fitted coefficients forces calibration to 1.00 by\n")
cat("construction (see header). Reported only to make that explicit -- it\n")
cat("is not evidence the reweighting is good.\n")
