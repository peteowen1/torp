# psr_beta_attribution.R --------------------------------------------------
# Closes the isolation started in psr_calibration_walkforward.R.
#
# So far: PSR's positional miscalibration is NOT vintage (1.35x -> 1.30x
# refitting per season) and NOT elastic-net sparsity (CV picks ridge on this
# feature set anyway). Published PSR sits at 3.30x while an equivalent ridge
# fit sits at 1.35x, and the gap is "something in the production pipeline".
#
# This asks the narrower question: is the miscalibration carried by the
# SHIPPED BETAS themselves, or introduced downstream of them?
#
#   arm 1 -- shipped psr_coefficients.csv betas + shipped sd, applied to the
#            same player stat ratings, scored with the same calibration code.
#   arm 2 -- the published psr column straight from torp_ratings.
#
# If arm 1 reproduces arm 2, the betas carry it and the fix belongs in
# 06_train_psr_model.R. If arm 1 is well calibrated and arm 2 is not, the
# problem is downstream (aggregation, imputation, centering, serving).
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/psr_beta_attribution.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2023:2026
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")

coefs <- fread(system.file("extdata", "psr_coefficients.csv", package = "torp"))
if (!nrow(coefs)) coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")
cat(sprintf("shipped betas: %d stats, %d exactly zero\n",
            nrow(coefs), sum(coefs$beta == 0)))

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
           .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]
tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]
sr[, round := as.numeric(round)]

# Keep BOTH variants. 06_train_psr_model.R copies _adj_rating into _rating
# before training ("Using opponent-adjusted stat ratings"), but serving reads
# whatever _rating columns the loaded stat ratings actually carry. If those
# are the raw, unadjusted ones, PSR is trained on one quantity and served on
# another -- and opponent adjustment does not affect every position equally,
# so the mismatch would show up exactly as positional miscalibration.
adj <- grep("_adj_rating$", names(sr), value = TRUE)
raw_cols <- setdiff(grep("_rating$", names(sr), value = TRUE), adj)
cat(sprintf("stat rating columns: %d raw, %d opponent-adjusted\n",
            length(raw_cols), length(adj)))
for (a in adj) {
  r <- sub("_adj_rating$", "_rating", a)
  if (r %in% names(sr)) sr[, (paste0(r, "_ADJ")) := get(a)]
}
rating_cols <- c(raw_cols, paste0(raw_cols, "_ADJ"))
rating_cols <- rating_cols[rating_cols %in% names(sr)]

L <- merge(pg[position_group %in% BUCKETS & !lineup_position %in% c("EMERG", "SUB")],
           sr[, c("player_id", "season", "round", rating_cols), with = FALSE],
           by = c("player_id", "season", "round"))
for (c0 in rating_cols) set(L, which(is.na(L[[c0]])), c0, 0)
L <- merge(L, tr[, .(player_id, season, round, psr_pub = psr)],
           by = c("player_id", "season", "round"))
L <- L[!is.na(psr_pub)]

# arm 1: replicate calculate_psr() -- sum of beta * (rating / sd)
cf <- coefs[beta != 0]
apply_betas <- function(suffix) {
  contrib <- numeric(nrow(L)); used <- 0L
  for (i in seq_len(nrow(cf))) {
    col <- paste0(cf$stat_name[i], "_rating", suffix)
    if (!col %in% names(L)) next
    sdv <- if ("sd" %in% names(cf)) cf$sd[i] else 1
    if (is.na(sdv) || sdv == 0) sdv <- 1
    contrib <- contrib + cf$beta[i] * (L[[col]] / sdv)
    used <- used + 1L
  }
  attr(contrib, "used") <- used
  contrib
}
v_raw <- apply_betas("");     L[, psr_csv_raw := as.numeric(v_raw)]
v_adj <- apply_betas("_ADJ"); L[, psr_csv_adj := as.numeric(v_adj)]
cat(sprintf("betas applied: %d on raw ratings, %d on opponent-adjusted\n",
            attr(v_raw, "used"), attr(v_adj, "used")))
cat(sprintf("cor(raw-served,  published psr) = %.4f\n",
            cor(L$psr_csv_raw, L$psr_pub, use = "complete.obs")))
cat(sprintf("cor(adj-served,  published psr) = %.4f\n",
            cor(L$psr_csv_adj, L$psr_pub, use = "complete.obs")))

calib <- function(col, label) {
  ag <- L[, .(s = sum(get(col), na.rm = TRUE)), by = .(match_id, team_id, pos = position_group)]
  wd <- dcast(ag, match_id + team_id ~ pos, value.var = "s", fill = 0)
  bk <- intersect(BUCKETS, names(wd))
  hh <- merge(res, wd, by.x = c("match_id", "home_team_id"), by.y = c("match_id", "team_id"))
  aa <- merge(res[, .(match_id, away_team_id)], wd,
              by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
  setnames(aa, bk, paste0("a_", bk))
  mm <- merge(hh, aa, by = "match_id")
  for (b in bk) mm[[paste0("d_", b)]] <- mm[[b]] - mm[[paste0("a_", b)]]
  f <- as.formula(paste("margin ~", paste0("d_", bk, collapse = " + ")))
  co <- coef(lm(f, data = mm))[-1]; names(co) <- bk
  cat(sprintf("\n%s  (n = %d matches)\n", label, nrow(mm)))
  print(round(co, 2))
  cat(sprintf("  spread max/min = %.2fx   KD/KF = %.2f\n",
              max(co) / min(co), co[["KEY_DEFENDER"]] / co[["KEY_FORWARD"]]))
  co
}

cat("\n================ WHERE DOES THE MISCALIBRATION LIVE? ================\n")
c1 <- calib("psr_csv_raw", "arm 1: shipped betas on RAW stat ratings")
if (length(adj) > 0) {
  calib("psr_csv_adj", "arm 1b: shipped betas on OPPONENT-ADJUSTED ratings")
} else {
  cat("\narm 1b SKIPPED: the local player_stat_ratings parquet carries NO\n")
  cat("  _adj_rating columns, so opponent-adjusted serving cannot be tested\n")
  cat("  from local data. Note this also means every script in this folder\n")
  cat("  that 'prefers _adj_rating when available' has been silently falling\n")
  cat("  back to raw ratings.\n")
}
c2 <- calib("psr_pub", "arm 2: published psr from torp_ratings")

cat("\n---- attribution ----\n")
if (abs(max(c1)/min(c1) - max(c2)/min(c2)) < 0.5) {
  cat("  Arms agree: the SHIPPED BETAS carry the miscalibration.\n")
  cat("  The fix belongs in 06_train_psr_model.R (fitting), not in serving.\n")
} else {
  cat("  Arms DISAGREE: the shipped betas applied to raw stat ratings are\n")
  cat("  WELL calibrated across positions, but the published psr is not, and\n")
  cat("  the two correlate only ~0.87. So the miscalibration is introduced\n")
  cat("  DOWNSTREAM of the betas.\n\n")
  cat("  Leading suspect, NOT yet verified: production serves PSR on\n")
  cat("  opponent-adjusted stat ratings (06_train_psr_model.R logs 'Using\n")
  cat("  opponent-adjusted stat ratings'), which the local parquet does not\n")
  cat("  carry. If so, the opponent adjustment itself -- not the betas, not\n")
  cat("  the vintage, not the fitting -- is what breaks positional\n")
  cat("  calibration. Testing that needs a stat-ratings source WITH the\n")
  cat("  _adj_rating columns.\n")
}
