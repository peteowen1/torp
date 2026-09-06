# B1 follow-up (plan §7.2) — paired bootstrap of the centring-taxonomy change,
# plus a replacement for the broken "spread" gate.
#
# psr_centering_taxonomy.R established that production centres PSR 6-way
# (pos_group) and that this reproduces published psr (E vs F: coefficients
# agree to ~0.05). Switching to 20-way lineup_position moves KD/KF 1.87 -> 1.24.
#
# Two problems with quoting that from marginal CIs:
#   1. The arms' marginal CIs overlap almost completely -- but they are fit on
#      THE SAME matches, so the paired difference is far more precise than
#      either margin. Round 1 never paired, and compared marginals instead.
#   2. The "spread" statistic (max/min of the six coefficients) is pathological:
#      MEDIUM_DEFENDER sits near 0.55 and its bootstrap distribution crosses
#      zero, so spread's own CI came back [-23.7, 28.0]. Every "3.18x -> 3.00x"
#      comparison in round 1 was quoting a statistic with a sign-flipping
#      sampling distribution. Replaced here by mean|beta - 1|, which is bounded,
#      well-behaved, and measures the same thing (distance from calibrated).
suppressMessages({library(arrow); library(data.table)})

D  <- "C:/dev/torpverse/torpdata/data/"
S  <- 2023:2026
SF <- 2021:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK",
        "MEDIUM_FORWARD","KEY_FORWARD")
NBOOT <- 2000

coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")[beta != 0]
rd <- function(pat, seasons) rbindlist(lapply(seasons, function(s)
  as.data.table(read_parquet(file.path(D, sprintf(pat, s))))), use.names = TRUE, fill = TRUE)

sr_full <- rd("player_stat_ratings_%d.parquet", SF); sr_full[, round := as.numeric(round)]
v <- numeric(nrow(sr_full))
for (i in seq_len(nrow(coefs))) {
  cc <- paste0(coefs$stat_name[i], "_rating"); if (!cc %in% names(sr_full)) next
  sdv <- coefs$sd[i]; if (is.na(sdv) || sdv == 0) sdv <- 1
  x <- sr_full[[cc]]; x[is.na(x)] <- 0
  v <- v + coefs$beta[i] * (x / sdv)
}
sr_full[, psr_raw_rep := v]

pg <- rd("player_game_%d.parquet", SF)[, .(player_id, match_id, season,
        round = as.numeric(round), team_id, position_group, lineup_position)]
res <- rd("results_%d.parquet", S)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]

wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) NA_real_ else weighted.mean(x[ok], w[ok]) }

sr_full[, c_pos := psr_raw_rep - wmean(psr_raw_rep, wt_80s), by = pos_group]
lp <- unique(pg[!is.na(lineup_position) & !lineup_position %in% c("EMERG","SUB"),
                .(player_id, season, round, lineup_position)],
             by = c("player_id","season","round"))
sr_full <- merge(sr_full, lp, by = c("player_id","season","round"), all.x = TRUE)
sr_full[, c_lp := psr_raw_rep - wmean(psr_raw_rep, wt_80s), by = lineup_position]

L <- merge(pg[season %in% S & position_group %in% BK &
              !lineup_position %in% c("EMERG","SUB")],
           sr_full[season %in% S, .(player_id, season, round, c_pos, c_lp)],
           by = c("player_id","season","round"))

# One design matrix per arm, on identical matches -- this is what makes the
# comparison paired.
mk <- function(col) {
  ag <- L[, .(s = sum(get(col), na.rm = TRUE)), by = .(match_id, team_id, pos = position_group)]
  wd <- dcast(ag, match_id + team_id ~ pos, value.var = "s", fill = 0)
  b2 <- intersect(BK, names(wd))
  h <- merge(res, wd, by.x = c("match_id","home_team_id"), by.y = c("match_id","team_id"))
  a <- merge(res[, .(match_id, away_team_id)], wd,
             by.x = c("match_id","away_team_id"), by.y = c("match_id","team_id"))
  setnames(a, b2, paste0("a_", b2))
  mm <- merge(h, a, by = "match_id")
  for (b in b2) mm[[paste0("d_", b)]] <- mm[[b]] - mm[[paste0("a_", b)]]
  setorder(mm, match_id)
  mm
}
A_pos <- mk("c_pos")   # production, 6-way
A_lp  <- mk("c_lp")    # candidate, 20-way
stopifnot(identical(A_pos$match_id, A_lp$match_id))
b2 <- intersect(BK, sub("^d_", "", grep("^d_", names(A_pos), value = TRUE)))
fml <- as.formula(paste("margin ~", paste0("d_", b2, collapse = "+")))

stats_of <- function(mm, idx = NULL) {
  d <- if (is.null(idx)) mm else mm[idx]
  co <- coef(lm(fml, data = d))[-1]; names(co) <- b2
  c(kdkf = unname(co[["KEY_DEFENDER"]] / co[["KEY_FORWARD"]]),
    miscal = unname(mean(abs(co - 1))),
    kd = unname(co[["KEY_DEFENDER"]]), kf = unname(co[["KEY_FORWARD"]]))
}

pt_pos <- stats_of(A_pos); pt_lp <- stats_of(A_lp)
n <- nrow(A_pos)
bs <- matrix(NA_real_, NBOOT, 4,
             dimnames = list(NULL, c("kdkf_pos","kdkf_lp","mis_pos","mis_lp")))
set.seed(20260727)
for (i in seq_len(NBOOT)) {
  idx <- sample.int(n, n, replace = TRUE)   # SAME resampled matches for both arms
  sp <- tryCatch(stats_of(A_pos, idx), error = function(e) NULL)
  sl <- tryCatch(stats_of(A_lp,  idx), error = function(e) NULL)
  if (is.null(sp) || is.null(sl)) next
  bs[i, ] <- c(sp["kdkf"], sl["kdkf"], sp["miscal"], sl["miscal"])
}
ci <- function(x) quantile(x, c(.025, .975), na.rm = TRUE)

cat(sprintf("matches: %d   NBOOT: %d\n\n", n, NBOOT))
cat("=== MARGINAL (what round 1 compared) ===\n")
cat(sprintf("  6-way  pos_group  [production]  KD/KF %.2f [%.2f, %.2f]   mean|b-1| %.3f [%.3f, %.3f]\n",
            pt_pos["kdkf"], ci(bs[,"kdkf_pos"])[1], ci(bs[,"kdkf_pos"])[2],
            pt_pos["miscal"], ci(bs[,"mis_pos"])[1], ci(bs[,"mis_pos"])[2]))
cat(sprintf("  20-way lineup_pos [candidate]   KD/KF %.2f [%.2f, %.2f]   mean|b-1| %.3f [%.3f, %.3f]\n",
            pt_lp["kdkf"], ci(bs[,"kdkf_lp"])[1], ci(bs[,"kdkf_lp"])[2],
            pt_lp["miscal"], ci(bs[,"mis_lp"])[1], ci(bs[,"mis_lp"])[2]))

d_kdkf <- bs[,"kdkf_lp"] - bs[,"kdkf_pos"]
d_mis  <- bs[,"mis_lp"]  - bs[,"mis_pos"]
cat("\n=== PAIRED DIFFERENCE (20-way minus 6-way, same matches) ===\n")
cat(sprintf("  d KD/KF     %+.3f  [%+.3f, %+.3f]   P(improves toward 1) = %.3f\n",
            pt_lp["kdkf"] - pt_pos["kdkf"], ci(d_kdkf)[1], ci(d_kdkf)[2],
            mean(abs(bs[,"kdkf_lp"] - 1) < abs(bs[,"kdkf_pos"] - 1), na.rm = TRUE)))
cat(sprintf("  d mean|b-1| %+.3f  [%+.3f, %+.3f]   P(improves)          = %.3f\n",
            pt_lp["miscal"] - pt_pos["miscal"], ci(d_mis)[1], ci(d_mis)[2],
            mean(d_mis < 0, na.rm = TRUE)))
cat(sprintf("\n  KD coef %.2f -> %.2f    KF coef %.2f -> %.2f\n",
            pt_pos["kd"], pt_lp["kd"], pt_pos["kf"], pt_lp["kf"]))

cat("\n=== why 'spread' (max/min) had to be retired ===\n")
co_pos <- coef(lm(fml, data = A_pos))[-1]; names(co_pos) <- b2
cat(sprintf("  min coefficient is %s = %.2f; a bootstrap denominator this close to 0\n",
            names(which.min(co_pos)), min(co_pos)))
cat("  makes max/min unbounded and sign-flipping. mean|b-1| replaces it.\n")
