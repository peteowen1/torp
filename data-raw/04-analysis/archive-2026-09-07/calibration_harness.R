# calibration_harness.R ---------------------------------------------------
# Reusable positional-calibration test: "if we tweak the weights, what
# happens to calibration?"
#
# The gate from docs/plans/FABLE-DEFENDER-VALUE-PLAN.md §1.2, turned into a
# function you can sweep. Regresses actual match margin on TOG-weighted,
# home-minus-away team-sum rating differences split by position bucket. A
# perfectly calibrated rating gives every bucket a coefficient of 1.00; what
# matters in practice is the SPREAD across buckets (today: 2.9x).
#
# Why this can be done without rebuilding the pipeline
# ----------------------------------------------------
# EPR is an unweighted sum of its four published channels
#   epr = recv_epr + disp_epr + spoil_epr + hitout_epr
# and the channels are already columns in torp_ratings.parquet. Because the
# EPR aggregation (decay + shrinkage) is LINEAR in the per-game EPV credit,
# scaling a channel's per-game credit by k scales its published channel
# rating by exactly k. So
#   epr(k) = recv + disp + k*spoil + hitout
# is an EXACT reconstruction of the rating torp would publish under a spoil
# weight k times the current one -- no retrain required.
#
# LIMITS (state these whenever quoting a sweep):
#   * Exact only for a pure RESCALE of a channel. It does not model
#     re-optimising decays/priors, nor any change that alters which events
#     earn credit (that needs a player_game_data rebuild).
#   * PSR is a single published column, so only a scalar multiplier on it is
#     available here -- per-stat repricing (WS3) needs the real refit.
#   * Coefficients are fitted on the same seasons the current constants were
#     tuned on, so the baseline enjoys a small look-ahead. Biases AGAINST
#     candidates, i.e. conservative.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/calibration_harness.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow)
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")

# ---- data (loaded once) --------------------------------------------------
.cal_env <- new.env()

cal_load <- function() {
  if (!is.null(.cal_env$L)) return(invisible())
  pg <- rbindlist(lapply(SEASONS, function(s)
    as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_ratings_%d.parquet", s))))),
    use.names = TRUE, fill = TRUE)
  res <- rbindlist(lapply(SEASONS, function(s)
    as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
    use.names = TRUE, fill = TRUE)
  tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))

  res <- res[!is.na(home_score) & !is.na(away_score),
             .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]
  pg <- pg[position_group %in% BUCKETS]
  pg[, round := as.numeric(round)]
  tr[, round := as.numeric(round)]

  L <- merge(pg[, .(player_id, match_id, season, round, team_id,
                    pos = position_group, tog)],
             tr[, .(player_id, season, round, recv_epr, disp_epr, spoil_epr,
                    hitout_epr, epr, psr)],
             by = c("player_id", "season", "round"))

  # Guard the identity the harness depends on.
  chk <- L[!is.na(epr), max(abs(recv_epr + disp_epr + spoil_epr + hitout_epr - epr))]
  cat(sprintf("EPR additivity check: max |sum(channels) - epr| = %.4f\n", chk))
  if (chk > 0.05) {
    stop("EPR is not the sum of its published channels; the harness reconstruction is invalid.")
  }

  .cal_env$L <- L
  .cal_env$res <- res
  invisible()
}

#' Positional calibration under a candidate weighting
#'
#' @param m_spoil,m_recv,m_disp,m_hitout Multipliers on each EPR channel
#'   (1 = current). Named with an `m_` prefix deliberately: a parameter called
#'   `psr` or `spoil_epr` is SHADOWED by the column of that name inside
#'   data.table's `[`, which silently squares the column instead of scaling it.
#' @param m_psr Multiplier on the PSR half (1 = current).
#' @param epr_weight Blend weight; default the shipped TORP_EPR_WEIGHT.
#' @return list(coefs = data.table, spread = max/min, kd_kf = ratio, r2 = numeric)
positional_calibration <- function(m_spoil = 1, m_recv = 1, m_disp = 1,
                                   m_hitout = 1, m_psr = 1,
                                   epr_weight = TORP_EPR_WEIGHT) {
  cal_load()
  L <- copy(.cal_env$L)
  L[, epr_c := m_recv * recv_epr + m_disp * disp_epr +
                m_spoil * spoil_epr + m_hitout * hitout_epr]
  L[, torp_c := epr_weight * epr_c + (1 - epr_weight) * m_psr * psr]

  agg <- L[, .(v = sum(torp_c * tog, na.rm = TRUE)), by = .(match_id, team_id, pos)]
  w <- dcast(agg, match_id + team_id ~ pos, value.var = "v", fill = 0)
  bs <- setdiff(names(w), c("match_id", "team_id"))
  setnames(w, bs, paste0("v_", bs))
  res <- .cal_env$res
  m <- merge(res, w, by.x = c("match_id", "home_team_id"),
             by.y = c("match_id", "team_id"))
  a <- merge(res[, .(match_id, away_team_id)], w,
             by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
  vc <- paste0("v_", BUCKETS)
  vc <- vc[vc %in% names(w)]
  setnames(a, vc, paste0("a_", vc))
  m <- merge(m, a, by = "match_id")
  for (v in vc) m[[paste0("d_", v)]] <- m[[v]] - m[[paste0("a_", v)]]

  terms <- paste0("d_", vc)
  fit <- lm(as.formula(paste("margin ~", paste(terms, collapse = " + "))), data = m)
  s <- summary(fit)
  co <- as.data.table(s$coefficients, keep.rownames = "term")
  setnames(co, c("term", "est", "se", "t", "p"))
  co <- co[term != "(Intercept)"]
  co[, bucket := sub("^d_v_", "", term)]
  co[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se)]

  kd <- co[bucket == "KEY_DEFENDER", est]
  kf <- co[bucket == "KEY_FORWARD", est]
  list(coefs = co[order(-est), .(bucket, est = round(est, 2),
                                 lo = round(lo, 2), hi = round(hi, 2))],
       spread = max(co$est) / min(co$est),
       kd_kf = kd / kf,
       r2 = s$r.squared,
       n = nrow(m))
}

#' Assert the harness still reproduces the published baseline
#'
#' Exists because a silent bug (a `psr` parameter shadowed by the `psr` column
#' inside data.table's `[`, squaring it instead of scaling it) produced a
#' plausible-looking but completely wrong table. Nothing flagged it except the
#' baseline failing to match defender_value_audit.R's known numbers. Any tweak
#' to this file must keep that check passing.
#' @keywords internal
cal_assert_baseline <- function(tol = 0.02) {
  b <- positional_calibration()
  want <- c(KEY_DEFENDER = 2.10, KEY_FORWARD = 1.27, MIDFIELDER = 1.58)
  got <- setNames(b$coefs$est, b$coefs$bucket)[names(want)]
  bad <- abs(got - want) > tol
  if (any(bad)) {
    stop(sprintf(
      "Harness no longer reproduces the published baseline (%s). Expected %s, got %s.",
      paste(names(want)[bad], collapse = ", "),
      paste(want[bad], collapse = "/"), paste(got[bad], collapse = "/")))
  }
  if (abs(b$r2 - 0.319) > 0.01) {
    stop(sprintf("Baseline R2 drifted: expected 0.319, got %.3f", b$r2))
  }
  cat("Baseline reproduction check: PASS\n")
  invisible(b)
}

# =========================================================================
if (sys.nframe() == 0L) {

  cat("\n===== BASELINE (shipped weights) =====\n")
  b <- cal_assert_baseline()
  print(b$coefs)
  cat(sprintf("\n  spread (max/min) = %.2fx   KD/KF = %.2fx   R2 = %.3f   n = %d\n",
              b$spread, b$kd_kf, b$r2, b$n))
  cat("\n  Perfect calibration = every bucket 1.00, spread 1.00x.\n")

  cat("\n\n===== SWEEP: spoil-channel multiplier =====\n")
  cat("Does up-weighting the defensive-actions channel flatten the spread?\n\n")
  sw <- rbindlist(lapply(c(1, 2, 3, 5, 8, 12, 20), function(k) {
    r <- positional_calibration(m_spoil = k)
    data.table(spoil_mult = k,
               KD = r$coefs[bucket == "KEY_DEFENDER", est],
               MD = r$coefs[bucket == "MEDIUM_DEFENDER", est],
               MID = r$coefs[bucket == "MIDFIELDER", est],
               KF = r$coefs[bucket == "KEY_FORWARD", est],
               spread = round(r$spread, 2),
               kd_kf = round(r$kd_kf, 2))
  }))
  print(sw)

  cat("\n\n===== SWEEP: EPR/PSR blend weight =====\n")
  sw2 <- rbindlist(lapply(c(0.3, 0.4, 0.5, 0.6, 0.7), function(w) {
    r <- positional_calibration(epr_weight = w)
    data.table(epr_weight = w,
               KD = r$coefs[bucket == "KEY_DEFENDER", est],
               KF = r$coefs[bucket == "KEY_FORWARD", est],
               spread = round(r$spread, 2),
               kd_kf = round(r$kd_kf, 2))
  }))
  print(sw2)

  cat("\nNOTE: a lower spread is necessary but NOT sufficient to ship. Any\n")
  cat("candidate must also clear the rolling-OOS match-model gate (plan §1.1)\n")
  cat("and a face-validity read (§6.4) before it goes near a release.\n")
}
