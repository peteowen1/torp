# B1 (plan §7.2) — does the PSR position-centring TAXONOMY explain the
# unexplained 2.08x vs 3.30x gap left open by §6.10?
#
# §6.10 reported an arm labelled "centered by lineup_position, as production
# does" at 2.08x spread, against published psr at 3.30x, and recorded the
# difference as unaccounted for. That label was wrong: `calculate_psr()`
# (psr.R:104-107) PREFERS lineup_position but falls back to pos_group, and the
# frame production feeds it -- load_player_stat_ratings(TRUE), built by
# player_skills_data.R -- carries pos_group only (verified: 234 columns, no
# lineup_position). So production centres 6-way and §6.10's arm B centred
# 20-way. Arm B was never a replication; it was an untested candidate.
#
# This script separates the three things §6.10 conflated:
#   taxonomy   6-way pos_group  vs  20-way lineup_position
#   weighting  unweighted       vs  wt_80s-weighted (production uses wt_80s)
#   frame      scoring subset   vs  full-history pooled (production pools all)
#
# If arm E (6-way, wt_80s, pooled) reproduces ~3.30x, the gap is fully
# explained and 20-way centring becomes a mechanism-based candidate fix.
suppressMessages({library(arrow); library(data.table)})

D  <- "C:/dev/torpverse/torpdata/data/"
S  <- 2023:2026          # scoring window (matches §6.10 so arms are comparable)
SF <- 2021:2026          # full frame production actually centres over
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK",
        "MEDIUM_FORWARD","KEY_FORWARD")
NBOOT <- 400

coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")[beta != 0]

rd <- function(pat, seasons) rbindlist(lapply(seasons, function(s)
  as.data.table(read_parquet(file.path(D, sprintf(pat, s))))),
  use.names = TRUE, fill = TRUE)

# --- stat ratings: the exact frame calculate_psr() receives -----------------
sr_full <- rd("player_stat_ratings_%d.parquet", SF)
sr_full[, round := as.numeric(round)]
stopifnot(!"lineup_position" %in% names(sr_full))   # the premise of this script

# psr_raw exactly as calculate_psr() computes it (beta * rating/sd, NA -> 0)
apply_betas <- function(dt) {
  v <- numeric(nrow(dt))
  for (i in seq_len(nrow(coefs))) {
    cc <- paste0(coefs$stat_name[i], "_rating"); if (!cc %in% names(dt)) next
    sdv <- coefs$sd[i]; if (is.na(sdv) || sdv == 0) sdv <- 1
    x <- dt[[cc]]; x[is.na(x)] <- 0
    v <- v + coefs$beta[i] * (x / sdv)
  }
  v
}
sr_full[, psr_raw_rep := apply_betas(sr_full)]

# --- lineup_position + team/match keys from player_game --------------------
pg <- rd("player_game_%d.parquet", SF)[, .(
  player_id, match_id, season, round = as.numeric(round), team_id,
  position_group, lineup_position)]

res <- rd("results_%d.parquet", S)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]

tr <- as.data.table(read_parquet(file.path(D, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]

# --- centring arms ----------------------------------------------------------
# Production: wt_80s-weighted mean of psr_raw within pos_group, over the pooled
# all-season frame. Compute every centring on sr_full (the production frame),
# then join to the scoring subset -- so frame effects are isolated, not baked in.
wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_) else weighted.mean(x[ok], w[ok])
}

# 6-way pos_group lives on sr_full already
sr_full[, c_pos_unw := psr_raw_rep - mean(psr_raw_rep, na.rm = TRUE), by = pos_group]
sr_full[, c_pos_wtd := psr_raw_rep - wmean(psr_raw_rep, wt_80s),      by = pos_group]

# 20-way lineup_position must be joined in (it is NOT in the production frame --
# that is precisely why production cannot centre on it today)
lp <- unique(pg[!is.na(lineup_position) & !lineup_position %in% c("EMERG","SUB"),
                .(player_id, season, round, lineup_position)],
             by = c("player_id","season","round"))
sr_full <- merge(sr_full, lp, by = c("player_id","season","round"), all.x = TRUE)
sr_full[, c_lp_unw := psr_raw_rep - mean(psr_raw_rep, na.rm = TRUE), by = lineup_position]
sr_full[, c_lp_wtd := psr_raw_rep - wmean(psr_raw_rep, wt_80s),      by = lineup_position]

keep <- c("player_id","season","round","psr_raw_rep",
          "c_pos_unw","c_pos_wtd","c_lp_unw","c_lp_wtd")

# --- scoring frame ----------------------------------------------------------
L <- merge(pg[season %in% S & position_group %in% BK &
              !lineup_position %in% c("EMERG","SUB")],
           sr_full[season %in% S, ..keep],
           by = c("player_id","season","round"))
L <- merge(L, tr[, .(player_id, season, round, psr_pub = psr)],
           by = c("player_id","season","round"))
L <- L[!is.na(psr_pub)]
cat(sprintf("scoring rows: %d over %d matches\n", nrow(L), uniqueN(L$match_id)))

# --- calibration gate with a match-level bootstrap ---------------------------
build_mm <- function(col) {
  ag <- L[, .(s = sum(get(col), na.rm = TRUE)), by = .(match_id, team_id, pos = position_group)]
  wd <- dcast(ag, match_id + team_id ~ pos, value.var = "s", fill = 0)
  b2 <- intersect(BK, names(wd))
  h <- merge(res, wd, by.x = c("match_id","home_team_id"), by.y = c("match_id","team_id"))
  a <- merge(res[, .(match_id, away_team_id)], wd,
             by.x = c("match_id","away_team_id"), by.y = c("match_id","team_id"))
  setnames(a, b2, paste0("a_", b2))
  mm <- merge(h, a, by = "match_id")
  for (b in b2) mm[[paste0("d_", b)]] <- mm[[b]] - mm[[paste0("a_", b)]]
  list(mm = mm, b2 = b2)
}

fit_co <- function(mm, b2) {
  co <- coef(lm(as.formula(paste("margin ~", paste0("d_", b2, collapse = "+"))),
                data = mm))[-1]
  names(co) <- b2
  co
}

calib <- function(col, label) {
  z <- build_mm(col); mm <- z$mm; b2 <- z$b2
  co <- fit_co(mm, b2)
  spread <- max(co) / min(co)
  kdkf   <- co[["KEY_DEFENDER"]] / co[["KEY_FORWARD"]]
  # match-level bootstrap: the §6.12 lesson is that point estimates on this
  # gate mislead, so every arm reports a CI or it does not get quoted.
  bs <- matrix(NA_real_, NBOOT, 2)
  n <- nrow(mm)
  for (i in seq_len(NBOOT)) {
    idx <- sample.int(n, n, replace = TRUE)
    cb <- tryCatch(fit_co(mm[idx], b2), error = function(e) NULL)
    if (is.null(cb) || any(is.na(cb))) next
    bs[i, ] <- c(max(cb) / min(cb), cb[["KEY_DEFENDER"]] / cb[["KEY_FORWARD"]])
  }
  q <- function(j) quantile(bs[, j], c(.025, .975), na.rm = TRUE)
  cat(sprintf("\n%s\n", label))
  print(round(co, 2))
  cat(sprintf("  spread %.2fx [%.2f, %.2f]   KD/KF %.2f [%.2f, %.2f]\n",
              spread, q(1)[1], q(1)[2], kdkf, q(2)[1], q(2)[2]))
  # dispersion, the Problem-B read
  sds <- L[, .(sd = sd(get(col), na.rm = TRUE)), by = position_group][order(position_group)]
  cat("  per-bucket SD: ",
      paste(sprintf("%s=%.2f", substr(sds$position_group, 1, 3), sds$sd), collapse = "  "), "\n")
  invisible(c(spread = spread, kdkf = kdkf))
}

cat("\n=== PSR position-centring: taxonomy x weighting ===\n")
calib("psr_raw_rep", "A: UNCENTERED (invalid -- encodes lineup composition, §6.10)")
calib("c_lp_unw",    "B: 20-way lineup_position, unweighted  (= §6.10's arm B, the '2.08x')")
calib("c_lp_wtd",    "C: 20-way lineup_position, wt_80s-weighted")
calib("c_pos_unw",   "D: 6-way pos_group, unweighted")
calib("c_pos_wtd",   "E: 6-way pos_group, wt_80s-weighted  <-- WHAT PRODUCTION ACTUALLY DOES")
calib("psr_pub",     "F: published psr (target for E)")
