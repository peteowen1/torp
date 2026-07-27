# A1 + A2 (plan §7.1) — rebuild the positional-calibration gate.
#
# Round 1 scored ten experiments on: margin ~ home-minus-away per-bucket TORP
# sums, summarised by max/min of the six coefficients. §7.7 showed that summary
# statistic is pathological (min coefficient ~0.55, bootstrap CI [-23.7, +28.0]).
# This script rebuilds the gate on three axes and measures which is sharpest.
#
# A1 -- TARGET. Split margin into points-for / points-against. Note the
# analytic caveat up front, because it bounds what A1 can deliver:
#     scored_home = a + G.H + D.A + e_h
#     scored_away = a + G.A + D.H + e_a
#     margin      = (G-D).(H-A) + (e_h - e_a)
# The margin-relevant combination is (G-D), and the margin regression already
# estimates it efficiently -- the mirrored design means the second row carries
# no extra information about it. So the for/against split should NOT be expected
# to tighten the calibration ratio. What it genuinely buys is a DECOMPOSITION:
# each bucket's margin coefficient splits into a contribution half (own rating
# raises own score) and a suppression half (own rating lowers opponent score).
# That is directly relevant to O2/Track D. Measured here rather than assumed.
#
# A2 -- ESTIMATOR. Three variance-reduction candidates:
#   M1 signed team fixed effects (absorbs persistent team strength -- the
#      mechanism §6.8 blamed for the production null; independent of the
#      ratings, unlike team_epr which is derived from them)
#   M2 collapse 6 collinear buckets to 3 (DEF/MID/FWD)
#   M3 ridge on the 6-bucket design
#
# Every statistic is reported with a match-level bootstrap CI, and the summary
# is mean|b-1| (bounded) not max/min (unbounded). Arms sharing matches are
# compared paired, per §7.7(c).
suppressMessages({library(arrow); library(data.table); library(MASS)})

D  <- "C:/dev/torpverse/torpdata/data/"
S  <- 2023:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK",
        "MEDIUM_FORWARD","KEY_FORWARD")
BK3 <- c(KEY_DEFENDER = "DEF", MEDIUM_DEFENDER = "DEF", MIDFIELDER = "MID",
         RUCK = "MID", MEDIUM_FORWARD = "FWD", KEY_FORWARD = "FWD")
NBOOT <- 1000
NSIM  <- 300

rd <- function(pat, seasons) rbindlist(lapply(seasons, function(s)
  as.data.table(read_parquet(file.path(D, sprintf(pat, s))))), use.names = TRUE, fill = TRUE)

pg <- rd("player_game_%d.parquet", S)[, .(player_id, match_id, season,
        round = as.numeric(round), team_id, position_group, lineup_position)]
res <- rd("results_%d.parquet", S)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, home_team_id, away_team_id,
             home_score, away_score, margin = home_score - away_score)]

tr <- as.data.table(read_parquet(file.path(D, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]
rating_col <- intersect(c("torp","torp_value","torp_rating"), names(tr))[1]
if (is.na(rating_col)) stop("no torp column in torp_ratings.parquet; cols: ",
                            paste(names(tr), collapse = ", "))
cat(sprintf("rating column: %s\n", rating_col))

L <- merge(pg[position_group %in% BK & !lineup_position %in% c("EMERG","SUB")],
           tr[, .(player_id, season, round, rating = get(rating_col))],
           by = c("player_id","season","round"))
L <- L[!is.na(rating)]

# --- per-team bucket sums ---------------------------------------------------
mk_wide <- function(bucket_map = NULL) {
  d <- copy(L)
  d[, pos := if (is.null(bucket_map)) position_group else bucket_map[position_group]]
  ag <- d[, .(s = sum(rating, na.rm = TRUE)), by = .(match_id, team_id, pos)]
  dcast(ag, match_id + team_id ~ pos, value.var = "s", fill = 0)
}

# --- MARGIN design (one row per match) --------------------------------------
mk_margin <- function(bucket_map = NULL) {
  wd <- mk_wide(bucket_map)
  bs <- setdiff(names(wd), c("match_id","team_id"))
  h <- merge(res, wd, by.x = c("match_id","home_team_id"), by.y = c("match_id","team_id"))
  a <- merge(res[, .(match_id, away_team_id)], wd,
             by.x = c("match_id","away_team_id"), by.y = c("match_id","team_id"))
  setnames(a, bs, paste0("a_", bs))
  # drop the join key before the final merge: `h` already carries away_team_id,
  # and leaving it on both sides silently renames it to .x/.y, which nulls out
  # mm$away_team_id and makes the signed team dummies degenerate
  a[, away_team_id := NULL]
  mm <- merge(h, a, by = "match_id")
  for (b in bs) mm[[paste0("d_", b)]] <- mm[[b]] - mm[[paste0("a_", b)]]
  # signed team dummies: +1 home, -1 away
  teams <- sort(unique(c(mm$home_team_id, mm$away_team_id)))
  for (t in teams[-1]) mm[[paste0("t_", t)]] <-
    as.integer(mm$home_team_id == t) - as.integer(mm$away_team_id == t)
  setorder(mm, match_id)
  list(mm = mm, bs = bs, tcols = paste0("t_", teams[-1]))
}

# --- FOR/AGAINST design (two rows per match) --------------------------------
mk_fa <- function() {
  wd <- mk_wide()
  bs <- setdiff(names(wd), c("match_id","team_id"))
  side <- rbind(
    res[, .(match_id, team_id = home_team_id, opp_id = away_team_id,
            scored = home_score, is_home = 1L)],
    res[, .(match_id, team_id = away_team_id, opp_id = home_team_id,
            scored = away_score, is_home = 0L)])
  x <- merge(side, wd, by = c("match_id","team_id"))
  o <- copy(wd); setnames(o, bs, paste0("o_", bs)); setnames(o, "team_id", "opp_id")
  x <- merge(x, o, by = c("match_id","opp_id"))
  teams <- sort(unique(x$team_id))
  for (t in teams[-1]) {
    x[[paste0("ow_", t)]] <- as.integer(x$team_id == t)
    x[[paste0("op_", t)]] <- as.integer(x$opp_id  == t)
  }
  setorder(x, match_id)
  list(x = x, bs = bs, tcols = c(paste0("ow_", teams[-1]), paste0("op_", teams[-1])))
}

MG  <- mk_margin()
MG3 <- mk_margin(BK3)
FA  <- mk_fa()
match_ids <- MG$mm$match_id
cat(sprintf("matches: %d   for/against rows: %d\n\n", length(match_ids), nrow(FA$x)))

# --- estimators -------------------------------------------------------------
fit_margin <- function(mm, bs, tcols = NULL, ridge_l = NULL) {
  rhs <- paste0("d_", bs)
  if (!is.null(tcols)) rhs <- c(rhs, tcols)
  f <- as.formula(paste("margin ~", paste(rhs, collapse = "+")))
  if (is.null(ridge_l)) {
    co <- coef(lm(f, data = mm))
  } else {
    co <- coef(MASS::lm.ridge(f, data = mm, lambda = ridge_l))
  }
  out <- co[paste0("d_", bs)]; names(out) <- bs
  out
}

fit_fa <- function(x, bs, tcols) {
  rhs <- c(bs, paste0("o_", bs), "is_home", tcols)
  f <- as.formula(paste("scored ~", paste(rhs, collapse = "+")))
  co <- coef(lm(f, data = x))
  off <- co[bs];               names(off) <- bs   # own rating -> own score
  def <- -co[paste0("o_", bs)]; names(def) <- bs   # own rating -> opp score, sign-flipped
  list(off = off, def = def, total = off + def)
}

summ <- function(co) c(kdkf = unname(co[["KEY_DEFENDER"]] / co[["KEY_FORWARD"]]),
                       miscal = unname(mean(abs(co - 1))))
summ3 <- function(co) c(kdkf = unname(co[["DEF"]] / co[["FWD"]]),
                        miscal = unname(mean(abs(co - 1))))

# --- point estimates --------------------------------------------------------
p_M0 <- fit_margin(MG$mm, MG$bs)
p_M1 <- fit_margin(MG$mm, MG$bs, MG$tcols)
p_M2 <- fit_margin(MG3$mm, MG3$bs, MG3$tcols)
p_M3 <- fit_margin(MG$mm, MG$bs, ridge_l = 5)
p_FA <- fit_fa(FA$x, FA$bs, FA$tcols)

cat("=== A1: does the margin coefficient decompose into contribution + suppression? ===\n")
dec <- data.table(bucket = FA$bs, contribution = round(p_FA$off, 2),
                  suppression = round(p_FA$def, 2), total = round(p_FA$total, 2),
                  margin_M0 = round(p_M0[FA$bs], 2))
dec[, supp_share := round(suppression / total, 2)]
print(dec)
cat("\n  (total should track margin_M0 -- if it does, the decomposition is a clean\n",
    "  split of the SAME quantity, not a different one)\n")

# --- bootstrap --------------------------------------------------------------
set.seed(20260727)
nb <- length(match_ids)
# precomputed match_id -> row indices for the two-row design (avoids an O(n^2)
# lookup inside the bootstrap loop)
fa_idx <- split(seq_len(nrow(FA$x)), FA$x$match_id)[as.character(match_ids)]
bs_out <- list(M0 = matrix(NA_real_, NBOOT, 2), M1 = matrix(NA_real_, NBOOT, 2),
               M2 = matrix(NA_real_, NBOOT, 2), M3 = matrix(NA_real_, NBOOT, 2),
               FA = matrix(NA_real_, NBOOT, 2))
for (i in seq_len(NBOOT)) {
  idx <- sample.int(nb, nb, replace = TRUE)
  try({
    bs_out$M0[i, ] <- summ(fit_margin(MG$mm[idx], MG$bs))
    bs_out$M1[i, ] <- summ(fit_margin(MG$mm[idx], MG$bs, MG$tcols))
    bs_out$M2[i, ] <- summ3(fit_margin(MG3$mm[idx], MG3$bs, MG3$tcols))
    bs_out$M3[i, ] <- summ(fit_margin(MG$mm[idx], MG$bs, ridge_l = 5))
    bs_out$FA[i, ] <- summ(fit_fa(FA$x[unlist(fa_idx[idx], use.names = FALSE)],
                                  FA$bs, FA$tcols)$total)
  }, silent = TRUE)
}
ciw <- function(m, j) { q <- quantile(m[, j], c(.025,.975), na.rm = TRUE); c(q, width = q[2]-q[1]) }

cat("\n=== A2: which estimator is sharpest? (match-level bootstrap, NBOOT =", NBOOT, ") ===\n")
rows <- list(
  c("M0 margin, 6 buckets [ROUND 1's GATE]", summ(p_M0)),
  c("M1 margin, 6 buckets + team FE",        summ(p_M1)),
  c("M2 margin, 3 buckets + team FE",        summ3(p_M2)),
  c("M3 margin, 6 buckets, ridge(5)",        summ(p_M3)),
  c("FA for/against total + team FE",        summ(p_FA$total)))
nm <- names(bs_out)
for (k in seq_along(nm)) {
  s <- as.numeric(rows[[k]][2:3])
  a <- ciw(bs_out[[nm[k]]], 1); b <- ciw(bs_out[[nm[k]]], 2)
  cat(sprintf("%-38s KD/KF %5.2f [%6.2f,%6.2f] w=%5.2f | mean|b-1| %.3f [%.3f,%.3f] w=%.3f\n",
              rows[[k]][1], s[1], a[1], a[2], a[3], s[2], b[1], b[2], b[3]))
}

# --- coverage simulation: does the sharpest estimator actually cover truth? --
# Generate margins from KNOWN bucket coefficients using the real design matrix,
# with residual sd matched to the data. An estimator that is merely narrow but
# biased must be rejected here, not adopted for being narrow.
cat("\n=== A2 coverage check (truth: KD=1.6, KF=1.0, others=1.0) ===\n")
true_b <- c(KEY_DEFENDER = 1.6, MEDIUM_DEFENDER = 1.0, MIDFIELDER = 1.0,
            RUCK = 1.0, MEDIUM_FORWARD = 1.0, KEY_FORWARD = 1.0)
X <- as.matrix(MG$mm[, paste0("d_", MG$bs), with = FALSE])
sig <- sd(residuals(lm(as.formula(paste("margin ~", paste0("d_", MG$bs, collapse = "+"))),
                       data = MG$mm)))
sim <- copy(MG$mm); sim3 <- copy(MG3$mm)
cov_hit <- c(M0 = 0, M1 = 0, M2 = 0); est <- list(M0 = c(), M1 = c(), M2 = c())
for (i in seq_len(NSIM)) {
  y <- as.numeric(X %*% true_b[MG$bs]) + rnorm(nrow(X), 0, sig)
  sim[, margin := y]; sim3[, margin := y]
  e0 <- fit_margin(sim, MG$bs); e1 <- fit_margin(sim, MG$bs, MG$tcols)
  e2 <- fit_margin(sim3, MG3$bs, MG3$tcols)
  est$M0 <- c(est$M0, e0[["KEY_DEFENDER"]] / e0[["KEY_FORWARD"]])
  est$M1 <- c(est$M1, e1[["KEY_DEFENDER"]] / e1[["KEY_FORWARD"]])
  est$M2 <- c(est$M2, e2[["DEF"]] / e2[["FWD"]])
}
for (k in names(est)) {
  v <- est[[k]]
  cat(sprintf("  %s  median KD/KF %5.2f   IQR width %5.2f   sd %5.2f\n",
              k, median(v, na.rm=TRUE), IQR(v, na.rm=TRUE), sd(v, na.rm=TRUE)))
}
cat("  (truth is 1.60 for M0/M1; M2's DEF/FWD is a different estimand -- compare\n",
    "   its dispersion, not its level)\n")
