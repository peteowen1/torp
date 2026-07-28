# Block 2, candidate 1 (plan §7.7) — score the PSR centring-taxonomy fix on the
# FULL window under the §7.11 standing rules.
#
# CANDIDATE: honour calculate_psr()'s already-documented preference for 20-way
# lineup_position. Production cannot today, because the frame it is fed
# (load_player_stat_ratings(TRUE)) carries only 6-way pos_group. The fix is
# plumbing -- carry lineup_position into that frame -- not a change to psr.R.
#
# Standing rules applied (§7.11):
#   - full 2021-2026 window, never a sub-window
#   - mean|beta - 1| and KD/KF, both with PAIRED bootstrap CIs
#   - no team-strength control (it destroys identification, §7.8c)
#   - face-validity read before any ship recommendation (§1.4 gate)
#
# FALLBACK DESIGN DECISION, made explicit because it is a real modelling choice:
# not every player-round has an on-field lineup_position (INT/SUB/EMERG, and
# missing team sheets). calculate_psr() currently falls back to a GLOBAL mean
# for such rows, which leaves their positional level structure in. The candidate
# uses a hierarchical fallback -- lineup_position, else pos_group, else global --
# which is strictly closer to the intent than either endpoint.
suppressMessages({library(arrow); library(data.table)})

D  <- "C:/dev/torpverse/torpdata/data/"
SF <- 2021:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
NBOOT <- 2000

coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")[beta != 0]
rd <- function(pat, ss) rbindlist(lapply(ss, function(s)
  as.data.table(read_parquet(file.path(D, sprintf(pat, s))))), use.names=TRUE, fill=TRUE)

sr <- rd("player_stat_ratings_%d.parquet", SF); sr[, round := as.numeric(round)]
v <- numeric(nrow(sr))
for (i in seq_len(nrow(coefs))) {
  cc <- paste0(coefs$stat_name[i], "_rating"); if (!cc %in% names(sr)) next
  sdv <- coefs$sd[i]; if (is.na(sdv) || sdv == 0) sdv <- 1
  x <- sr[[cc]]; x[is.na(x)] <- 0
  v <- v + coefs$beta[i] * (x / sdv)
}
sr[, psr_raw := v]

pg <- rd("player_game_%d.parquet", SF)[, .(player_id, match_id, season,
        round = as.numeric(round), team_id, position_group, lineup_position)]
res <- rd("results_%d.parquet", SF)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]

wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) NA_real_ else weighted.mean(x[ok], w[ok]) }

# join the 20-way role (absent from the production frame -- that is the defect)
lp <- unique(pg[!is.na(lineup_position) & !lineup_position %in% c("EMERG","SUB"),
                .(player_id, season, round, lineup_position)],
             by = c("player_id","season","round"))
sr <- merge(sr, lp, by = c("player_id","season","round"), all.x = TRUE)
cat(sprintf("lineup_position coverage in the stat-ratings frame: %.1f%% of %s rows\n",
            100*mean(!is.na(sr$lineup_position)), format(nrow(sr), big.mark=",")))

# ARM C's taxonomy: the 20-way lineup codes collapsed to 9 roles. Left/right
# symmetry (BPL/BPR, HFFL/HFFR, WL/WR) is meaningless for rating purposes and
# only halves the sample behind each centring mean, so it is pooled. Role
# distinctions the 6-way taxonomy loses (key back vs pocket vs half-back) are
# kept. This is the "collapsed 8-10 role taxonomy" O1 leans toward.
ROLE9 <- c(FB="KEY_BACK", CHB="KEY_BACK",
           BPL="POCKET_BACK", BPR="POCKET_BACK",
           HBFL="HALF_BACK", HBFR="HALF_BACK",
           C="CENTRE_WING", WL="CENTRE_WING", WR="CENTRE_WING",
           R="ROVER", RR="ROVER", RK="RUCK",
           CHF="KEY_FWD", FF="KEY_FWD",
           FPL="POCKET_FWD", FPR="POCKET_FWD",
           HFFL="HALF_FWD", HFFR="HALF_FWD")
sr[, role9 := unname(ROLE9[lineup_position])]

# ARM A -- production: 6-way pos_group, wt_80s-weighted
sr[, psr_prod := psr_raw - wmean(psr_raw, wt_80s), by = pos_group]
# ARM B -- candidate: raw 20-way, hierarchical fallback to 6-way then global
sr[, psr_cand := NA_real_]
sr[!is.na(lineup_position), psr_cand := psr_raw - wmean(psr_raw, wt_80s), by = lineup_position]
sr[is.na(lineup_position), psr_cand := psr_raw - wmean(psr_raw, wt_80s), by = pos_group]
sr[is.na(psr_cand), psr_cand := psr_raw - wmean(psr_raw, wt_80s)]
# ARM C -- revised candidate: 9-way collapsed role, same fallback chain
sr[, psr_role := NA_real_]
sr[!is.na(role9), psr_role := psr_raw - wmean(psr_raw, wt_80s), by = role9]
sr[is.na(role9),  psr_role := psr_raw - wmean(psr_raw, wt_80s), by = pos_group]
sr[is.na(psr_role), psr_role := psr_raw - wmean(psr_raw, wt_80s)]

L <- merge(pg[position_group %in% BK & !lineup_position %in% c("EMERG","SUB")],
           sr[, .(player_id, season, round, psr_prod, psr_cand, psr_role)],
           by = c("player_id","season","round"))
cat(sprintf("scoring rows: %s over %s matches\n",
            format(nrow(L), big.mark=","), format(uniqueN(L$match_id), big.mark=",")))

mk <- function(col) {
  ag <- L[, .(s = sum(get(col), na.rm=TRUE)), by = .(match_id, team_id, pos = position_group)]
  wd <- dcast(ag, match_id + team_id ~ pos, value.var = "s", fill = 0)
  b2 <- intersect(BK, names(wd))
  h <- merge(res, wd, by.x=c("match_id","home_team_id"), by.y=c("match_id","team_id"))
  a <- merge(res[, .(match_id, away_team_id)], wd,
             by.x=c("match_id","away_team_id"), by.y=c("match_id","team_id"))
  setnames(a, b2, paste0("a_", b2)); a[, away_team_id := NULL]
  mm <- merge(h, a, by = "match_id")
  for (b in b2) mm[[paste0("d_", b)]] <- mm[[b]] - mm[[paste0("a_", b)]]
  setorder(mm, match_id); mm
}
A <- mk("psr_prod"); B <- mk("psr_cand"); C <- mk("psr_role")
stopifnot(identical(A$match_id, B$match_id), identical(A$match_id, C$match_id))
b2 <- intersect(BK, sub("^d_","", grep("^d_", names(A), value=TRUE)))
fml <- as.formula(paste("margin ~", paste0("d_", b2, collapse="+")))

st <- function(mm, idx=NULL) {
  co <- coef(lm(fml, data = if (is.null(idx)) mm else mm[idx]))[-1]; names(co) <- b2
  c(kdkf = unname(co[["KEY_DEFENDER"]]/co[["KEY_FORWARD"]]),
    mis  = unname(mean(abs(co - 1))),
    kd = unname(co[["KEY_DEFENDER"]]), kf = unname(co[["KEY_FORWARD"]]))
}
pA <- st(A); pB <- st(B); pC <- st(C); n <- nrow(A)
bs <- matrix(NA_real_, NBOOT, 6,
             dimnames=list(NULL, c("kA","kB","kC","mA","mB","mC")))
set.seed(20260727)
for (i in seq_len(NBOOT)) {
  idx <- sample.int(n, n, replace=TRUE)
  sa <- tryCatch(st(A, idx), error=function(e) NULL)
  sb <- tryCatch(st(B, idx), error=function(e) NULL)
  sc <- tryCatch(st(C, idx), error=function(e) NULL)
  if (is.null(sa) || is.null(sb) || is.null(sc)) next
  bs[i, ] <- c(sa["kdkf"], sb["kdkf"], sc["kdkf"], sa["mis"], sb["mis"], sc["mis"])
}
q <- function(x) quantile(x, c(.025,.975), na.rm=TRUE)

cat(sprintf("\n=== FULL WINDOW %d-%d, %d matches, NBOOT=%d ===\n",
            min(SF), max(SF), n, NBOOT))
for (z in list(list("A production 6-way   ", pA, "kA","mA"),
               list("B candidate 20-way   ", pB, "kB","mB"),
               list("C revised 9-way role ", pC, "kC","mC"))) {
  cat(sprintf("%s KD/KF %.2f [%.2f, %.2f]   mean|b-1| %.3f [%.3f, %.3f]\n",
              z[[1]], z[[2]]["kdkf"], q(bs[,z[[3]]])[1], q(bs[,z[[3]]])[2],
              z[[2]]["mis"], q(bs[,z[[4]]])[1], q(bs[,z[[4]]])[2]))
}
for (z in list(list("B vs A (raw 20-way)", "kB","mB", pB),
               list("C vs A (9-way role)", "kC","mC", pC))) {
  dk <- bs[,z[[2]]]-bs[,"kA"]; dm <- bs[,z[[3]]]-bs[,"mA"]
  cat(sprintf("\nPAIRED %s  d KD/KF %+.3f [%+.3f, %+.3f]  P(toward 1) = %.3f\n",
              z[[1]], z[[4]]["kdkf"]-pA["kdkf"], q(dk)[1], q(dk)[2],
              mean(abs(bs[,z[[2]]]-1) < abs(bs[,"kA"]-1), na.rm=TRUE)))
  cat(sprintf("       %s  d mean|b-1| %+.3f [%+.3f, %+.3f]  P(improves) = %.3f\n",
              strrep(" ", nchar(z[[1]])), z[[4]]["mis"]-pA["mis"],
              q(dm)[1], q(dm)[2], mean(dm < 0, na.rm=TRUE)))
}
cat(sprintf("\nKD coef %.2f -> B %.2f -> C %.2f    KF coef %.2f -> B %.2f -> C %.2f\n",
            pA["kd"], pB["kd"], pC["kd"], pA["kf"], pB["kf"], pC["kf"]))

# --- §1.4 face-validity gate ------------------------------------------------
cat("\n=== FACE VALIDITY (§1.4 gate): top key defenders, latest full season ===\n")
sn <- max(sr$season[sr$season < max(sr$season)], na.rm = TRUE)
nm <- unique(sr[, .(player_id, player_name)], by = "player_id")
fv <- L[season == sn & position_group == "KEY_DEFENDER"]
fv <- fv[, .(prod = mean(psr_prod, na.rm=TRUE), cand = mean(psr_cand, na.rm=TRUE),
             role = mean(psr_role, na.rm=TRUE),
             rounds = .N, n_lp = uniqueN(lineup_position)), by = player_id][rounds >= 10]
fv <- merge(fv, nm, by = "player_id", all.x = TRUE)
fv[, `:=`(rank_prod = frank(-prod), rank_cand = frank(-cand), rank_role = frank(-role))]
cat(sprintf("season %d, %d key defenders with >=10 rounds\n", sn, nrow(fv)))
cat("n_lp = how many distinct lineup positions he was named in that season\n\n")
print(head(fv[order(-cand), .(player_name, n_lp,
                              prod = round(prod,2), cand = round(cand,2), role = round(role,2),
                              r_prod = as.integer(rank_prod), r_cand = as.integer(rank_cand),
                              r_role = as.integer(rank_role))], 12))
cat(sprintf("\nSpearman vs production:  raw 20-way %.3f   9-way role %.3f\n",
            cor(fv$rank_prod, fv$rank_cand, method="spearman"),
            cor(fv$rank_prod, fv$rank_role, method="spearman")))
cat("A low value means the arm reorders who the best key defenders are.\n")
# Does the raw 20-way arm reward positional churn? Correlate a player's rank
# IMPROVEMENT with how many different positions he was named in.
fv[, gain_cand := rank_prod - rank_cand][, gain_role := rank_prod - rank_role]
cat(sprintf("\ncor(rank gain, n distinct lineup positions):  raw 20-way %+.3f   9-way role %+.3f\n",
            cor(fv$gain_cand, fv$n_lp), cor(fv$gain_role, fv$n_lp)))
cat("Positive = the arm promotes players who moved around, which is positional\n")
cat("churn being rewarded as if it were skill -- a face-validity failure, not a finding.\n")
