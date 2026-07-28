# Block 2 re-score (plan §7.15) — score the centring candidate against a CLEAN
# baseline and with the two effects SEPARATED.
#
# §7.12's comparison is superseded on two counts: it used a hand-built 9-way
# grouping (§7.14's clustering gives a better, symmetric one) and it conflated
# two distinct changes. The candidate alters:
#     (i)  TEMPORAL RESOLUTION -- production centres on pos_group, which is
#          derived from PBP position_group and varies in only 0.6% of
#          player-seasons (i.e. season-constant). lineup_position is a weekly
#          team-sheet role and varies for 77.8% of player-seasons. Switching
#          source therefore starts tracking in-season role changes, quite apart
#          from any change in granularity.
#     (ii) GRANULARITY -- 6 buckets to 9.
# Attributing a gain to "finer buckets" without separating (i) is exactly the
# mistake §7.8 made in the other direction. Three arms isolate them:
#     A  production  : 6-way, season-constant, PBP-derived   [baseline]
#     B  corrected-6 : 6-way, WEEKLY, from lineup_position   [isolates (i)]
#     C  role-9      : 9-way, WEEKLY, from lineup_position   [adds (ii)]
#
# The corrected 6-way map is symmetric, per §7.14's clustering: the extreme
# posts (FB, FF) are singletons and the centre-half codes group with their
# pockets. That is a CHANGE from §7.13's proposed fix, which had CHF joining FF
# as a key forward -- the clustering evidence supersedes the height/label
# reasoning that produced it.
#
# The outcome-side buckets (which players count as KEY_DEFENDER / KEY_FORWARD
# for the KD/KF ratio) are held FIXED on PBP position_group across all arms, so
# only the centring taxonomy varies and the comparison stays apples-to-apples.
suppressMessages({library(arrow); library(data.table)})

D  <- "C:/dev/torpverse/torpdata/data/"
SF <- 2021:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
NBOOT <- 2000

# corrected 6-way, symmetric (§7.14d)
FIX6 <- c(FB="KEY_DEFENDER",
          CHB="MEDIUM_DEFENDER", BPL="MEDIUM_DEFENDER", BPR="MEDIUM_DEFENDER",
          HBFL="MEDIUM_DEFENDER", HBFR="MEDIUM_DEFENDER",
          C="MIDFIELDER", WL="MIDFIELDER", WR="MIDFIELDER", R="MIDFIELDER", RR="MIDFIELDER",
          RK="RUCK",
          FF="KEY_FORWARD",
          CHF="MEDIUM_FORWARD", FPL="MEDIUM_FORWARD", FPR="MEDIUM_FORWARD",
          HFFL="MEDIUM_FORWARD", HFFR="MEDIUM_FORWARD")

# ARM D -- Pete's preference (2026-07-27): centre-half back and centre-half
# forward are KEY posts, grouped with FB and FF respectively. He flagged this as
# a reservation he could not argue without his own analysis, so it is scored
# here as a first-class arm rather than left as an opinion. This is the single
# most consequential remaining taxonomy choice -- see the face-validity output.
PETE6 <- c(FB="KEY_DEFENDER", CHB="KEY_DEFENDER",
           BPL="MEDIUM_DEFENDER", BPR="MEDIUM_DEFENDER",
           HBFL="MEDIUM_DEFENDER", HBFR="MEDIUM_DEFENDER",
           C="MIDFIELDER", WL="MIDFIELDER", WR="MIDFIELDER", R="MIDFIELDER", RR="MIDFIELDER",
           RK="RUCK",
           FF="KEY_FORWARD", CHF="KEY_FORWARD",
           FPL="MEDIUM_FORWARD", FPR="MEDIUM_FORWARD",
           HFFL="MEDIUM_FORWARD", HFFR="MEDIUM_FORWARD")

# data-driven 9-way (§7.14c clustering, keeping Pete's INSIDE_MID/WING split
# which the clustering reproduced exactly)
ROLE9 <- c(FB="FULL_BACK",
           CHB="TALL_BACK", BPL="TALL_BACK", BPR="TALL_BACK",
           HBFL="HALF_BACK", HBFR="HALF_BACK",
           WL="WING", WR="WING",
           C="INSIDE_MID", R="INSIDE_MID", RR="INSIDE_MID",
           RK="RUCK",
           FF="FULL_FORWARD",
           CHF="TALL_FORWARD", FPL="TALL_FORWARD", FPR="TALL_FORWARD",
           HFFL="HALF_FORWARD", HFFR="HALF_FORWARD")

rd <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(D, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names = TRUE, fill = TRUE)

coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")[beta != 0]
sr <- rd("player_stat_ratings_%d.parquet", SF); sr[, round := as.numeric(round)]
v <- numeric(nrow(sr))
for (i in seq_len(nrow(coefs))) {
  cc <- paste0(coefs$stat_name[i], "_rating"); if (!cc %in% names(sr)) next
  sdv <- coefs$sd[i]; if (is.na(sdv) || sdv == 0) sdv <- 1
  x <- sr[[cc]]; x[is.na(x)] <- 0; v <- v + coefs$beta[i]*(x/sdv)
}
sr[, psr_raw := v]

pg <- rd("player_game_%d.parquet", SF)[, .(player_id, match_id, season,
        round = as.numeric(round), team_id, position_group, lineup_position)]
res <- rd("results_%d.parquet", SF)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]

lp <- unique(pg[!is.na(lineup_position) & !lineup_position %in% c("INT","EMERG","SUB"),
                .(player_id, season, round, lineup_position)],
             by = c("player_id","season","round"))
sr <- merge(sr, lp, by = c("player_id","season","round"), all.x = TRUE)
sr[, `:=`(fix6 = unname(FIX6[lineup_position]), role9 = unname(ROLE9[lineup_position]),
          pete6 = unname(PETE6[lineup_position]))]

wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) NA_real_ else weighted.mean(x[ok], w[ok]) }

# ARM A -- production
sr[, psr_A := psr_raw - wmean(psr_raw, wt_80s), by = pos_group]
# ARM B -- corrected 6-way, weekly (fallback to pos_group where no team sheet)
sr[, psr_B := NA_real_]
sr[!is.na(fix6), psr_B := psr_raw - wmean(psr_raw, wt_80s), by = fix6]
sr[is.na(fix6),  psr_B := psr_raw - wmean(psr_raw, wt_80s), by = pos_group]
# ARM C -- data-driven 9-way, weekly, same fallback
sr[, psr_C := NA_real_]
sr[!is.na(role9), psr_C := psr_raw - wmean(psr_raw, wt_80s), by = role9]
sr[is.na(role9),  psr_C := psr_raw - wmean(psr_raw, wt_80s), by = pos_group]
# ARM D -- Pete's centre-halves-are-key variant, weekly, same fallback
sr[, psr_D := NA_real_]
sr[!is.na(pete6), psr_D := psr_raw - wmean(psr_raw, wt_80s), by = pete6]
sr[is.na(pete6),  psr_D := psr_raw - wmean(psr_raw, wt_80s), by = pos_group]

L <- merge(pg[position_group %in% BK & !lineup_position %in% c("EMERG","SUB")],
           sr[, .(player_id, season, round, psr_A, psr_B, psr_C, psr_D)],
           by = c("player_id","season","round"))
cat(sprintf("scoring rows: %s\n", format(nrow(L), big.mark=",")))

mk <- function(col) {
  ag <- L[, .(s = sum(get(col), na.rm=TRUE)), by = .(match_id, team_id, pos = position_group)]
  wd <- dcast(ag, match_id + team_id ~ pos, value.var = "s", fill = 0)
  b2 <- intersect(BK, names(wd))
  h <- merge(res, wd, by.x=c("match_id","home_team_id"), by.y=c("match_id","team_id"))
  a <- merge(res[, .(match_id, away_team_id)], wd,
             by.x=c("match_id","away_team_id"), by.y=c("match_id","team_id"))
  setnames(a, b2, paste0("a_", b2)); a[, away_team_id := NULL]
  mm <- merge(h, a, by="match_id")
  for (b in b2) mm[[paste0("d_", b)]] <- mm[[b]] - mm[[paste0("a_", b)]]
  setorder(mm, match_id); mm
}
A <- mk("psr_A"); B <- mk("psr_B"); C <- mk("psr_C"); Dm <- mk("psr_D")
stopifnot(identical(A$match_id, B$match_id), identical(A$match_id, C$match_id),
          identical(A$match_id, Dm$match_id))
b2 <- intersect(BK, sub("^d_","", grep("^d_", names(A), value=TRUE)))
fml <- as.formula(paste("margin ~", paste0("d_", b2, collapse="+")))
st <- function(mm, idx=NULL) {
  co <- coef(lm(fml, data = if (is.null(idx)) mm else mm[idx]))[-1]; names(co) <- b2
  c(kdkf = unname(co[["KEY_DEFENDER"]]/co[["KEY_FORWARD"]]),
    mis  = unname(mean(abs(co-1))), kd = unname(co[["KEY_DEFENDER"]]),
    kf = unname(co[["KEY_FORWARD"]]))
}
pA <- st(A); pB <- st(B); pC <- st(C); pD <- st(Dm); n <- nrow(A)
bs <- matrix(NA_real_, NBOOT, 8,
             dimnames=list(NULL, c("kA","kB","kC","kD","mA","mB","mC","mD")))
set.seed(20260727)
for (i in seq_len(NBOOT)) {
  ix <- sample.int(n, n, replace=TRUE)
  sa <- tryCatch(st(A,ix), error=function(e) NULL); sb <- tryCatch(st(B,ix), error=function(e) NULL)
  sc <- tryCatch(st(C,ix), error=function(e) NULL); sd <- tryCatch(st(Dm,ix), error=function(e) NULL)
  if (is.null(sa)||is.null(sb)||is.null(sc)||is.null(sd)) next
  bs[i,] <- c(sa["kdkf"], sb["kdkf"], sc["kdkf"], sd["kdkf"],
              sa["mis"], sb["mis"], sc["mis"], sd["mis"])
}
q <- function(x) quantile(x, c(.025,.975), na.rm=TRUE)
cat(sprintf("\n=== FULL WINDOW %d-%d, %d matches, NBOOT=%d ===\n", min(SF), max(SF), n, NBOOT))
for (z in list(list("A production 6-way, season-const", pA,"kA","mA"),
               list("B corrected 6-way, WEEKLY      ", pB,"kB","mB"),
               list("C data-driven 9-way, WEEKLY    ", pC,"kC","mC"),
               list("D Pete: centre-halves are KEY  ", pD,"kD","mD")))
  cat(sprintf("%s  KD/KF %.2f [%.2f, %.2f]   mean|b-1| %.3f [%.3f, %.3f]\n", z[[1]],
              z[[2]]["kdkf"], q(bs[,z[[3]]])[1], q(bs[,z[[3]]])[2],
              z[[2]]["mis"], q(bs[,z[[4]]])[1], q(bs[,z[[4]]])[2]))

cat("\n--- effect decomposition, paired ---\n")
dm_B <- bs[,"mB"]-bs[,"mA"]; dm_C <- bs[,"mC"]-bs[,"mA"]; dm_CB <- bs[,"mC"]-bs[,"mB"]
cat(sprintf("(i)  weekly resolution   B-A : d mean|b-1| %+.3f [%+.3f, %+.3f]  P(improves) %.3f\n",
            pB["mis"]-pA["mis"], q(dm_B)[1], q(dm_B)[2], mean(dm_B<0, na.rm=TRUE)))
cat(sprintf("(ii) + finer buckets     C-B : d mean|b-1| %+.3f [%+.3f, %+.3f]  P(improves) %.3f\n",
            pC["mis"]-pB["mis"], q(dm_CB)[1], q(dm_CB)[2], mean(dm_CB<0, na.rm=TRUE)))
cat(sprintf("     combined            C-A : d mean|b-1| %+.3f [%+.3f, %+.3f]  P(improves) %.3f\n",
            pC["mis"]-pA["mis"], q(dm_C)[1], q(dm_C)[2], mean(dm_C<0, na.rm=TRUE)))
dk_C <- bs[,"kC"]-bs[,"kA"]
cat(sprintf("\n     KD/KF C-A : %+.3f [%+.3f, %+.3f]  P(toward 1) %.3f\n",
            pC["kdkf"]-pA["kdkf"], q(dk_C)[1], q(dk_C)[2],
            mean(abs(bs[,"kC"]-1) < abs(bs[,"kA"]-1), na.rm=TRUE)))
cat(sprintf("     KD coef %.2f -> B %.2f -> C %.2f   KF %.2f -> B %.2f -> C %.2f\n",
            pA["kd"],pB["kd"],pC["kd"], pA["kf"],pB["kf"],pC["kf"]))

# --- face validity ----------------------------------------------------------
cat("\n=== FACE VALIDITY (§1.4): 2025 key defenders, >=10 rounds ===\n")
nm <- unique(sr[, .(player_id, player_name)], by="player_id")
fv <- L[season==2025 & position_group=="KEY_DEFENDER",
        .(A=mean(psr_A,na.rm=TRUE), B=mean(psr_B,na.rm=TRUE), C=mean(psr_C,na.rm=TRUE),
          D=mean(psr_D,na.rm=TRUE), rounds=.N), by=player_id][rounds>=10]
fv <- merge(fv, nm, by="player_id", all.x=TRUE)
fv[, `:=`(rA=frank(-A), rB=frank(-B), rC=frank(-C), rD=frank(-D))]
print(head(fv[order(-B), .(player_name, prod=round(A,2), corr6=round(B,2), pete=round(D,2),
                           r_prod=as.integer(rA), r_corr6=as.integer(rB),
                           r_pete=as.integer(rD))], 12))
cat(sprintf("\nSpearman vs production:  corrected-6 %.3f   role-9 %.3f   Pete-6 %.3f\n",
            cor(fv$rA, fv$rB, method="spearman"), cor(fv$rA, fv$rC, method="spearman"),
            cor(fv$rA, fv$rD, method="spearman")))
cat("\n--- the CHB question, isolated: who moves between arm B and arm D? ---\n")
fv[, shift := rB - rD]   # positive = Pete's arm ranks him HIGHER
print(head(fv[order(-abs(shift)), .(player_name, r_corr6=as.integer(rB),
                                    r_pete=as.integer(rD), shift=as.integer(shift))], 10))
