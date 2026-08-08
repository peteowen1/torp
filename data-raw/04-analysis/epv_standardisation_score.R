# D-DEF1 (plan §7.18) — score the EPV position-variance standardisation under
# the rebuilt gate, full window, with face validity and top-end impact.
#
# THE CANDIDATE. create_player_game_data() step 7 subtracts a within-position
# mean and stops:  adj = (p80 - mean_pos) * tog.  Standardisation rescales too:
#     adj_std = (p80 - mean_pos) / sd_pos * S * tog
# with S the pooled weighted SD per channel, so overall units are preserved and
# only BETWEEN-position spread differences change. §6.12 found this is the only
# tested change that lifts key-defender SPREAD (the Problem-B/ceiling defect),
# while leaving calibration (Problem A) essentially unmoved.
#
# WHAT IT ASSUMES, STATED PLAINLY AND NOT WAVED THROUGH. Forcing equal
# within-position variance asserts that every position group *should* have the
# same spread of player value. The calibration gate implies it; football does
# not obviously. If key defenders genuinely vary less in impact than key
# forwards do, this change manufactures spread that is not there. That is a
# claim to defend in prose, and it is the reason D-DEF1 is Pete's call and not
# a CV score.
#
# FOUR ARMS (A is the baseline rebuild; D is what would actually ship):
#   A  current EPV centring        + published PSR
#   B  EPV STANDARDISED            + published PSR          <- D-DEF1 alone
#   C  EPV standardised            + PSR standardised
#   D  EPV standardised            + PSR weekly-centred AND standardised
#                                                           <- full package
suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DD <- "C:/dev/torpverse/torpdata/data/"
SEASONS <- 2021:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
CH <- c("recv","disp","spoil","hitout")
NBOOT <- 1000
DECAY  <- c(recv=EPR_DECAY_RECV, disp=EPR_DECAY_DISP, spoil=EPR_DECAY_SPOIL, hitout=EPR_DECAY_HITOUT)
PGAMES <- c(recv=EPR_PRIOR_GAMES_RECV, disp=EPR_PRIOR_GAMES_DISP,
            spoil=EPR_PRIOR_GAMES_SPOIL, hitout=EPR_PRIOR_GAMES_HITOUT)
PRATE  <- c(recv=EPR_PRIOR_RATE_RECV, disp=EPR_PRIOR_RATE_DISP,
            spoil=EPR_PRIOR_RATE_SPOIL, hitout=EPR_PRIOR_RATE_HITOUT)
PETE6 <- c(FB="KEY_DEFENDER", CHB="KEY_DEFENDER",
           BPL="MEDIUM_DEFENDER", BPR="MEDIUM_DEFENDER",
           HBFL="MEDIUM_DEFENDER", HBFR="MEDIUM_DEFENDER",
           C="MIDFIELDER", WL="MIDFIELDER", WR="MIDFIELDER", R="MIDFIELDER", RR="MIDFIELDER",
           RK="RUCK", FF="KEY_FORWARD", CHF="KEY_FORWARD",
           FPL="MEDIUM_FORWARD", FPR="MEDIUM_FORWARD",
           HFFL="MEDIUM_FORWARD", HFFR="MEDIUM_FORWARD")

rdp <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(DD, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names=TRUE, fill=TRUE)

wmean <- function(x,w) sum(x*w, na.rm=TRUE)/sum(w[!is.na(x)], na.rm=TRUE)
wsd <- function(x,w) { m <- wmean(x,w)
  sqrt(sum(w*(x-m)^2, na.rm=TRUE)/sum(w[!is.na(x)], na.rm=TRUE)) }

pg <- rdp("player_game_%d.parquet", SEASONS)
pg[, round := as.numeric(round)]
pg[, tog_safe := pmax(data.table::fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]

# GUARD (added 2026-07-27 after the absurdity check fired). Standardising by
# dividing by sd_pos is unsafe for a channel that is positionally EXCLUSIVE.
# `hitout` is ruck-only: non-ruck positions have near-zero hitout variance
# (sd 0.14-0.32) while the pooled S is 1.241, so the amplifier S/sd_pos runs
# 4-9x for outfield positions and 1,240,950x for EMERG (sd_pos = 0.000 on 4
# rows). That manufactures enormous ratings for any ruck named at an outfield
# post -- Mark Blicavs, named across 9 different lineup positions in 2025,
# went 1.12 -> 4.06 and into the overall top 10.
# Two guarded arms are scored alongside the naive one:
#   adjs  naive     : divide by sd_pos, all four channels        [§6.12 as written]
#   adjg  guarded   : amplifier capped at AMP_CAP
#   adjn  no-hitout : standardise recv/disp/spoil only
AMP_CAP <- 2.0
for (ch in CH) {
  raw <- paste0(ch,"_epv"); if (!raw %in% names(pg)) { cat("missing",raw,"\n"); next }
  p80 <- paste0(".p80_",ch); pg[, (p80) := get(raw)/tog_safe]
  pg[, .m := wmean(get(p80), tog_safe), by=lineup_position]
  pg[, .s := wsd(get(p80), tog_safe),   by=lineup_position]
  S <- wsd(pg[[p80]], pg$tog_safe)
  pg[, .amp  := pmin(S/pmax(.s, 1e-6), 1e9)]
  pg[, .ampg := pmin(.amp, AMP_CAP)]
  pg[, (paste0("adjc_",ch)) := (get(p80) - .m) * tog_safe]
  pg[, (paste0("adjs_",ch)) := (get(p80) - .m) * .amp  * tog_safe]
  pg[, (paste0("adjg_",ch)) := (get(p80) - .m) * .ampg * tog_safe]
  pg[, (paste0("adjn_",ch)) := if (ch == "hitout") (get(p80) - .m) * tog_safe
                               else (get(p80) - .m) * .amp * tog_safe]
  cat(sprintf("  %-7s pooled S %.3f | amplifier median %.2f max %.0f\n",
              ch, S, median(pg$.amp, na.rm=TRUE), max(pg$.amp, na.rm=TRUE)))
  pg[, c(".m",".s",".amp",".ampg") := NULL]
}

setorder(pg, player_id, utc_start_time); pg[, .date := as.Date(utc_start_time)]
run_decay <- function(x, dates, lam) {
  n <- length(x); out <- numeric(n); s <- 0; prev <- dates[1]
  for (i in seq_len(n)) { s <- s*exp(-as.numeric(dates[i]-prev)/lam); prev <- dates[i]
    out[i] <- s; s <- s + x[i] }
  out
}
cat("Aggregating EPR (2 arms x 4 channels)...\n")
for (ch in CH) {
  lam <- DECAY[[ch]]
  pg[, .den := run_decay(tog_safe, .date, lam), by=player_id]
  den <- pg$.den + PGAMES[[ch]]
  for (arm in c("adjc","adjs","adjg","adjn")) {
    pg[, .S := run_decay(get(paste0(arm,"_",ch))*tog_safe, .date, lam), by=player_id]
    pg[, (paste0("epr_",arm,"_",ch)) := (EPR_LOADING_DEFAULT*.S + PGAMES[[ch]]*PRATE[[ch]])/den]
  }
  pg[, c(".den",".S") := NULL]
}
pg[, epr_cur := rowSums(.SD), .SDcols=paste0("epr_adjc_",CH)]
pg[, epr_std := rowSums(.SD), .SDcols=paste0("epr_adjs_",CH)]
pg[, epr_grd := rowSums(.SD), .SDcols=paste0("epr_adjg_",CH)]
pg[, epr_noh := rowSums(.SD), .SDcols=paste0("epr_adjn_",CH)]

# ---- PSR variants ----------------------------------------------------------
coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")[beta != 0]
sr <- rdp("player_stat_ratings_%d.parquet", SEASONS); sr[, round := as.numeric(round)]
v <- numeric(nrow(sr))
for (i in seq_len(nrow(coefs))) {
  cc <- paste0(coefs$stat_name[i],"_rating"); if (!cc %in% names(sr)) next
  sdv <- coefs$sd[i]; if (is.na(sdv)||sdv==0) sdv <- 1
  x <- sr[[cc]]; x[is.na(x)] <- 0; v <- v + coefs$beta[i]*(x/sdv)
}
sr[, psr_raw := v]
lp <- unique(pg[, .(player_id, season, round, lineup_position)], by=c("player_id","season","round"))
sr <- merge(sr, lp, by=c("player_id","season","round"), all.x=TRUE)
sr[, pete6 := unname(PETE6[lineup_position])]
sr[, psr_wk := NA_real_]
sr[!is.na(pete6), psr_wk := psr_raw - wmean(psr_raw, wt_80s), by=pete6]
sr[is.na(pete6),  psr_wk := psr_raw - wmean(psr_raw, wt_80s), by=pos_group]

tr <- as.data.table(read_parquet(file.path(DD,"torp_ratings.parquet")))
tr[, round := as.numeric(round)]
L <- merge(pg[position_group %in% BK,
              .(player_id, match_id, season, round, team_id, pos=position_group,
                lineup_position, tog_safe, epr_cur, epr_std, epr_grd, epr_noh)],
           tr[, .(player_id, player_name, season, round, epr_pub=epr, psr_pub=psr, torp_pub=torp)],
           by=c("player_id","season","round"))
L <- merge(L, sr[, .(player_id, season, round, psr_wk)], by=c("player_id","season","round"))
L <- L[!is.na(torp_pub) & !is.na(psr_wk)]
cat(sprintf("reconstruction: cor(epr_cur, published epr) = %.4f  (%s rows)\n",
            cor(L$epr_cur, L$epr_pub, use="complete.obs"), format(nrow(L), big.mark=",")))

# standardise both PSR variants within lineup_position (mirrors §6.12)
L[, psr_std := psr_pub / pmax(wsd(psr_pub, tog_safe),1e-6) * wsd(L$psr_pub, L$tog_safe),
  by=lineup_position]
L[, psr_wk_std := psr_wk / pmax(wsd(psr_wk, tog_safe),1e-6) * wsd(L$psr_wk, L$tog_safe),
  by=lineup_position]

EW <- TORP_EPR_WEIGHT
L[, `:=`(torp_A = EW*epr_cur + (1-EW)*psr_pub,
         torp_B = EW*epr_std + (1-EW)*psr_pub,
         torp_C = EW*epr_std + (1-EW)*psr_std,
         torp_D = EW*epr_std + (1-EW)*psr_wk_std,
         torp_G = EW*epr_grd + (1-EW)*psr_wk_std,
         torp_N = EW*epr_noh + (1-EW)*psr_wk_std)]

res <- rdp("results_%d.parquet", SEASONS)
res <- res[!is.na(home_score)&!is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin=home_score-away_score)]
mk <- function(col) {
  ag <- L[, .(s=sum(get(col), na.rm=TRUE)), by=.(match_id, team_id, pos)]
  wd <- dcast(ag, match_id+team_id ~ pos, value.var="s", fill=0)
  b2 <- intersect(BK, names(wd))
  h <- merge(res, wd, by.x=c("match_id","home_team_id"), by.y=c("match_id","team_id"))
  a <- merge(res[,.(match_id, away_team_id)], wd,
             by.x=c("match_id","away_team_id"), by.y=c("match_id","team_id"))
  setnames(a, b2, paste0("a_",b2)); a[, away_team_id := NULL]
  mm <- merge(h, a, by="match_id")
  for (b in b2) mm[[paste0("d_",b)]] <- mm[[b]] - mm[[paste0("a_",b)]]
  setorder(mm, match_id); mm
}
M <- lapply(c("torp_A","torp_B","torp_C","torp_D","torp_G","torp_N"), mk)
names(M) <- c("A","B","C","D","G","N")
b2 <- intersect(BK, sub("^d_","",grep("^d_", names(M$A), value=TRUE)))
fml <- as.formula(paste("margin ~", paste0("d_",b2,collapse="+")))
st <- function(mm, ix=NULL) {
  co <- coef(lm(fml, data=if(is.null(ix)) mm else mm[ix]))[-1]; names(co) <- b2
  c(kdkf=unname(co[["KEY_DEFENDER"]]/co[["KEY_FORWARD"]]), mis=unname(mean(abs(co-1))))
}
pt <- lapply(M, st); n <- nrow(M$A)
bs <- array(NA_real_, c(NBOOT,6,2),
            dimnames=list(NULL,c("A","B","C","D","G","N"),c("kdkf","mis")))
set.seed(20260727)
for (i in seq_len(NBOOT)) {
  ix <- sample.int(n,n,replace=TRUE)
  ok <- TRUE; tmp <- list()
  for (a in names(M)) { r <- tryCatch(st(M[[a]],ix), error=function(e) NULL)
    if (is.null(r)) { ok <- FALSE; break }; tmp[[a]] <- r }
  if (!ok) next
  for (a in names(M)) bs[i,a,] <- c(tmp[[a]]["kdkf"], tmp[[a]]["mis"])
}
q <- function(x) quantile(x,c(.025,.975),na.rm=TRUE)
lab <- c(A="A current (baseline)             ", B="B EPV STANDARDISED naive [D-DEF1]",
         C="C EPV+PSR standardised          ", D="D full package, NAIVE std       ",
         G="G full package, amplifier<=2.0  ", N="N full package, no hitout std   ")
cat(sprintf("\n=== calibration, %d matches, NBOOT=%d ===\n", n, NBOOT))
for (a in names(M))
  cat(sprintf("%s KD/KF %.2f [%.2f, %.2f]  mean|b-1| %.3f [%.3f, %.3f]\n", lab[a],
              pt[[a]]["kdkf"], q(bs[,a,"kdkf"])[1], q(bs[,a,"kdkf"])[2],
              pt[[a]]["mis"],  q(bs[,a,"mis"])[1],  q(bs[,a,"mis"])[2]))
for (a in c("B","C","D","G","N")) {
  d <- bs[,a,"mis"]-bs[,"A","mis"]
  cat(sprintf("  paired %s-A : d mean|b-1| %+.3f [%+.3f, %+.3f]  P(improves) %.3f\n",
              a, pt[[a]]["mis"]-pt[["A"]]["mis"], q(d)[1], q(d)[2], mean(d<0,na.rm=TRUE)))
}

# ---- the ceiling -----------------------------------------------------------
E <- L[order(player_id, season, round)][, .SD[.N], by=.(player_id, season)]
cat("\n=== THE CEILING (end-of-season TORP, 2021-2026) ===\n")
cc <- E[, .(n=.N,
            sd_A=round(sd(torp_A,na.rm=TRUE),2), sd_B=round(sd(torp_B,na.rm=TRUE),2),
            sd_D=round(sd(torp_D,na.rm=TRUE),2), sd_G=round(sd(torp_G,na.rm=TRUE),2),
            sd_N=round(sd(torp_N,na.rm=TRUE),2),
            max_A=round(max(torp_A,na.rm=TRUE),2), max_B=round(max(torp_B,na.rm=TRUE),2),
            max_D=round(max(torp_D,na.rm=TRUE),2), max_G=round(max(torp_G,na.rm=TRUE),2),
            max_N=round(max(torp_N,na.rm=TRUE),2)), by=pos][order(-max_A)]
print(cc)
g <- function(col) max(E[pos=="KEY_FORWARD"][[col]],na.rm=TRUE)/max(E[pos=="KEY_DEFENDER"][[col]],na.rm=TRUE)
cat(sprintf("\nbest KF / best KD:  A %.2fx | B %.2fx | D %.2fx | G %.2fx | N %.2fx\n",
            g("torp_A"), g("torp_B"), g("torp_D"), g("torp_G"), g("torp_N")))
for (s in c(2025,2024)) {
  e <- E[season==s]
  cat(sprintf("%d key defenders in overall top 20:  A %d  ->  B %d  ->  D %d\n", s,
              sum(head(e[order(-torp_A)],20)$pos=="KEY_DEFENDER"),
              sum(head(e[order(-torp_B)],20)$pos=="KEY_DEFENDER"),
              sum(head(e[order(-torp_D)],20)$pos=="KEY_DEFENDER")))
}

cat("\n=== TOP 15 KEY DEFENDERS, 2025 ===\n")
k <- E[season==2025 & pos=="KEY_DEFENDER"]
k[, `:=`(rA=frank(-torp_A), rD=frank(-torp_D))]
print(head(k[order(-torp_D), .(player_name, torp_A=round(torp_A,2), torp_B=round(torp_B,2),
        torp_D=round(torp_D,2), rank_A=as.integer(rA), rank_D=as.integer(rD))], 15))

cat("\n=== FACE VALIDITY: best key-defender seasons under the full package ===\n")
print(head(E[pos=="KEY_DEFENDER"][order(-torp_D),
             .(player_name, season, torp_A=round(torp_A,2), torp_D=round(torp_D,2))], 12))
cat(sprintf("\nSpearman (2025 key defenders) A vs B %.3f | A vs D %.3f\n",
            cor(k$rA, frank(-k$torp_B), method="spearman"), cor(k$rA, k$rD, method="spearman")))
cat("\n=== absurdity check: does the guard fix the ruck blow-up? ===\n")
cat("-- top 10 under NAIVE standardisation (torp_D) --\n")
print(head(E[season==2025][order(-torp_D), .(player_name, pos, torp_A=round(torp_A,2),
        torp_D=round(torp_D,2), torp_G=round(torp_G,2), torp_N=round(torp_N,2))], 10))
cat("-- top 10 under GUARDED standardisation (torp_G, amplifier<=2) --\n")
print(head(E[season==2025][order(-torp_G), .(player_name, pos, torp_A=round(torp_A,2),
        torp_G=round(torp_G,2))], 10))
cat("-- the specific case that fired the check --\n")
print(E[season==2025 & grepl("Blicavs|Gawn|English", player_name),
        .(player_name, pos, torp_A=round(torp_A,2), torp_D=round(torp_D,2),
          torp_G=round(torp_G,2), torp_N=round(torp_N,2))])
