# Plan §7.17 — "can we make the same improvements for EPR?" (Pete, 2026-07-27)
#
# SHORT ANSWER: EPR already has the improvement PSR was missing.
# create_player_game_data() step 7 (player_credit.R:326) centres every EPV
# channel with group_by(lineup_position) -- the WEEKLY 20-way team-sheet role.
# PSR was the laggard, centring on a season-constant 6-way label. So the
# §7.15 fix does not transfer; EPR was never behind on that axis.
#
# But §7.15 also found that GRANULARITY contributes nothing (6-way beat 9-way
# marginally). EPR sits at 20-way, finer than the 6 that proved sufficient for
# PSR. So the analogous question for EPR runs the OTHER WAY: is its centring
# too fine, and is it paying a noise cost for buckets nobody needed?
#
# And EPR has a defect PSR cannot have. group_by(lineup_position) is applied
# with NO filtering, so INT, SUB and EMERG each become their own centring
# bucket. An interchange player is centred against other interchange players --
# a pool mixing every position on the ground. PSR never had this problem
# because pos_group is a player-level classification with no bench category.
#
# THREE ARMS (mirroring §7.15's decomposition discipline):
#   A  current   : 20-way weekly, INT/SUB/EMERG as their own buckets  [baseline]
#   B  int-fix   : 20-way weekly, bench rows fall back to position_group
#   C  coarse    : 6-way weekly, bench rows fall back to position_group
# A->B isolates the bench-pooling defect; B->C isolates granularity.
suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
BUCKETS  <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK",
              "MEDIUM_FORWARD","KEY_FORWARD")
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

pg <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", s))))),
  use.names=TRUE, fill=TRUE)
pg[, round := as.numeric(round)]
pg[, tog_safe := pmax(data.table::fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]

# ---- diagnostic: how bad is the bench bucket? ------------------------------
cat("=== EPR's centring buckets: how much of the data is bench? ===\n")
bench <- c("INT","SUB","EMERG")
cat(sprintf("rows: %s | bench-coded: %s (%.1f%%)\n",
            format(nrow(pg), big.mark=","),
            format(sum(pg$lineup_position %in% bench), big.mark=","),
            100*mean(pg$lineup_position %in% bench)))
cat("\nposition_group composition INSIDE the INT centring bucket:\n")
print(pg[lineup_position=="INT" & position_group %in% BUCKETS,
         .(n=.N, pct=round(100*.N/sum(pg$lineup_position=="INT" &
                                      pg$position_group %in% BUCKETS),1)),
         by=position_group][order(-n)])
cat("\n-> every position is pooled into one centring group. A key defender named\n")
cat("   on the bench is centred against midfielders and forwards.\n")

# ---- three centring keys ---------------------------------------------------
pg[, key_A := lineup_position]
pg[, key_B := fifelse(lineup_position %in% bench | is.na(lineup_position),
                      position_group, lineup_position)]
pg[, m6 := unname(PETE6[lineup_position])]
pg[, key_C := fifelse(is.na(m6), position_group, m6)]

wmean <- function(x,w) sum(x*w, na.rm=TRUE)/sum(w[!is.na(x)], na.rm=TRUE)

for (ch in CH) {
  raw <- paste0(ch,"_epv"); if (!raw %in% names(pg)) { cat("missing",raw,"\n"); next }
  p80 <- paste0(".p80_",ch); pg[, (p80) := get(raw)/tog_safe]
  for (arm in c("A","B","C")) {
    kk <- paste0("key_",arm)
    pg[, .mtmp := wmean(get(p80), tog_safe), by = c(kk)]
    pg[, (paste0("adj",arm,"_",ch)) := (get(p80) - .mtmp) * tog_safe]
  }
  pg[, .mtmp := NULL]
}

setorder(pg, player_id, utc_start_time)
pg[, .date := as.Date(utc_start_time)]
run_decay <- function(x, dates, lam) {
  n <- length(x); out <- numeric(n); s <- 0; prev <- dates[1]
  for (i in seq_len(n)) {
    s <- s*exp(-as.numeric(dates[i]-prev)/lam); prev <- dates[i]
    out[i] <- s; s <- s + x[i]
  }
  out
}
cat("\nAggregating (3 arms x 4 channels)...\n")
for (ch in CH) {
  lam <- DECAY[[ch]]
  pg[, .den := run_decay(tog_safe, .date, lam), by=player_id]
  den <- pg$.den + PGAMES[[ch]]
  for (arm in c("A","B","C")) {
    pg[, .S := run_decay(get(paste0("adj",arm,"_",ch))*tog_safe, .date, lam), by=player_id]
    pg[, (paste0("epr",arm,"_",ch)) :=
         (EPR_LOADING_DEFAULT*.S + PGAMES[[ch]]*PRATE[[ch]])/den]
  }
  pg[, c(".den",".S") := NULL]
}
for (arm in c("A","B","C"))
  pg[, (paste0("epr_",arm)) := rowSums(.SD), .SDcols=paste0("epr",arm,"_",CH)]

tr <- as.data.table(read_parquet(file.path(DATA_DIR,"torp_ratings.parquet")))
tr[, round := as.numeric(round)]
L <- merge(pg[position_group %in% BUCKETS,
              .(player_id, match_id, season, round, team_id, pos=position_group,
                tog_safe, epr_A, epr_B, epr_C)],
           tr[, .(player_id, season, round, epr_pub=epr, psr_pub=psr, torp_pub=torp)],
           by=c("player_id","season","round"))
L <- L[!is.na(torp_pub)]
cat(sprintf("\nreconstruction check: cor(arm A, published EPR) = %.4f  (%s rows)\n",
            cor(L$epr_A, L$epr_pub, use="complete.obs"), format(nrow(L), big.mark=",")))
cat("  production also applies an opponent adjustment, so exact equality is not\n")
cat("  expected; a high correlation means the rebuild reproduces the live path.\n")

res <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
  use.names=TRUE, fill=TRUE)
res <- res[!is.na(home_score)&!is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin=home_score-away_score)]

EW <- TORP_EPR_WEIGHT
for (arm in c("A","B","C")) L[, (paste0("torp_",arm)) := EW*get(paste0("epr_",arm)) + (1-EW)*psr_pub]

mk <- function(col) {
  ag <- L[, .(s=sum(get(col), na.rm=TRUE)), by=.(match_id, team_id, pos)]
  wd <- dcast(ag, match_id+team_id ~ pos, value.var="s", fill=0)
  b2 <- intersect(BUCKETS, names(wd))
  h <- merge(res, wd, by.x=c("match_id","home_team_id"), by.y=c("match_id","team_id"))
  a <- merge(res[,.(match_id, away_team_id)], wd,
             by.x=c("match_id","away_team_id"), by.y=c("match_id","team_id"))
  setnames(a, b2, paste0("a_",b2)); a[, away_team_id := NULL]
  mm <- merge(h, a, by="match_id")
  for (b in b2) mm[[paste0("d_",b)]] <- mm[[b]] - mm[[paste0("a_",b)]]
  setorder(mm, match_id); mm
}
MA <- mk("torp_A"); MB <- mk("torp_B"); MC <- mk("torp_C")
b2 <- intersect(BUCKETS, sub("^d_","",grep("^d_", names(MA), value=TRUE)))
fml <- as.formula(paste("margin ~", paste0("d_",b2,collapse="+")))
st <- function(mm, ix=NULL) {
  co <- coef(lm(fml, data=if(is.null(ix)) mm else mm[ix]))[-1]; names(co) <- b2
  c(kdkf=unname(co[["KEY_DEFENDER"]]/co[["KEY_FORWARD"]]), mis=unname(mean(abs(co-1))))
}
pA <- st(MA); pB <- st(MB); pC <- st(MC); n <- nrow(MA)
bs <- matrix(NA_real_, NBOOT, 6, dimnames=list(NULL,c("kA","kB","kC","mA","mB","mC")))
set.seed(20260727)
for (i in seq_len(NBOOT)) {
  ix <- sample.int(n,n,replace=TRUE)
  sa<-tryCatch(st(MA,ix),error=function(e)NULL); sb<-tryCatch(st(MB,ix),error=function(e)NULL)
  sc<-tryCatch(st(MC,ix),error=function(e)NULL)
  if (is.null(sa)||is.null(sb)||is.null(sc)) next
  bs[i,] <- c(sa["kdkf"],sb["kdkf"],sc["kdkf"],sa["mis"],sb["mis"],sc["mis"])
}
q <- function(x) quantile(x,c(.025,.975),na.rm=TRUE)
cat(sprintf("\n=== TORP calibration, %d matches, NBOOT=%d ===\n", n, NBOOT))
for (z in list(list("A current  20-way, bench pooled",pA,"kA","mA"),
               list("B int-fix  20-way, bench->PBP  ",pB,"kB","mB"),
               list("C coarse    6-way, bench->PBP  ",pC,"kC","mC")))
  cat(sprintf("%s  KD/KF %.2f [%.2f, %.2f]  mean|b-1| %.3f [%.3f, %.3f]\n", z[[1]],
              z[[2]]["kdkf"], q(bs[,z[[3]]])[1], q(bs[,z[[3]]])[2],
              z[[2]]["mis"], q(bs[,z[[4]]])[1], q(bs[,z[[4]]])[2]))
dBA <- bs[,"mB"]-bs[,"mA"]; dCB <- bs[,"mC"]-bs[,"mB"]; dCA <- bs[,"mC"]-bs[,"mA"]
cat(sprintf("\n(i)  bench fix   B-A : d mean|b-1| %+.3f [%+.3f, %+.3f]  P(improves) %.3f\n",
            pB["mis"]-pA["mis"], q(dBA)[1], q(dBA)[2], mean(dBA<0,na.rm=TRUE)))
cat(sprintf("(ii) + coarser   C-B : d mean|b-1| %+.3f [%+.3f, %+.3f]  P(improves) %.3f\n",
            pC["mis"]-pB["mis"], q(dCB)[1], q(dCB)[2], mean(dCB<0,na.rm=TRUE)))
cat(sprintf("     combined    C-A : d mean|b-1| %+.3f [%+.3f, %+.3f]  P(improves) %.3f\n",
            pC["mis"]-pA["mis"], q(dCA)[1], q(dCA)[2], mean(dCA<0,na.rm=TRUE)))

cat("\n=== key-defender top end (end-of-season TORP) ===\n")
E <- L[order(player_id, season, round)][, .SD[.N], by=.(player_id, season)]
print(E[, .(n=.N, sd=round(sd(torp_A,na.rm=TRUE),2),
            sd_C=round(sd(torp_C,na.rm=TRUE),2),
            max=round(max(torp_A,na.rm=TRUE),2),
            max_C=round(max(torp_C,na.rm=TRUE),2)), by=pos][order(-max_C)])
