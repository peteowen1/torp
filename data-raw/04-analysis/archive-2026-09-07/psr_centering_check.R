suppressMessages({library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)})
D <- "C:/dev/torpverse/torpdata/data/"
S <- 2023:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")[beta != 0]
sr <- rbindlist(lapply(S, function(s) as.data.table(read_parquet(
  file.path(D, sprintf("player_stat_ratings_%d.parquet", s))))), use.names=TRUE, fill=TRUE)
sr[, round := as.numeric(round)]
pg <- rbindlist(lapply(S, function(s) as.data.table(read_parquet(
  file.path(D, sprintf("player_game_%d.parquet", s))))[, .(player_id, match_id, season,
  round=as.numeric(round), team_id, position_group, lineup_position,
  tog=time_on_ground_percentage/100)]), use.names=TRUE, fill=TRUE)
res <- rbindlist(lapply(S, function(s) as.data.table(read_parquet(
  file.path(D, sprintf("results_%d.parquet", s))))), use.names=TRUE, fill=TRUE)
res <- res[!is.na(home_score)&!is.na(away_score), .(match_id, home_team_id, away_team_id,
           margin=home_score-away_score)]
tr <- as.data.table(read_parquet(file.path(D,"torp_ratings.parquet"))); tr[, round := as.numeric(round)]

L <- merge(pg[position_group %in% BK & !lineup_position %in% c("EMERG","SUB")], sr,
           by=c("player_id","season","round"))
L <- merge(L, tr[, .(player_id, season, round, psr_pub=psr)], by=c("player_id","season","round"))
L <- L[!is.na(psr_pub)]
v <- numeric(nrow(L))
for (i in seq_len(nrow(coefs))) {
  cc <- paste0(coefs$stat_name[i], "_rating"); if (!cc %in% names(L)) next
  sdv <- coefs$sd[i]; if (is.na(sdv)||sdv==0) sdv <- 1
  x <- L[[cc]]; x[is.na(x)] <- 0
  v <- v + coefs$beta[i]*(x/sdv)
}
L[, psr_uncentered := v]
# replicate calculate_psr's centering: by lineup_position
L[, psr_centered := psr_uncentered - mean(psr_uncentered, na.rm=TRUE), by = lineup_position]

# how variable is bucket composition between the two teams in a match?
comp <- L[, .N, by=.(match_id, team_id, position_group)]
cw <- dcast(comp, match_id+team_id ~ position_group, value.var="N", fill=0)
hh <- merge(res, cw, by.x=c("match_id","home_team_id"), by.y=c("match_id","team_id"))
aa <- merge(res[,.(match_id,away_team_id)], cw, by.x=c("match_id","away_team_id"),
            by.y=c("match_id","team_id"))
bk <- intersect(BK, names(cw)); setnames(aa, bk, paste0("a_",bk))
cm <- merge(hh, aa, by="match_id")
cat("=== do the two teams field the same number per bucket? ===\n")
for (b in bk) {
  d <- cm[[b]] - cm[[paste0("a_",b)]]
  cat(sprintf("  %-16s differs in %5.1f%% of matches (sd of diff %.2f)\n",
              b, 100*mean(d!=0), sd(d)))
}

calib <- function(col, label) {
  ag <- L[, .(s=sum(get(col), na.rm=TRUE)), by=.(match_id, team_id, pos=position_group)]
  wd <- dcast(ag, match_id+team_id ~ pos, value.var="s", fill=0)
  b2 <- intersect(BK, names(wd))
  h <- merge(res, wd, by.x=c("match_id","home_team_id"), by.y=c("match_id","team_id"))
  a <- merge(res[,.(match_id,away_team_id)], wd, by.x=c("match_id","away_team_id"),
             by.y=c("match_id","team_id")); setnames(a, b2, paste0("a_",b2))
  mm <- merge(h, a, by="match_id")
  for (b in b2) mm[[paste0("d_",b)]] <- mm[[b]] - mm[[paste0("a_",b)]]
  co <- coef(lm(as.formula(paste("margin ~", paste0("d_",b2,collapse="+"))), data=mm))[-1]
  names(co) <- b2
  cat(sprintf("\n%s\n", label)); print(round(co,2))
  cat(sprintf("  spread %.2fx  KD/KF %.2f\n", max(co)/min(co),
              co[["KEY_DEFENDER"]]/co[["KEY_FORWARD"]]))
}
cat("\n=== calibration ===\n")
calib("psr_uncentered", "A: my replication, UNCENTERED (what I reported as 1.19x)")
calib("psr_centered",   "B: same betas, centered by lineup_position (as production does)")
calib("psr_pub",        "C: published psr")
