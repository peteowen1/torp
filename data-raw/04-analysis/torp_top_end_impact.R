# Block 2 (plan §7.16) — what do top-end TORP ratings actually become for key
# defenders under the adopted centring change?
#
# Everything so far has been calibration coefficients. This translates it into
# the thing Pete actually reads: the leaderboard.
#
# IMPORTANT SCOPE. Only ONE change is applied here -- PSR centred on the weekly
# team-sheet role instead of a season-constant label (§7.15 arm D, Pete's
# centre-halves-are-key variant, chosen because §7.15b showed it is free).
# It enters TORP at HALF weight (torp = 0.5*epr + 0.5*psr, TORP_EPR_WEIGHT).
# NOT applied: the §6.12 EPV position-variance standardisation (that is the
# Problem-B/ceiling lever and is still an unadopted candidate), and the §7.10
# D50 disposal candidate (unscored). So this is a lower bound on what the
# program could move, not the finished article.
suppressMessages({library(arrow); library(data.table)})

D  <- "C:/dev/torpverse/torpdata/data/"
SF <- 2021:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
rd <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(D, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names = TRUE, fill = TRUE)

PETE6 <- c(FB="KEY_DEFENDER", CHB="KEY_DEFENDER",
           BPL="MEDIUM_DEFENDER", BPR="MEDIUM_DEFENDER",
           HBFL="MEDIUM_DEFENDER", HBFR="MEDIUM_DEFENDER",
           C="MIDFIELDER", WL="MIDFIELDER", WR="MIDFIELDER", R="MIDFIELDER", RR="MIDFIELDER",
           RK="RUCK", FF="KEY_FORWARD", CHF="KEY_FORWARD",
           FPL="MEDIUM_FORWARD", FPR="MEDIUM_FORWARD",
           HFFL="MEDIUM_FORWARD", HFFR="MEDIUM_FORWARD")

tr <- as.data.table(read_parquet(file.path(D, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]
cat("torp_ratings cols:", paste(intersect(c("torp","epr","psr","player_name","season","round"),
                                          names(tr)), collapse=", "), "\n")
# verify the published blend is what we think before rebuilding it
chk <- tr[!is.na(torp) & !is.na(epr) & !is.na(psr)]
cat(sprintf("check torp == 0.5*epr + 0.5*psr : max abs deviation %.4f on %s rows\n",
            max(abs(chk$torp - (0.5*chk$epr + 0.5*chk$psr))), format(nrow(chk), big.mark=",")))

# --- rebuild psr under the candidate centring -------------------------------
coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")[beta != 0]
sr <- rd("player_stat_ratings_%d.parquet", SF); sr[, round := as.numeric(round)]
v <- numeric(nrow(sr))
for (i in seq_len(nrow(coefs))) {
  cc <- paste0(coefs$stat_name[i], "_rating"); if (!cc %in% names(sr)) next
  sdv <- coefs$sd[i]; if (is.na(sdv) || sdv == 0) sdv <- 1
  x <- sr[[cc]]; x[is.na(x)] <- 0; v <- v + coefs$beta[i]*(x/sdv)
}
sr[, psr_raw := v]

pg <- rd("player_game_%d.parquet", SF)[, .(player_id, season, round = as.numeric(round),
        position_group, lineup_position)]
lp <- unique(pg[!is.na(lineup_position) & !lineup_position %in% c("INT","EMERG","SUB"),
                .(player_id, season, round, lineup_position)],
             by = c("player_id","season","round"))
sr <- merge(sr, lp, by = c("player_id","season","round"), all.x = TRUE)
sr[, pete6 := unname(PETE6[lineup_position])]
wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) NA_real_ else weighted.mean(x[ok], w[ok]) }
sr[, psr_new := NA_real_]
sr[!is.na(pete6), psr_new := psr_raw - wmean(psr_raw, wt_80s), by = pete6]
sr[is.na(pete6),  psr_new := psr_raw - wmean(psr_raw, wt_80s), by = pos_group]

# --- rebuild TORP -----------------------------------------------------------
X <- merge(tr[, .(player_id, player_name, season, round, epr, psr, torp)],
           sr[, .(player_id, season, round, psr_new)],
           by = c("player_id","season","round"))
X <- X[!is.na(epr) & !is.na(psr_new)]
X[, torp_new := 0.5*epr + 0.5*psr_new]

# season-modal bucket per player
bucket <- pg[position_group %in% BK, .N, by=.(player_id, season, position_group)]
bucket <- bucket[order(-N)][, .SD[1], by=.(player_id, season)][, .(player_id, season, position_group)]
X <- merge(X, bucket, by = c("player_id","season"))

# end-of-season rating = last round each player appears
E <- X[order(player_id, season, round)][, .SD[.N], by = .(player_id, season)]
cat(sprintf("\nplayer-seasons: %s\n", format(nrow(E), big.mark=",")))

sep <- function(t) cat("\n", strrep("=", 68), "\n", t, "\n", strrep("=", 68), "\n", sep="")

sep("1. TOP 15 KEY DEFENDERS, 2025 (end-of-season TORP)")
k25 <- E[season == 2025 & position_group == "KEY_DEFENDER"]
k25[, `:=`(r_old = frank(-torp), r_new = frank(-torp_new))]
print(head(k25[order(-torp_new), .(player_name,
        torp_old = round(torp,2), torp_new = round(torp_new,2),
        delta = round(torp_new - torp, 2),
        rank_old = as.integer(r_old), rank_new = as.integer(r_new))], 15))

sep("2. ALL-TIME TOP 15 KEY-DEFENDER SEASONS, 2021-2026")
kd <- E[position_group == "KEY_DEFENDER"]
print(head(kd[order(-torp_new), .(player_name, season,
        torp_old = round(torp,2), torp_new = round(torp_new,2),
        delta = round(torp_new - torp,2))], 15))
cat("\n-- for comparison, the same list under PUBLISHED torp --\n")
print(head(kd[order(-torp), .(player_name, season, torp_old = round(torp,2),
                              torp_new = round(torp_new,2))], 10))

sep("3. THE CEILING (Problem B): does the key-defender top end move?")
cmp <- E[, .(n = .N,
             sd_old = round(sd(torp, na.rm=TRUE),2), sd_new = round(sd(torp_new, na.rm=TRUE),2),
             max_old = round(max(torp, na.rm=TRUE),2), max_new = round(max(torp_new, na.rm=TRUE),2),
             mean_old = round(mean(torp, na.rm=TRUE),2), mean_new = round(mean(torp_new, na.rm=TRUE),2)),
         by = position_group][order(-max_new)]
print(cmp)
bk <- function(col) max(E[position_group=="KEY_DEFENDER"][[col]], na.rm=TRUE)
bf <- function(col) max(E[position_group=="KEY_FORWARD"][[col]], na.rm=TRUE)
ba <- function(col) max(E[[col]], na.rm=TRUE)
cat(sprintf("\nbest key forward / best key defender : %.2fx  ->  %.2fx\n",
            bf("torp")/bk("torp"), bf("torp_new")/bk("torp_new")))
cat(sprintf("best player overall / best key defender: %.2fx  ->  %.2fx\n",
            ba("torp")/bk("torp"), ba("torp_new")/bk("torp_new")))

sep("4. TOP 20 OVERALL: how many key defenders make it?")
for (s in c(2025, 2024)) {
  e <- E[season == s]
  o <- head(e[order(-torp)], 20); n <- head(e[order(-torp_new)], 20)
  cat(sprintf("\n%d  published top 20: %d key defenders  |  candidate top 20: %d\n",
              s, sum(o$position_group=="KEY_DEFENDER"), sum(n$position_group=="KEY_DEFENDER")))
  kdn <- n[position_group=="KEY_DEFENDER"]
  if (nrow(kdn)) print(kdn[, .(player_name, torp_new = round(torp_new,2),
                               rank = as.integer(frank(-n$torp_new))[match(player_id, n$player_id)])])
}
