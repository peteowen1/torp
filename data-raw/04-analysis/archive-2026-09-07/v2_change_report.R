# Plan §7.23 — v1 -> v2 change report.
#
# The artifact a promotion decision needs (RATING-VERSIONING-PLAN.md §2.4 step
# 2: "soak — compare, review face validity, update the explainer prose"). This
# does NOT publish anything. It answers, from local data: if v2 were promoted
# tomorrow, exactly what would change?
#
# v2 = the adopted set (torp 46df81c / 81365d7):
#   - EPV position adjustment RESCALES as well as recentres, on recv/disp/spoil
#     (hitout excluded -- it is ruck-exclusive and rescaling it manufactures
#     outliers, §7.18)
#   - PSR centred on the WEEKLY 6-way lineup role instead of the season-constant
#     pos_group (§7.15)
#   - corrected lineup taxonomy: CHF -> KEY_FORWARD, FPL/FPR -> MEDIUM_FORWARD,
#     CHB grouped with FB (§7.13/§7.14)
suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DD <- "C:/dev/torpverse/torpdata/data/"
SEASONS <- 2021:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
CH <- c("recv","disp","spoil","hitout")
DECAY  <- c(recv=EPR_DECAY_RECV, disp=EPR_DECAY_DISP, spoil=EPR_DECAY_SPOIL, hitout=EPR_DECAY_HITOUT)
PGAMES <- c(recv=EPR_PRIOR_GAMES_RECV, disp=EPR_PRIOR_GAMES_DISP,
            spoil=EPR_PRIOR_GAMES_SPOIL, hitout=EPR_PRIOR_GAMES_HITOUT)
PRATE  <- c(recv=EPR_PRIOR_RATE_RECV, disp=EPR_PRIOR_RATE_DISP,
            spoil=EPR_PRIOR_RATE_SPOIL, hitout=EPR_PRIOR_RATE_HITOUT)
rdp <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(DD, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names=TRUE, fill=TRUE)
wmean <- function(x,w) sum(x*w, na.rm=TRUE)/sum(w[!is.na(x)], na.rm=TRUE)
wsd <- function(x,w){ m <- wmean(x,w); sqrt(sum(w*(x-m)^2, na.rm=TRUE)/sum(w[!is.na(x)], na.rm=TRUE)) }

pg <- rdp("player_game_%d.parquet", SEASONS)
pg[, round := as.numeric(round)]
pg[, tog_safe := pmax(data.table::fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]

# ---- EPR under both vintages ------------------------------------------------
for (ch in CH) {
  raw <- paste0(ch,"_epv"); if (!raw %in% names(pg)) next
  p80 <- paste0(".p80_",ch); pg[, (p80) := get(raw)/tog_safe]
  pg[, .m := wmean(get(p80), tog_safe), by=lineup_position]
  pg[, .s := wsd(get(p80), tog_safe),   by=lineup_position]
  S <- wsd(pg[[p80]], pg$tog_safe)
  pg[, (paste0("v1_",ch)) := (get(p80) - .m) * tog_safe]
  pg[, (paste0("v2_",ch)) := if (ch %in% EPV_STANDARDISE_CHANNELS)
       (get(p80) - .m) * pmin(S/pmax(.s,1e-6), 1e9) * tog_safe
     else (get(p80) - .m) * tog_safe]
  pg[, c(".m",".s") := NULL]
}
setorder(pg, player_id, utc_start_time); pg[, .date := as.Date(utc_start_time)]
run_decay <- function(x, dates, lam) {
  n <- length(x); out <- numeric(n); s <- 0; prev <- dates[1]
  for (i in seq_len(n)) { s <- s*exp(-as.numeric(dates[i]-prev)/lam); prev <- dates[i]
    out[i] <- s; s <- s + x[i] }
  out
}
for (ch in CH) {
  lam <- DECAY[[ch]]
  pg[, .den := run_decay(tog_safe, .date, lam), by=player_id]
  for (v in c("v1","v2")) {
    pg[, .S := run_decay(get(paste0(v,"_",ch))*tog_safe, .date, lam), by=player_id]
    pg[, (paste0("epr_",v,"_",ch)) :=
         (EPR_LOADING_DEFAULT*.S + PGAMES[[ch]]*PRATE[[ch]])/(.den + PGAMES[[ch]])]
  }
  pg[, c(".den",".S") := NULL]
}
for (v in c("v1","v2")) pg[, (paste0("epr_",v)) := rowSums(.SD), .SDcols=paste0("epr_",v,"_",CH)]

# ---- PSR under both vintages ------------------------------------------------
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
sr[, lpg := unname(LINEUP_POSITION_GROUP_MAP[lineup_position])]
sr[, psr_v1 := psr_raw - wmean(psr_raw, wt_80s), by=pos_group]
sr[, psr_v2 := NA_real_]
sr[!is.na(lpg), psr_v2 := psr_raw - wmean(psr_raw, wt_80s), by=lpg]
sr[is.na(lpg),  psr_v2 := psr_raw - wmean(psr_raw, wt_80s), by=pos_group]
# PSR_POSITION_STANDARDISE: rescale within the SAME group used for centring,
# with the degenerate-SD fallback. Omitting this is what made the first
# implementation differ from the arm that was actually scored (§7.24).
if (isTRUE(PSR_POSITION_STANDARDISE)) {
  pooled_psr <- wsd(sr$psr_v2, sr$wt_80s)
  sr[!is.na(lpg), .g_sd := wsd(psr_v2, wt_80s), by = lpg]
  sr[!is.na(lpg) & !is.na(.g_sd) & .g_sd > 1e-6,
     psr_v2 := psr_v2 / .g_sd * pooled_psr]
  sr[, .g_sd := NULL]
}

L <- merge(pg[position_group %in% BK, .(player_id, season, round, player_name,
                                        pos=position_group, epr_v1, epr_v2)],
           sr[, .(player_id, season, round, psr_v1, psr_v2)],
           by=c("player_id","season","round"))
L <- L[!is.na(psr_v1) & !is.na(psr_v2)]
EW <- TORP_EPR_WEIGHT
L[, `:=`(torp_v1 = EW*epr_v1 + (1-EW)*psr_v1,
         torp_v2 = EW*epr_v2 + (1-EW)*psr_v2)]
L[, delta := torp_v2 - torp_v1]

sep <- function(t) cat("\n", strrep("=", 70), "\n", t, "\n", strrep("=", 70), "\n", sep="")
sep("1. HEADLINE: how much moves, and how much of it matters")
cat(sprintf("player-rounds compared: %s\n", format(nrow(L), big.mark=",")))
cat(sprintf("correlation(v1, v2)   : %.4f\n", cor(L$torp_v1, L$torp_v2)))
cat(sprintf("mean |delta|          : %.3f TORP points\n", mean(abs(L$delta))))
cat(sprintf("90th pct |delta|      : %.3f | max |delta| : %.3f\n",
            quantile(abs(L$delta), .9), max(abs(L$delta))))
cat(sprintf("share moving > 0.5 pts: %.1f%%  | > 1.0 pts: %.1f%%\n",
            100*mean(abs(L$delta) > 0.5), 100*mean(abs(L$delta) > 1)))

sep("2. BY POSITION — who gains, who loses")
print(L[, .(n = .N,
            mean_v1 = round(mean(torp_v1),3), mean_v2 = round(mean(torp_v2),3),
            mean_delta = round(mean(delta),3),
            sd_v1 = round(sd(torp_v1),3), sd_v2 = round(sd(torp_v2),3)),
        by = pos][order(-mean_delta)])

sep("3. END-OF-SEASON RATINGS — the numbers people actually quote")
E <- L[order(player_id, season, round)][, .SD[.N], by=.(player_id, season)]
print(E[, .(n=.N, sd_v1=round(sd(torp_v1),2), sd_v2=round(sd(torp_v2),2),
            max_v1=round(max(torp_v1),2), max_v2=round(max(torp_v2),2)),
        by=pos][order(-max_v1)])
kf <- function(col) max(E[pos=="KEY_FORWARD"][[col]])/max(E[pos=="KEY_DEFENDER"][[col]])
cat(sprintf("\nbest key forward / best key defender: %.2fx -> %.2fx\n",
            kf("torp_v1"), kf("torp_v2")))

sep("4. BIGGEST MOVERS (end-of-season, |delta| ranked)")
E[, d := torp_v2 - torp_v1]
print(head(E[order(-abs(d)), .(player_name, season, pos,
        v1=round(torp_v1,2), v2=round(torp_v2,2), delta=round(d,2))], 15))

sep("5. RANK STABILITY — does the leaderboard reorder?")
for (s in c(2025, 2024)) {
  e <- E[season == s]
  cat(sprintf("  %d overall Spearman %.4f | top-20 overlap %d/20 | key-def Spearman %.4f\n",
              s, cor(frank(-e$torp_v1), frank(-e$torp_v2), method="spearman"),
              length(intersect(head(e[order(-torp_v1)]$player_id, 20),
                               head(e[order(-torp_v2)]$player_id, 20))),
              cor(frank(-e[pos=="KEY_DEFENDER"]$torp_v1),
                  frank(-e[pos=="KEY_DEFENDER"]$torp_v2), method="spearman")))
}

sep("6. PROSE CHECK — figures the TORP explainer may quote")
cat("  best key-defender season:\n")
print(head(E[pos=="KEY_DEFENDER"][order(-torp_v2),
             .(player_name, season, v1=round(torp_v1,2), v2=round(torp_v2,2))], 5))
cat("\n  best season overall:\n")
print(head(E[order(-torp_v2), .(player_name, season, pos,
                                v1=round(torp_v1,2), v2=round(torp_v2,2))], 5))
