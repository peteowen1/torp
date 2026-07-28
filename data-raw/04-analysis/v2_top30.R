# Current top 30 players under v1 (published) and v2 (adopted, unpublished).
# Latest available round of the most recent season.
suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
DD <- "C:/dev/torpverse/torpdata/data/"; SEASONS <- 2021:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
CH <- c("recv","disp","spoil","hitout")
DECAY <- c(recv=EPR_DECAY_RECV, disp=EPR_DECAY_DISP, spoil=EPR_DECAY_SPOIL, hitout=EPR_DECAY_HITOUT)
PGAMES <- c(recv=EPR_PRIOR_GAMES_RECV, disp=EPR_PRIOR_GAMES_DISP,
            spoil=EPR_PRIOR_GAMES_SPOIL, hitout=EPR_PRIOR_GAMES_HITOUT)
PRATE <- c(recv=EPR_PRIOR_RATE_RECV, disp=EPR_PRIOR_RATE_DISP,
           spoil=EPR_PRIOR_RATE_SPOIL, hitout=EPR_PRIOR_RATE_HITOUT)
rdp <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(DD, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names=TRUE, fill=TRUE)
wmean <- function(x,w) sum(x*w, na.rm=TRUE)/sum(w[!is.na(x)], na.rm=TRUE)
wsd <- function(x,w){ m <- wmean(x,w); sqrt(sum(w*(x-m)^2, na.rm=TRUE)/sum(w[!is.na(x)], na.rm=TRUE)) }

pg <- rdp("player_game_%d.parquet", SEASONS); pg[, round := as.numeric(round)]
pg[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage),100,
                              time_on_ground_percentage)/100, 0.1)]
for (ch in CH) {
  raw <- paste0(ch,"_epv"); p80 <- paste0(".p80_",ch); pg[, (p80) := get(raw)/tog_safe]
  pg[, .m := wmean(get(p80), tog_safe), by=lineup_position]
  pg[, .s := wsd(get(p80), tog_safe),   by=lineup_position]
  S <- wsd(pg[[p80]], pg$tog_safe)
  pg[, (paste0("v1_",ch)) := (get(p80) - .m) * tog_safe]
  pg[, (paste0("v2_",ch)) := if (ch %in% EPV_STANDARDISE_CHANNELS)
       (get(p80) - .m) * pmin(S/pmax(.s,1e-6),1e9) * tog_safe else (get(p80) - .m) * tog_safe]
  pg[, c(".m",".s") := NULL]
}
setorder(pg, player_id, utc_start_time); pg[, .date := as.Date(utc_start_time)]
rdc <- function(x, dates, lam) { n <- length(x); out <- numeric(n); s <- 0; prev <- dates[1]
  for (i in seq_len(n)) { s <- s*exp(-as.numeric(dates[i]-prev)/lam); prev <- dates[i]
    out[i] <- s; s <- s + x[i] }; out }
for (ch in CH) { lam <- DECAY[[ch]]
  pg[, .den := rdc(tog_safe, .date, lam), by=player_id]
  for (v in c("v1","v2")) { pg[, .S := rdc(get(paste0(v,"_",ch))*tog_safe, .date, lam), by=player_id]
    pg[, (paste0("epr_",v,"_",ch)) := (EPR_LOADING_DEFAULT*.S + PGAMES[[ch]]*PRATE[[ch]])/(.den+PGAMES[[ch]])] }
  pg[, c(".den",".S") := NULL] }
for (v in c("v1","v2")) pg[, (paste0("epr_",v)) := rowSums(.SD), .SDcols=paste0("epr_",v,"_",CH)]

coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")[beta != 0]
sr <- rdp("player_stat_ratings_%d.parquet", SEASONS); sr[, round := as.numeric(round)]
v <- numeric(nrow(sr))
for (i in seq_len(nrow(coefs))) { cc <- paste0(coefs$stat_name[i],"_rating")
  if (!cc %in% names(sr)) next; sdv <- coefs$sd[i]; if (is.na(sdv)||sdv==0) sdv <- 1
  x <- sr[[cc]]; x[is.na(x)] <- 0; v <- v + coefs$beta[i]*(x/sdv) }
sr[, psr_raw := v]
lp <- unique(pg[, .(player_id, season, round, lineup_position)], by=c("player_id","season","round"))
sr <- merge(sr, lp, by=c("player_id","season","round"), all.x=TRUE)
sr[, lpg := unname(LINEUP_POSITION_GROUP_MAP[lineup_position])]
sr[, psr_v1 := psr_raw - wmean(psr_raw, wt_80s), by=pos_group]
sr[, psr_v2 := NA_real_]
sr[!is.na(lpg), psr_v2 := psr_raw - wmean(psr_raw, wt_80s), by=lpg]
sr[is.na(lpg),  psr_v2 := psr_raw - wmean(psr_raw, wt_80s), by=pos_group]
if (isTRUE(PSR_POSITION_STANDARDISE)) {
  pooled <- wsd(sr$psr_v2, sr$wt_80s)
  sr[!is.na(lpg), .gsd := wsd(psr_v2, wt_80s), by=lpg]
  sr[!is.na(lpg) & !is.na(.gsd) & .gsd > 1e-6, psr_v2 := psr_v2/.gsd*pooled]
  sr[, .gsd := NULL]
}

L <- merge(pg[, .(player_id, season, round, player_name, pos = position_group,
                  lineup_position, epr_v1, epr_v2)],
           sr[, .(player_id, season, round, psr_v1, psr_v2)],
           by = c("player_id","season","round"))
L <- L[!is.na(psr_v1) & !is.na(psr_v2)]
EW <- TORP_EPR_WEIGHT
L[, `:=`(torp_v1 = EW*epr_v1 + (1-EW)*psr_v1, torp_v2 = EW*epr_v2 + (1-EW)*psr_v2)]

# latest rating per player in the most recent season
S_LAST <- max(L$season)
cur <- L[season == S_LAST][order(player_id, round)][, .SD[.N], by = player_id]
cat(sprintf("season %d, latest round %d, %d players\n\n",
            S_LAST, max(cur$round), nrow(cur)))

cur[, `:=`(r1 = frank(-torp_v1), r2 = frank(-torp_v2))]
cat("=== TOP 30 under v2 (adopted) ===\n")
print(head(cur[order(-torp_v2), .(rank = as.integer(r2), player_name,
        position = pos, lineup = lineup_position,
        torp_v2 = round(torp_v2,2), torp_v1 = round(torp_v1,2),
        v1_rank = as.integer(r1))], 30))

cat("\n=== TOP 30 under v1 (currently published), for comparison ===\n")
print(head(cur[order(-torp_v1), .(rank = as.integer(r1), player_name,
        position = pos, torp_v1 = round(torp_v1,2),
        torp_v2 = round(torp_v2,2), v2_rank = as.integer(r2))], 30))

cat("\n=== position mix of the top 30 ===\n")
mix <- merge(head(cur[order(-torp_v1)], 30)[, .(v1 = .N), by = pos],
             head(cur[order(-torp_v2)], 30)[, .(v2 = .N), by = pos],
             by = "pos", all = TRUE)
mix[is.na(v1), v1 := 0L][is.na(v2), v2 := 0L]
print(mix[order(-v2)])
cat(sprintf("\ntop-30 overlap between vintages: %d of 30\n",
            length(intersect(head(cur[order(-torp_v1)], 30)$player_id,
                             head(cur[order(-torp_v2)], 30)$player_id))))
