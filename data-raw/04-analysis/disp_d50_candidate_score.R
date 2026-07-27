# Plan §7.19 — score the D50-conditional disposal candidate (§7.10d).
#
# THE FINDING THAT MOTIVATES IT. A key defender's `disp` credit is -0.214/game
# and 86% of it forms inside his own defensive 50, where it is negative. The
# pos_team == -1 branch of the disposal formula is globally undifferentiated
# (1.10x KD/KF) but 8.18x conditioned on D50 -- the most defender-concentrated
# lever the program has found, ahead of intercept marks' 7x.
#
# THE CANDIDATE. Scale the negative (pos_team == -1) disposal credit for
# disposals inside the defending team's own 50:
#     epv_disp(s) = epv_disp - (1 - s) * D50NEG
# where D50NEG is that player-game's D50 negative-branch credit. s = 1 is
# current behaviour; s < 1 softens the penalty a defender takes for disposing
# under pressure in the one part of the ground he is paid to occupy.
#
# PHYSICAL BOUNDS, IMPOSED UP FRONT. s is constrained to [0, 1]. s > 1 would
# penalise D50 turnovers MORE than the EPV they actually cost, and s < 0 would
# pay a defender for turning the ball over in front of goal. §6.11 died because
# an optimiser was allowed outside the physical range and produced a stable-
# looking fit that was an artifact of two parameters trading off. The bound is
# not a tuning choice here, it is the definition of the quantity.
#
# GATE. Full 2021-2026 window, mean|b-1| and KD/KF with PAIRED bootstrap CIs,
# no team-strength control (§7.8c), plus the §1.4 face-validity read.
suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DD <- "C:/dev/torpverse/torpdata/data/"
SEASONS <- 2021:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
CH <- c("recv","disp","spoil","hitout")
GRID <- c(1.00, 0.75, 0.50, 0.25, 0.00)
NBOOT <- 800
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

# ---- 1. D50 negative-branch component per player-game, from PBP ------------
PB <- c("match_id","player_id","description","disposal","delta_epv","pos_team",
        "x","goal_x","contest_target_id","venue_length")
cat("Extracting the D50 negative-branch component from PBP...\n")
comp <- rbindlist(lapply(SEASONS, function(s) {
  f <- file.path(DD, sprintf("pbp_data_%d_all.parquet", s))
  if (!file.exists(f)) return(NULL)
  d <- as.data.table(arrow::open_dataset(f) |>
         dplyr::select(dplyr::any_of(PB)) |> dplyr::collect())
  d <- d[!is.na(player_id) & !is.na(delta_epv)]
  d <- d[disposal == 1 | grepl("^(Kick|Handball)", description)]
  d[, scale := fifelse(!is.na(contest_target_id), 1/3, EPV_DISP_SCALE)]
  d[, att_x := x * sign(goal_x)]
  d[, halfL := fifelse(is.na(venue_length), 165, venue_length)/2]
  d[pos_team == -1 & att_x < -(halfL - 50),
    .(d50neg = sum(delta_epv * scale, na.rm = TRUE)), by = .(player_id, match_id)]
}), use.names=TRUE, fill=TRUE)
cat(sprintf("player-games with a D50 negative component: %s\n",
            format(nrow(comp), big.mark=",")))

pg <- rdp("player_game_%d.parquet", SEASONS)
pg[, round := as.numeric(round)]
pg[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage), 100,
                              time_on_ground_percentage)/100, 0.1)]
pg <- merge(pg, comp, by = c("player_id","match_id"), all.x = TRUE)
pg[is.na(d50neg), d50neg := 0]
cat(sprintf("mean D50 negative component by bucket:\n"))
print(pg[position_group %in% BK, .(mean_d50neg = round(mean(d50neg),3)),
         by=position_group][order(mean_d50neg)])

setorder(pg, player_id, utc_start_time); pg[, .date := as.Date(utc_start_time)]
run_decay <- function(x, dates, lam) {
  n <- length(x); out <- numeric(n); s <- 0; prev <- dates[1]
  for (i in seq_len(n)) { s <- s*exp(-as.numeric(dates[i]-prev)/lam); prev <- dates[i]
    out[i] <- s; s <- s + x[i] }
  out
}

res <- rdp("results_%d.parquet", SEASONS)
res <- res[!is.na(home_score)&!is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin=home_score-away_score)]
tr <- as.data.table(read_parquet(file.path(DD,"torp_ratings.parquet")))
tr[, round := as.numeric(round)]

# ---- 2. one EPR rebuild per candidate scale --------------------------------
# Not a linear combination: the adopted standardisation divides by a
# within-position SD that itself moves when the credit changes, so each scale
# needs its own rebuild rather than a reweighting of a cached one.
build_torp <- function(s) {
  d <- copy(pg)
  d[, disp_epv_s := disp_epv - (1 - s) * d50neg]
  for (ch in CH) {
    raw <- if (ch == "disp") "disp_epv_s" else paste0(ch, "_epv")
    if (!raw %in% names(d)) next
    p80 <- paste0(".p80_", ch); d[, (p80) := get(raw)/tog_safe]
    d[, .m := wmean(get(p80), tog_safe), by=lineup_position]
    d[, .s := wsd(get(p80), tog_safe),   by=lineup_position]
    S <- wsd(d[[p80]], d$tog_safe)
    std <- ch %in% EPV_STANDARDISE_CHANNELS
    d[, (paste0("adj_",ch)) := if (std)
        (get(p80) - .m) * pmin(S/pmax(.s,1e-6), 1e9) * tog_safe
      else (get(p80) - .m) * tog_safe]
    d[, c(".m",".s") := NULL]
  }
  for (ch in CH) {
    lam <- DECAY[[ch]]
    d[, .den := run_decay(tog_safe, .date, lam), by=player_id]
    d[, .S := run_decay(get(paste0("adj_",ch))*tog_safe, .date, lam), by=player_id]
    d[, (paste0("e_",ch)) := (EPR_LOADING_DEFAULT*.S + PGAMES[[ch]]*PRATE[[ch]])/(.den + PGAMES[[ch]])]
    d[, c(".den",".S") := NULL]
  }
  d[, epr_s := rowSums(.SD), .SDcols=paste0("e_",CH)]
  L <- merge(d[position_group %in% BK,
               .(player_id, match_id, season, round, team_id, pos=position_group,
                 player_name, epr_s)],
             tr[, .(player_id, season, round, psr_pub=psr)],
             by=c("player_id","season","round"))
  L <- L[!is.na(psr_pub)]
  L[, torp_s := TORP_EPR_WEIGHT*epr_s + (1-TORP_EPR_WEIGHT)*psr_pub]
  L
}

mk <- function(L) {
  ag <- L[, .(v=sum(torp_s, na.rm=TRUE)), by=.(match_id, team_id, pos)]
  wd <- dcast(ag, match_id+team_id ~ pos, value.var="v", fill=0)
  b2 <- intersect(BK, names(wd))
  h <- merge(res, wd, by.x=c("match_id","home_team_id"), by.y=c("match_id","team_id"))
  a <- merge(res[,.(match_id, away_team_id)], wd,
             by.x=c("match_id","away_team_id"), by.y=c("match_id","team_id"))
  setnames(a, b2, paste0("a_",b2)); a[, away_team_id := NULL]
  mm <- merge(h, a, by="match_id")
  for (b in b2) mm[[paste0("d_",b)]] <- mm[[b]] - mm[[paste0("a_",b)]]
  setorder(mm, match_id); mm
}

cat("\nRebuilding EPR for each candidate scale...\n")
Ls <- lapply(GRID, function(s) { cat("  s =", s, "\n"); build_torp(s) })
names(Ls) <- as.character(GRID)
Ms <- lapply(Ls, mk)
stopifnot(length(unique(vapply(Ms, nrow, integer(1)))) == 1L)
b2 <- intersect(BK, sub("^d_","", grep("^d_", names(Ms[[1]]), value=TRUE)))
fml <- as.formula(paste("margin ~", paste0("d_",b2,collapse="+")))
st <- function(mm, ix=NULL) {
  co <- coef(lm(fml, data=if(is.null(ix)) mm else mm[ix]))[-1]; names(co) <- b2
  c(kdkf=unname(co[["KEY_DEFENDER"]]/co[["KEY_FORWARD"]]), mis=unname(mean(abs(co-1))))
}
pt <- lapply(Ms, st); n <- nrow(Ms[[1]])
bs <- array(NA_real_, c(NBOOT, length(GRID), 2),
            dimnames=list(NULL, as.character(GRID), c("kdkf","mis")))
set.seed(20260727)
for (i in seq_len(NBOOT)) {
  ix <- sample.int(n, n, replace=TRUE); ok <- TRUE; tmp <- list()
  for (k in names(Ms)) { r <- tryCatch(st(Ms[[k]], ix), error=function(e) NULL)
    if (is.null(r)) { ok <- FALSE; break }; tmp[[k]] <- r }
  if (!ok) next
  for (k in names(Ms)) bs[i,k,] <- c(tmp[[k]]["kdkf"], tmp[[k]]["mis"])
}
q <- function(x) quantile(x, c(.025,.975), na.rm=TRUE)
cat(sprintf("\n=== D50 negative-credit scale sweep, %d matches, NBOOT=%d ===\n", n, NBOOT))
cat("s = 1.00 is current behaviour; s is physically bounded to [0, 1].\n\n")
for (k in names(Ms))
  cat(sprintf("  s = %-5s  KD/KF %5.2f [%5.2f, %5.2f]   mean|b-1| %.3f [%.3f, %.3f]\n",
              k, pt[[k]]["kdkf"], q(bs[,k,"kdkf"])[1], q(bs[,k,"kdkf"])[2],
              pt[[k]]["mis"], q(bs[,k,"mis"])[1], q(bs[,k,"mis"])[2]))
base <- "1"
cat("\n--- paired against s = 1.00 (current) ---\n")
for (k in setdiff(names(Ms), base)) {
  d <- bs[,k,"mis"] - bs[,base,"mis"]
  cat(sprintf("  s = %-5s  d mean|b-1| %+.3f [%+.3f, %+.3f]  P(improves) %.3f\n",
              k, pt[[k]]["mis"]-pt[[base]]["mis"], q(d)[1], q(d)[2], mean(d<0, na.rm=TRUE)))
}

# ---- 3. ceiling + face validity at the best interior scale ------------------
best <- names(which.min(vapply(pt, function(z) z["mis"], numeric(1))))
cat(sprintf("\nbest scale on mean|b-1|: s = %s\n", best))
E1 <- Ls[[base]][order(player_id, season, round)][, .SD[.N], by=.(player_id, season)]
E2 <- Ls[[best]][order(player_id, season, round)][, .SD[.N], by=.(player_id, season)]
cmp <- merge(E1[, .(player_id, season, pos, player_name, torp_cur=torp_s)],
             E2[, .(player_id, season, torp_new=torp_s)], by=c("player_id","season"))
cat("\n=== ceiling ===\n")
print(cmp[, .(n=.N, sd_cur=round(sd(torp_cur),2), sd_new=round(sd(torp_new),2),
              max_cur=round(max(torp_cur),2), max_new=round(max(torp_new),2)),
          by=pos][order(-max_cur)])
cat("\n=== face validity: 2025 key defenders ===\n")
k25 <- cmp[season==2025 & pos=="KEY_DEFENDER"]
k25[, `:=`(r_cur=frank(-torp_cur), r_new=frank(-torp_new))]
print(head(k25[order(-torp_new), .(player_name, torp_cur=round(torp_cur,2),
        torp_new=round(torp_new,2), r_cur=as.integer(r_cur), r_new=as.integer(r_new))], 10))
cat(sprintf("\nSpearman: %.3f\n", cor(k25$r_cur, k25$r_new, method="spearman")))
