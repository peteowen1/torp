# recv_defensive_reprice.R ------------------------------------------------
# The defensive levers inside recv_epv, which the spoil-channel work missed.
#
# defensive_credit_optimise.R repriced the eight stats in epv_spoil and moved
# calibration only 3.18x -> 3.00x, because that channel is ~11% of a key
# defender's rating SD. But defensive value also flows through RECV, which is
# 34% (0.44 of 1.29) -- three times the lever -- and its defensive parameters
# have never been tuned:
#
#   EPV_RECV_INTERCEPT_MARK_SCALE = 1.0000   <- intercept marks worth exactly
#                                               the same as any other
#                                               defending-team reception
#   EPV_RECV_NEG_MULT             = 1.0000
#   EPV_RECV_NEG_OFFSET           = 0.0000
#   contest_share                 = 1/3      <- defender's cut of an aerial
#                                               contest
#
# Those are round numbers; every optimiser-produced constant in this file
# family looks like 0.0737 / 0.2980 / -0.1882. They are placeholders.
#
# LINEARITY (same trick as the spoil work)
#   epv_recv is linear in each of these, so:
#     epv_recv(s_im, c) = epv_recv_current
#                       + (s_im - 1) * IM          [intercept-mark component]
#                       + (c/(1/3) - 1) * contest_epv
#   where IM is the intercept-mark reception sum at unit scale, recovered from
#   PBP with the same predicate create_player_game_data() uses. Position
#   adjustment, decay and Bayesian shrinkage are all linear too, so each
#   candidate is a linear combination -- no pipeline rebuild.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/recv_defensive_reprice.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")

load_stub <- function(stub, season) {
  fs <- list.files(DATA_DIR, pattern = sprintf("^%s_%d_\\d+\\.parquet$", stub, season),
                   full.names = TRUE)
  rbindlist(lapply(fs, function(f) as.data.table(read_parquet(f))), use.names = TRUE, fill = TRUE)
}

# ---- intercept-mark reception component, per player-game ----------------
# Mirrors create_player_game_data() step 2: credited to lead_player_id,
# excluding rows already handled as 3-way contests.
cat("Extracting intercept-mark components from PBP...\n")
im_list <- list()
for (s in SEASONS) {
  pb <- load_stub("pbp_data", s)
  if (!nrow(pb)) next
  has_contest <- "contest_target_id" %in% names(pb)
  pb[, .is_ct := if (has_contest) !is.na(contest_target_id) else FALSE]
  pb[, .is_im := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]
  im <- pb[.is_ct == FALSE & .is_im == TRUE & !is.na(lead_player_id), .(
    im_base = sum((EPV_RECV_NEG_MULT * delta_epv * pos_team) + EPV_RECV_NEG_OFFSET,
                  na.rm = TRUE),
    n_im = .N
  ), by = .(player_id = lead_player_id, match_id)]
  im_list[[as.character(s)]] <- im
  cat(sprintf("  %d: %s intercept-mark receptions\n", s, format(sum(im$n_im), big.mark = ",")))
}
IM <- rbindlist(im_list)

pg <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
pg[, round := as.numeric(round)]
pg[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage), 100,
                              time_on_ground_percentage) / 100, 0.1)]
pg <- merge(pg, IM, by = c("player_id", "match_id"), all.x = TRUE)
set(pg, which(is.na(pg$im_base)), "im_base", 0)
set(pg, which(is.na(pg$n_im)), "n_im", 0L)
set(pg, which(is.na(pg$contest_epv)), "contest_epv", 0)
cat(sprintf("\nplayer-games: %s | with an intercept mark: %.1f%%\n",
            format(nrow(pg), big.mark = ","), 100 * mean(pg$n_im > 0)))
cat("mean intercept marks per game by position:\n")
print(pg[position_group %in% BUCKETS,
         .(per_game = round(mean(n_im), 2),
           im_base = round(mean(im_base), 3),
           contest_epv = round(mean(contest_epv), 3)), by = position_group][order(-per_game)])

# ---- position-adjust each channel, exactly as step 7 does ---------------
adjust <- function(x) {
  p80 <- x / pg$tog_safe
  pg[, .tmp := p80]
  pg[, .adj := (.tmp - stats::weighted.mean(.tmp, tog_safe, na.rm = TRUE)) * tog_safe,
     by = lineup_position]
  out <- pg$.adj
  pg[, c(".tmp", ".adj") := NULL]
  out
}
pg[, im_adj := adjust(im_base)]
pg[, ce_adj := adjust(contest_epv)]

# ---- point-in-time decayed aggregation on the RECV decay ----------------
setorder(pg, player_id, utc_start_time)
pg[, .date := as.Date(utc_start_time)]
lam <- EPR_DECAY_RECV; K <- EPR_PRIOR_GAMES_RECV; LOAD <- EPR_LOADING_DEFAULT
prior_decayed <- function(x, dates) {
  n <- length(x); out <- numeric(n); s <- 0; prev <- dates[1]
  for (i in seq_len(n)) {
    s <- s * exp(-as.numeric(dates[i] - prev) / lam); prev <- dates[i]
    out[i] <- s; s <- s + x[i]
  }
  out
}
cat("\nAggregating decayed prior sums (recv decay)...\n")
pg[, .den := prior_decayed(tog_safe, .date), by = player_id]
pg[, .S_im := prior_decayed(im_adj * tog_safe, .date), by = player_id]
pg[, .S_ce := prior_decayed(ce_adj * tog_safe, .date), by = player_id]
den <- pg$.den + K
pg[, A_im := (LOAD * .S_im) / den]
pg[, A_ce := (LOAD * .S_ce) / den]

# ---- calibration under candidate (intercept-mark scale, contest share) --
tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]
res <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]

L <- merge(pg[position_group %in% BUCKETS,
              .(player_id, match_id, season, round, team_id,
                pos = position_group, tog_safe, A_im, A_ce)],
           tr[, .(player_id, season, round, torp)],
           by = c("player_id", "season", "round"))
L <- L[!is.na(torp)]

agg <- L[, .(T_pub = sum(torp * tog_safe), S_im = sum(A_im * tog_safe),
             S_ce = sum(A_ce * tog_safe)), by = .(match_id, team_id, pos)]
w <- dcast(agg, match_id + team_id ~ pos, value.var = c("T_pub", "S_im", "S_ce"), fill = 0)
m <- merge(res, w, by.x = c("match_id", "home_team_id"), by.y = c("match_id", "team_id"))
a <- merge(res[, .(match_id, away_team_id)], w,
           by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
vc <- setdiff(names(w), c("match_id", "team_id")); setnames(a, vc, paste0("a_", vc))
m <- merge(m, a, by = "match_id")
D <- data.table(margin = m$margin)
for (v in vc) D[[v]] <- m[[v]] - m[[paste0("a_", v)]]
cat(sprintf("design: %d matches\n", nrow(D)))

EW <- TORP_EPR_WEIGHT
calib <- function(s_im, share) {
  d_im <- s_im - 1
  d_ce <- share / (1 / 3) - 1
  X <- vapply(BUCKETS, function(b)
    D[[paste0("T_pub_", b)]] + EW * (d_im * D[[paste0("S_im_", b)]] +
                                     d_ce * D[[paste0("S_ce_", b)]]),
    numeric(nrow(D)))
  colnames(X) <- BUCKETS
  co <- coef(lm(D$margin ~ X))[-1]; names(co) <- BUCKETS
  co
}

cat("\n===== BASELINE (intercept-mark scale 1.0, contest share 1/3) =====\n")
c0 <- calib(1, 1/3); print(round(c0, 2))
cat(sprintf("  spread %.2fx   KD/KF %.2f\n", max(c0)/min(c0),
            c0[["KEY_DEFENDER"]]/c0[["KEY_FORWARD"]]))

cat("\n===== SWEEP: intercept-mark scale (contest share held at 1/3) =====\n")
sw <- rbindlist(lapply(c(1, 2, 3, 5, 8, 12), function(s) {
  co <- calib(s, 1/3)
  data.table(im_scale = s, KD = round(co[["KEY_DEFENDER"]], 2),
             MD = round(co[["MEDIUM_DEFENDER"]], 2),
             MID = round(co[["MIDFIELDER"]], 2), KF = round(co[["KEY_FORWARD"]], 2),
             spread = round(max(co)/min(co), 2),
             kd_kf = round(co[["KEY_DEFENDER"]]/co[["KEY_FORWARD"]], 2))
}))
print(sw)

cat("\n===== SWEEP: contest share (intercept-mark scale held at 1.0) =====\n")
sw2 <- rbindlist(lapply(c(1/3, 0.5, 0.75, 1.0, 1.5), function(cc) {
  co <- calib(1, cc)
  data.table(contest_share = round(cc, 3), KD = round(co[["KEY_DEFENDER"]], 2),
             MD = round(co[["MEDIUM_DEFENDER"]], 2), KF = round(co[["KEY_FORWARD"]], 2),
             spread = round(max(co)/min(co), 2),
             kd_kf = round(co[["KEY_DEFENDER"]]/co[["KEY_FORWARD"]], 2))
}))
print(sw2)

cat("\n===== JOINT OPTIMISATION =====\n")
obj <- function(p) {
  co <- calib(p[1], p[2])
  if (any(!is.finite(co)) || any(co <= 0)) return(1e6)
  sd(co) / mean(co)
}
o <- optim(c(1, 1/3), obj, method = "Nelder-Mead", control = list(maxit = 2000))
co <- calib(o$par[1], o$par[2])
cat(sprintf("  best intercept-mark scale = %.2f, contest share = %.3f\n", o$par[1], o$par[2]))
print(round(co, 2))
cat(sprintf("  spread %.2fx   KD/KF %.2f   (baseline %.2fx / %.2f)\n",
            max(co)/min(co), co[["KEY_DEFENDER"]]/co[["KEY_FORWARD"]],
            max(c0)/min(c0), c0[["KEY_DEFENDER"]]/c0[["KEY_FORWARD"]]))
