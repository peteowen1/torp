# Fit, apply, verify and report the constants v3 will actually ship with.
#
# Everything structural is decided by the time this runs; this is the last
# numerical step. Order matters and each stage depends on the one before, which
# is why it is one script rather than four:
#
#   1  build the player-game frame on the final structure
#   2  fit EPV3_POINTS_SCALE at the EPR layer against xmargin
#   3  apply it, with each EPR_PRIOR_RATE_* carrying the same factor, and REBUILD
#      -- every channel coefficient must come back 1.000. A real rebuild, not an
#      analytic multiplication: this is the step that caught the 0.919 fallback
#      bug, and an analytic check would have missed it because it assumes the
#      very linearity that was broken.
#   4  re-estimate prior_games on the finished channels (a within/between
#      variance ratio, so it is invariant to the scale but NOT to the structure)
#   5  rebuild once more with the measured shrinkage and report the leaderboard
#
# Configuration comes from the command line so the structure is recorded in the
# invocation rather than edited into the file:
#   Rscript epv3_finalise.R <tag> <recv_neg_mult> <ruck_swing> <stop_zero_sum>
#
# PERFORMANCE: 1 player-game build (~4 min) + 3 rating builds (~2 min each).
# ~12 min. Run detached.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

A <- commandArgs(trailingOnly = TRUE)
TAG        <- if (length(A) > 0) A[1] else "final"
RECV_NEG   <- if (length(A) > 1) as.numeric(A[2]) else 1.0
RUCK_SWING <- if (length(A) > 2) as.numeric(A[3]) else 1.0
STOP_ZS    <- if (length(A) > 3) as.logical(A[4]) else TRUE

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, paste0("epv3_finalise_", TAG, ".txt")), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")

BASE_HITOUT <- EPV_HITOUT_WT / EPV_RUCK_SWING_SCALE
BASE_HTA    <- EPV_HITOUT_ADV_WT / EPV_RUCK_SWING_SCALE
BASE_RC     <- EPV_RUCK_CONTEST_WT / EPV_RUCK_SWING_SCALE
BASE_LOSS   <- EPV_RUCK_LOSS_WT / EPV_RUCK_SWING_SCALE

say("=== Finalising v3: ", TAG, " ===")
say("run at ", format(Sys.time()))
say("EPV_RECV_NEG_MULT ", RECV_NEG, " | ruck swing x", RUCK_SWING,
    " | EPV3_STOP_ZERO_SUM ", STOP_ZS)
say("structure: 3 channels, RAW contest merge, contest NOT standardised")

pbp    <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams  <- load_teams(TRUE); chains <- load_chains(TRUE)
shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res <- as.data.table(load_results(TRUE)); xg <- as.data.table(load_xg(TRUE))
tgt <- merge(res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin) & is.finite(xmargin)]

STRUCT <- list(EPV3_CHANNELS = 3L, EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
               EPV_STANDARDISE_CHANNELS = c("recv", "disp"),
               EPV3_STOP_ZERO_SUM = STOP_ZS,
               EPV_RUCK_LOSS_WT = BASE_LOSS * RUCK_SWING)

build_pgd <- function() {
  f <- file.path(OUT_DIR, paste0("epv3_fin_pgd_", TAG, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing pgd")
    d <- as.data.table(read_parquet(f))
  } else {
    p <- default_epv_params()
    p$recv_neg_mult   <- RECV_NEG
    p$hitout_wt       <- BASE_HITOUT * RUCK_SWING
    p$hitout_adv_wt   <- BASE_HTA * RUCK_SWING
    p$ruck_contest_wt <- BASE_RC * RUCK_SWING
    d <- as.data.table(create_player_game_data(pbp, stats_, teams, chains,
                                               epv_params = p, epv_engine = "v3"))
    write_parquet(d, f)
  }
  setattr(d, "epv_engine", "v3"); d
}
build_ratings <- function(pgd, sfx) {
  f <- file.path(OUT_DIR, paste0("epv3_fin_rt_", TAG, "_", sfx, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing ratings {sfx}")
    return(as.data.table(read_parquet(f))) }
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", "v3")
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) {
    out <- calculate_torp(out, psr_df)
  }
  out <- as.data.table(out); write_parquet(out, f); out
}
CH3 <- c("epr_recv", "epr_disp", "epr_spoil")
fit3 <- function(rt, target = "xmargin") {
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", CH3), with = FALSE], a[, c("match_id", CH3), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH3) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id"); setorder(m, match_id)
  fml <- as.formula(paste(target, "~ 0 +", paste0("d_", CH3, collapse = " + ")))
  co <- summary(lm(fml, data = m))$coefficients
  cut <- floor(nrow(m) / 2)
  list(coef = setNames(co[, 1], CH3), t = setNames(co[, 3], CH3),
       se = setNames(co[, 2], CH3),
       sd = vapply(CH3, function(v) sd(m[[paste0("d_", v)]]), numeric(1)),
       half1 = coef(lm(fml, data = m[1:cut])),
       half2 = coef(lm(fml, data = m[(cut + 1):nrow(m)])), n = nrow(m))
}
yoy <- function(rt, col) {
  eos <- rt[, .SD[which.max(round)], by = .(player_id, season)]
  a <- eos[, c("player_id", "season", col), with = FALSE]; setnames(a, col, "v")
  b <- copy(a)[, season := season - 1]; setnames(b, "v", "v2")
  m <- merge(a, b, by = c("player_id", "season"))[is.finite(v) & is.finite(v2)]
  round(cor(m$v, m$v2), 4)
}

# --- 1-2. build and fit ----------------------------------------------------
set_const(c(STRUCT, list(
  EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
  EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7,
  EPR_PRIOR_RATE_SPOIL = -0.3, EPR_PRIOR_RATE_HITOUT = 0,
  EPR_PRIOR_GAMES_RECV = 3, EPR_PRIOR_GAMES_DISP = 3,
  EPR_PRIOR_GAMES_SPOIL = 3, EPR_PRIOR_GAMES_HITOUT = 3)))
pgd <- build_pgd()
rt0 <- build_ratings(pgd, "unscaled")
f0  <- fit3(rt0)
say("")
say("=== fitted points constants (EPR layer, xmargin, ", f0$n, " matches) ===")
say_dt(data.table(channel = c("recv", "disp", "contest"),
                  coef = round(f0$coef, 4), se = round(f0$se, 4), t = round(f0$t, 2),
                  half1 = round(f0$half1, 3), half2 = round(f0$half2, 3)), 5)
PTS <- c(recv = unname(f0$coef[["epr_recv"]]), disp = unname(f0$coef[["epr_disp"]]),
         cont_aerial = unname(f0$coef[["epr_spoil"]]), cont_stop = 1)

# --- 3. apply and VERIFY on a real rebuild ---------------------------------
set_const(c(STRUCT, list(
  EPV3_POINTS_SCALE = PTS,
  EPR_PRIOR_RATE_RECV = -0.7 * PTS[["recv"]],
  EPR_PRIOR_RATE_DISP = -0.7 * PTS[["disp"]],
  EPR_PRIOR_RATE_SPOIL = -0.3 * PTS[["cont_aerial"]],
  EPR_PRIOR_RATE_HITOUT = 0,
  EPR_PRIOR_GAMES_RECV = 3, EPR_PRIOR_GAMES_DISP = 3,
  EPR_PRIOR_GAMES_SPOIL = 3, EPR_PRIOR_GAMES_HITOUT = 3)))
rt1 <- build_ratings(pgd, "scaled")
f1  <- fit3(rt1)
say("")
say("=== VERIFY: every channel must read 1.000 on a real rebuild ===")
say_dt(data.table(channel = c("recv", "disp", "contest"),
                  coef = round(f1$coef, 5), t = round(f1$t, 2),
                  sd_points = round(f1$sd, 3),
                  share_pct = round(100 * f1$sd^2 / sum(f1$sd^2), 1)), 5)
ok <- max(abs(f1$coef - 1)) < 5e-3
say("VERDICT: ", if (ok) "MET" else paste0("NOT MET (worst ", round(max(abs(f1$coef - 1)), 4), ")"))

# --- 4. prior_games on the finished channels -------------------------------
say("")
say("=== measured shrinkage on the finished structure ===")
d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
setattr(d, "epv_engine", "v3")
d <- centre_epv_by_position(d)
d[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage), 100,
                             time_on_ground_percentage) / 100, 0.1)]
sfx <- if (all(paste0("epv_", c("recv", "disp", "spoil"), "_oadj") %in% names(d))) "_oadj" else "_adj"
hi <- d[time_on_ground_percentage > 50]
PG <- rbindlist(lapply(c("recv", "disp", "spoil"), function(c) {
  x <- hi[[paste0("epv_", c, sfx)]] / hi$tog_safe
  ok2 <- is.finite(x)
  agg <- data.table(pid = hi$player_id[ok2], x = x[ok2])[
    , .(n = .N, m = mean(x), ss = sum((x - mean(x))^2)), by = pid][n >= 3]
  s2w <- sum(agg$ss) / sum(agg$n - 1)
  tau2 <- var(agg$m) - s2w * mean(1 / agg$n)
  data.table(channel = c, prior_games = round(if (is.finite(tau2) && tau2 > 0) s2w / tau2 else NA_real_, 2),
             n_players = nrow(agg))
}))
say_dt(PG, 5)
say("production today: 3.0 on every channel")

# --- 5. final rebuild with the measured shrinkage --------------------------
set_const(list(EPR_PRIOR_GAMES_RECV = PG[channel == "recv", prior_games],
               EPR_PRIOR_GAMES_DISP = PG[channel == "disp", prior_games],
               EPR_PRIOR_GAMES_SPOIL = PG[channel == "spoil", prior_games]))
rt2 <- build_ratings(pgd, "final")
f2  <- fit3(rt2)
say("")
say("=== after shrinkage: the constants shift, so refit once more ===")
say_dt(data.table(channel = c("recv", "disp", "contest"),
                  coef = round(f2$coef, 4), t = round(f2$t, 2)), 5)
PTS2 <- PTS * c(f2$coef[["epr_recv"]], f2$coef[["epr_disp"]], f2$coef[["epr_spoil"]], 1)
say("")
say("=== FINAL CONSTANTS ===")
say(sprintf("EPV_RECV_NEG_MULT     <- %.4f", RECV_NEG))
say(sprintf("EPV_RUCK_SWING_SCALE  <- %.4f", RUCK_SWING))
say(sprintf("EPV3_STOP_ZERO_SUM    <- %s", STOP_ZS))
say(sprintf("EPV3_POINTS_SCALE     <- c(recv = %.4f, disp = %.4f, cont_aerial = %.4f, cont_stop = 1)",
            PTS2[["recv"]], PTS2[["disp"]], PTS2[["cont_aerial"]]))
say(sprintf("EPR_PRIOR_GAMES_RECV  <- %.2f", PG[channel == "recv", prior_games]))
say(sprintf("EPR_PRIOR_GAMES_DISP  <- %.2f", PG[channel == "disp", prior_games]))
say(sprintf("EPR_PRIOR_GAMES_SPOIL <- %.2f", PG[channel == "spoil", prior_games]))

# --- quality report --------------------------------------------------------
say("")
say("=== QUALITY: channel repeatability ===")
for (c in CH3) say(sprintf("  %-10s y-o-y %.4f", c, yoy(rt2, c)))
say(sprintf("  %-10s y-o-y %.4f", "epr", yoy(rt2, "epr")))

cu <- rt2[season == max(season)][round == max(round)]
rk <- if ("torp_value" %in% names(cu)) "torp_value" else "epr"
say("")
say("=== top 40 on ", rk, ", by position ===")
pool <- cu[!is.na(position_group), .(pool = .N), by = position_group]
inn  <- cu[order(-get(rk))][1:40][!is.na(position_group), .(top40 = .N), by = position_group]
cmp <- merge(pool, inn, by = "position_group", all.x = TRUE)
cmp[is.na(top40), top40 := 0L]; cmp[, expected := round(40 * pool / sum(pool), 1)]
setorder(cmp, -top40); say_dt(cmp, 10)
say("positions absent from the top 40: ",
    if (nrow(cmp[top40 == 0])) paste(cmp[top40 == 0, position_group], collapse = ", ") else "none")

say("")
say("=== top 25 ===")
shw <- intersect(c("player_name", "position_group", rk, "epr", "psr", CH3), names(cu))
t25 <- cu[order(-get(rk))][1:25, ..shw]
for (c in setdiff(shw, c("player_name", "position_group"))) t25[, (c) := round(get(c), 2)]
say_dt(t25, 25)

say("")
say("=== per-position level and spread of the published rating ===")
say_dt(cu[!is.na(position_group), .(n = .N,
          mean = round(mean(get(rk), na.rm = TRUE), 3),
          sd = round(sd(get(rk), na.rm = TRUE), 3),
          p95 = round(quantile(get(rk), .95, na.rm = TRUE), 3)),
       by = position_group][order(-sd)], 10)

saveRDS(list(points_scale = PTS2, prior_games = PG, verify = f1$coef,
             recv_neg = RECV_NEG, ruck_swing = RUCK_SWING, stop_zs = STOP_ZS),
        file.path(OUT_DIR, paste0("epv3_finalise_", TAG, ".rds")))
say("")
say("done ", format(Sys.time()))
close(con)
cat("\nDone\n")
