# The contest channel, built from measurement rather than from a regression fit.
#
# WHY NOT JUST USE THE FITTED CONSTANT. The EPR-layer fit says a unit of
# stoppage value is worth 7.50 points of margin. Three checks say do not ship
# that number:
#   - it falls to 4.39 the moment PSR is in the regression, so ~40% of it is
#     team strength the ruck merely travels with;
#   - split-half it reads 4.94 then 10.09, a 2x swing, and the aerial channel
#     changes SIGN (1.17 -> -0.27). Those are fits, not constants;
#   - epv_cont_stop is a linear function of hitout COUNTS by construction -- it
#     is the box-score carve-out. Multiplying a count by 7.5 makes hitout volume
#     12% of the rating, and the contest leaderboard duly fills with high-volume
#     ruckmen rather than good ones.
#
# WHAT TO USE INSTEAD. EPV_RUCK_SWING_SCALE already exists for exactly this, and
# it is measured by accounting rather than by regression: a ruck contest carries
# a mean EPV swing of 0.3925, the three box weights pay 0.1249 of it, so the
# weights under-pay by 3.14x. That route has no team-quality channel to be
# confounded by. Four routes bracket it at 3.14-7.2; 3.14 is the one that does
# not come from fitting margin.
#
# AND FIX THE COUNT PROBLEM AT SOURCE. EPV3_STOP_ZERO_SUM turns the attendance
# term into a ledger -- `ruck_contests - hitouts` is what this ruck LOST, since
# a contest has exactly two rucks. A differential cannot be gamed by volume the
# way a count can, so amplifying it is safe in a way amplifying a count is not.
# That is the real reason to want it, and it is testable: count-dependence
# should FALL while repeatability holds.
#
# Four arms, 2x2:
#   swing 1.0 / ledger off   = the raw-merge arm already measured (contest 2.6%)
#   swing 3.14 / ledger off  = amplification alone
#   swing 1.0  / ledger on   = the ledger alone
#   swing 3.14 / ledger on   = both
#
# All three channels, raw merge, contest NOT standardised -- standardising a
# role-concentrated channel normalises away the only thing it measures, which is
# already why `hitout` is excluded from EPV_STANDARDISE_CHANNELS.
#
# PERFORMANCE: 3 player-game builds (~4 min each) + 4 rating builds (~2 min).
# ~20 min. Run detached.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_contest_ledger_arm.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")

# The base weights, i.e. what they are with EPV_RUCK_SWING_SCALE = 1. Read off
# the live constants and divided back out, so this cannot drift from them.
BASE_HITOUT <- EPV_HITOUT_WT / EPV_RUCK_SWING_SCALE
BASE_HTA    <- EPV_HITOUT_ADV_WT / EPV_RUCK_SWING_SCALE
BASE_RC     <- EPV_RUCK_CONTEST_WT / EPV_RUCK_SWING_SCALE
BASE_LOSS   <- EPV_RUCK_LOSS_WT / EPV_RUCK_SWING_SCALE

say("=== Contest channel: measured amplification + the win/loss ledger ===")
say("run at ", format(Sys.time()))
say(sprintf("base ruck weights (swing scale 1): hitout %.4f  to_adv %.4f  contest %.4f  loss %.4f",
            BASE_HITOUT, BASE_HTA, BASE_RC, BASE_LOSS))

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

build_pgd <- function(tag, swing) {
  f <- file.path(OUT_DIR, paste0("epv3_cal_pgd_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing pgd {tag}")
    d <- as.data.table(read_parquet(f))
  } else {
    p <- default_epv_params()
    p$hitout_wt       <- BASE_HITOUT * swing
    p$hitout_adv_wt   <- BASE_HTA * swing
    p$ruck_contest_wt <- BASE_RC * swing
    d <- as.data.table(create_player_game_data(pbp, stats_, teams, chains,
                                               epv_params = p, epv_engine = "v3"))
    write_parquet(d, f)
  }
  setattr(d, "epv_engine", "v3"); d
}
build_ratings <- function(pgd, tag) {
  f <- file.path(OUT_DIR, paste0("epv3_cal_rt_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing ratings {tag}")
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
fit3 <- function(rt, target = "xmargin", split = FALSE) {
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", CH3), with = FALSE], a[, c("match_id", CH3), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH3) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id"); setorder(m, match_id)
  fml <- as.formula(paste(target, "~ 0 +", paste0("d_", CH3, collapse = " + ")))
  co <- summary(lm(fml, data = m))$coefficients
  out <- list(coef = setNames(co[, 1], CH3), t = setNames(co[, 3], CH3),
              sd_raw = vapply(CH3, function(v) sd(m[[paste0("d_", v)]]), numeric(1)))
  if (split) {
    cut <- floor(nrow(m) / 2)
    out$half1 <- coef(lm(fml, data = m[1:cut]))
    out$half2 <- coef(lm(fml, data = m[(cut + 1):nrow(m)]))
  }
  out
}
yoy <- function(rt, col) {
  eos <- rt[, .SD[which.max(round)], by = .(player_id, season)]
  a <- eos[, c("player_id", "season", col), with = FALSE]; setnames(a, col, "v")
  b <- copy(a)[, season := season - 1]; setnames(b, "v", "v2")
  m <- merge(a, b, by = c("player_id", "season"))[is.finite(v) & is.finite(v2)]
  round(cor(m$v, m$v2), 4)
}

BASE <- list(EPV3_CHANNELS = 3L, EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
             EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
             EPV_STANDARDISE_CHANNELS = c("recv", "disp"),
             EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7,
             EPR_PRIOR_RATE_SPOIL = -0.3, EPR_PRIOR_RATE_HITOUT = 0)

ARMS <- list(
  `swing1_plain`    = list(tag = "3ch_raw_nostd",     swing = 1.00, ledger = FALSE),
  `swing3.14_plain` = list(tag = "3ch_s314_nostd",    swing = 3.14, ledger = FALSE),
  `swing1_ledger`   = list(tag = "3ch_ledg_nostd",    swing = 1.00, ledger = TRUE),
  `swing3.14_ledger`= list(tag = "3ch_s314_ledg",     swing = 3.14, ledger = TRUE)
)

rows <- list()
for (nm in names(ARMS)) {
  a <- ARMS[[nm]]
  cli::cli_h1(nm)
  set_const(c(BASE, list(EPV3_STOP_ZERO_SUM = a$ledger,
                         EPV_RUCK_LOSS_WT = BASE_LOSS * a$swing)))
  pgd <- build_pgd(a$tag, a$swing)
  rt  <- build_ratings(pgd, a$tag)
  f   <- fit3(rt, split = TRUE)
  sd_pts <- f$sd_raw * f$coef
  share  <- 100 * sd_pts^2 / sum(sd_pts^2)

  say("")
  say("=== ", nm, "  (ruck swing x", a$swing, ", win/loss ledger ",
      if (a$ledger) "ON" else "off", ") ===")
  say_dt(data.table(channel = CH3, coef = round(f$coef, 4), t = round(f$t, 2),
                    sd_raw = round(f$sd_raw, 4), sd_points = round(sd_pts, 3),
                    share_pct = round(share, 1)), 5)
  say("split-half stability of the contest constant: ",
      round(f$half1[["d_epr_spoil"]], 3), " / ", round(f$half2[["d_epr_spoil"]], 3),
      "   (full ", round(f$coef[["epr_spoil"]], 3), ")")
  say("contest repeatability y-o-y: ", yoy(rt, "epr_spoil"))
  say("COUNT-DEPENDENCE at the value layer:")
  say(sprintf("  cor(epv_cont_stop, hitouts) %.4f   cor(epv_spoil, hitouts) %.4f   cor(epv_spoil, ruck_contests) %.4f",
              cor(pgd$epv_cont_stop, pgd$hitouts, use = "complete.obs"),
              cor(pgd$epv_spoil, pgd$hitouts, use = "complete.obs"),
              cor(pgd$epv_spoil, pgd$ruck_contests, use = "complete.obs")))

  cu <- rt[season == max(season)][round == max(round)]
  cu[, epr_cal := Reduce(`+`, lapply(seq_along(CH3),
        function(i) f$coef[[i]] * fifelse(is.finite(get(CH3[i])), get(CH3[i]), 0)))]
  cu <- cu[is.finite(epr_cal)]
  pool <- cu[!is.na(position_group), .(pool = .N), by = position_group]
  inn  <- cu[order(-epr_cal)][1:40][!is.na(position_group), .(top40 = .N), by = position_group]
  cmp <- merge(pool, inn, by = "position_group", all.x = TRUE)
  cmp[is.na(top40), top40 := 0L]; cmp[, expected := round(40 * pool / sum(pool), 1)]
  setorder(cmp, -top40)
  say(""); say("top 40 by position:"); say_dt(cmp, 10)
  say(""); say("top 12:")
  t12 <- cu[order(-epr_cal)][1:12, c("player_name", "position_group", "epr_cal", CH3), with = FALSE]
  for (c in c("epr_cal", CH3)) t12[, (c) := round(get(c), 2)]
  say_dt(t12, 12)
  say(""); say("contest-channel leaders:")
  say_dt(cu[order(-epr_spoil)][1:10, .(player_name, position_group,
                                       contest = round(epr_spoil, 3))], 10)

  rows[[nm]] <- data.table(arm = nm, swing = a$swing, ledger = a$ledger,
    contest_coef = round(f$coef[["epr_spoil"]], 3),
    contest_t = round(f$t[["epr_spoil"]], 2),
    contest_share = round(share[["epr_spoil"]], 1),
    contest_yoy = yoy(rt, "epr_spoil"),
    cor_hitouts = round(cor(pgd$epv_spoil, pgd$hitouts, use = "complete.obs"), 3),
    half1 = round(f$half1[["d_epr_spoil"]], 2), half2 = round(f$half2[["d_epr_spoil"]], 2),
    rucks_top40 = cmp[position_group == "RUCK", top40],
    keydef_top40 = cmp[position_group == "KEY_DEFENDER", top40],
    n_missing = nrow(cmp[top40 == 0]))
}

say("")
say("=== SIDE BY SIDE ===")
say_dt(rbindlist(rows), 6)
say("")
say("What to read. contest_share rising is only good if cor_hitouts is NOT")
say("rising with it, and if half1/half2 stay close. A share bought with volume")
say("dependence or an unstable constant is not value.")

saveRDS(rbindlist(rows), file.path(OUT_DIR, "epv3_contest_ledger_arm.rds"))
say("")
say("done ", format(Sys.time()))
close(con)
cat("\nDone\n")
