# How should contest value be structured? Four arms, one decision.
#
# THE PROBLEM THIS EXISTS TO SOLVE. Calibrating the two contest sub-components
# to points and then merging them produced a channel that reads 0.023 points per
# unit at t 0.27 and carries 0.0% of the rating's variance -- it disappeared.
# The cause is not the calibration, it is what the merged channel is made of.
# Points-calibration weights the stoppage part ~16x the aerial part, so the
# merge becomes a RUCK quantity; the merged channel then goes into the `spoil`
# slot, which .position_adjust() STANDARDISES -- dividing by the within-role SD
# and multiplying by the pooled SD. Standardising a role-concentrated quantity
# normalises away the only thing it measures: rucks' spread is shrunk to the
# pooled spread and everyone else's near-zero spread is inflated to it. This is
# the same failure mode that already excludes `hitout` from
# EPV_STANDARDISE_CHANNELS; the 3-channel merge walked the ruck signal straight
# back into it.
#
# The four arms separate the two decisions that got tangled together:
#
#            merge                      contest standardised?
#   4ch      not merged (4 channels)    aerial yes, stoppage no   <- production's own answer
#   3raw     raw sum                    yes
#   3raw_ns  raw sum                    no
#   3pts_ns  points-calibrated sum      no
#
# (points-calibrated + standardised is the combination already measured and
# rejected above, so it is not rerun.)
#
# Each arm is judged on all four of the things asked for, not on the fit alone:
# the calibration, the spread across channels, whether every position is still
# in the top 40 and who leads the contest channel, and year-over-year
# repeatability.
#
# WHY THE SCALES CAN BE APPLIED ANALYTICALLY. EPR is exactly linear in its
# channel when the shrinkage prior carries the same factor -- .bayesian_shrink()
# returns (sum(w*v) + pg*rate)/(sum(w) + pg), so scaling v and rate by k scales
# the result by k. So one rating build per arm suffices, and only the chosen arm
# needs a real rebuild to confirm it.
#
# PERFORMANCE: 3 player-game builds (~6 min each) + 4 rating builds (~2 min).
# ~25 min. Run detached.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_contest_structure.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")

CAL <- readRDS(file.path(OUT_DIR, "epv3_calibrate_final.rds"))
say("=== Contest structure: merge rule x standardisation ===")
say("run at ", format(Sys.time()))
say("sub-scales from the 4-channel fit: ",
    paste(names(CAL$sub_scale), round(CAL$sub_scale, 4), sep = "=", collapse = ", "))

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

build_pgd <- function(tag) {
  f <- file.path(OUT_DIR, paste0("epv3_cal_pgd_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing pgd {tag}")
    d <- as.data.table(read_parquet(f))
  } else {
    d <- as.data.table(create_player_game_data(pbp, stats_, teams, chains,
                                               epv_engine = "v3"))
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

# Fit the channel scales at the EPR layer against xmargin.
fit_ch <- function(rt, chans, target = "xmargin") {
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", chans), with = FALSE], a[, c("match_id", chans), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in chans) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id")
  co <- summary(lm(as.formula(paste(target, "~ 0 +", paste0("d_", chans, collapse = " + "))),
                   data = m))$coefficients
  list(coef = setNames(co[, 1], chans), t = setNames(co[, 3], chans),
       sd_raw = vapply(chans, function(v) sd(m[[paste0("d_", v)]]), numeric(1)))
}
yoy <- function(rt, col) {
  eos <- rt[, .SD[which.max(round)], by = .(player_id, season)]
  a <- eos[, c("player_id", "season", col), with = FALSE]; setnames(a, col, "v")
  b <- copy(a)[, season := season - 1]; setnames(b, "v", "v2")
  m <- merge(a, b, by = c("player_id", "season"))[is.finite(v) & is.finite(v2)]
  round(cor(m$v, m$v2), 4)
}

STD_ALL <- c("recv", "disp", "spoil")
STD_NO_CONTEST <- c("recv", "disp")
BASE <- list(EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
             EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7,
             EPR_PRIOR_RATE_SPOIL = -0.3)

ARMS <- list(
  `4ch`     = list(tag = "4ch",         ch = 4L, sub = c(cont_aerial = 1, cont_stop = 1),
                   std = STD_ALL, hitout_prior = -0.3,
                   chans = c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")),
  `3raw`    = list(tag = "3ch_raw_ns0", ch = 3L, sub = c(cont_aerial = 1, cont_stop = 1),
                   std = STD_ALL, hitout_prior = 0,
                   chans = c("epr_recv", "epr_disp", "epr_spoil")),
  `3raw_ns` = list(tag = "3ch_raw_nostd", ch = 3L, sub = c(cont_aerial = 1, cont_stop = 1),
                   std = STD_NO_CONTEST, hitout_prior = 0,
                   chans = c("epr_recv", "epr_disp", "epr_spoil")),
  `3pts_ns` = list(tag = "3ch_pts_nostd", ch = 3L, sub = CAL$sub_scale,
                   std = STD_NO_CONTEST, hitout_prior = 0,
                   chans = c("epr_recv", "epr_disp", "epr_spoil"))
)

summary_rows <- list()
for (nm in names(ARMS)) {
  a <- ARMS[[nm]]
  cli::cli_h1(nm)
  set_const(c(BASE, list(EPV3_CHANNELS = a$ch, EPV3_SUB_SCALE = a$sub,
                         EPV_STANDARDISE_CHANNELS = a$std,
                         EPR_PRIOR_RATE_HITOUT = a$hitout_prior)))
  pgd <- build_pgd(a$tag)
  rt  <- build_ratings(pgd, a$tag)
  f   <- fit_ch(rt, a$chans)

  # Applying the fitted scale is exactly a multiplication of each channel, so
  # the calibrated SDs follow without a rebuild. The chosen arm is rebuilt for
  # real afterwards; this is a comparison, not the ship.
  sd_pts <- f$sd_raw * f$coef
  share <- 100 * sd_pts^2 / sum(sd_pts^2)

  say("")
  say("=== ARM ", nm, "  (channels ", a$ch, ", contest standardised: ",
      if ("spoil" %in% a$std) "yes" else "no",
      ", merge: ", if (a$sub[["cont_stop"]] == 1) "raw" else "points", ") ===")
  say_dt(data.table(channel = a$chans, coef = round(f$coef, 4), t = round(f$t, 2),
                    sd_raw = round(f$sd_raw, 4), sd_points = round(sd_pts, 3),
                    share_pct = round(share, 1)), 6)

  cs <- if (a$ch == 3L) "epr_spoil" else NULL
  if (!is.null(cs)) say("contest repeatability (y-o-y): ", yoy(rt, cs))
  say("channel repeatability: ",
      paste(a$chans, vapply(a$chans, function(c) yoy(rt, c), numeric(1)), sep = "=", collapse = "  "))

  # Leaderboard on the CALIBRATED epr, rebuilt from scaled channels.
  cu <- rt[season == max(season)][round == max(round)]
  cu[, epr_cal := Reduce(`+`, lapply(seq_along(a$chans),
        function(i) f$coef[[i]] * fifelse(is.finite(get(a$chans[i])), get(a$chans[i]), 0)))]
  cu <- cu[is.finite(epr_cal)]
  pool <- cu[!is.na(position_group), .(pool = .N), by = position_group]
  inn  <- cu[order(-epr_cal)][1:40][!is.na(position_group), .(top40 = .N), by = position_group]
  cmp <- merge(pool, inn, by = "position_group", all.x = TRUE)
  cmp[is.na(top40), top40 := 0L]; cmp[, expected := round(40 * pool / sum(pool), 1)]
  setorder(cmp, -top40)
  say("")
  say("top 40 on the calibrated EPR, by position:")
  say_dt(cmp, 10)
  say("zero-represented: ", paste(cmp[top40 == 0, position_group], collapse = ", "))
  say("")
  say("top 15:")
  shw <- c("player_name", "position_group", "epr_cal", a$chans)
  t15 <- cu[order(-epr_cal)][1:15, ..shw]
  for (c in setdiff(shw, c("player_name", "position_group"))) t15[, (c) := round(get(c), 2)]
  say_dt(t15, 15)
  if (!is.null(cs)) {
    say("")
    say("contest-channel leaders:")
    say_dt(cu[order(-get(cs))][1:10, .(player_name, position_group,
                                       contest = round(get(cs), 3))], 10)
  }

  summary_rows[[nm]] <- data.table(
    arm = nm,
    contest_t = round(if (a$ch == 3L) f$t[["epr_spoil"]] else f$t[["epr_hitout"]], 2),
    contest_share_pct = round(if (a$ch == 3L) share[["epr_spoil"]] else
                              share[["epr_spoil"]] + share[["epr_hitout"]], 1),
    contest_yoy = if (a$ch == 3L) yoy(rt, "epr_spoil") else NA_real_,
    n_pos_missing = nrow(cmp[top40 == 0]),
    keydef_top40 = cmp[position_group == "KEY_DEFENDER", top40],
    ruck_top40 = cmp[position_group == "RUCK", top40])
}

say("")
say("=== SIDE BY SIDE ===")
say_dt(rbindlist(summary_rows), 6)
say("")
say("Read contest_share_pct together with contest_t. A share that only exists")
say("because the channel is noisy is not value; a channel with a real")
say("coefficient and a small share is telling you contest value IS small.")

saveRDS(summary_rows, file.path(OUT_DIR, "epv3_contest_structure.rds"))
say("")
say("done ", format(Sys.time()))
close(con)
cat("\nDone\n")
