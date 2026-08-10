# Fit the v3 points calibration on the structure that will actually ship.
#
# The constants on disk were fitted on a FOUR-channel build with
# EPV_RECV_NEG_MULT = 1. Both of those have since changed, and a points constant
# is only valid for the structure it was fitted on, so they are refitted here
# rather than carried across.
#
# Three stages, each one rebuild:
#
#   A  4 channels, sub-scales all 1   -> fit the four sub-component scales.
#      This is the ONLY structure where EPR sees cont_aerial and cont_stop
#      separately, so it is the only place the ratio between them is
#      identifiable.
#   B  3 channels, EPV3_SUB_SCALE = A -> fit the three top-level channel scales.
#      The sub-scales make the merge blend by POINTS instead of by variance.
#   C  3 channels, EPV3_POINTS_SCALE = B, priors carrying the same factor
#      -> every channel coefficient must come back 1.000. If it does not, the
#      scale is not reaching EPR cleanly and nothing downstream is trustworthy.
#
# Everything is at the EPR layer against xmargin -- production's convention, and
# the layer the live EPV_POINTS_SCALE = 0.919 is expressed in. Quoting an EPV
# layer number here would be a different quantity, not a competing estimate.
#
# PERFORMANCE: 2 player-game builds (~5 min each) + 3 rating builds (~3 min
# each) ~= 25 min. Run detached:
#   Start-Process Rscript -ArgumentList '"<this file>"'

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_calibrate_final.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

set_const <- function(...) {
  vals <- list(...)
  for (nm in names(vals)) assignInNamespace(nm, vals[[nm]], ns = "torp")
}

say("=== v3 points calibration, refitted on the shipping structure ===")
say("run at ", format(Sys.time()))
say("EPV_RECV_NEG_MULT = ", EPV_RECV_NEG_MULT, "  (0 = the negative reception term is dropped)")

pbp    <- load_pbp(TRUE)
stats_ <- load_player_stats(TRUE)
teams  <- load_teams(TRUE)
chains <- load_chains(TRUE)
shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res <- as.data.table(load_results(TRUE))
xg  <- as.data.table(load_xg(TRUE))
tgt <- merge(res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin) & is.finite(xmargin)]
say("target matches: ", nrow(tgt))

build_pgd <- function(tag) {
  f <- file.path(OUT_DIR, paste0("epv3_cal_pgd_", tag, ".parquet"))
  if (file.exists(f)) {
    cli::cli_alert_info("Reusing cached pgd {tag}")
    d <- as.data.table(read_parquet(f))
  } else {
    d <- as.data.table(create_player_game_data(pbp, stats_, teams, chains,
                                               epv_engine = "v3"))
    write_parquet(d, f)
  }
  # The engine tag does NOT survive a parquet round-trip, and
  # centre_epv_by_position() keys the per-channel scale on it -- so a cached
  # frame would silently skip the scaling, which is the exact failure the tag
  # was introduced to prevent.
  setattr(d, "epv_engine", "v3")
  d
}

build_ratings <- function(pgd, tag) {
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
  out <- as.data.table(out)
  write_parquet(out, file.path(OUT_DIR, paste0("epv3_cal_rt_", tag, ".parquet")))
  out
}

# Points of margin per unit of EPR, one coefficient per channel, no intercept.
fit_channels <- function(rt, chans, target = "xmargin") {
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", chans), with = FALSE],
             a[, c("match_id", chans), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in chans) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id")
  f <- lm(as.formula(paste(target, "~ 0 +", paste0("d_", chans, collapse = " + "))), data = m)
  co <- summary(f)$coefficients
  list(coef = setNames(co[, 1], sub("^d_", "", rownames(co))),
       t = setNames(co[, 3], sub("^d_", "", rownames(co))),
       sd = vapply(chans, function(v) sd(m[[paste0("d_", v)]]), numeric(1)),
       n = nrow(m))
}

# --------------------------------------------------------------------------
# A. four channels -- the only structure where the two contest components are
#    separately visible to EPR, so the only place their ratio is identifiable.
# --------------------------------------------------------------------------
say("")
say("=== A. four channels: fit the sub-component scales ===")
set_const(EPV3_CHANNELS = 4L,
          EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
          EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
          EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7,
          EPR_PRIOR_RATE_SPOIL = -0.3, EPR_PRIOR_RATE_HITOUT = -0.3)
pgd4 <- build_pgd("4ch")
rt4  <- build_ratings(pgd4, "4ch")
SUB <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
LBL <- c(epr_recv = "recv", epr_disp = "disp",
         epr_spoil = "cont_aerial", epr_hitout = "cont_stop")
fa <- fit_channels(rt4, SUB)
say("matches in fit: ", fa$n)
say_dt(data.table(sub_component = LBL[names(fa$coef)],
                  coef = round(fa$coef, 4), t = round(fa$t, 2)), 5)
sub_scale <- c(cont_aerial = unname(fa$coef[["epr_spoil"]]),
               cont_stop   = unname(fa$coef[["epr_hitout"]]))
say("")
say("EPV3_SUB_SCALE <- c(cont_aerial = ", round(sub_scale[["cont_aerial"]], 4),
    ", cont_stop = ", round(sub_scale[["cont_stop"]], 4), ")")

# --------------------------------------------------------------------------
# B. three channels, sub-scales applied at the EPV layer before the merge.
# --------------------------------------------------------------------------
say("")
say("=== B. three channels, merged in POINTS: fit the channel scales ===")
set_const(EPV3_CHANNELS = 3L, EPV3_SUB_SCALE = sub_scale,
          EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
          EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7,
          EPR_PRIOR_RATE_SPOIL = -0.3, EPR_PRIOR_RATE_HITOUT = 0)
pgd3 <- build_pgd("3ch_sub")
rt3  <- build_ratings(pgd3, "3ch_sub")
CH3 <- c("epr_recv", "epr_disp", "epr_spoil")
fb <- fit_channels(rt3, CH3)
say_dt(data.table(channel = c("recv", "disp", "contest"),
                  coef = round(fb$coef, 4), t = round(fb$t, 2)), 5)
pts <- c(recv = unname(fb$coef[["epr_recv"]]), disp = unname(fb$coef[["epr_disp"]]),
         cont_aerial = unname(fb$coef[["epr_spoil"]]), cont_stop = 1)
say("")
say("EPV3_POINTS_SCALE <- c(recv = ", round(pts[["recv"]], 4),
    ", disp = ", round(pts[["disp"]], 4),
    ", cont_aerial = ", round(pts[["cont_aerial"]], 4), ", cont_stop = 1)")
say("  (cont_aerial scales the MERGED contest channel under 3 channels;")
say("   cont_stop is inert because the hitout slot holds nothing.)")

# --------------------------------------------------------------------------
# C. apply and verify. Every coefficient must return 1.000. The priors carry
#    the same factor or EPR is a blend of scaled and unscaled parts.
# --------------------------------------------------------------------------
say("")
say("=== C. applied: every channel must read 1.000 ===")
set_const(EPV3_POINTS_SCALE = pts,
          EPR_PRIOR_RATE_RECV = -0.7 * pts[["recv"]],
          EPR_PRIOR_RATE_DISP = -0.7 * pts[["disp"]],
          EPR_PRIOR_RATE_SPOIL = -0.3 * pts[["cont_aerial"]],
          EPR_PRIOR_RATE_HITOUT = 0)
rt3c <- build_ratings(pgd3, "3ch_final")
fc <- fit_channels(rt3c, CH3)
say_dt(data.table(channel = c("recv", "disp", "contest"),
                  coef = round(fc$coef, 5), t = round(fc$t, 2),
                  sd_points = round(fc$sd, 3)), 5)
say("VERDICT: ", if (max(abs(fc$coef - 1)) < 5e-3) "MET" else "NOT MET")

say("")
say("--- variance shares, in points of margin (team-difference scale) ---")
shares <- 100 * fc$sd^2 / sum(fc$sd^2)
say_dt(data.table(channel = c("recv", "disp", "contest"),
                  sd_points = round(fc$sd, 3), share_pct = round(shares, 1)), 5)

say("")
say("--- cross-check on ACTUAL margin (xmargin is the quieter target) ---")
fm <- fit_channels(rt3c, CH3, target = "margin")
say_dt(data.table(channel = c("recv", "disp", "contest"),
                  coef = round(fm$coef, 4), t = round(fm$t, 2)), 5)

saveRDS(list(sub_scale = sub_scale, points_scale = pts,
             verify = fc$coef, shares = shares),
        file.path(OUT_DIR, "epv3_calibrate_final.rds"))
say("")
say("done ", format(Sys.time()))
close(con)
cat("\nDone\n")
