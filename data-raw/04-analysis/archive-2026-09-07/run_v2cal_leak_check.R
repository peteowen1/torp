# Does the v2 per-channel calibration survive fitting its constants honestly?
#
# THE LEAK. The four channel scales were fitted by regressing xmargin on the EPR
# channel diffs over ALL 1,241 matches, and the EPR gate then evaluated "out of
# sample" on those same matches. So the ratings being scored already embed four
# constants that saw the future. The measured gain was large -- OOS MAE 25.894
# -> 25.458, and every other row improving -- and a gain that size deserves the
# check before it is quoted again.
#
# It is a SMALL leak on the face of it: four parameters from ~2,500 team-matches
# is not a regime where overfitting usually bites. And it is pre-existing rather
# than new -- production's own EPV_POINTS_SCALE = 0.919 was fitted the same way
# on 1,121 matches, as was v3's EPV3_POINTS_SCALE. But "everyone already does it"
# is not evidence, and the honest version is cheap.
#
# THE HONEST VERSION. Fit the scale on seasons strictly before a cutoff, apply
# it, and score only seasons at or after the cutoff. The constants then never
# saw the matches they are judged on.
#
# THREE ARMS at the cutoff:
#   1  global 0.919        production's scale, itself fitted on everything --
#                          left as is, because it IS what production does
#   2  per-channel, leaky  fitted on all seasons (what was measured before)
#   3  per-channel, clean  fitted on seasons < cutoff only
#
# Arm 3 against arm 1 is the honest gain. Arm 3 against arm 2 is the size of the
# leak. If arm 3 keeps most of arm 2's gain, the result stands.
#
# ~10 min: three rating builds plus lm fits.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
source("C:/dev/torpverse/torp/data-raw/04-analysis/benchmark_epr_gate.R")

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
CUTOFF <- 2024L
sink(file.path(OUT_DIR, "v2cal_leak_check.txt"), split = TRUE)
cat("=== is the v2 calibration gain a fitting artefact? ===\nrun at", format(Sys.time()),
    "\nscale fitted on seasons <", CUTOFF, "; scored on", CUTOFF, "onward\n")

set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")
with_const <- function(l, expr) {
  old <- lapply(names(l), function(nm) get(nm, envir = asNamespace("torp")))
  names(old) <- names(l); set_const(l); on.exit(set_const(old), add = TRUE); force(expr)
}

teams <- load_teams(TRUE); res <- as.data.table(load_results(TRUE))
xg <- as.data.table(load_xg(TRUE))
shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
tgt <- merge(res[, .(match_id = as.character(match_id), season,
                     margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin) & is.finite(xmargin)]
pgd <- as.data.table(read_parquet(file.path(OUT_DIR, "v2v3_pgd_v2.parquet")))
setattr(pgd, "epv_engine", "v2")

V2_BASE <- list(EPV_ENGINE = "v2", EPV3_CHANNELS = 3L,
                EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
                EPV3_STOP_ZERO_SUM = FALSE,
                EPV_STANDARDISE_CHANNELS = c("recv", "disp", "spoil"),
                EPV_DIFFICULTY_SPLIT = FALSE,
                EPR_PRIOR_GAMES_RECV = 3, EPR_PRIOR_GAMES_DISP = 3,
                EPR_PRIOR_GAMES_SPOIL = 3, EPR_PRIOR_GAMES_HITOUT = 3)
GLOBAL <- c(V2_BASE, list(EPV_PER_CHANNEL_POINTS_SCALE = FALSE, EPV_POINTS_SCALE = 0.919,
  EPV3_POINTS_SCALE = c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1),
  EPR_PRIOR_RATE_RECV = -0.7 * 0.919, EPR_PRIOR_RATE_DISP = -0.7 * 0.919,
  EPR_PRIOR_RATE_SPOIL = -0.3 * 0.919, EPR_PRIOR_RATE_HITOUT = -0.3 * 0.919))
per_channel <- function(pts) c(V2_BASE, list(
  EPV_PER_CHANNEL_POINTS_SCALE = TRUE, EPV_POINTS_SCALE = 1, EPV3_POINTS_SCALE = pts,
  EPR_PRIOR_RATE_RECV = -0.7 * pts[["recv"]], EPR_PRIOR_RATE_DISP = -0.7 * pts[["disp"]],
  EPR_PRIOR_RATE_SPOIL = -0.3 * pts[["cont_aerial"]],
  EPR_PRIOR_RATE_HITOUT = -0.3 * pts[["cont_stop"]]))
UNIT <- c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1)

build_ratings <- function(tag) {
  f <- file.path(OUT_DIR, paste0("v2cal_rt_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("Reusing ratings {tag}")
    return(as.data.table(read_parquet(f))) }
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", "v2")
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) out <- calculate_torp(out, psr_df)
  out <- as.data.table(out); write_parquet(out, f); out
}
CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
fit_ch <- function(rt, seasons = NULL) {
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", CH), with = FALSE], a[, c("match_id", CH), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id")
  if (!is.null(seasons)) m <- m[season %in% seasons]
  cat(sprintf("  fitted on %d matches\n", nrow(m)))
  stats::coef(stats::lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", CH, collapse = " + "))), data = m))
}
as_pts <- function(co) c(recv = unname(co[[1]]), disp = unname(co[[2]]),
                         cont_aerial = unname(co[[3]]), cont_stop = unname(co[[4]]))

rt0 <- with_const(c(V2_BASE, list(EPV_PER_CHANNEL_POINTS_SCALE = TRUE, EPV_POINTS_SCALE = 1,
                                  EPV3_POINTS_SCALE = UNIT,
                                  EPR_PRIOR_RATE_RECV = -0.7, EPR_PRIOR_RATE_DISP = -0.7,
                                  EPR_PRIOR_RATE_SPOIL = -0.3, EPR_PRIOR_RATE_HITOUT = -0.3)),
                  build_ratings("unscaled"))
cat("\n-- leaky scale (all seasons) --\n");  PTS_ALL <- as_pts(fit_ch(rt0))
cat("-- clean scale (seasons < ", CUTOFF, ") --\n", sep = "")
PTS_PRE <- as_pts(fit_ch(rt0, seasons = sort(unique(tgt$season))[sort(unique(tgt$season)) < CUTOFF]))
cat(sprintf("  all:   recv %.4f disp %.4f spoil %.4f hitout %.4f\n",
            PTS_ALL[[1]], PTS_ALL[[2]], PTS_ALL[[3]], PTS_ALL[[4]]))
cat(sprintf("  pre:   recv %.4f disp %.4f spoil %.4f hitout %.4f\n",
            PTS_PRE[[1]], PTS_PRE[[2]], PTS_PRE[[3]], PTS_PRE[[4]]))
cat("  If these are close, the leak has little to give and the gain is real.\n")

rt_g <- with_const(GLOBAL, build_ratings("global"))
rt_l <- with_const(per_channel(PTS_ALL), build_ratings("percal"))
rt_c <- with_const(per_channel(PTS_PRE), build_ratings("percal_clean"))

# Score only seasons at or after the cutoff, so the clean arm's constants never
# saw the matches being judged.
score <- function(rt, label) {
  g <- bm_epr_gate(pgd[season >= CUTOFF], rt, res[season >= CUTOFF], label)
  cat(sprintf("  %-24s OOS MAE %.4f | within-team coef %.4f (t %.1f) | pts conceded %+.4f\n",
              label, g$oos_mae, g$fixed_effects$coef, g$fixed_effects$t, g$defence_fe$coef))
  g
}
cat("\n=== SCORED ON SEASONS ", CUTOFF, "+ ONLY ===\n", sep = "")
a <- score(rt_g, "global 0.919")
b <- score(rt_l, "per-channel, leaky")
d <- score(rt_c, "per-channel, CLEAN")

cat("\n=== VERDICT INPUT ===\n")
cat(sprintf("  honest gain (clean vs global): OOS MAE %+.4f\n", d$oos_mae - a$oos_mae))
cat(sprintf("  size of the leak (clean vs leaky): OOS MAE %+.4f\n", d$oos_mae - b$oos_mae))
cat("\n  Negative honest gain = the calibration really helps. A leak term near\n")
cat("  zero means the constants were never doing the work; a large one means\n")
cat("  the earlier -0.44 was partly a fitting artefact and the clean number is\n")
cat("  the one to quote.\n")

saveRDS(list(all = PTS_ALL, pre = PTS_PRE, global = a, leaky = b, clean = d),
        file.path(OUT_DIR, "v2cal_leak_check.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
