# One more rebuild, with the EXACT constants that will sit in constants_ratings.R,
# and refit. Every channel must read 1.000.
#
# WHY THIS EXISTS AS A SEPARATE STEP. epv3_finalise.R fits the scale, applies it,
# verifies, then re-fits once more after the measured shrinkage goes in --
# because shrinkage changes the EPR values and therefore the coefficients. That
# last refit produces the shipping constants but is itself unverified: it is one
# Newton step, not a fixed point.
#
# Two reasons the step does not land exactly on 1.000 in one go:
#   - `adjust_epv_for_opponents()` is NOT linear in a single channel. It forms
#     `.abs_total` as a sum of abs() ACROSS channels, so scaling channels by very
#     different factors (1.85, 1.34, 0.48) shifts the denominator. The first
#     verify pass showed exactly this: recv and disp landed on 1.00009 while
#     contest, whose factor is furthest from the others, came back 0.98946.
#   - shrinkage and scale interact through the prior term.
#
# So iterate to a fixed point and report the residual rather than assuming it.
# The spec is "one unit = one point"; a 1% residual is small but it is not the
# claim, and the claim is checkable.
#
# Rscript epv3_verify_shipping_constants.R <tag> [max_iters]
# PERFORMANCE: ~2 min per iteration (rating build only -- the player-game frame
# is reused).

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

A <- commandArgs(trailingOnly = TRUE)
TAG   <- if (length(A) > 0) A[1] else "ship"
ITERS <- if (length(A) > 1) as.integer(A[2]) else 3L
TOL   <- 2e-3

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, paste0("epv3_verify_", TAG, ".txt")), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")

FIN <- readRDS(file.path(OUT_DIR, paste0("epv3_finalise_", TAG, ".rds")))
PG  <- FIN$prior_games
say("=== Iterating the points calibration to a fixed point ===")
say("run at ", format(Sys.time()))
say("starting from epv3_finalise_", TAG, ".rds")
say("prior_games ", paste(PG$channel, PG$prior_games, sep = "=", collapse = ", "))

teams <- load_teams(TRUE)
shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
res <- as.data.table(load_results(TRUE)); xg <- as.data.table(load_xg(TRUE))
tgt <- merge(res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin) & is.finite(xmargin)]

pgd <- as.data.table(read_parquet(file.path(OUT_DIR, paste0("epv3_fin_pgd_", TAG, ".parquet"))))
setattr(pgd, "epv_engine", "v3")

CH3 <- c("epr_recv", "epr_disp", "epr_spoil")
build_and_fit <- function(pts) {
  set_const(list(
    EPV3_CHANNELS = 3L, EPV3_SUB_SCALE = c(cont_aerial = 1, cont_stop = 1),
    EPV_STANDARDISE_CHANNELS = c("recv", "disp"), EPV3_STOP_ZERO_SUM = TRUE,
    EPV3_POINTS_SCALE = pts,
    EPR_PRIOR_RATE_RECV  = -0.7 * pts[["recv"]],
    EPR_PRIOR_RATE_DISP  = -0.7 * pts[["disp"]],
    EPR_PRIOR_RATE_SPOIL = -0.3 * pts[["cont_aerial"]],
    EPR_PRIOR_RATE_HITOUT = 0,
    EPR_PRIOR_GAMES_RECV  = PG[channel == "recv", prior_games],
    EPR_PRIOR_GAMES_DISP  = PG[channel == "disp", prior_games],
    EPR_PRIOR_GAMES_SPOIL = PG[channel == "spoil", prior_games],
    EPR_PRIOR_GAMES_HITOUT = 3))
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  setattr(d, "epv_engine", "v3")
  d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) {
    out <- calculate_torp(out, psr_df)
  }
  out <- as.data.table(out)
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(out), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", CH3), with = FALSE], a[, c("match_id", CH3), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in CH3) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id")
  co <- summary(lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", CH3, collapse = " + "))),
                   data = m))$coefficients
  list(coef = setNames(co[, 1], CH3), t = setNames(co[, 3], CH3),
       sd = vapply(CH3, function(v) sd(m[[paste0("d_", v)]]), numeric(1)), rt = out)
}

pts <- FIN$points_scale
best <- NULL
for (i in seq_len(ITERS)) {
  f <- build_and_fit(pts)
  worst <- max(abs(f$coef - 1))
  say("")
  say("--- iteration ", i, " ---")
  say_dt(data.table(channel = c("recv", "disp", "contest"),
                    scale_applied = round(pts[1:3], 4),
                    coef = round(f$coef, 5), t = round(f$t, 2)), 5)
  say("worst deviation from 1.000: ", signif(worst, 3))
  best <- list(pts = pts, fit = f, worst = worst)
  if (worst < TOL) { say("CONVERGED"); break }
  pts <- pts * c(f$coef[["epr_recv"]], f$coef[["epr_disp"]], f$coef[["epr_spoil"]], 1)
}

say("")
say("=== SHIPPING CONSTANTS ===")
say(sprintf("EPV3_POINTS_SCALE <- c(recv = %.4f, disp = %.4f, cont_aerial = %.4f, cont_stop = 1)",
            best$pts[["recv"]], best$pts[["disp"]], best$pts[["cont_aerial"]]))
say(sprintf("EPR_PRIOR_RATE_RECV  <- -0.7000 * %.4f", best$pts[["recv"]]))
say(sprintf("EPR_PRIOR_RATE_DISP  <- -0.7000 * %.4f", best$pts[["disp"]]))
say(sprintf("EPR_PRIOR_RATE_SPOIL <- -0.3000 * %.4f", best$pts[["cont_aerial"]]))
for (ch in c("recv", "disp", "spoil")) {
  say(sprintf("EPR_PRIOR_GAMES_%-6s <- %.2f", toupper(ch), PG[channel == ch, prior_games]))
}
say("")
say("residual on 'one unit = one point': ", signif(best$worst, 3),
    if (best$worst < TOL) "  (converged)" else "  (NOT converged -- see the opponent-adjustment note in the header)")
say("")
say("variance shares, in points:")
sdp <- best$fit$sd
say_dt(data.table(channel = c("recv", "disp", "contest"),
                  sd_points = round(sdp, 3),
                  share_pct = round(100 * sdp^2 / sum(sdp^2), 1),
                  t = round(best$fit$t, 2)), 5)

write_parquet(best$fit$rt, file.path(OUT_DIR, paste0("epv3_rt_SHIPPING_", TAG, ".parquet")))
saveRDS(list(points_scale = best$pts, prior_games = PG, residual = best$worst),
        file.path(OUT_DIR, paste0("epv3_shipping_constants_", TAG, ".rds")))
say("")
say("done ", format(Sys.time()))
close(con)
cat("\nDone\n")
