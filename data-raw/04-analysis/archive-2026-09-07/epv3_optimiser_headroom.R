# Is a parameter optimiser worth building? And what should it optimise against?
#
# stats-discipline rule 3: size the effect BEFORE building the fix. Error
# reduction is QUADRATIC -- removing an independent error component of spread s
# from total spread sigma buys s^2/2sigma^2, so a mechanism worth 6% of the error
# buys 0.2%.
#
# THE SCREEN. Freely reweighting the four EPR channels is an UPPER BOUND on what
# tuning EPR_DECAY_* / EPR_PRIOR_RATE_* / EPR_PRIOR_GAMES_* can achieve, because
# those parameters only rescale and reshape channels -- they cannot create signal
# a free linear reweighting could not also exploit. And this is in-sample, which
# upper-bounds any OOS run again. So a null here is CONCLUSIVE and costs minutes
# instead of the ~25 minutes per evaluation a real optimiser would need.
#
# THE TARGET QUESTION. Actual margin carries shot-conversion luck that no player
# rating should be asked to predict. xscore margin strips it. Whether that is
# worth using depends on how much noise it actually removes, which is measured
# here rather than assumed.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_optimiser_headroom.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Optimiser headroom: is this worth building? ===")

r3 <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_ratings_v3.parquet")))[!is.na(epr)]
r2 <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_ratings_v2.parquet")))[!is.na(epr)]
res <- as.data.table(load_results(TRUE))
say("ratings rows (non-NA epr): v3 ", nrow(r3), " | v2 ", nrow(r2))

# ---- Target noise: margin vs xscore margin ---------------------------------
say("")
say("=== 1. TARGET: how noisy is margin, and does xscore margin help? ===")
xg <- tryCatch(as.data.table(load_xg(TRUE)), error = function(e) NULL)
if (is.null(xg)) {
  say("load_xg() unavailable -- cannot compare targets.")
} else {
  say("xg rows ", format(nrow(xg), big.mark = ","), " | cols: ",
      paste(utils::head(names(xg), 25), collapse = ", "))
  xcol <- intersect(c("xscore", "x_score", "expected_score", "xpoints", "shot_xscore"),
                    names(xg))
  tcol <- intersect(c("team", "team_name", "shot_team"), names(xg))
  say("xscore column found: ", if (length(xcol)) xcol[1] else "NONE",
      " | team column: ", if (length(tcol)) tcol[1] else "NONE")
  if (length(xcol) && length(tcol) && "match_id" %in% names(xg)) {
    xs <- xg[, .(xs = sum(get(xcol[1]), na.rm = TRUE)),
             by = .(match_id, team = get(tcol[1]))]
    rr <- res[, .(match_id = as.character(match_id), home = home_team_name,
                  away = away_team_name, margin = home_score - away_score)]
    h <- merge(rr, xs, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
    a <- merge(rr, xs, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
    mm <- merge(h[, .(match_id, margin, xs_h = xs)],
                a[, .(match_id, xs_a = xs)], by = "match_id")
    mm[, xmargin := xs_h - xs_a]
    say("")
    say("matched matches: ", nrow(mm))
    say("sd(margin)  = ", round(sd(mm$margin), 3))
    say("sd(xmargin) = ", round(sd(mm$xmargin), 3))
    say("cor(margin, xmargin) = ", round(cor(mm$margin, mm$xmargin), 4))
    say("sd(margin - xmargin) = ", round(sd(mm$margin - mm$xmargin), 3),
        "   <- the conversion luck a rating should NOT be asked to predict")
    say("")
    say("Share of margin variance that is NOT xmargin: ",
        round(100 * (1 - cor(mm$margin, mm$xmargin)^2), 1), "%")
    say("If that share is large, optimising against margin spends most of its")
    say("statistical power fitting shot luck.")
    arrow::write_parquet(mm, file.path(OUT_DIR, "epv3_xmargin.parquet"))
  }
}

# ---- The upper-bound screen ------------------------------------------------
say("")
say("=== 2. SCREEN: upper bound on what channel reweighting can buy ===")
say("(in-sample, free weights -- both of which inflate it. A null is conclusive.)")

CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
team_epr <- function(r) {
  # A team's rating for a match is the sum over its players on that round's
  # ratings. Ratings are per (player, season, round), so join on the fixture.
  r[, .(recv = sum(epr_recv, na.rm = TRUE), disp = sum(epr_disp, na.rm = TRUE),
        cont = sum(epr_spoil, na.rm = TRUE), stop = sum(epr_hitout, na.rm = TRUE),
        epr = sum(epr, na.rm = TRUE), n = .N),
    by = .(team, season, round)]
}
tcol <- intersect(c("team", "team_name"), names(r3))
if (length(tcol) == 0) {
  say("No team column on the ratings frame; columns are: ",
      paste(names(r3), collapse = ", "))
} else {
  setnames(r3, tcol[1], "team"); setnames(r2, tcol[1], "team")
  t3 <- team_epr(r3); t2 <- team_epr(r2)
  say("team-round rating rows: v3 ", nrow(t3))

  rr <- res[, .(match_id = as.character(match_id), season = as.integer(season),
                round = as.integer(round_number),
                home = home_team_name, away = away_team_name,
                margin = home_score - away_score)]
  rr <- rr[is.finite(margin) & is.finite(season) & is.finite(round)]
  say("results with season/round: ", nrow(rr))

  build <- function(tt, label) {
    h <- merge(rr, tt, by.x = c("home", "season", "round"),
               by.y = c("team", "season", "round"))
    a <- merge(rr, tt, by.x = c("away", "season", "round"),
               by.y = c("team", "season", "round"))
    m <- merge(h[, .(match_id, margin, recv, disp, cont, stop, epr)],
               a[, .(match_id, recv, disp, cont, stop, epr)],
               by = "match_id", suffixes = c("_h", "_a"))
    for (v in c("recv", "disp", "cont", "stop", "epr")) {
      m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
    }
    m[, arm := label]
    m
  }
  m3 <- build(t3, "v3"); m2 <- build(t2, "v2")
  say("matched team-match rows: v3 ", nrow(m3), " | v2 ", nrow(m2))

  screen <- function(m, label, target = "margin") {
    if (nrow(m) < 100) return(NULL)
    y <- m[[target]]
    # CONSTRAINED: the channels enter only through their sum, i.e. the current
    # behaviour where every channel is already on a common scale.
    f0 <- lm(y ~ d_epr, data = m)
    # FREE: each channel gets its own weight. This is the upper bound, because
    # any decay/prior retuning only rescales and reshapes channels and cannot
    # exploit signal a free linear reweighting could not.
    f1 <- lm(y ~ d_recv + d_disp + d_cont + d_stop, data = m)
    data.table(
      arm = label, target = target, n = nrow(m),
      R2_summed = round(summary(f0)$r.squared, 5),
      R2_free   = round(summary(f1)$r.squared, 5),
      dR2       = round(summary(f1)$r.squared - summary(f0)$r.squared, 5),
      MAE_summed = round(mean(abs(residuals(f0))), 4),
      MAE_free   = round(mean(abs(residuals(f1))), 4),
      dMAE       = round(mean(abs(residuals(f1))) - mean(abs(residuals(f0))), 4)
    )
  }
  out <- rbindlist(list(screen(m3, "v3"), screen(m2, "v2")), fill = TRUE)
  say("")
  say_dt(out, 8)
  say("")
  say("dMAE is NEGATIVE when free weights help. This is the ABSOLUTE CEILING on")
  say("what an EPR-parameter optimiser could recover, and it is in-sample.")
  say("The gap it would need to close is v3's +0.184 MAE against v2.")

  # Same screen against xscore margin, if available.
  xm <- tryCatch(as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_xmargin.parquet"))),
                 error = function(e) NULL)
  if (!is.null(xm)) {
    m3x <- merge(m3, xm[, .(match_id, xmargin)], by = "match_id")
    m2x <- merge(m2, xm[, .(match_id, xmargin)], by = "match_id")
    say("")
    say("--- same screen, target = xscore margin ---")
    say_dt(rbindlist(list(screen(m3x, "v3", "xmargin"), screen(m2x, "v2", "xmargin")),
                     fill = TRUE), 8)
  }

  say("")
  say("--- fitted free weights (v3, margin) -- do they even want reweighting? ---")
  f1 <- lm(margin ~ d_recv + d_disp + d_cont + d_stop, data = m3)
  say_dt(as.data.table(round(summary(f1)$coefficients, 4), keep.rownames = "term"), 8)
  say("If the four slopes are similar, the channels are ALREADY on a common")
  say("scale and there is nothing for a reweighting optimiser to find.")
}

close(con)
cat("\nWrote ", OUT, "\n")
