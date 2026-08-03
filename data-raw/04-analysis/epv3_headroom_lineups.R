# Optimiser headroom, measured on PRODUCTION team features.
#
# The first screen (epv3_optimiser_headroom.R) summed every rated player per
# team-round. Production does neither of those things: .build_team_ratings_df()
# filters to the named lineup (emergencies excepted), weights each player's EPR
# by POSITION_AVG_TOG[lineup_position] before summing, and imputes missing
# ratings with the per-channel prior rather than dropping them. So the earlier
# per-channel coefficients -- including a NEGATIVE contest weight that contradicts
# the +0.2656 measured at the EPV layer -- were computed on features production
# never sees.
#
# This uses the production function directly rather than reimplementing it, so
# there is no second version of the logic to drift.
#
# Two questions:
#   1. Is the reweighting ceiling (0.58 MAE in-sample, unfixed) real?
#   2. Does the contest channel's sign flip survive lineups? If it does, that is
#      a finding about v3 that matters more than the optimiser.

suppressPackageStartupMessages({ library(data.table); library(arrow); library(dplyr) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_headroom_lineups.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

teams  <- load_teams(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
xg  <- as.data.table(load_xg(TRUE))
res <- as.data.table(load_results(TRUE))

say("=== Optimiser headroom on PRODUCTION team features ===")
say("teams (lineup) rows ", format(nrow(teams), big.mark = ","))

build <- function(f, label) {
  r <- as.data.frame(arrow::read_parquet(file.path(OUT_DIR, f)))
  t <- as.data.table(.build_team_ratings_df(teams, r, psr_df))
  t[, arm := label]
  t
}
t3 <- build("epv3_ratings_v3.parquet", "v3")
t2 <- build("epv3_ratings_v2.parquet", "v2")
say("team-match rows: v3 ", nrow(t3), " | v2 ", nrow(t2))
say("mean players counted per team: ", round(mean(t3$count), 2),
    "   [should be ~22-23, NOT the whole list]")

# Anchor: a team's summed EPR must be built from a plausible lineup size.
stopifnot(mean(t3$count) > 18, mean(t3$count) < 30)

pair <- function(t, label) {
  h <- t[team_type == "home"]; a <- t[team_type == "away"]
  cols <- c("epr", "epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
  m <- merge(h[, c("match_id", cols), with = FALSE],
             a[, c("match_id", cols), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in cols) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m[, arm := label]
  m
}
p3 <- pair(t3, "v3"); p2 <- pair(t2, "v2")

tgt <- merge(
  res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
  xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
  by = "match_id", all.x = TRUE)
p3 <- merge(p3, tgt, by = "match_id"); p2 <- merge(p2, tgt, by = "match_id")
say("matched matches: v3 ", nrow(p3), " (with xmargin ", sum(is.finite(p3$xmargin)), ")")

screen <- function(m, label, target) {
  m <- m[is.finite(get(target))]
  y <- m[[target]]
  f0 <- lm(y ~ d_epr, data = m)                                   # current behaviour
  f1 <- lm(y ~ d_epr_recv + d_epr_disp + d_epr_spoil + d_epr_hitout, data = m)  # ceiling
  data.table(arm = label, target = target, n = nrow(m),
             R2_summed = round(summary(f0)$r.squared, 5),
             R2_free   = round(summary(f1)$r.squared, 5),
             MAE_summed = round(mean(abs(residuals(f0))), 4),
             MAE_free   = round(mean(abs(residuals(f1))), 4),
             dMAE = round(mean(abs(residuals(f1))) - mean(abs(residuals(f0))), 4))
}

say("")
say("=== CEILING: what free channel reweighting buys (in-sample) ===")
say_dt(rbindlist(list(
  screen(p3, "v3", "margin"),  screen(p2, "v2", "margin"),
  screen(p3, "v3", "xmargin"), screen(p2, "v2", "xmargin")
)), 8)
say("")
say("dMAE negative = free weights help. This is the ABSOLUTE ceiling on what an")
say("EPR decay/prior optimiser can recover: in-sample, free weights, no penalty.")
say("The gap to close is v3's +0.184 MAE against v2.")
say("Earlier UNFIXED screen claimed -0.5836 on margin; compare.")

say("")
say("=== the contest sign question ===")
for (tg in c("margin", "xmargin")) {
  m <- p3[is.finite(get(tg))]
  f <- lm(as.formula(paste(tg, "~ d_epr_recv + d_epr_disp + d_epr_spoil + d_epr_hitout")), data = m)
  say("")
  say("v3, target = ", tg, ":")
  say_dt(as.data.table(round(summary(f)$coefficients, 4), keep.rownames = "term"), 8)
}
say("")
say("d_epr_spoil is the CONTEST channel (v3 aliases cont_aerial into that slot).")
say("At the EPV layer its margin coefficient was +0.2656 (t 9.34). The unfixed")
say("screen read -0.60. Whatever it reads HERE is the trustworthy number,")
say("because these are the features production actually builds.")

say("")
say("--- collinearity, since sign flips usually come from it ---")
m <- p3[is.finite(margin)]
cm <- cor(m[, .(d_epr_recv, d_epr_disp, d_epr_spoil, d_epr_hitout)])
say_dt(as.data.table(round(cm, 3), keep.rownames = "channel"), 6)

close(con)
cat("\nWrote ", OUT, "\n")
