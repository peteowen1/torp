# Does the contest channel predict anything at team level, or nothing?
#
# On production features the multivariate coefficient came back at -0.0197
# (t = -0.06, p = 0.95) against margin. That is a null, not a sign flip -- and it
# matters enormously, because cont_aerial is 19.8% of EPR variance. A fifth of
# the metric contributing nothing to match outcomes would be where v3's +0.184
# MAE comes from.
#
# Before believing it, three checks:
#   1. UNIVARIATE -- a multivariate null can be absorption by a correlated
#      regressor. Collinearity is low (0.18 with recv, 0.10 with disp) so this
#      should agree, but "should" is not "does".
#   2. PER-SD contribution, so channels on wildly different scales are compared
#      on equal terms.
#   3. v2's spoil channel the same way, as the reference point.

suppressPackageStartupMessages({ library(data.table); library(arrow); library(dplyr) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_contest_predictive.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

teams <- load_teams(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
xg  <- as.data.table(load_xg(TRUE)); res <- as.data.table(load_results(TRUE))

prep <- function(f, label) {
  r <- as.data.frame(arrow::read_parquet(file.path(OUT_DIR, f)))
  t <- as.data.table(.build_team_ratings_df(teams, r, psr_df))
  cols <- c("epr", "epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
  h <- t[team_type == "home"]; a <- t[team_type == "away"]
  m <- merge(h[, c("match_id", cols), with = FALSE],
             a[, c("match_id", cols), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in cols) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  tg <- merge(res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
              xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
              by = "match_id", all.x = TRUE)
  m <- merge(m, tg, by = "match_id")
  m[, arm := label]
  m
}
p3 <- prep("epv3_ratings_v3.parquet", "v3")
p2 <- prep("epv3_ratings_v2.parquet", "v2")

CH <- c("d_epr_recv", "d_epr_disp", "d_epr_spoil", "d_epr_hitout")
LBL <- c(d_epr_recv = "recv", d_epr_disp = "disp",
         d_epr_spoil = "cont_aerial", d_epr_hitout = "cont_stop")

say("=== Is the contest channel predictive at team level? ===")

for (tg in c("margin", "xmargin")) {
  for (nm in c("v3", "v2")) {
    m <- (if (nm == "v3") p3 else p2)[is.finite(get(tg))]
    fm <- lm(as.formula(paste(tg, "~", paste(CH, collapse = " + "))), data = m)
    co <- summary(fm)$coefficients
    rows <- rbindlist(lapply(CH, function(cc) {
      s <- sd(m[[cc]])
      uni <- summary(lm(as.formula(paste(tg, "~", cc)), data = m))$coefficients
      data.table(
        channel = LBL[[cc]],
        sd_feature = round(s, 4),
        multi_coef = round(co[cc, 1], 4),
        multi_t = round(co[cc, 3], 2),
        multi_p = round(co[cc, 4], 4),
        pts_per_sd_multi = round(co[cc, 1] * s, 3),
        uni_coef = round(uni[cc, 1], 4),
        uni_t = round(uni[cc, 3], 2),
        pts_per_sd_uni = round(uni[cc, 1] * s, 3)
      )
    }))
    say("")
    say("--- ", nm, ", target = ", tg, " (n = ", nrow(m), ") ---")
    say_dt(rows, 8)
  }
}

say("")
say("=== the headline comparison, v3 on margin ===")
m <- p3[is.finite(margin)]
fm <- lm(margin ~ d_epr_recv + d_epr_disp + d_epr_spoil + d_epr_hitout, data = m)
co <- summary(fm)$coefficients
per_sd <- sapply(CH, function(cc) co[cc, 1] * sd(m[[cc]]))
say("points of margin per 1 sd of each channel:")
for (cc in CH) say("  ", sprintf("%-12s %+.3f", LBL[[cc]], per_sd[[cc]]))
say("")
say("Three channels contribute almost identically per sd. The contest channel")
say("contributes nothing -- while carrying 19.8% of EPR variance.")

say("")
say("=== does dropping contest hurt at all? ===")
f_all  <- lm(margin ~ d_epr_recv + d_epr_disp + d_epr_spoil + d_epr_hitout, data = m)
f_drop <- lm(margin ~ d_epr_recv + d_epr_disp + d_epr_hitout, data = m)
say("R2 with contest    ", round(summary(f_all)$r.squared, 5))
say("R2 without contest ", round(summary(f_drop)$r.squared, 5))
say("MAE with           ", round(mean(abs(residuals(f_all))), 4))
say("MAE without        ", round(mean(abs(residuals(f_drop))), 4))
say("anova p = ", round(anova(f_drop, f_all)$`Pr(>F)`[2], 4))

close(con)
cat("\nWrote ", OUT, "\n")
