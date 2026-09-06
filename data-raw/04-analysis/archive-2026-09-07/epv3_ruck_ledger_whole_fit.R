# Did the ruck ledger improve the WHOLE fit, or just move signal between
# channels?
#
# The ledger raised the contest channel's t from 4.43 to 4.84 -- and I reported
# that as a win before checking the other two, which both FELL (recv 8.72 ->
# 8.09, disp 8.91 -> 8.12). Three channels, one up and two down, is the exact
# shape of a redistribution rather than an improvement, and individual t
# statistics cannot tell them apart.
#
# The number that can is the fit as a whole: R2 and residual MAE against
# xmargin, plus the same against actual margin as a cross-check. If the ledger
# is genuinely cleaner the whole fit improves; if it is redistribution the whole
# fit is flat or worse and the contest gain was borrowed from its neighbours.
#
# Also checks the cont_stop coefficient's jump 6.81 -> 18.01. A channel that has
# to be multiplied 18x to read in points is a small-spread channel, and small
# spread means whatever noise it carries gets amplified with it.
#
# PERFORMANCE: both rating frames are cached; this is linear fits only. Seconds.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_ruck_ledger_wholefit.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

teams  <- load_teams(TRUE)
res    <- as.data.table(load_results(TRUE))
xg     <- as.data.table(load_xg(TRUE))
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
tgt <- merge(res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin) & is.finite(xmargin)]

SUB <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")

assess <- function(file, label) {
  rt <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, file)))
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", SUB), with = FALSE],
             a[, c("match_id", SUB), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in SUB) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id")

  out <- list(label = label, sd_stop = sd(m$d_epr_hitout))
  for (tg in c("xmargin", "margin")) {
    # four-component fit (before the merge)
    f4 <- lm(as.formula(paste(tg, "~ 0 +", paste0("d_", SUB, collapse = " + "))), data = m)
    k <- coef(f4)
    # three-channel fit, sub-components calibrated then merged
    mc <- copy(m)
    for (v in SUB) mc[, (paste0("d_", v)) := get(paste0("d_", v)) * k[[paste0("d_", v)]]]
    mc[, d_cont := d_epr_spoil + d_epr_hitout]
    f3 <- lm(as.formula(paste(tg, "~ 0 + d_epr_recv + d_epr_disp + d_cont")), data = mc)
    out[[paste0("r2_4_", tg)]]  <- summary(f4)$r.squared
    out[[paste0("r2_3_", tg)]]  <- summary(f3)$r.squared
    out[[paste0("mae_3_", tg)]] <- mean(abs(residuals(f3)))
    out[[paste0("n_", tg)]]     <- nrow(mc)
  }
  out
}

say("=== Ruck ledger: whole-fit check ===")
say("The contest channel gained t 4.43 -> 4.84 while recv and disp both fell.")
say("One up, two down is redistribution's signature. This asks whether the fit")
say("as a whole improved.")

A <- assess("epv3_ratings_v3.parquet",   "attendance (current)")
B <- assess("epv3_rt_ruckledger.parquet", "ledger (zero-sum)")

say("")
say("=== WHOLE-FIT COMPARISON ===")
tab <- data.table(
  arm       = c(A$label, B$label),
  R2_4comp  = round(c(A$r2_4_xmargin, B$r2_4_xmargin), 5),
  R2_3chan  = round(c(A$r2_3_xmargin, B$r2_3_xmargin), 5),
  MAE_3chan = round(c(A$mae_3_xmargin, B$mae_3_xmargin), 4),
  sd_stop   = round(c(A$sd_stop, B$sd_stop), 4)
)
say("target = xmargin")
say_dt(tab, 4)
say("")
say("target = margin (cross-check)")
say_dt(data.table(
  arm       = c(A$label, B$label),
  R2_4comp  = round(c(A$r2_4_margin, B$r2_4_margin), 5),
  R2_3chan  = round(c(A$r2_3_margin, B$r2_3_margin), 5),
  MAE_3chan = round(c(A$mae_3_margin, B$mae_3_margin), 4)), 4)

dR2 <- B$r2_3_xmargin - A$r2_3_xmargin
dMAE <- B$mae_3_xmargin - A$mae_3_xmargin
say("")
say("=== VERDICT ===")
say("delta R2 (3-channel, xmargin) ", round(dR2, 5))
say("delta MAE (3-channel, xmargin) ", round(dMAE, 4), "   (negative = better)")
say("stoppage channel sd ", round(A$sd_stop, 4), " -> ", round(B$sd_stop, 4),
    "  (ratio ", round(B$sd_stop / A$sd_stop, 3), ")")
say("")
if (dR2 > 0.002 && dMAE < 0) {
  say("REAL IMPROVEMENT. The whole fit is better, so the contest gain was not")
  say("borrowed from recv and disp.")
} else if (abs(dR2) <= 0.002) {
  say("REDISTRIBUTION, not improvement. The whole fit is flat -- the contest")
  say("channel's t rose because signal MOVED into it from recv and disp, not")
  say("because the ledger created any. The contest t of 4.84 must not be quoted")
  say("as evidence the ruck metric got better.")
} else {
  say("WORSE. The ledger costs whole-fit accuracy.")
}
say("")
say("On the coefficient jump 6.81 -> 18.01: the stoppage channel's spread")
say("shrank by the ratio above, so it needs a proportionally larger multiplier")
say("to read in points. That is arithmetic, not evidence of value -- and a")
say("channel amplified 18x carries its noise up with it.")

close(con)
cat("\nDone\n")
