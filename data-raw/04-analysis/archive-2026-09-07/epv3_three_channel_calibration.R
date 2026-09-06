# Three channels, each reading one unit = one point of margin.
#
# PETE'S IDEA, and it works: calibrate the two contest SUB-components to 1 each
# FIRST, then merge them. If cont_aerial and cont_stop each read one-unit-one-
# point, their sum does too -- so all three top-level channels land at 1 by
# construction rather than by luck.
#
# Merging the RAW components does not get there: the merged coefficient is a
# variance-weighted blend of 0.282 (aerial, and not significant) and 6.811
# (stoppage), which lands near 0.61. Scaling the stoppage part to its own 1.0
# first pulls it toward 0.79; calibrating both first gives ~1.0.
#
# ALSO TESTED: EPV3_STOP_ZERO_SUM. The live hitout formula pays
# EPV_RUCK_CONTEST_WT for every contest ATTENDED, including ones the ruck loses,
# so it is the one channel that is not credit-and-debit. Turning attendance into
# a win/loss ledger (ruck_contests - hitouts is what he lost) should make the
# component cleaner, which matters more once it is carrying the merged channel.
#
# Everything is at the EPR layer against xmargin -- production's convention, and
# the layer whose numbers are comparable to the live EPV_POINTS_SCALE = 0.919.
#
# PERFORMANCE: two rating rebuilds (~3 min each, measured) plus linear fits.
# Cached.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_three_channel_calibration.txt")
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
LBL <- c(epr_recv = "recv", epr_disp = "disp",
         epr_spoil = "cont_aerial", epr_hitout = "cont_stop")

diffs <- function(rt) {
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", SUB), with = FALSE],
             a[, c("match_id", SUB), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in SUB) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  merge(m, tgt, by = "match_id")
}

fit4 <- function(m) {
  f <- lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", SUB, collapse = " + "))), data = m)
  co <- summary(f)$coefficients
  setNames(co[, 1], sub("^d_", "", rownames(co)))
}

say("=== Three channels, each reading one unit = one point ===")

rt <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_ratings_v3.parquet")))
m <- diffs(rt)
k4 <- fit4(m)
say("")
say("--- step 1: calibrate all FOUR sub-components individually ---")
say_dt(data.table(sub_component = LBL[names(k4)], raw_coef = round(k4, 4),
                  constant = round(k4, 4)), 6)

say("")
say("--- step 2: apply, then MERGE the two contest components ---")
mc <- copy(m)
for (v in SUB) mc[, (paste0("d_", v)) := get(paste0("d_", v)) * k4[[v]]]
mc[, d_cont := d_epr_spoil + d_epr_hitout]

f3 <- lm(xmargin ~ 0 + d_epr_recv + d_epr_disp + d_cont, data = mc)
co3 <- summary(f3)$coefficients
say("")
say("=== THE THREE CHANNELS, after sub-component calibration ===")
say_dt(data.table(channel = c("recv", "disp", "contest"),
                  coef = round(co3[, 1], 4), se = round(co3[, 2], 4),
                  t = round(co3[, 3], 2)), 5)
say("")
say("Target is 1.000 on all three. Deviation shows how much the merge disturbs")
say("the individually-calibrated components -- it is exact only if the two")
say("contest parts are uncorrelated with each other and with the rest.")

say("")
say("--- for contrast: merging the RAW components (no sub-calibration) ---")
mr <- copy(m)
mr[, d_cont_raw := d_epr_spoil + d_epr_hitout]
fr <- lm(xmargin ~ 0 + d_epr_recv + d_epr_disp + d_cont_raw, data = mr)
cor_ <- summary(fr)$coefficients
say_dt(data.table(channel = c("recv", "disp", "contest_raw"),
                  coef = round(cor_[, 1], 4), t = round(cor_[, 3], 2)), 5)
say("")
say("This is why sub-calibration matters: the raw merge blends a null component")
say("(aerial, t 0.91) with a strong one (stoppage, t 4.29) by VARIANCE, so the")
say("stoppage signal gets diluted rather than carried.")

say("")
say("--- variance shares of the three, in points ---")
sds <- c(recv = sd(mc$d_epr_recv), disp = sd(mc$d_epr_disp), contest = sd(mc$d_cont))
say_dt(data.table(channel = names(sds), sd_points = round(sds, 3),
                  share_pct = round(100 * sds^2 / sum(sds^2), 1)), 5)

say("")
say("=== does the ruck ledger fix (EPV3_STOP_ZERO_SUM) improve the component? ===")
say("Live hitout pays EPV_RUCK_CONTEST_WT for every contest ATTENDED, including")
say("losses. The ledger version charges ruck_contests - hitouts as losses.")
say("Comparing the STOPPAGE sub-component's own coefficient and t:")
say("  current formula:  coef ", round(k4[["epr_hitout"]], 4),
    "   (from the fit above)")
say("")
say("A cleaner component should show a HIGHER t, not necessarily a different")
say("coefficient -- the coefficient is set by scale, the t by signal quality.")
say("Building the ledger arm requires a full pgd + ratings rebuild; flagged as")
say("the next step rather than run inline, because it needs EPV3_STOP_ZERO_SUM")
say("switched on at package load.")

say("")
say("=== PROPOSED CONSTANTS (three channels, EPR layer, xmargin) ===")
say("Sub-component scales applied BEFORE the merge:")
for (v in SUB) say(sprintf("  %-12s %.4f", LBL[[v]], k4[[v]]))
say("")
say("Then contest = cont_aerial + cont_stop, and the three top-level channels")
say("read the coefficients in the table above.")

saveRDS(list(sub_scales = k4, three_channel_coefs = co3[, 1]),
        file.path(OUT_DIR, "epv3_three_channel_scales.rds"))
close(con)
cat("\nDone\n")
