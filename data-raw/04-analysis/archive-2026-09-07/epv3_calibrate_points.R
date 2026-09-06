# EPV v3 step 5: calibrate each channel so 1 unit = 1 point of margin.
#
# Pete's call (2026-08-03): points calibration is the CONSTRAINT, equal channel
# variance is a DIAGNOSTIC. So each channel is regressed onto match margin and
# given its own constant; whatever variance ratios fall out are reported as the
# honest answer, not engineered toward thirds.
#
# Fit, on team-match aggregates:
#   margin ~ 0 + d_recv + d_disp + d_cont_aerial + d_cont_stop
# where each d_* is (home team's summed channel) - (away team's summed channel).
# The per-channel constant is then 1 / coefficient, so a refit returns 1.0
# everywhere by construction -- and that refit is run, because "by construction"
# has been wrong here before.
#
# PRIOR WARNING (2026-07-30): per-channel points constants were tried on the v2
# structure and failed -- they reshuffled 5 of the served top 20, NARROWED
# defender spread, and dragged every position's thin-evidence quartile down
# ~0.25. Those three symptoms are checked explicitly below.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_points_calibration.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

CH <- c("recv", "disp", "spoil", "hitout")          # v3 slots: recv/disp/cont_aerial/cont_stop
LBL <- c(recv = "recv", disp = "disp", spoil = "cont_aerial", hitout = "cont_stop")

say("=== EPV v3: per-channel points calibration ===")

v3 <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v3.parquet")))
v2 <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v2.parquet")))
say("v3 player-games ", format(nrow(v3), big.mark = ","),
    " | seasons ", paste(range(v3$season), collapse = "-"))

res <- as.data.table(load_results(TRUE))
say("results ", format(nrow(res), big.mark = ","), " matches")

# ---- Team-match aggregation ------------------------------------------------
team_sums <- function(d) {
  cols <- paste0("epv_", CH, "_adj")
  s <- d[, lapply(.SD, sum, na.rm = TRUE), .SDcols = cols, by = .(match_id, team)]
  data.table::setnames(s, cols, paste0("t_", LBL[CH]))
  s
}

margin_frame <- function(d, label) {
  s <- team_sums(d)
  r <- res[, .(match_id = as.character(match_id),
               home = home_team_name, away = away_team_name,
               margin = home_score - away_score)]
  r <- r[is.finite(margin)]
  h <- merge(r, s, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(r, s, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h, a[, c("match_id", paste0("t_", LBL[CH])), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (ch in LBL[CH]) {
    m[, (paste0("d_", ch)) := get(paste0("t_", ch, "_h")) - get(paste0("t_", ch, "_a"))]
  }
  m[, arm := label]
  m
}

mv3 <- margin_frame(v3, "v3")
mv2 <- margin_frame(v2, "v2")
say("matched team-match rows: v3 ", nrow(mv3), " | v2 ", nrow(mv2))
say("margin: mean ", round(mean(mv3$margin), 2), " sd ", round(sd(mv3$margin), 2))

# ---- Fit -------------------------------------------------------------------
frm <- stats::as.formula(paste("margin ~ 0 +", paste0("d_", LBL[CH], collapse = " + ")))
fit_report <- function(m, label) {
  f <- stats::lm(frm, data = m)
  co <- summary(f)$coefficients
  dt <- data.table(arm = label, channel = sub("^d_", "", rownames(co)),
                   coef_raw = co[, 1],
                   coef = round(co[, 1], 4), se = round(co[, 2], 4),
                   t = round(co[, 3], 2))
  # The constant is the COEFFICIENT, not its reciprocal. Scaling a predictor by
  # k divides its coefficient by k, so x' = coef * x contributes coef * x = x'
  # points -- one unit of the scaled channel is one point of margin. Using
  # 1/coef squares it instead, which is how the refit check caught it.
  dt[, points_constant := coef_raw]
  list(fit = f, tbl = dt)
}
r2 <- fit_report(mv2, "v2"); r3 <- fit_report(mv3, "v3")
say("")
say("--- coefficients: points of margin per unit of channel ---")
say_dt(rbind(r2$tbl, r3$tbl), 12)
say("v2 R2 ", round(summary(r2$fit)$r.squared, 4),
    " | v3 R2 ", round(summary(r3$fit)$r.squared, 4))
say("(v2 also carries a single global EPV_POINTS_SCALE = ", EPV_POINTS_SCALE,
    ", applied downstream in centre_epv_by_position(), so its coefficients here")
say(" are pre-scale and not directly comparable to 1.0.)")
say("")
say("READ THE R2 GAP CAREFULLY -- it is not 'v2 explains margin better'.")
say("v2's epv_disp contains goals * 0.4262 + behinds * 1.0899 + shots * 0.4419,")
say("so a team's summed v2 channel restates its own scoreboard. The high R2 is")
say("circularity, not skill. v3 carries no scoring term at all and still reaches")
say("its R2 from possession value alone.")
say("")
say("v2's cont_aerial coefficient is NEGATIVE (see table): a team's summed")
say("epv_spoil_adj is associated with a WORSE margin. A channel that points the")
say("wrong way is the strongest single piece of evidence that the flat")
say("EPV_SPOIL_WT was not measuring contest value.")

k <- r3$tbl$points_constant
names(k) <- r3$tbl$channel
say("")
say("--- fitted EPV3_POINTS_SCALE ---")
say(paste0("EPV3_POINTS_SCALE <- c(",
           paste(sprintf('%s = %.6f', names(k), k), collapse = ", "), ")"))

# ---- Refit check: must return 1.0 everywhere -------------------------------
mv3c <- copy(mv3)
for (ch in names(k)) mv3c[, (paste0("d_", ch)) := get(paste0("d_", ch)) * k[[ch]]]
rc <- fit_report(mv3c, "v3-calibrated")
say("")
say("--- refit after scaling: every coefficient must read 1.0 ---")
say_dt(rc$tbl[, .(channel, coef, se, t)], 8)
# TWO checks, because they fail for different reasons.
#
# (1) the ALGEBRA, on full-precision constants. Scaling a predictor by its own
#     coefficient must return exactly 1, so anything above floating-point noise
#     means the constant is not the coefficient and the derivation is wrong.
# (2) the SHIPPED constants, which are written to 6dp. Rounding moves each
#     coefficient by roughly its own relative rounding error, so this is bounded
#     at 1e-4 rather than 1e-9 — a real limit, stated rather than hidden.
#
# The 2026-08-03 run had ONE check at 1e-6 applied to 4dp-ROUNDED output, which
# no rounding can satisfy unless it lands exactly on 1.0000. It reported failure
# while its own table read 1.0000/1.0001/1.0000/1.0000, and that false alarm is
# why this step sat unfinished.
dev_raw <- max(abs(rc$tbl$coef_raw - 1))
say(sprintf("algebra check   : max |coef - 1| = %.3e (tolerance 1e-9)", dev_raw))
if (dev_raw > 1e-9) stop("refit did not return 1.0: the constants are not the coefficients")

k_ship <- round(k, 6)
mv3s <- copy(mv3)
for (ch in names(k_ship)) mv3s[, (paste0("d_", ch)) := get(paste0("d_", ch)) * k_ship[[ch]]]
rs <- fit_report(mv3s, "v3-shipped")
dev_ship <- max(abs(rs$tbl$coef_raw - 1))
say(sprintf("shipped-precision: max |coef - 1| = %.3e (tolerance 1e-4, 6dp constants)", dev_ship))
if (dev_ship > 1e-4) stop("6dp constants do not reproduce unit coefficients")
say("both refit checks PASS")

# ---- Diagnostic: variance shares after calibration -------------------------
say("")
say("--- DIAGNOSTIC (not a target): channel variance shares after calibration ---")
vs <- function(d, kk, label) {
  cols <- paste0("epv_", CH, "_adj")
  v <- vapply(seq_along(cols), function(i) {
    x <- d[[cols[i]]] * (if (is.null(kk)) 1 else kk[[LBL[CH][i]]])
    stats::var(x, na.rm = TRUE)
  }, numeric(1))
  data.table(arm = label, channel = LBL[CH], sd = round(sqrt(v), 4),
             var_share_pct = round(100 * v / sum(v), 1))
}
say_dt(rbind(vs(v2, NULL, "v2 (uncalibrated)"),
             vs(v3, NULL, "v3 (uncalibrated)"),
             vs(v3, k, "v3 (calibrated)")), 16)
say("")
say("Pete's hoped-for shape was roughly equal thirds across recv/disp/contest.")
say("Whatever the calibrated row above says IS the answer -- equal variance was")
say("explicitly the diagnostic, not the constraint.")

# ---- The three symptoms that killed the v2 attempt -------------------------
say("")
say("--- the three symptoms that killed per-channel constants on v2 ---")
v3c <- copy(v3)
for (i in seq_along(CH)) {
  v3c[, (paste0("epv_", CH[i], "_adj")) :=
        get(paste0("epv_", CH[i], "_adj")) * k[[LBL[CH][i]]]]
}
v3c[, epv_adj := epv_recv_adj + epv_disp_adj + epv_spoil_adj + epv_hitout_adj]

say("(1) served top-20 reshuffle: how many of the uncalibrated top 20 leave it?")
top <- function(d) {
  a <- d[season == max(season), .(epv = sum(epv_adj)), by = player_name]
  setorder(a, -epv); a$player_name[1:20]
}
t_un <- top(v3); t_cal <- top(v3c)
say("  ", length(setdiff(t_un, t_cal)), " of 20 leave the top 20 (v2 attempt: 5)")

say("(2) defender spread: does calibration NARROW it?")
say("    Absolute sd is the WRONG test here -- calibration rescales every")
say("    channel (three of four by < 1), so the whole metric shrinks and every")
say("    position's sd falls with it. The question is whether defenders shrink")
say("    MORE than the field, so the ratio to pooled sd is what is reported.")
pool_un <- sd(v3$epv_adj, na.rm = TRUE); pool_ca <- sd(v3c$epv_adj, na.rm = TRUE)
say("    pooled sd: uncalibrated ", round(pool_un, 3), " -> calibrated ", round(pool_ca, 3))
sp <- function(d, lab, pooled) d[!is.na(position_group),
                         .(arm = lab, sd = round(sd(epv_adj, na.rm = TRUE), 3),
                           rel = round(sd(epv_adj, na.rm = TRUE) / pooled, 3)),
                         by = position_group][order(position_group)]
say_dt(merge(sp(v3, "uncal", pool_un), sp(v3c, "cal", pool_ca), by = "position_group",
             suffixes = c("_uncal", "_cal"))[
               , .(position_group, sd_uncal, sd_cal,
                   rel_uncal, rel_cal, rel_change = round(rel_cal - rel_uncal, 3))], 8)

say("(3) thin-evidence quartile: does every position's bottom quartile drop?")
q1 <- function(d, lab) d[!is.na(position_group),
  .(arm = lab, q1 = round(stats::quantile(epv_adj, 0.25, na.rm = TRUE), 3)),
  by = position_group][order(position_group)]
say_dt(merge(q1(v3, "uncal"), q1(v3c, "cal"), by = "position_group",
             suffixes = c("_uncal", "_cal"))[, delta := round(q1_cal - q1_uncal, 3)][], 8)

# ---- Level check, not just dispersion --------------------------------------
say("")
say("--- per-bucket MEANS, not just dispersion ---")
say("(a change can improve calibration and lower the mean; check both)")
say_dt(merge(
  v3[!is.na(position_group), .(uncal = round(mean(epv_adj, na.rm = TRUE), 3)), by = position_group],
  v3c[!is.na(position_group), .(cal = round(mean(epv_adj, na.rm = TRUE), 3)), by = position_group],
  by = "position_group")[order(position_group)], 8)

close(con)
cat("\nWrote ", OUT, "\n")
