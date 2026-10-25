# Derive SIM_NOISE_SD: the per-game margin noise the season and finals sims draw.
#
# Run from the torpverse root (arrow needs PowerShell on Windows):
#   powershell.exe -Command 'Rscript "torp/data-raw/05-validation/derive_sim_noise_sd.R"'
#
# The sims draw a result as Normal(estimate, SIM_NOISE_SD) on top of two other
# per-game noise terms already inside `estimate` (R/simulate.R):
#   - team quality residuals, drawn once per sim per team: N(mean, residual_se)
#   - injury noise, drawn per team per game: N(0, SIM_INJURY_SD)
# So a simulated margin's total spread is
#   sqrt(SIM_NOISE_SD^2 + 2 * mean(residual_se^2) + 2 * SIM_INJURY_SD^2).
# That total should match the match model's real out-of-sample error, which is
# what .heldout_margin_sigma() measures on honest predictions (torp#274: the
# live-locked rows plus the backfill_predictions_<season> files, never the
# in-sample 2021-2025 predictions_<season> rows). Solving for SIM_NOISE_SD
# gives the value below.
#
# Until 2026-10-26 the sims used 26 + |estimate|/3. On the honest rows the
# error is nearly flat in |pred| (slope ~0.06 points per point, p = 0.15,
# against the 0.33 that formula assumed), so it was too tight for close games
# and too loose for blowouts. This script prints that check too.

suppressMessages(devtools::load_all("torp", quiet = TRUE))
suppressMessages(devtools::load_all("torpmodels", quiet = TRUE))

rows <- torp:::.load_heldout_margin_rows()
rows <- rows[is.finite(rows$margin) & is.finite(rows$pred_margin), ]
cat("honest rows:", nrow(rows), "\n")
stopifnot(nrow(rows) >= MARGIN_SIGMA_MIN_N)
sigma <- torp:::.heldout_margin_sigma(rows)

res <- torp:::.extract_team_residuals(season = get_afl_season())
stopifnot(!is.null(res), nrow(res) >= 18)
cat("team residual_se: n", nrow(res), " mean", round(mean(res$residual_se), 3),
    " range", paste(round(range(res$residual_se), 3), collapse = "-"), "\n")
var_ratings <- 2 * mean(res$residual_se^2)
var_injury  <- 2 * SIM_INJURY_SD^2
noise_sd <- sqrt(sigma^2 - var_ratings - var_injury)

cat(sprintf("honest margin error SD   %.2f over %d matches\n", sigma, nrow(rows)))
cat(sprintf("  minus team residuals   %.2f^2  (2 x mean residual_se^2)\n", sqrt(var_ratings)))
cat(sprintf("  minus injury noise     %.2f^2  (2 x SIM_INJURY_SD^2)\n", sqrt(var_injury)))
cat(sprintf("=> SIM_NOISE_SD          %.2f  (in use: %s)\n", noise_sd, SIM_NOISE_SD))

# Check: is the error flat in |pred|? (the old formula assumed a 1/3 slope)
err <- rows$margin - rows$pred_margin
ap <- abs(rows$pred_margin)
bin <- cut(ap, c(0, 6, 12, 24, Inf), right = FALSE)
other <- var_ratings + var_injury
tab <- do.call(rbind, lapply(split(seq_along(err), bin), function(i) data.frame(
  n = length(i), mean_abs_pred = mean(ap[i]), actual_sd = sd(err[i]),
  old_sim_total = sqrt((26 + mean(ap[i]) / 3)^2 + other),
  new_sim_total = sqrt(noise_sd^2 + other))))
cat("\nError SD by |predicted margin| (points; the sim totals should match actual_sd):\n")
print(round(tab, 2))
print(summary(lm(abs(err) ~ ap))$coefficients)

# Win-probability scoring of the two rules (lower is better)
won <- as.numeric(rows$margin > 0)
score <- function(sd) {
  p <- pmin(pmax(pnorm(rows$pred_margin / sd), 1e-12), 1 - 1e-12)
  c(log_loss = -mean(won * log(p) + (1 - won) * log(1 - p)), brier = mean((p - won)^2))
}
cat("\nlog-loss / Brier, n =", nrow(rows), "(lower is better)\n")
print(round(rbind(old_26_plus_abs_over_3 = score(sqrt((26 + ap / 3)^2 + other)),
                  new_flat = score(sqrt(noise_sd^2 + other))), 4))
