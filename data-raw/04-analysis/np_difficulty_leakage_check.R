# Is the disposal-difficulty model leaking its own target?
# =============================================================================
# Pete, 2026-09-10, on being told the p_hat = 1 rows were "the model being
# right": "is this data leakage - this seems far too confident."
#
# In build_disposal_events(), all three of these come off the SAME resolving
# row (`out_*`, found by scanning 1-6 rows ahead past in-flight annotations):
#
#   turnover = out_tid != team_id                    <- THE TARGET
#   kick_len = sqrt((out_x - x)^2 + (out_y - y)^2)   <- a FEATURE, from out_x/out_y
#   fwd_gain = out_x - x                             <- a FEATURE, from out_x
#
# So two features are functions of the coordinates of the event whose team
# membership is the label. fit_disposal_models()'s own docstring says "Every
# term must be knowable BEFORE the disposal resolves ... the outcome
# description is not and must never appear" -- the outcome DESCRIPTION was
# excluded, but the outcome COORDINATES came in through kick_len and fwd_gain.
#
# The mechanism to look for: a free kick against, a smother, holding the ball
# are all recorded at or near the disposal spot, so kick_len ~ 0 and the model
# can read "turnover" straight off it without knowing any football.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_difficulty_leakage_check.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
de  <- as.data.table(build_disposal_events(ch, pbp))
de  <- de[!is.na(turnover)]
say("disposals: ", format(nrow(de), big.mark = ","),
    "   overall turnover rate ", round(100 * mean(de$turnover), 1), "%")

say("\n=== turnover rate by kick_len, the suspect feature ===")
de[, kl_bin := cut(kick_len, breaks = c(-Inf, 0.5, 1, 2, 5, 10, 20, 30, 40, 50, Inf),
                   labels = c("0-0.5", "0.5-1", "1-2", "2-5", "5-10", "10-20",
                              "20-30", "30-40", "40-50", "50+"))]
print(de[, .(n = .N, turnover_rate = round(100 * mean(turnover), 1)), by = kl_bin][order(kl_bin)])

say("\n=== the extreme: how deterministic is kick_len ~ 0? ===")
for (thr in c(0.01, 0.1, 0.5, 1, 2)) {
  s <- de[kick_len <= thr]
  if (nrow(s) == 0) next
  say("  kick_len <= ", formatC(thr, width = 4), " : n=", formatC(nrow(s), width = 6),
      "   turnover rate ", formatC(100 * mean(s$turnover), format = "f", digits = 2), "%")
}

say("\n=== and fwd_gain, the other one derived from out_x ===")
de[, fg_bin := cut(fwd_gain, breaks = c(-Inf, -20, -5, -0.5, 0.5, 5, 20, Inf))]
print(de[, .(n = .N, turnover_rate = round(100 * mean(turnover), 1)), by = fg_bin][order(fg_bin)])

say("\n=== what ARE the near-zero-length disposals? ===")
say("(if these are frees/smothers recorded at the disposal spot, the feature is")
say(" reading the outcome, not the decision)")
print(head(de[kick_len <= 1, .N, by = out_desc][order(-N)], 10))

say("\n=== a leak-free sanity model: can kick_len ALONE predict the target? ===")
say("A feature knowable before the kick should not, on its own, separate the")
say("outcome nearly perfectly.")
fit <- stats::glm(turnover ~ kick_len, data = de, family = stats::binomial())
p1 <- stats::predict(fit, type = "response")
ll <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p)) }
base <- mean(de$turnover)
say("  intercept-only log loss : ", round(ll(de$turnover, rep(base, nrow(de))), 5))
say("  kick_len ALONE log loss : ", round(ll(de$turnover, p1), 5))
say("  (the full 8-feature GAM scored 0.42554 out-of-fold)")

say("\n=== how many of the p_hat >= 0.999 rows are short disposals? ===")
tm <- fread("data-raw/outputs/np_difficulty_terms_2025_2026.csv")
tm[, match_id := as.character(match_id)]
hot <- tm[substr(match_id, 5, 8) == as.character(SEASON) & p_hat >= 0.999,
          .(match_id, display_order)]
h <- merge(hot, de[, .(match_id, display_order, kick_len, turnover, out_desc)],
           by = c("match_id", "display_order"))
say("  p_hat >= 0.999 rows matched: ", nrow(h))
say("  median kick_len there: ", round(median(h$kick_len, na.rm = TRUE), 2),
    "   vs ", round(median(de$kick_len, na.rm = TRUE), 2), " overall")
say("  share with kick_len <= 2: ", round(100 * mean(h$kick_len <= 2, na.rm = TRUE), 1),
    "%   vs ", round(100 * mean(de$kick_len <= 2, na.rm = TRUE), 1), "% overall")
