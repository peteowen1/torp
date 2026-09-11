# Are the contest model's 99% predictions EARNED? (issue #209)
# =============================================================================
# Pete: "I thought we got rid of 99% odds of a contest win?"
#
# Two different models, and only one was changed:
#
#   DISPOSAL model (epv_difficulty.R)  -- #210 removed its leaked features.
#                                         Now 0 rows at p >= 0.99, p99.9 = 0.7402.
#   CONTEST model  (epv_v3.R)          -- untouched. Still 18 rows at p >= 0.99,
#                                         max exactly 1.0. All 18 survivors are
#                                         from here.
#
# #213 cleared the contest model of LEAKAGE (physically plausible kick lengths,
# smooth monotone gradient). It never checked whether the confident predictions
# are CORRECT. Those are independent properties: a clean model can still be
# badly overconfident.
#
# AND THE GRADIENT TABLE IN np_209_true_mechanism.R CANNOT ANSWER IT. That read
# from the terms table, whose contested rows survive
# `def_win == TRUE | out_desc %chin% EPV3_DUEL_OUT` (epv_net_points.R:870) --
# filtered ON the outcome. That is why it showed a flat ~88-90% def-win rate in
# every p_hat band, including bands below 0.5. A selection artifact, not
# calibration. It has to be measured on the unfiltered build_aerial_contests()
# table, out of fold.
#
# WHY IT DECIDES WHAT #209 IS:
#   well calibrated at the top  -> the model is right, the contest really was
#                                  ~certain, and #209 is purely Pete's design
#                                  question: pay for an expected win, or not?
#   overconfident at the top    -> #209 also has a MODEL defect underneath, and
#                                  the credit collapse is partly the model being
#                                  wrong rather than the outcome being unsurprising.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_calibration_top_end.R"'
suppressMessages({library(data.table); library(mgcv); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
set.seed(20260911)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
cst <- as.data.table(build_aerial_contests(ch, pbp))
cst[, y := as.integer(def_win)]
cst[, abs_ky := abs(kick_y)]
say("aerial contests: ", format(nrow(cst), big.mark = ","),
    "   base rate, defence wins: ", round(100 * mean(cst$y), 1), "%")

# Out of fold, grouped by match -- an in-sample fit would flatter the top end,
# which is exactly the region in question.
K <- 5
mids <- unique(cst$match_id)
cst <- merge(cst, data.table(match_id = mids,
                             fold = sample(rep_len(1:K, length(mids)))), by = "match_id")
rhs <- y ~ s(att_x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
  s(exp_pts) + i50f

R <- rbindlist(lapply(1:K, function(k) {
  say("fold ", k, " ...")
  g <- mgcv::bam(rhs, data = droplevels(cst[fold != k]),
                 family = stats::binomial(), discrete = TRUE)
  data.table(y = cst[fold == k, y],
             p = as.numeric(stats::predict(g, newdata = cst[fold == k], type = "response")))
}))

say("\n=== calibration across the range ===")
say("predicted is the model's mean p in the band; actual is how often the")
say("defence really won. They should match. n is the band's size.")
R[, band := cut(p, c(-Inf, .1, .3, .5, .7, .8, .9, .95, .99, Inf),
                labels = c("<.1", ".1-.3", ".3-.5", ".5-.7", ".7-.8", ".8-.9",
                           ".9-.95", ".95-.99", ">=.99"))]
print(R[, .(n = .N,
            predicted = round(mean(p), 4),
            actual = round(mean(y), 4),
            gap = round(mean(p) - mean(y), 4)), by = band][order(band)])

say("\n=== the top end, where #209 lives ===")
for (thr in c(0.95, 0.99, 0.999)) {
  s <- R[p >= thr]
  if (nrow(s) > 0) {
    say("  p >= ", thr, " : n ", formatC(nrow(s), width = 5),
        "   mean predicted ", round(mean(s$p), 5),
        "   ACTUAL def-win ", round(mean(s$y), 4),
        "   (", sum(s$y), " of ", nrow(s), ")")
  } else {
    say("  p >= ", thr, " : no rows out of fold")
  }
}

say("\n=== VERDICT ===")
hi <- R[p >= 0.99]
if (nrow(hi) == 0) {
  say("  No out-of-fold row reaches 0.99, though the production regime (fit on")
  say("  2025, score 2026) does produce 18. Different regime, so this neither")
  say("  confirms nor refutes -- rerun scoring 2026 off a 2025 fit to settle it.")
} else {
  act <- mean(hi$y)
  say("  ", nrow(hi), " rows at p >= 0.99; the defence actually won ",
      round(100 * act, 1), "% of them.")
  if (act >= 0.95) {
    say("  EARNED. The model is right at the top end, so #209 is purely a design")
    say("  question: should an expected win be paid at all?")
  } else {
    say("  NOT EARNED. The model is overconfident where it matters most, so #209")
    say("  has a model defect underneath the design question -- the credit")
    say("  collapse is partly the model being wrong, not the outcome being dull.")
  }
}
