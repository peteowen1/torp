# Pete's contest population: DUEL EVIDENCE REQUIRED (issues #209, #210)
# =============================================================================
# Decided 2026-09-11 after reading real chain sequences. A row is a contest only
# where chains shows an actual duel:
#
#   KEEP  a Contest Target row was logged in the in-flight span, OR
#         the outcome is Contested Mark / Pack Mark (P) / Pack Mark (O), OR
#         the outcome is a Spoil variant, OR
#         the outcome is Mark Fumbled / Mark Dropped   <- Pete added these
#   (and Contest Target -> Free For is kept by the first clause, also Pete)
#
#   DROP  plain receptions with no duel evidence: Uncontested Mark and Mark On
#         Lead. These are two different NON-contests -- a teammate marking the
#         kick (it worked) and an opponent marking it unopposed (an intercept).
#         Neither is a duel, and the trace that settled it was:
#             572  Kick Inside 50 Result   -     -                54  15
#             573  Uncontested Mark        GWS   Jayden Laverde   54  15
#         Richmond kicked inside 50, a GWS defender marked it UNCONTESTED.
#
# WHY THIS MIGHT FIX THE CALIBRATION WITHOUT ANY RECALIBRATION. The old
# population mixed three events whose def_win rates were 13.1% (reception),
# 41.6% (contested mark) and 100% (spoil). Which event a row was nearly
# determined the answer, and the model could not see the category -- out_desc is
# correctly excluded and geometry recovers it at AUC 0.555. So it chased an
# unseeable categorical, which is how a model ends up well calibrated below 0.8
# and significantly overconfident above it.
#
# TWO DEDUPLICATION TRAPS, both found in the traces and both handled here:
#   - Contested Mark -> Contested Mark (1,576) is ONE event in two coordinate
#     frames, same player, coordinates negated. Not two contestants.
#   - Mark Fumbled -> Mark Dropped (1,477 of 1,477) is likewise one event on two
#     rows at the same x/y.
#   build_aerial_contests() keys on the KICK and picks one resolving row via
#   .olag, so it should already collapse these -- verified below rather than
#   assumed.
#
# MEASURED, NOT SHIPPED. This changes nothing; it reports what the rule gives.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_population_pete_rule.R"'
suppressMessages({library(data.table); library(mgcv); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
set.seed(20260911)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]

# Widen the outcome set so the newly-included categories can even be reached,
# then apply Pete's duel-evidence filter. Uses the production builder.
WIDE <- unique(c(EPV3_AERIAL_OUT, "Mark Fumbled", "Mark Dropped", "Dropped Mark",
                 "Free For"))
orig <- EPV3_AERIAL_OUT
assignInNamespace("EPV3_AERIAL_OUT", WIDE, ns = "torp")
cst <- as.data.table(build_aerial_contests(ch, pbp))
assignInNamespace("EPV3_AERIAL_OUT", orig, ns = "torp")

cst[, `:=`(y = as.integer(def_win), has_target = !is.na(target_pid))]
DUEL_OUTS <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)",
               "Spoil", "Spoil gaining possession", "Spoil ineffective",
               "Mark Fumbled", "Mark Dropped", "Dropped Mark")
cst[, keep := has_target | out_desc %chin% DUEL_OUTS]

say("=== the rule applied ===")
say("widened build: ", format(nrow(cst), big.mark = ","), " rows")
print(cst[, .(n = .N, def_win_pct = round(100 * mean(y), 1)), by = keep][order(-n)])

say("\n=== what is KEPT, by outcome ===")
print(cst[keep == TRUE, .(n = .N, def_win_pct = round(100 * mean(y), 1),
                          with_target = sum(has_target)), by = out_desc][order(-n)])
say("\n=== what is DROPPED, by outcome ===")
print(cst[keep == FALSE, .(n = .N, def_win_pct = round(100 * mean(y), 1)),
          by = out_desc][order(-n)])

say("\n=== dedup check: is any kick counted twice? ===")
say("The frame duplicate (Contested Mark -> Contested Mark, negated coords) and")
say("Mark Fumbled -> Mark Dropped are each one event on two rows.")
d <- cst[keep == TRUE]
say("  rows: ", format(nrow(d), big.mark = ","))
say("  distinct (match_id, kick_do): ", format(uniqueN(d, by = c("match_id", "kick_do")), big.mark = ","))
say("  => duplicates: ", nrow(d) - uniqueN(d, by = c("match_id", "kick_do")))

# --- does the new population calibrate? --------------------------------------
say("\n=== the decisive test: does it calibrate now? ===")
say("Same features, same folds-by-match, out of fold. The old population was")
say("significantly overconfident from 0.8 to 0.99 (by up to 13.6 points).")
d[, abs_ky := abs(kick_y)]
mids <- unique(d$match_id)
d <- merge(d, data.table(match_id = mids, fold = sample(rep_len(1:5, length(mids)))),
           by = "match_id")
rhs <- y ~ s(att_x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) + s(exp_pts) + i50f
R <- rbindlist(lapply(1:5, function(k) {
  g <- mgcv::bam(rhs, data = droplevels(d[fold != k]),
                 family = stats::binomial(), discrete = TRUE)
  data.table(y = d[fold == k, y],
             p = as.numeric(stats::predict(g, newdata = d[fold == k], type = "response")))
}))
say("base rate, defence wins: ", round(100 * mean(d$y), 1), "%")
R[, band := cut(p, c(-Inf, .1, .3, .5, .7, .8, .9, .95, .99, Inf),
                labels = c("<.1", ".1-.3", ".3-.5", ".5-.7", ".7-.8", ".8-.9",
                           ".9-.95", ".95-.99", ">=.99"))]
cal <- R[, .(n = .N, predicted = round(mean(p), 4), actual = round(mean(y), 4),
             gap = round(mean(p) - mean(y), 4)), by = band][order(band)]
print(cal)
say("\nsaturation: rows at p >= 0.99: ", R[p >= 0.99, .N],
    "   max p ", round(max(R$p), 5))

say("\n=== VERDICT ===")
top <- cal[band %chin% c(".8-.9", ".9-.95", ".95-.99")]
worst <- if (nrow(top)) max(abs(top$gap), na.rm = TRUE) else NA_real_
say("  worst |gap| in the 0.8-0.99 region: ", round(worst, 4),
    "   (old population: 0.1355)")
if (is.finite(worst) && worst < 0.06) {
  say("  FIXED BY THE POPULATION. The overconfidence was a specification problem,")
  say("  not a calibration one -- no recalibration layer needed.")
} else {
  say("  STILL MISCALIBRATED. The population change alone does not fix it;")
  say("  recalibration is still on the table.")
}
