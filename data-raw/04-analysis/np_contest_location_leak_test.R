# Do the contest model's LOCATION features leak the outcome? (issues #209, #210)
# =============================================================================
# Pete: "any leakage in the contest one?"
#
# What #213 actually tested: whether `kick_len` is physically plausible. It
# mostly is (median 33.8m; 16 of 50,050 over 100m). That is ONE test.
#
# What #213 did NOT test, and asserted instead: that `att_x = out_x`,
# `abs_y = |out_y|`, `goal_dist` and `i50f` -- every location feature, all
# derived from the resolving row -- are legitimate because the KICKER chooses
# where the ball lands, so "given the ball arrived here, who won it" is a fair
# conditional question. That is an argument, not a measurement.
#
# The signature is uncomfortable enough to force the measurement: a strictly
# pre-kick arm (kick origin + exp_pts only) keeps just 25.7% of the model's gain,
# so the landing-point features carry ~74% -- the same order as the disposal
# leak's 77%.
#
# THE SPECIFIC UNTESTED CHANNEL. Chains logs a Spoil somewhere. If it records
# where the DEFENDER punched from rather than where the ball was heading, then
# out_x/out_y is partly CREATED BY who won, and spoils are overwhelmingly
# defensive wins -- so it feeds straight into `def_win`.
#
# Note EPV3_CONTEST_POPULATION is "all", so the fit also includes Uncontested
# Mark and Mark On Lead, which are receptions rather than duels.
#
# THREE TESTS, verdict rules written before looking:
#
#  (1) GEOMETRY BY OUTCOME, KICK ORIGIN HELD FIXED. For kicks from the same
#      origin cell, is the recorded resolution position systematically different
#      for a Spoil than for a Contested Mark? Both are contested aerial
#      outcomes, so a large systematic gap is the outcome placing the
#      coordinate, not the kick.
#        LEAK if the spoil/mark difference in kick_len is large (>5m) and in a
#        consistent direction within origin cells.
#
#  (2) CAN THE COORDINATES RECONSTRUCT THE OUTCOME TYPE? out_desc is correctly
#      excluded as a feature. If (out_x, out_y) given the kick origin predicts
#      Spoil-vs-Mark well, the coordinates carry the outcome anyway.
#        LEAK if AUC for predicting spoil-vs-mark from geometry alone is high
#        (>0.75) -- the model can then infer the outcome type, which is nearly
#        the target.
#
#  (3) THE HONEST BASELINE. Everything strictly knowable before the ball lands.
#      Sizes what is at stake rather than proving anything by itself.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_location_leak_test.R"'
suppressMessages({library(data.table); library(mgcv); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
set.seed(20260911)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
cst <- as.data.table(build_aerial_contests(ch, pbp))
cst[, `:=`(y = as.integer(def_win), abs_ky = abs(kick_y))]
cst[, otype := fcase(
  grepl("^Spoil", out_desc), "spoil",
  out_desc %chin% c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)"), "contested_mark",
  default = "reception")]

say("EPV3_CONTEST_POPULATION = ", EPV3_CONTEST_POPULATION,
    "  (so receptions are in the fit, not just duels)")
say("\ncontests by outcome type -- def_win is the TARGET:")
print(cst[, .(n = .N, pct = round(100 * .N / nrow(cst), 1),
              def_win_pct = round(100 * mean(y), 1),
              median_kick_len = round(median(kick_len), 1),
              median_out_x = round(median(out_x), 1)), by = otype][order(-n)])
say("\nIf def_win_pct is near 0 or 100 within a type, then knowing the TYPE is")
say("almost knowing the ANSWER -- which is why out_desc is excluded. The")
say("question is whether the coordinates smuggle it back in.")

# --- (1) geometry by outcome, holding the kick origin fixed -------------------
say("\n=== (1) same kick origin, different outcome: does the geometry differ? ===")
cst[, ox_cell := cut(kick_x, seq(-100, 100, by = 20))]
cst[, oy_cell := cut(kick_y, seq(-80, 80, by = 40))]
g <- cst[otype %chin% c("spoil", "contested_mark"),
         .(n = .N, med_len = median(kick_len)), by = .(ox_cell, oy_cell, otype)]
w <- dcast(g, ox_cell + oy_cell ~ otype, value.var = c("n", "med_len"))
w <- w[!is.na(n_spoil) & !is.na(n_contested_mark) &
         n_spoil >= 30 & n_contested_mark >= 30]
w[, diff_m := med_len_spoil - med_len_contested_mark]
say("origin cells with 30+ of each: ", nrow(w))
if (nrow(w) > 0) {
  say("  median kick_len difference (spoil - contested mark), in metres:")
  say("    mean ", round(mean(w$diff_m), 2), "m   median ", round(median(w$diff_m), 2),
      "m   range ", round(min(w$diff_m), 1), " to ", round(max(w$diff_m), 1))
  say("    cells where spoils are recorded FURTHER out: ", w[diff_m > 0, .N], " of ", nrow(w))
}

# --- (2) can geometry reconstruct the outcome type? --------------------------
say("\n=== (2) can the coordinates alone tell a spoil from a contested mark? ===")
d2 <- cst[otype %chin% c("spoil", "contested_mark")]
d2[, is_spoil := as.integer(otype == "spoil")]
mids <- unique(d2$match_id)
d2 <- merge(d2, data.table(match_id = mids,
                           fold = sample(rep_len(1:5, length(mids)))), by = "match_id")
auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (as.numeric(n1) * as.numeric(n0)) }
P <- rbindlist(lapply(1:5, function(k) {
  m <- mgcv::bam(is_spoil ~ s(att_x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist),
                 data = droplevels(d2[fold != k]), family = stats::binomial(), discrete = TRUE)
  data.table(y = d2[fold == k, is_spoil],
             p = as.numeric(stats::predict(m, newdata = d2[fold == k], type = "response")))
}))
a <- auc(P$y, P$p)
say("  AUC predicting spoil-vs-contested-mark from GEOMETRY ONLY: ", round(a, 4))
say("  (0.5 = geometry knows nothing about the outcome type; 1.0 = it knows it)")

# --- (3) the honest pre-kick baseline ----------------------------------------
say("\n=== (3) what is knowable strictly before the ball lands? ===")
cst2 <- merge(cst, data.table(match_id = mids,
                              fold = sample(rep_len(1:5, length(mids)))),
              by = "match_id", all.x = TRUE)
cst2 <- cst2[!is.na(fold)]
ll <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p)) }
FORMS <- list(
  SHIPPED = y ~ s(att_x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) + s(exp_pts) + i50f,
  PREKICK = y ~ s(kick_x, abs_ky) + s(exp_pts))
R <- rbindlist(lapply(1:5, function(k) {
  o <- data.table(y = cst2[fold == k, y])
  for (nm in names(FORMS)) {
    m <- mgcv::bam(FORMS[[nm]], data = droplevels(cst2[fold != k]),
                   family = stats::binomial(), discrete = TRUE)
    o[, (nm) := as.numeric(stats::predict(m, newdata = cst2[fold == k], type = "response"))]
  }
  o
}))
b <- ll(R$y, rep(mean(R$y), nrow(R)))
say("  logloss, lower is better. intercept only ", round(b, 5))
for (nm in names(FORMS)) {
  say("  ", formatC(nm, width = -8), round(ll(R$y, R[[nm]]), 5),
      "   keeps ", round(100 * (b - ll(R$y, R[[nm]])) / (b - ll(R$y, R$SHIPPED)), 1),
      "% of SHIPPED's gain")
}

say("\n=== VERDICT ===")
say("Rules were written before looking. Test (2) is the decisive one: if")
say("geometry alone identifies the outcome TYPE, the coordinates carry the")
say("outcome regardless of any argument about who chooses the landing spot.")
if (a > 0.75) {
  say("  LEAK INDICATED. AUC ", round(a, 3), " > 0.75 -- the coordinates largely")
  say("  reconstruct spoil-vs-mark, and knowing that is close to knowing def_win.")
  say("  The 'the kicker chose the spot' argument does not survive this.")
} else if (a > 0.65) {
  say("  PARTIAL. AUC ", round(a, 3), " -- the geometry carries real outcome-type")
  say("  information but does not determine it. Needs the row-level look with Pete.")
} else {
  say("  NO LEAK INDICATED by this test. AUC ", round(a, 3), " -- geometry barely")
  say("  distinguishes the outcome type, so conditioning on the landing spot is")
  say("  defensible and the model's confidence is about the kick, not the result.")
}
