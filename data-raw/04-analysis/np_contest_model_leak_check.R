# Does the AERIAL CONTEST model leak its target the same way? (issue #210)
# =============================================================================
# The disposal model's leak is settled: kick_len / fwd_gain come off the same
# resolving row as `turnover`, and they are now out of the fit.
#
# But the v4 Net Points engine runs a SECOND model on the same shape, and it
# OVERWRITES p_hat on the rows it covers (epv_net_points.R:879). In
# build_aerial_contests():
#
#   def_win  = out_tid != kick_tid        <- the TARGET
#   att_x    = out_x                      \
#   abs_y    = |out_y|                     |  ALL from the resolving row
#   goal_dist, i50f = f(att_x, abs_y)      |
#   kick_len, fwd_gain = f(out_x, out_y)  /
#
# On its face that is worse than the disposal model, where at least x/abs_y were
# the disposer's own position. So the question has to be asked -- but it may be
# INNOCENT, and for a specific reason: `out_desc` is restricted to aerial
# outcomes (marks, spoils), so the resolving row is where the ball ARRIVED. A
# kicker chooses where the ball lands; the contest winner does not. Conditioning
# on "given the ball arrived here, who won it" is a legitimate question, and then
# kick_len really is the kick's length.
#
# THE DISCRIMINATING TEST is the same one that exposed the disposal leak, because
# it does not depend on any of that reasoning:
#
#   (1) Is kick_len PHYSICALLY PLAUSIBLE? A real kick is at most ~60m. If the
#       contest table shows 115m medians on the confident rows, the resolving row
#       is not the ball's arrival and the innocence argument is dead.
#   (2) Does .olag behave? If contests resolve at 1 row with sane distances, the
#       "next event is intrinsically distant" mechanism is absent here.
#   (3) Does p_hat saturate? Saturation is the symptom that started all of this.
#   (4) Does a CLEAN arm -- only what is knowable before the kick lands, i.e. the
#       kick's own position and exp_pts -- cost anything like 44%?
#
# Read (4) carefully in BOTH directions. A large drop is NOT proof of leakage
# here: the contest location is genuinely informative and genuinely knowable-ish.
# It is (1) that decides. (4) only sizes the exposure.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_model_leak_check.R"'
suppressMessages({library(data.table); library(mgcv); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
set.seed(20260911)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]

cst <- as.data.table(build_aerial_contests(ch, pbp))
cst[, y := as.integer(def_win)]
say("aerial contests: ", format(nrow(cst), big.mark = ","),
    "   defence wins ", round(100 * mean(cst$y), 1), "%")

say("\n=== (1) is kick_len physically plausible? ===")
say("A real kick is at most ~60m. This is the test the disposal model FAILED:")
say("its confident rows had a median kick_len of 115.8m.")
print(cst[, .(n = .N,
              min = round(min(kick_len), 1),
              p25 = round(quantile(kick_len, .25), 1),
              median = round(median(kick_len), 1),
              p95 = round(quantile(kick_len, .95), 1),
              max = round(max(kick_len), 1),
              pct_over_60m = round(100 * mean(kick_len > 60), 1))])

# COUNTS, not percentages. This block exists because the first version of this
# script printed `pct_over_100m` rounded to one decimal: 16 rows in 50,050 is
# 0.032%, which prints as "0.0", and that was then written into NEWS.md and the
# commit message as "NONE over 100m" -- a false absolute, contradicted by the
# `max` column in the very same table, which read 143.3m.
#
# A percentage cannot express "none". Only a count can. The house rule is to
# never quote a derived statistic without the raw count behind it, and this is
# what breaking it looks like.
say("\ncounts above each threshold (a rounded percentage CANNOT say 'none'):")
for (thr in c(60, 80, 100, 120, 140)) {
  n <- sum(cst$kick_len > thr)
  say("  kick_len > ", formatC(thr, width = 3), "m : ", formatC(n, width = 6),
      "   (", format(round(100 * n / nrow(cst), 4), nsmall = 4), "%)")
}
n100 <- sum(cst$kick_len > 100)
if (n100 > 0) {
  say("\n  ", n100, " contests exceed 100m -- physically impossible as one kick.")
  say("  Their shape decides whether they are the same leak in miniature:")
  print(cst[kick_len > 100, .(n = .N), by = .(out_desc, def_win)][order(-n)])
}

say("\ndefence-win rate by kick_len band (a real kick-length effect should be")
say("smooth and monotone-ish; a leak shows a step at the impossible end):")
cst[, band := cut(kick_len, c(-Inf, 15, 25, 35, 45, 55, 60, 80, Inf),
                  labels = c("<15", "15-25", "25-35", "35-45", "45-55",
                             "55-60", "60-80", ">80"))]
print(cst[, .(n = .N, def_win_pct = round(100 * mean(y), 1)), by = band][order(band)])

say("\n=== (2) how far ahead does the contest scan go? ===")
say("(rebuilt here: build_aerial_contests() does not return .olag)")
p <- data.table(match_id = ch$match_id, display_order = ch$display_order,
                description = ch$description)
setorder(p, match_id, display_order)
for (k in 1:6) p[, (paste0("f", k)) := shift(description, k, type = "lead"), by = match_id]
p[, .olag := fcase(!(f1 %chin% CHAINS_INFLIGHT_DESCS), 1L,
                   !(f2 %chin% CHAINS_INFLIGHT_DESCS), 2L,
                   !(f3 %chin% CHAINS_INFLIGHT_DESCS), 3L,
                   !(f4 %chin% CHAINS_INFLIGHT_DESCS), 4L,
                   !(f5 %chin% CHAINS_INFLIGHT_DESCS), 5L,
                   !(f6 %chin% CHAINS_INFLIGHT_DESCS), 6L, default = NA_integer_)]
cl <- merge(cst[, .(match_id, display_order = kick_do, kick_len, y)],
            p[, .(match_id, display_order, .olag)],
            by = c("match_id", "display_order"))
print(cl[, .(n = .N, pct = round(100 * .N / nrow(cl), 1),
             median_kick_len = round(median(kick_len), 1),
             def_win_pct = round(100 * mean(y), 1)), by = .olag][order(.olag)])

# --- (3) and (4): fit both arms on the same match-grouped folds ----------------
K <- 5
mids <- unique(cst$match_id)
cst <- merge(cst, data.table(match_id = mids,
                             fold = sample(rep_len(1:K, length(mids)))), by = "match_id")

FORMS <- list(
  # as shipped
  SHIPPED = y ~ s(att_x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
    s(exp_pts) + i50f,
  # keep the contest LOCATION, drop only the two the disposal model dropped
  NO_LEN  = y ~ s(att_x, abs_y) + s(goal_dist) + s(exp_pts) + i50f,
  # strictly pre-kick: the kicker's own position and what the situation was worth
  CLEAN   = y ~ s(kick_x, abs_ky) + s(exp_pts))
cst[, abs_ky := abs(kick_y)]

ll <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p)) }

R <- rbindlist(lapply(1:K, function(k) {
  tr <- cst[fold != k]; te <- cst[fold == k]
  say("fold ", k, " ...")
  o <- data.table(y = te$y)
  for (nmf in names(FORMS)) {
    g <- mgcv::bam(FORMS[[nmf]], data = droplevels(tr),
                   family = stats::binomial(), discrete = TRUE)
    o[, (nmf) := as.numeric(stats::predict(g, newdata = te, type = "response"))]
  }
  o
}))

base_ll <- ll(R$y, rep(mean(R$y), nrow(R)))
say("\n=== (3)+(4) out-of-fold, ", format(nrow(R), big.mark = ","), " contests ===")
say("logloss: lower is better. intercept only: ", round(base_ll, 5))
say("kept_pct: share of SHIPPED's gain over the base rate that the arm retains.")
say("n_p99 / max_p: the saturation symptom -- p_hat = 1 pays the loser nothing.")
print(rbindlist(lapply(names(FORMS), function(nmf) {
  p <- R[[nmf]]
  data.table(arm = nmf, logloss = round(ll(R$y, p), 5),
             kept_pct = round(100 * (base_ll - ll(R$y, p)) /
                                (base_ll - ll(R$y, R$SHIPPED)), 1),
             n_p99 = sum(p >= 0.99), n_p001 = sum(p <= 0.001),
             max_p = round(max(p), 5), min_p = round(min(p), 5))
})))

say("\nVERDICT RULE, decided before looking: if (1) shows kick_len physically")
say("plausible and (2) shows contests resolving close by, the location")
say("conditioning is legitimate and this model is NOT the disposal leak again --")
say("whatever (4) costs. If (1) shows 100m+ contests, it is.")
