# Can capping kick_len fix the leak cheaply? (issue #210)
# =============================================================================
# Dropping kick_len and fwd_gain costs 0.050 log loss and keeps 56% of the
# model's gain over base. Before accepting that price, test the cheaper repair.
#
# The leak's signature is IMPLAUSIBLE lengths: median 115m on the rows the model
# is most certain about, against 16.6m overall. A real kick is at most ~60m. So
# a length above that is not a kick length at all -- it is the distance to
# wherever the ball was next touched, which is downstream of the outcome.
#
# Capping at 60m keeps the legitimate part (a 45m kick really is harder to
# retain than a 15m one -- that is football, and it is knowable from the kick)
# while removing the part that can only be known after possession changed.
#
# Four arms, same 5 folds grouped by match:
#   LEAKY   as shipped
#   CAP60   kick_len and fwd_gain capped to +/-60m
#   CAP40   capped to +/-40m, a stricter reading of "plausible"
#   CLEAN   both features dropped
#
# The test is NOT just log loss. A cap that keeps the log loss but still
# saturates p_hat at 1 has not fixed anything, because #209 is downstream of
# saturation, so the p >= 0.99 count matters as much as the fit.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_kick_len_cap_test.R"'
suppressMessages({library(data.table); library(mgcv); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
set.seed(20260910)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
de  <- as.data.table(build_disposal_events(ch, pbp))
de  <- de[!is.na(turnover)]
de[, y := as.integer(turnover)]

say("disposals: ", format(nrow(de), big.mark = ","),
    "   turnover rate ", round(100 * mean(de$y), 1), "%")
say("kick_len over 60m: ", round(100 * mean(de$kick_len > 60), 1), "% of rows")
say("turnover rate among those: ", round(100 * de[kick_len > 60, mean(y)], 1),
    "%   vs ", round(100 * de[kick_len <= 60, mean(y)], 1), "% at or under 60m")

de[, `:=`(kick_len60 = pmin(kick_len, 60), fwd_gain60 = pmax(pmin(fwd_gain, 60), -60),
          kick_len40 = pmin(kick_len, 40), fwd_gain40 = pmax(pmin(fwd_gain, 40), -40))]

K <- 5
mids <- unique(de$match_id)
de <- merge(de, data.table(match_id = mids,
                           fold = sample(rep_len(1:K, length(mids)))), by = "match_id")

FORMS <- list(
  LEAKY = y ~ s(x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
    s(exp_pts) + is_handball + i50f,
  CAP60 = y ~ s(x, abs_y) + s(kick_len60) + s(fwd_gain60) + s(goal_dist) +
    s(exp_pts) + is_handball + i50f,
  CAP40 = y ~ s(x, abs_y) + s(kick_len40) + s(fwd_gain40) + s(goal_dist) +
    s(exp_pts) + is_handball + i50f,
  CLEAN = y ~ s(x, abs_y) + s(goal_dist) + s(exp_pts) + is_handball + i50f)

ll <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p)) }

R <- rbindlist(lapply(1:K, function(k) {
  tr <- de[fold != k]; te <- de[fold == k]
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
say("\n=== out-of-fold, ", format(nrow(R), big.mark = ","), " disposals ===")
say("intercept only: ", round(base_ll, 5), "\n")
res <- rbindlist(lapply(names(FORMS), function(nmf) {
  p <- R[[nmf]]
  data.table(arm = nmf, logloss = round(ll(R$y, p), 5),
             kept_pct = round(100 * (base_ll - ll(R$y, p)) / (base_ll - ll(R$y, R$LEAKY)), 1),
             n_p99 = sum(p >= 0.99), n_p999 = sum(p >= 0.999),
             max_p = round(max(p), 5), sd_p = round(sd(p), 3))
}))
print(res)

say("\nReading it: kept_pct is the share of LEAKY's gain over the base rate that")
say("the arm retains. n_p99 is what issue #209 is downstream of -- an arm that")
say("keeps the fit but still saturates has not fixed the problem.")
