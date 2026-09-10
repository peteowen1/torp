# What does the difficulty model look like with the leak removed? (issue #210)
# =============================================================================
# kick_len and fwd_gain are both computed from the resolving row's coordinates,
# which is the same row whose team membership IS the target. The in-flight rows
# do not offer a clean substitute: only 38.7% of kicks have one with
# coordinates, and their median implied length is 0.0m because they are
# recorded at the KICK's position, not the ball's landing point.
#
# So the honest question is what the model has WITHOUT them. Two arms, same
# 5 folds grouped by match:
#
#   LEAKY  x, abs_y, kick_len, fwd_gain, goal_dist, exp_pts, is_handball, i50f
#   CLEAN  x, abs_y,                     goal_dist, exp_pts, is_handball, i50f
#
# CLEAN uses only pre-disposal state: where he is, how far from goal, what the
# situation is worth, and whether it is a handball. Everything in it is known
# before the ball leaves his boot.
#
# What matters is not just the log-loss gap but what happens to the SHAPE of
# p_hat -- specifically whether the clean model still produces the p_hat = 1
# rows that issue #209 is about.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_difficulty_clean_baseline.R"'
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

K <- 5
mids <- unique(de$match_id)
de <- merge(de, data.table(match_id = mids,
                           fold = sample(rep_len(1:K, length(mids)))), by = "match_id")

rhs_leaky <- y ~ s(x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
  s(exp_pts) + is_handball + i50f
rhs_clean <- y ~ s(x, abs_y) + s(goal_dist) + s(exp_pts) + is_handball + i50f

ll <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p)) }

R <- rbindlist(lapply(1:K, function(k) {
  tr <- de[fold != k]; te <- de[fold == k]
  say("fold ", k, " ...")
  gL <- mgcv::bam(rhs_leaky, data = droplevels(tr), family = stats::binomial(), discrete = TRUE)
  gC <- mgcv::bam(rhs_clean, data = droplevels(tr), family = stats::binomial(), discrete = TRUE)
  data.table(y = te$y,
             leaky = as.numeric(stats::predict(gL, newdata = te, type = "response")),
             clean = as.numeric(stats::predict(gC, newdata = te, type = "response")))
}))

base_ll <- ll(R$y, rep(mean(R$y), nrow(R)))
say("\n=== out-of-fold, ", format(nrow(R), big.mark = ","), " disposals ===")
say("intercept only : ", round(base_ll, 5))
say("LEAKY (shipped): ", round(ll(R$y, R$leaky), 5))
say("CLEAN          : ", round(ll(R$y, R$clean), 5))
say("\ncost of removing the leak: ",
    round(ll(R$y, R$clean) - ll(R$y, R$leaky), 5), " log loss")
say("of the leaky model's total gain over base (",
    round(base_ll - ll(R$y, R$leaky), 5), "), the clean model keeps ",
    round(100 * (base_ll - ll(R$y, R$clean)) / (base_ll - ll(R$y, R$leaky)), 1), "%")

say("\n=== does the CLEAN model still produce near-certain rows? ===")
say("(this is what issue #209 is downstream of: p_hat = 1 zeroes the")
say(" defence's share, because it is paid (1 - p_hat) * surprise)")
for (v in c("leaky", "clean")) {
  say("  ", formatC(v, width = -6), ": p >= 0.99 -> ", formatC(R[get(v) >= 0.99, .N], width = 6),
      " rows   p >= 0.999 -> ", formatC(R[get(v) >= 0.999, .N], width = 6),
      " rows   max p ", formatC(max(R[[v]]), format = "f", digits = 5))
}

say("\n=== spread of p_hat: the leak makes the model far more opinionated ===")
print(data.table(
  arm = c("leaky", "clean"),
  p05 = c(round(quantile(R$leaky, .05), 3), round(quantile(R$clean, .05), 3)),
  median = c(round(median(R$leaky), 3), round(median(R$clean), 3)),
  p95 = c(round(quantile(R$leaky, .95), 3), round(quantile(R$clean, .95), 3)),
  sd = c(round(sd(R$leaky), 3), round(sd(R$clean), 3))))
