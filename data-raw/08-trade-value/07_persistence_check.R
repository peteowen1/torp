# Does compounding the one-season carry-over shrink proven players too fast?
#
# 03_project_value.R moves a rating one season at a time with a fitted
# carry-over (~0.85 of the distance from average). Eight steps of that
# compound to 0.85^7 ~ 0.32. If ratings actually persist more over k seasons
# than the one-step slope compounded k times, long-horizon values for steady
# stars are too low. Two checks on end-of-season TORP, 2021-2026:
#   1. k-season-ahead slope (rating in t+k on rating in t, players rated in
#      both) against the one-step slope to the power k;
#   2. does last season's rating add anything to the one-step forecast once
#      this season's is known? A positive weight means a player's history
#      matters beyond his latest rating.

suppressMessages({ library(data.table); library(splines) })
invisible(capture.output(source("data-raw/08-trade-value/02_age_survival.R")))
e <- end[!is.na(torp), .(player_id, season, torp, age, games)]
m <- mean(e$torp)
cat("end-of-season ratings:", nrow(e), " mean TORP", round(m, 2), "\n")

# 1. k-season-ahead slopes
res <- rbindlist(lapply(1:5, function(k) {
  nx <- e[, .(player_id, season = season - k, torp_k = torp)]
  d <- merge(e, nx, by = c("player_id", "season"))
  f <- lm(torp_k ~ torp, data = d)
  data.table(k = k, n = nrow(d), slope = coef(f)[["torp"]])
}))
b1 <- res[k == 1, slope]
res[, `:=`(compounded = b1^k, ratio = slope / b1^k)]
cat("\nk-season-ahead slope vs one-step slope compounded (ratio > 1: ratings persist more than the projection assumes):\n")
print(res[, .(k, n, slope = round(slope, 3), compounded = round(compounded, 3), ratio = round(ratio, 2))])

# Same for players already rated well above average (top 100 at season t)
e[, rk := frank(-torp, ties.method = "min"), by = season]
res_top <- rbindlist(lapply(1:5, function(k) {
  nx <- e[, .(player_id, season = season - k, torp_k = torp)]
  d <- merge(e[rk <= 100], nx, by = c("player_id", "season"))
  data.table(k = k, n = nrow(d), mean_now = mean(d$torp), mean_later = mean(d$torp_k),
             kept_share = (mean(d$torp_k) - m) / (mean(d$torp) - m))
}))
cat("\nplayers ranked top 100 in season t: share of their distance above average still there k seasons later (survivors only):\n")
print(res_top[, .(k, n, mean_now = round(mean_now, 2), mean_later = round(mean_later, 2),
                  kept_share = round(kept_share, 2), compounded = round(b1^k, 2))])

# 2. does last season's rating help the one-step forecast?
prev <- e[, .(player_id, season = season + 1L, torp_prev = torp)]
nx1 <- e[, .(player_id, season = season - 1L, torp_next = torp)]
d2 <- merge(merge(e, prev, by = c("player_id", "season")), nx1, by = c("player_id", "season"))
f0 <- lm(torp_next ~ torp + ns(age, 3), data = d2)
f1 <- lm(torp_next ~ torp + torp_prev + ns(age, 3), data = d2)
cat("\none-step forecast with and without last season's rating (n", nrow(d2), "):\n")
cat("  adj R2 without:", round(summary(f0)$adj.r.squared, 4), " with:", round(summary(f1)$adj.r.squared, 4), "\n")
print(round(coef(summary(f1))[c("torp", "torp_prev"), ], 3))
# out of sample: train on t <= 2023, test t = 2024-2025
tr <- d2[season <= 2023]; te <- d2[season >= 2024]
rmse <- function(f) round(sqrt(mean((te$torp_next - predict(f, te))^2)), 3)
cat("  holdout t = 2024-25 (n", nrow(te), ") RMSE without:", rmse(lm(formula(f0), tr)), " with:", rmse(lm(formula(f1), tr)), "\n")
