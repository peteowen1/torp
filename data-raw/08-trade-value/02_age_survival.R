# Aging and list-survival curves on published TORP (plan step 3).
#
# Plan: docs/plans/TRADE-VALUE.md. Decision (c): model survival explicitly and
# show the aging curve with and without imputing a next-season rating for the
# players who drop off lists, because the dropped players are 1-2 TORP worse
# at every age. Also tests whether draft position adds anything beyond the
# current rating for players early in their careers.
#
# Reads the PUBLISHED ratings release directly: load_torp_ratings() prefers
# the local torpdata/data mirror, which is not refreshed by a republish.
#
# Output: torpdata/data/raw/trade_value/02_age_survival_*.csv (gitignored)

suppressMessages({
  library(data.table)
  library(splines)
})

out_dir <- "C:/dev/torpverse/torpdata/data/raw/trade_value"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Data ----
td <- tempfile(); dir.create(td)
system2("gh", c("release", "download", "ratings-data", "--repo", "peteowen1/torpdata",
                "--pattern", "torp_ratings.parquet", "--dir", td))
r <- as.data.table(arrow::read_parquet(file.path(td, "torp_ratings.parquet")))
setorder(r, player_id, season, round)
end <- r[, .SD[.N], by = .(player_id, season)][, .(player_id, player_name, season, age, torp)]
# Anchor: the v17 republish, not a stale copy.
stopifnot(end[player_name == "Lachie Neale" & season == 2026, torp] == -0.23)

# Games actually played per season, from the per-game ratings release.
pg <- rbindlist(lapply(2021:2026, function(s) {
  f <- file.path(td, paste0("player_game_ratings_", s, ".parquet"))
  system2("gh", c("release", "download", "player_game_ratings-data", "--repo", "peteowen1/torpdata",
                  "--pattern", basename(f), "--dir", td))
  d <- as.data.table(arrow::read_parquet(f, col_select = c("player_id", "season")))
  d[, .(games = .N), by = .(player_id, season)]
}))
end <- merge(end, pg, by = c("player_id", "season"), all.x = TRUE)
end[is.na(games), games := 0L]

cat("end-of-season rows:", nrow(end), " seasons:", paste(range(end$season), collapse = "-"),
    " torp NA:", sum(is.na(end$torp)), "\n")
print(end[, .(players = .N, torp_na = sum(is.na(torp)), mean_games = round(mean(games), 1)), by = season][order(season)])

# Draft position from the Footywire harvest, joined by name. Names that are
# not unique on either side are dropped rather than guessed.
fw <- fread("C:/dev/torpverse/torpdata/data/raw/footywire_drafts.csv")[is_pass == FALSE]
fw_ok <- fw[, .N, by = player][N == 1, player]
tr_ok <- end[, uniqueN(player_id), by = player_name][V1 == 1, player_name]
dk <- fw[player %in% fw_ok & player %in% tr_ok, .(player_name = player, draft_year, pick)]
end <- merge(end, dk, by = "player_name", all.x = TRUE)
cat("players with a national-draft pick joined:", end[!is.na(pick), uniqueN(player_id)],
    " of", uniqueN(end$player_id), " (ambiguous names dropped:",
    length(setdiff(intersect(fw$player, end$player_name), intersect(fw_ok, tr_ok))), ")\n")

# ---- 2. Year pairs ----
nxt <- end[, .(player_id, season = season - 1L, torp_next = torp, games_next = games)]
p <- merge(end[season <= 2025], nxt, by = c("player_id", "season"), all.x = TRUE)
p[, survived := !is.na(torp_next)]
p <- p[!is.na(torp) & !is.na(age)]
cat("\nyear pairs:", nrow(p), " survived:", sum(p$survived), " dropped:", sum(!p$survived), "\n")

# ---- 3. Survival: P(rated next season | age, rating, games) ----
surv <- glm(survived ~ ns(age, 3) + torp + log1p(games), data = p, family = binomial)
cat("\nsurvival model (logistic):\n"); print(round(coef(summary(surv)), 3))

# ---- 4. Aging, three ways ----
grid <- data.table(age = 19:35)
mean_t <- mean(p$torp)

# (a) delta method on survivors: mean change by whole-year age
p[, age_i := floor(age)]
delta <- p[survived == TRUE, .(n = .N, delta = mean(torp_next - torp)), by = age_i]

# (b) across-player regression on survivors
fit_s <- lm(torp_next ~ torp * ns(age, 3), data = p[survived == TRUE])
# (c) same, with drop-outs imputed: next rating = their current as-of rating,
#     which is already a shrunk estimate (plan section 1 step 5)
p[, torp_next_imp := fifelse(survived, torp_next, torp)]
fit_i <- lm(torp_next_imp ~ torp * ns(age, 3), data = p)

pred_change <- function(fit) predict(fit, newdata = grid[, .(age, torp = mean_t)]) - mean_t
curve <- grid[, .(age,
                  p_survive = round(predict(surv, newdata = data.table(age, torp = mean_t,
                                                                        games = median(p$games)),
                                            type = "response"), 3),
                  change_survivors = round(pred_change(fit_s), 3),
                  change_imputed = round(pred_change(fit_i), 3))]
curve <- merge(curve, delta[, .(age = age_i, delta_n = n, delta_raw = round(delta, 3))],
               by = "age", all.x = TRUE)
cat("\nexpected one-season TORP change for an average-rated player (", round(mean_t, 2), "):\n")
print(curve)
fwrite(curve, file.path(out_dir, "02_age_survival_curve.csv"))

# How much the next rating keeps of this one (regression to the mean), by age
slope <- sapply(c(21, 25, 29, 33), function(a) {
  d <- data.table(age = a, torp = c(0, 1))
  round(diff(predict(fit_s, newdata = d)), 3)
})
cat("\nnext-season TORP per +1 TORP now (survivors), ages 21/25/29/33:", slope, "\n")

# ---- 5. Out-of-sample: fit 2021->2024 pairs, predict 2025->2026 ----
tr <- p[season <= 2023 & survived == TRUE]; te <- p[season == 2025 & survived == TRUE]
f_oos <- lm(torp_next ~ torp * ns(age, 3), data = tr)
rmse <- function(e) round(sqrt(mean(e^2)), 3)
cat("\nholdout 2025->2026 (n", nrow(te), ") RMSE, lower is better:",
    " flat (next = now):", rmse(te$torp_next - te$torp),
    " age model:", rmse(te$torp_next - predict(f_oos, newdata = te)), "\n")

# ---- 6. Does draft pick add signal early in a career? ----
early <- p[survived == TRUE & !is.na(pick) & season - draft_year <= 3]
m0 <- lm(torp_next ~ torp + ns(age, 3), data = early)
m1 <- lm(torp_next ~ torp + ns(age, 3) + log(pick), data = early)
cat("\nearly-career pairs (<= 3 seasons since draft, survivors):", nrow(early), "\n")
cat("adj R2 without pick:", round(summary(m0)$adj.r.squared, 4),
    " with log(pick):", round(summary(m1)$adj.r.squared, 4), "\n")
print(round(coef(summary(m1))["log(pick)", ], 4))
pc <- cor(resid(lm(torp_next ~ torp + ns(age, 3), data = early)),
          resid(lm(log(pick) ~ torp + ns(age, 3), data = early)))
cat("partial correlation of log(pick) with next TORP, net of rating and age:", round(pc, 3), "\n")
# Same question for survival: do higher picks get kept longer at equal rating?
s1 <- glm(survived ~ ns(age, 3) + torp + log1p(games) + log(pick),
          data = p[!is.na(pick) & season - draft_year <= 3], family = binomial)
cat("survival, early career: log(pick) coefficient\n"); print(round(coef(summary(s1))["log(pick)", ], 4))

# Out-of-sample check on the pick term: fit early-career pairs up to 2023,
# score 2024 and 2025 pairs. In-sample signal alone is not enough to adopt it.
e_tr <- early[season <= 2023]; e_te <- early[season >= 2024]
g0 <- lm(torp_next ~ torp + ns(age, 3), data = e_tr)
g1 <- lm(torp_next ~ torp + ns(age, 3) + log(pick), data = e_tr)
err0 <- e_te$torp_next - predict(g0, e_te); err1 <- e_te$torp_next - predict(g1, e_te)
cat("\nearly-career holdout 2024-25 (n", nrow(e_te), ", train n", nrow(e_tr), ") RMSE without pick:", rmse(err0),
    " with pick:", rmse(err1), "\n")
set.seed(1)
bd <- replicate(2000, { i <- sample(nrow(e_te), replace = TRUE); sqrt(mean(err1[i]^2)) - sqrt(mean(err0[i]^2)) })
cat("RMSE difference (with - without), bootstrap 95% CI:", round(quantile(bd, c(.025, .975)), 3), "\n")
cat("log(pick) coefficient on the training years:", round(coef(g1)[["log(pick)"]], 3), "\n")
