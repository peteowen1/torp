# Does the disposal-difficulty model leave signal on the table?
# =============================================================================
# Pete, 2026-09-10, on being shown that Schultz's inside-50 kick scored
# p_hat = 0.821 while comparable kicks turn over 53.9%: "test this as an xgb.cv
# as well, see how the two models compare."
#
# The shipped model is a GAM:
#   turnover ~ s(x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
#              s(exp_pts) + is_handball + i50f
#
# It has x/y and exp_pts. It does NOT have the preceding event, and the raw
# rates say that carries a lot: turnover runs 20.7% after a Contested Mark
# against 45.7% after a Loose Ball Get.
#
# Three arms, same folds, same target, so the comparison isolates one thing at
# a time:
#   A  GAM, shipped features                     (the incumbent)
#   B  XGBoost, shipped features                 (is it the FUNCTIONAL FORM?)
#   C  XGBoost, shipped features + prev_desc     (is it the MISSING FEATURE?)
#
# B - A prices the model class. C - B prices the feature. Reporting both
# separately matters: a single "xgb beats gam" number cannot tell them apart,
# and the fix is completely different in each case.
#
# Grouped by match_id so a fold never splits a game (rows within a match share
# state and would leak). Metrics: log loss and AUC, plus calibration in the
# high-p_hat region, which is where the Schultz row and issue #209 both live.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_difficulty_gam_vs_xgb.R"'
suppressMessages({
  library(data.table); library(mgcv); library(xgboost)
  devtools::load_all(quiet = TRUE)
})
options(torp.local_data_dir = NA)
set.seed(20260910)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]

de <- as.data.table(build_disposal_events(ch, pbp))
de <- de[!is.na(turnover)]
say("disposals: ", format(nrow(de), big.mark = ","),
    "   turnover rate ", round(100 * mean(de$turnover), 1), "%")

# --- add the preceding chains description ------------------------------------
seq <- ch[, .(match_id = as.character(match_id), display_order, description)]
setorder(seq, match_id, display_order)
seq[, prev_desc := shift(description), by = match_id]
de <- merge(de, seq[, .(match_id, display_order, prev_desc)],
            by = c("match_id", "display_order"), all.x = TRUE)
de[is.na(prev_desc), prev_desc := "(none)"]
# keep the tail small: anything rare becomes "other"
keep <- de[, .N, by = prev_desc][N >= 300]$prev_desc
de[!prev_desc %chin% keep, prev_desc := "other"]
de[, prev_desc := factor(prev_desc)]
say("prev_desc levels kept: ", length(levels(de$prev_desc)))

FEATS <- c("x", "abs_y", "kick_len", "fwd_gain", "goal_dist", "exp_pts",
           "is_handball", "i50f")
de <- de[stats::complete.cases(de[, ..FEATS])]
de[, y := as.integer(turnover)]

# --- grouped folds: a match never spans two folds -----------------------------
K <- 5
mids <- unique(de$match_id)
fold_of <- data.table(match_id = mids, fold = sample(rep_len(1:K, length(mids))))
de <- merge(de, fold_of, by = "match_id")
say("folds: ", K, " grouped by match (", length(mids), " matches)")

logloss <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p)) }
auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - as.numeric(n1) * (as.numeric(n1) + 1) / 2) / (as.numeric(n1) * as.numeric(n0)) }

mm <- function(dt, feats) {
  stats::model.matrix(~ . - 1, data = droplevels(dt[, ..feats]))
}

rhs_gam <- ~ s(x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
  s(exp_pts) + is_handball + i50f

res <- list()
for (k in 1:K) {
  tr <- de[fold != k]; te <- de[fold == k]
  say("fold ", k, ": train ", format(nrow(tr), big.mark = ","),
      "  test ", format(nrow(te), big.mark = ","))

  # A: the shipped GAM
  gm <- mgcv::bam(stats::update(rhs_gam, y ~ .), data = droplevels(tr),
                  family = stats::binomial(), discrete = TRUE)
  pA <- as.numeric(stats::predict(gm, newdata = te, type = "response"))

  # xgb.train/xgb.DMatrix rather than xgboost(): the high-level wrapper renamed
  # its arguments and now demands a factor y, while this path is stable.
  PAR <- list(objective = "binary:logistic", eval_metric = "logloss",
              max_depth = 5, eta = 0.05, subsample = 0.8,
              colsample_bytree = 0.8, nthread = 4)
  boost <- function(feats) {
    xtr <- mm(tr, feats); xte <- mm(te, feats)
    common <- intersect(colnames(xtr), colnames(xte))
    dtr <- xgboost::xgb.DMatrix(data = xtr[, common, drop = FALSE], label = tr$y)
    dte <- xgboost::xgb.DMatrix(data = xte[, common, drop = FALSE])
    m <- xgboost::xgb.train(params = PAR, data = dtr, nrounds = 400, verbose = 0)
    as.numeric(stats::predict(m, dte))
  }

  # B: xgboost, same features -- is the gap the FUNCTIONAL FORM?
  pB <- boost(FEATS)
  # C: xgboost + prev_desc -- is the gap the MISSING FEATURE?
  pC <- boost(c(FEATS, "prev_desc"))

  res[[k]] <- data.table(fold = k, y = te$y, A = pA, B = pB, C = pC)
}
R <- rbindlist(res)

say("\n=== out-of-fold, ", format(nrow(R), big.mark = ","), " disposals ===")
say("lower log loss is better; higher AUC is better")
out <- data.table(
  arm = c("A  GAM, shipped features",
          "B  XGB, shipped features",
          "C  XGB + prev_desc"),
  logloss = c(logloss(R$y, R$A), logloss(R$y, R$B), logloss(R$y, R$C)),
  auc     = c(auc(R$y, R$A), auc(R$y, R$B), auc(R$y, R$C)))
out[, `:=`(logloss = round(logloss, 5), auc = round(auc, 5))]
print(out)
say("\nB - A (model class) : logloss ", round(out$logloss[2] - out$logloss[1], 5))
say("C - B (prev_desc)   : logloss ", round(out$logloss[3] - out$logloss[2], 5))

say("\n=== calibration in the high-p_hat region (where issue #209 lives) ===")
say("predicted vs actual turnover rate, by predicted decile")
for (nmA in c("A", "B", "C")) {
  R[, bin := cut(get(nmA), breaks = quantile(get(nmA), 0:10 / 10), include.lowest = TRUE)]
  cal <- R[, .(n = .N, pred = round(mean(get(nmA)), 3), actual = round(mean(y), 3)), by = bin]
  setorder(cal, bin)
  say("\n  arm ", nmA, ":")
  print(cal[(.N - 2):.N])   # top three deciles only
}

say("\n=== how many rows does each arm push to p >= 0.99? ===")
for (nmA in c("A", "B", "C"))
  say("  arm ", nmA, ": ", format(R[get(nmA) >= 0.99, .N], big.mark = ","),
      " rows, actual turnover rate ",
      round(100 * R[get(nmA) >= 0.99, mean(y)], 1), "%")
