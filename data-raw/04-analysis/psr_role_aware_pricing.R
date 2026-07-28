# psr_role_aware_pricing.R ------------------------------------------------
# WS3: does letting PSR price defensive stats DIFFERENTLY BY POSITION close
# the key-defender calibration gap?
#
# WHY PSR RATHER THAN THE EPV CREDIT WEIGHTS
# ------------------------------------------
# defensive_credit_optimise.R tried the mechanism fix at the EPV layer --
# reprice spoils/intercepts/one-percenters/etc -- and moved the calibration
# spread only 3.18x -> 3.00x. The variance decomposition says why: the whole
# spoil channel is just ~11% of a key defender's rating SD (0.144 of 1.29).
# PSR is 54% (0.70 of 1.29). That is where the leverage is.
#
# And PSR today applies ONE global beta vector across all six position
# groups (psr.R:104, `mat %*% betas`) -- a key defender's intercept and a
# midfielder's intercept are priced identically, though `intercepts` carries
# the single largest beta in the model (6.77). That is a modelling
# constraint, not a design choice.
#
# DESIGN
#   Pooled (baseline) : margin ~ team-sum stat ratings, one beta per stat.
#                       Reproduces the shipped model's structure.
#   Role-aware        : adds per-position-bucket deviation features for the
#                       DEFENSIVE stats only, ridge-penalised and shrunk
#                       toward the pooled solution (penalty.factor low on
#                       global terms, high on deviations, so lambda -> large
#                       recovers the pooled model exactly).
#
# Restricting interactions to defensive stats is deliberate: 48 stats x 6
# buckets x 2 sides is 576 features on ~950 training matches, which is not a
# test of anything. The hypothesis is about defensive actions, so that is
# where the extra freedom goes.
#
# EVALUATED ON BOTH AXES
#   1. Prediction -- held-out season margin MAE/RMSE. Guards against buying
#      calibration with accuracy.
#   2. Positional calibration -- the section 1.2 gate, run on the resulting
#      PSR. This is the thing WS3 is actually for.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/psr_role_aware_pricing.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow); library(data.table); library(glmnet)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
TEST_FROM <- 2025
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")
DEF_STATS <- c("intercepts", "spoils", "one_percenters", "rebound50s",
               "intercept_marks", "contest_def_one_on_ones", "contest_def_losses",
               "def_half_pressure_acts", "tackles", "pressure_acts")

sr <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_stat_ratings_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
pg <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", s))))[
    , .(player_id, match_id, season, round = as.numeric(round), team_id,
        position_group, lineup_position)]),
  use.names = TRUE, fill = TRUE)
res <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, season = as.integer(substr(utc_start_time, 1, 4)),
             home_team_id, away_team_id, margin = home_score - away_score,
             match_date = as.Date(substr(utc_start_time, 1, 10)))]

sr[, round := as.numeric(round)]
rating_cols <- grep("_rating$", names(sr), value = TRUE)
# Prefer opponent-adjusted where present, matching 06_train_psr_model.R.
adj <- grep("_adj_rating$", rating_cols, value = TRUE)
if (length(adj) > 0) {
  for (a in adj) {
    r <- sub("_adj_rating$", "_rating", a)
    if (r %in% names(sr)) sr[, (r) := get(a)]
  }
}
rating_cols <- setdiff(grep("_rating$", names(sr), value = TRUE), adj)
cat(sprintf("stat rating columns: %d\n", length(rating_cols)))

L <- merge(pg[position_group %in% BUCKETS & !lineup_position %in% c("EMERG", "SUB")],
           sr[, c("player_id", "season", "round", rating_cols), with = FALSE],
           by = c("player_id", "season", "round"))
for (c0 in rating_cols) set(L, which(is.na(L[[c0]])), c0, 0)
cat(sprintf("player-match rows with ratings: %s\n", format(nrow(L), big.mark = ",")))

def_cols <- intersect(paste0(DEF_STATS, "_rating"), rating_cols)
cat(sprintf("defensive stats given position interactions: %d (%s)\n",
            length(def_cols), paste(sub("_rating$", "", def_cols), collapse = ", ")))

# ---- team-level features: global sums + per-bucket defensive sums --------
glob <- L[, lapply(.SD, sum, na.rm = TRUE), .SDcols = rating_cols,
          by = .(match_id, team_id)]
bysq <- L[, lapply(.SD, sum, na.rm = TRUE), .SDcols = def_cols,
          by = .(match_id, team_id, position_group)]
byw <- dcast(bysq, match_id + team_id ~ position_group, value.var = def_cols, fill = 0)
team <- merge(glob, byw, by = c("match_id", "team_id"))

side <- function(id_col, prefix) {
  x <- merge(res[, c("match_id", id_col), with = FALSE], team,
             by.x = c("match_id", id_col), by.y = c("match_id", "team_id"))
  fc <- setdiff(names(x), c("match_id", id_col))
  setnames(x, fc, paste0(prefix, fc))
  x[, (id_col) := NULL][]
}
M <- merge(res, side("home_team_id", "h_"), by = "match_id")
M <- merge(M, side("away_team_id", "a_"), by = "match_id")
cat(sprintf("matches: %d\n", nrow(M)))

glob_feat <- c(paste0("h_", rating_cols), paste0("a_", rating_cols))
dev_feat_base <- setdiff(names(team), c("match_id", "team_id", rating_cols))
dev_feat <- c(paste0("h_", dev_feat_base), paste0("a_", dev_feat_base))
glob_feat <- glob_feat[glob_feat %in% names(M)]
dev_feat  <- dev_feat[dev_feat %in% names(M)]
cat(sprintf("features: %d global + %d role-deviation\n", length(glob_feat), length(dev_feat)))

X_all <- as.matrix(M[, c(glob_feat, dev_feat), with = FALSE])
train <- which(M$season < TEST_FROM)
test  <- which(M$season >= TEST_FROM)
sds <- apply(X_all[train, ], 2, sd); sds[sds == 0 | is.na(sds)] <- 1
X <- sweep(X_all, 2, sds, "/")
y <- M$margin
anchor <- as.Date("2024-12-31")
wts <- exp(as.numeric(-(anchor - M$match_date)) / MATCH_WEIGHT_DECAY_DAYS)
wts <- wts / mean(wts)

fit_eval <- function(cols, pf, label) {
  idx <- match(cols, colnames(X))
  Xi <- X[, idx, drop = FALSE]
  set.seed(3)
  cv <- cv.glmnet(Xi[train, , drop = FALSE], y[train], alpha = 0,
                  weights = wts[train], penalty.factor = pf, nfolds = 10)
  p <- as.numeric(predict(cv, Xi[test, , drop = FALSE], s = "lambda.min"))
  cat(sprintf("\n%-22s test MAE %.3f  RMSE %.3f  (n_test = %d)\n",
              label, mean(abs(y[test] - p)), sqrt(mean((y[test] - p)^2)), length(test)))
  list(cv = cv, cols = cols, idx = idx, pred = p)
}

cat("\n================ PREDICTION (holdout", TEST_FROM, "+) ================\n")
m_pool <- fit_eval(glob_feat, rep(1, length(glob_feat)), "pooled (baseline)")
all_feat <- c(glob_feat, dev_feat)
pf_role <- c(rep(0.2, length(glob_feat)), rep(1, length(dev_feat)))
m_role <- fit_eval(all_feat, pf_role, "role-aware")

# ---- positional calibration of the resulting player-level PSR -----------
psr_of <- function(mod) {
  b <- as.numeric(coef(mod$cv, s = "lambda.min"))[-1]
  names(b) <- colnames(X)[mod$idx]
  # Player contribution: global beta for every stat, plus the bucket
  # deviation beta for defensive stats in that player's bucket. Home-side
  # betas are used (the metric is side-agnostic by construction).
  contrib <- numeric(nrow(L))
  for (s in rating_cols) {
    hn <- paste0("h_", s)
    if (!hn %in% names(b)) next
    contrib <- contrib + (b[[hn]] / sds[[hn]]) * L[[s]]
  }
  for (s in def_cols) for (bk in BUCKETS) {
    hn <- paste0("h_", s, "_", bk)
    if (!hn %in% names(b)) next
    sel <- L$position_group == bk
    contrib[sel] <- contrib[sel] + (b[[hn]] / sds[[hn]]) * L[[s]][sel]
  }
  contrib
}

calib_of <- function(v, label) {
  T <- data.table(match_id = L$match_id, team_id = L$team_id,
                  pos = L$position_group, v = v)
  ag <- T[, .(s = sum(v, na.rm = TRUE)), by = .(match_id, team_id, pos)]
  wd <- dcast(ag, match_id + team_id ~ pos, value.var = "s", fill = 0)
  hh <- merge(res, wd, by.x = c("match_id", "home_team_id"),
              by.y = c("match_id", "team_id"))
  aa <- merge(res[, .(match_id, away_team_id)], wd,
              by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
  bk <- intersect(BUCKETS, names(wd))
  setnames(aa, bk, paste0("a_", bk))
  mm <- merge(hh, aa, by = "match_id")
  for (b in bk) mm[[paste0("d_", b)]] <- mm[[b]] - mm[[paste0("a_", b)]]
  f <- as.formula(paste("margin ~", paste0("d_", bk, collapse = " + ")))
  co <- coef(lm(f, data = mm))[-1]; names(co) <- bk
  cat(sprintf("\n%s\n", label)); print(round(co, 2))
  cat(sprintf("  spread max/min = %.2fx   KD/KF = %.2f\n",
              max(co) / min(co), co[["KEY_DEFENDER"]] / co[["KEY_FORWARD"]]))
  co
}

cat("\n========== POSITIONAL CALIBRATION OF THE PSR COMPONENT ==========\n")
cat("(coefficients on the PSR half alone, not full TORP -- compare like with like)\n")
c_pool <- calib_of(psr_of(m_pool), "pooled PSR")
c_role <- calib_of(psr_of(m_role), "role-aware PSR")

cat("\n---- role-aware defensive betas that differ most from pooled ----\n")
b <- as.numeric(coef(m_role$cv, s = "lambda.min"))[-1]
names(b) <- colnames(X)[m_role$idx]
rows <- rbindlist(lapply(def_cols, function(s) {
  g <- if (paste0("h_", s) %in% names(b)) b[[paste0("h_", s)]] else NA_real_
  rbindlist(lapply(BUCKETS, function(bk) {
    hn <- paste0("h_", s, "_", bk)
    if (!hn %in% names(b)) return(NULL)
    data.table(stat = sub("_rating$", "", s), bucket = bk,
               global = round(g, 3), deviation = round(b[[hn]], 3))
  }))
}))
print(head(rows[order(-abs(deviation))], 15))
