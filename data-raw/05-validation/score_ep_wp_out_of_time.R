# EP / WP out-of-time scorecard (review W13, docs/reviews/FABLE-FULL-REVIEW-2026-11-15.md
# section 4 A-B and findings 1.8 / 1.9).
#
# Scores the STORED play-by-play probabilities (what the site and the ratings
# read) on a season the published EP / WP models did not train on:
#   A. EP: five-class log loss against label_ep, Centre Bounce rows dropped;
#      mean predicted vs realised next score; predicted vs observed class shares;
#      log loss by quarter. Baseline: the scored season's own class shares
#      (generous to the baseline, it sees the answer's mix).
#   B. WP: log loss and calibration slope by quarter against label_wp, draws
#      dropped (slope 1.00 is right, below 1 is over-confident). Baseline: a
#      coin flip, 0.6931.
#
# The rows are only a test of the CURRENT models if they were scored by them:
# the script compares each row's pbp_model_id with the published models' id
# and says so (torp#319 stamps).
#
# Rows inside a match are not independent: the match count is the real sample.
# Between quarters EP log loss moved 1.3423-1.3445 on 2026, so EP differences
# under ~0.002 mean nothing; a one-season WP slope moves by ~0.1.
#
# Usage (from C:/dev/torpverse):
#   powershell.exe -Command 'Rscript torp/data-raw/05-validation/score_ep_wp_out_of_time.R [season] [out.csv]'
# season defaults to the newest season with play-by-play that neither model
# trained on. Refuses (exit 1) a season inside either model's training window.
# Review reference on 2026 (stored rows, 2026-11-15): EP 1.3435 (357,270 rows,
# 218 matches), WP 0.4430 (357,967 rows, 215 matches).

suppressMessages({
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
out_csv <- if (length(args) >= 2) args[2] else NA_character_

# Which seasons did the published models train on? ----
meta_seasons <- function(model) {
  m <- attr(torpmodels::load_torp_model(model), "torp_meta")
  s <- m$seasons
  if (is.null(s)) stop(model, " model carries no torp_meta$seasons; cannot tell what it trained on")
  r <- as.integer(strsplit(as.character(s), "-", fixed = TRUE)[[1]])
  list(seasons = seq(min(r), max(r)), trained_at = m$trained_at)
}
ep_m <- suppressMessages(meta_seasons("ep"))
wp_m <- suppressMessages(meta_seasons("wp"))
trained <- union(ep_m$seasons, wp_m$seasons)

season <- if (length(args) >= 1) as.integer(args[1]) else {
  cand <- setdiff(seq(max(trained) + 1L, get_afl_season()), trained)
  if (!length(cand)) stop("no season after the training window ", paste(range(trained), collapse = "-"))
  max(cand)
}
cat(sprintf("EP trained on %s (%s), WP on %s (%s); scoring %d\n",
            paste(range(ep_m$seasons), collapse = "-"), ep_m$trained_at,
            paste(range(wp_m$seasons), collapse = "-"), wp_m$trained_at, season))
if (season %in% trained) {
  cat("REFUSED:", season, "is inside a model's training window; that is not an out-of-time score.\n")
  quit(status = 1)
}

pbp <- as.data.table(load_pbp(season))
need <- c("match_id", "period", "description", "label_ep", "label_wp",
          "opp_goal", "opp_behind", "behind", "goal", "no_score", "exp_pts", "wp")
miss <- setdiff(need, names(pbp))
if (length(miss)) stop("pbp ", season, " lacks: ", paste(miss, collapse = ", "))
cat(sprintf("pbp %d: %s rows, %d matches, periods %s\n", season, format(nrow(pbp), big.mark = ","),
            uniqueN(pbp$match_id), paste(sort(unique(pbp$period)), collapse = ",")))

# Were these rows scored by the published models? ----
cur_id <- tryCatch(.current_pbp_model_id(), error = function(e) NA_character_)
ids <- if ("pbp_model_id" %in% names(pbp)) pbp$pbp_model_id else rep(NA_character_, nrow(pbp))
ids[is.na(ids) | !nzchar(ids)] <- PBP_MODEL_UNSTAMPED
id_tab <- as.data.table(table(pbp_model_id = ids))
id_tab[, current := pbp_model_id == cur_id]
cat("\nRows by the model set that scored them (current = the published EP/WP/calibration files):\n")
print(id_tab, row.names = FALSE)
on_current <- mean(ids == cur_id)
if (is.na(cur_id) || on_current < 1) {
  cat(sprintf("NOTE: %.1f%% of rows are on the current models (current id %s). The numbers below score the",
              100 * on_current, cur_id),
      "STORED rows, so they test whichever models wrote them, not necessarily today's.\n")
  if (season == 2026L) cat("      2026: rounds processed before the 2026-07-22 model upload were scored by older models",
                           "(docs/plans/WPA-REFRESH-GUARD.md); rows were stamped only from 2026-11-17.\n")
}

ll <- function(p) -mean(log(pmin(pmax(p, 1e-15), 1)))
cls <- c("opp_goal", "opp_behind", "behind", "goal", "no_score")
pts <- c(-6, -1, 1, 6, 0)

# A. EP ----
ep <- pbp[description != "Centre Bounce" & !is.na(label_ep) & !is.na(exp_pts) &
            !is.na(opp_goal) & !is.na(no_score)]
P <- as.matrix(ep[, ..cls])
y <- as.integer(ep$label_ep) + 1L
stopifnot(all(y %in% 1:5), max(abs(rowSums(P) - 1)) < 1e-4)
p_true <- P[cbind(seq_len(nrow(P)), y)]
real <- pts[y]
base_p <- tabulate(y, 5) / length(y)
ep_ll <- ll(p_true)
slope_ep <- unname(coef(lm(real ~ ep$exp_pts))[2])
cat(sprintf("\n=== A. EP on %d (lower log loss is better) ===\n", season))
cat(sprintf("rows %s, matches %d | log loss %.4f | baseline (season's own class shares) %.4f\n",
            format(nrow(ep), big.mark = ","), uniqueN(ep$match_id), ep_ll, ll(base_p[y])))
cat(sprintf("mean predicted next score %.3f, realised %.3f (points, from the team in possession); slope of realised on predicted %.2f (1 is right)\n",
            mean(ep$exp_pts), mean(real), slope_ep))
cat("\nClass shares, predicted vs observed:\n")
print(data.table(class = cls, predicted = round(colMeans(P), 3), observed = round(base_p, 3)), row.names = FALSE)
ep[, p_true := p_true]
cat("\nEP log loss by quarter (lower is better):\n")
print(ep[, .(rows = .N, log_loss = round(ll(p_true), 4)), keyby = .(quarter = period)], row.names = FALSE)

# B. WP ----
wpd <- pbp[!is.na(wp) & label_wp %in% c(0, 1)]
wslope <- function(w, l) unname(coef(suppressWarnings(glm(l ~ qlogis(pmin(pmax(w, 1e-6), 1 - 1e-6)),
                                                          family = binomial())))[2])
wl <- function(w, l) ll(ifelse(l == 1, w, 1 - w))
wp_ll <- wl(wpd$wp, wpd$label_wp)
cat(sprintf("\n=== B. WP on %d (lower log loss is better; slope 1.00 is right, below 1 is over-confident) ===\n", season))
cat(sprintf("rows %s, decided matches %d | log loss %.4f | coin flip 0.6931 | slope %.3f\n",
            format(nrow(wpd), big.mark = ","), uniqueN(wpd$match_id), wp_ll, wslope(wpd$wp, wpd$label_wp)))
byq <- wpd[, .(rows = .N, log_loss = round(wl(wp, label_wp), 4), slope = round(wslope(wp, label_wp), 3)),
           keyby = .(quarter = period)]
print(byq, row.names = FALSE)

if (!is.na(out_csv)) {
  res <- rbind(
    data.table(season, metric = "ep_log_loss", quarter = "all", rows = nrow(ep), value = ep_ll),
    data.table(season, metric = "ep_mean_pred_minus_realised", quarter = "all", rows = nrow(ep),
               value = mean(ep$exp_pts) - mean(real)),
    data.table(season, metric = "wp_log_loss", quarter = "all", rows = nrow(wpd), value = wp_ll),
    byq[, .(season, metric = "wp_slope", quarter = as.character(quarter), rows, value = slope)]
  )
  res[, `:=`(pbp_on_current_models = on_current, ep_trained_at = ep_m$trained_at)]
  fwrite(res, out_csv)
  cat("\nwrote", out_csv, "\n")
}
