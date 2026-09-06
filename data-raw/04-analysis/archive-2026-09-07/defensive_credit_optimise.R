# defensive_credit_optimise.R ---------------------------------------------
# Stage 2 of the mechanism-level fix. Given the per-stat channel components
# built by defensive_credit_reprice.R, find EPV credit weights for the eight
# defensive actions that flatten the positional calibration spread -- i.e.
# make a key defender's rating point worth the same margin as a forward's,
# by repricing what defenders actually DO rather than by multiplying their
# rating by 1.65 after the fact.
#
# WHY THIS IS NOT THE DEGENERATE PROBLEM
# --------------------------------------
# weight_optimisation.R noted that per-BUCKET multipliers can force every
# calibration coefficient to 1.00 for free -- a reparameterisation, not a
# finding. This is different: there are only EIGHT weights and they are
# shared across all six position buckets. Nothing here can target a bucket
# directly. The only reason repricing moves positions differently is that
# they genuinely accumulate different defensive actions. A solution that
# flattens the spread is therefore a real statement about action value.
#
# Candidate rating (delta form, so the published baseline is preserved and
# only the reweighting effect is applied):
#   torp(w) = torp_published + TORP_EPR_WEIGHT * sum_s (w_s - w0_s) * A_s
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/defensive_credit_optimise.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow); library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2021:2026
BUCKETS  <- c("KEY_DEFENDER", "MEDIUM_DEFENDER", "MIDFIELDER", "RUCK",
              "MEDIUM_FORWARD", "KEY_FORWARD")

cache <- readRDS(file.path(DATA_DIR, "..", "reprice_cache.rds"))
pg <- as.data.table(cache$pg); STATS <- cache$STATS; W0 <- cache$W0
Acols <- paste0(".A_", STATS)

tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))
tr[, round := as.numeric(round)]
res <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("results_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, home_team_id, away_team_id, margin = home_score - away_score)]

L <- merge(pg[position_group %in% BUCKETS],
           tr[, .(player_id, season, round, torp)],
           by = c("player_id", "season", "round"))
L <- L[!is.na(torp)]
setnames(L, "position_group", "pos")

# Team-sum per (match, team, bucket): the published rating, and each stat's
# channel component. TOG-weighted, matching the calibration gate.
agg <- L[, c(.(T_pub = sum(torp * tog_safe, na.rm = TRUE)),
             lapply(.SD, function(v) sum(v * tog_safe, na.rm = TRUE))),
         .SDcols = Acols, by = .(match_id, team_id, pos)]
vv <- c("T_pub", Acols)
w <- dcast(agg, match_id + team_id ~ pos, value.var = vv, fill = 0)

m <- merge(res, w, by.x = c("match_id", "home_team_id"),
           by.y = c("match_id", "team_id"))
a <- merge(res[, .(match_id, away_team_id)], w,
           by.x = c("match_id", "away_team_id"), by.y = c("match_id", "team_id"))
vc <- setdiff(names(w), c("match_id", "team_id"))
setnames(a, vc, paste0("a_", vc))
m <- merge(m, a, by = "match_id")
D <- data.table(margin = m$margin)
for (v in vc) D[[v]] <- m[[v]] - m[[paste0("a_", v)]]
cat(sprintf("design: %d matches\n", nrow(D)))

EW <- TORP_EPR_WEIGHT
bucket_feature <- function(delta, b) {
  x <- D[[paste0("T_pub_", b)]]
  for (i in seq_along(STATS)) {
    x <- x + EW * delta[i] * D[[paste0(".A_", STATS[i], "_", b)]]
  }
  x
}

calib <- function(w_vec) {
  delta <- w_vec - W0
  X <- vapply(BUCKETS, function(b) bucket_feature(delta, b), numeric(nrow(D)))
  colnames(X) <- BUCKETS
  fit <- lm(D$margin ~ X)
  co <- coef(fit)[-1]
  names(co) <- BUCKETS
  co
}

score <- function(w_vec, lambda = 0.05) {
  co <- calib(w_vec)
  if (any(!is.finite(co)) || any(co <= 0)) return(1e6)
  # Scale-invariant dispersion of the per-bucket coefficients.
  cv <- sd(co) / mean(co)
  pen <- lambda * sum(((w_vec - W0) / pmax(abs(W0), 0.05))^2) / length(W0)
  cv + pen
}

cat("\n===== BASELINE (shipped weights) =====\n")
co0 <- calib(W0)
print(round(co0, 2))
cat(sprintf("  spread max/min = %.2fx   CV = %.3f   KD/KF = %.2f\n",
            max(co0) / min(co0), sd(co0) / mean(co0),
            co0[["KEY_DEFENDER"]] / co0[["KEY_FORWARD"]]))

cat("\n===== OPTIMISING (free signs, shrunk toward shipped) =====\n")
set.seed(11)
best <- NULL
for (trial in 1:6) {
  start <- if (trial == 1) W0 else W0 * exp(rnorm(length(W0), 0, 0.6))
  o <- optim(start, score, method = "Nelder-Mead",
             control = list(maxit = 4000, reltol = 1e-10))
  if (is.null(best) || o$value < best$value) best <- o
}
w_free <- best$par; names(w_free) <- names(W0)
co_f <- calib(w_free)
cat("weights (shipped -> optimised):\n")
print(data.table(stat = names(W0), shipped = round(W0, 4),
                 optimised = round(w_free, 4),
                 ratio = round(w_free / W0, 2)))
print(round(co_f, 2))
cat(sprintf("  spread max/min = %.2fx   CV = %.3f   KD/KF = %.2f\n",
            max(co_f) / min(co_f), sd(co_f) / mean(co_f),
            co_f[["KEY_DEFENDER"]] / co_f[["KEY_FORWARD"]]))

cat("\n===== OPTIMISING (sign-constrained: defensive acts >= 0) =====\n")
POS <- c("spoils", "intercepts", "one_percenters", "tackles")
score_c <- function(par) {
  w_vec <- par
  w_vec[POS] <- abs(w_vec[POS])
  score(w_vec)
}
best_c <- NULL
for (trial in 1:6) {
  start <- if (trial == 1) W0 else W0 * exp(rnorm(length(W0), 0, 0.6))
  o <- optim(start, score_c, method = "Nelder-Mead",
             control = list(maxit = 4000, reltol = 1e-10))
  if (is.null(best_c) || o$value < best_c$value) best_c <- o
}
w_con <- best_c$par; names(w_con) <- names(W0); w_con[POS] <- abs(w_con[POS])
co_c <- calib(w_con)
print(data.table(stat = names(W0), shipped = round(W0, 4),
                 optimised = round(w_con, 4), ratio = round(w_con / W0, 2)))
print(round(co_c, 2))
cat(sprintf("  spread max/min = %.2fx   CV = %.3f   KD/KF = %.2f\n",
            max(co_c) / min(co_c), sd(co_c) / mean(co_c),
            co_c[["KEY_DEFENDER"]] / co_c[["KEY_FORWARD"]]))

saveRDS(list(W0 = W0, w_free = w_free, w_con = w_con,
             co0 = co0, co_free = co_f, co_con = co_c),
        file.path(DATA_DIR, "..", "reprice_solution.rds"))
cat("\nSaved reprice_solution.rds\n")
