# Plan §7.31 — DRAPM on points, with a TORP-informed prior (RAPM+SPM shape).
#
# WHY THIS TARGET. §7.22 built a general RAPM engine and scored five flavours.
# The prevention-residual target FAILED (OOS R² +0.045, coefficient reliability
# 0.375, and cor(DRAPM-points, DRAPM-prevention) = −0.052) because the
# prevention residual is a residual of a residual and strips real signal along
# with the confound. DRAPM-on-points survived: reliability 0.678, credible
# names (Lever, May, Battle, Henry, Rampe, Moore). So suppression is pursued
# here on ACTUAL POINTS CONCEDED rather than on a scrubbed residual.
#
# WHY A PRIOR. Plain ridge shrinks toward ZERO, which is a statement that the
# average player has no effect -- true on average and useless per player. Worse,
# it leaves teammates who always play together mutually unidentified, so a
# strong defensive unit's players each collect the shared credit. §7.22e caught
# exactly that: four same-era Melbourne players in the top 12.
#
# The fix is pannaverse's RAPM+SPM shape -- shrink toward an informed box-score
# prior instead of toward zero. Implemented as a ridge with prior mean m:
#     minimise ||y - Xb||² + λ||b - m||²
# which is a plain ridge on the residual y - Xm, with b = m + b_resid. glmnet's
# `offset` does the first part.
#
# GATES, per §7.22's discipline: regularised coefficients always look
# plausible, so face validity and in-sample fit are inadmissible. Judged on
#   1. out-of-sample prediction on held-out MATCHES
#   2. split-half reliability of the coefficients themselves
#   3. a CO-APPEARANCE diagnostic: how separable is a defender from his
#      teammates at all? A rating that is really a team rating must be caught
#      here, not in the leaderboard.
suppressMessages({
  library(arrow); library(data.table); library(Matrix); library(glmnet)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
try(clear_skip_markers(), silent = TRUE)

DD <- "C:/dev/torpverse/torpdata/data/"
SEASONS <- 2021:2026
MIN_GAMES <- 10L
rdp <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(DD, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names = TRUE, fill = TRUE)

pg <- rdp("player_game_%d.parquet", SEASONS)
pg <- pg[!is.na(team_id) & !is.na(player_id)]
pg[, round := as.numeric(round)]
pg[, tog := pmax(pmin(fifelse(is.na(time_on_ground_percentage), 80,
                              time_on_ground_percentage)/100, 1), 0.05)]
res <- rdp("results_%d.parquet", SEASONS)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, season, home_team_id, away_team_id, home_score, away_score)]

# published v2 ratings supply the prior
tr <- as.data.table(load_torp_ratings()); tr[, round := as.numeric(round)]
pg <- merge(pg, tr[, .(player_id, season, round, torp)],
            by = c("player_id","season","round"), all.x = TRUE)

gcount <- pg[, .N, by = player_id]
pg[, pid := fifelse(player_id %in% gcount[N >= MIN_GAMES, player_id],
                    player_id, "__REPLACEMENT__")]
players <- sort(unique(pg$pid)); P <- length(players)
pg[, pcol := match(pid, players)]
cat(sprintf("players with >=%d games: %d (of %d)\n", MIN_GAMES, P - 1L, nrow(gcount)))

# ---- offense/defense split design: target = points SCORED by this team -----
sides <- rbind(
  res[, .(match_id, season, team_id = home_team_id, opp_id = away_team_id, y = home_score)],
  res[, .(match_id, season, team_id = away_team_id, opp_id = home_team_id, y = away_score)])
sides[, rid := .I]
app <- pg[, .(tog = max(tog), torp = max(torp, na.rm = TRUE)), by = .(match_id, team_id, pcol)]
app[!is.finite(torp), torp := 0]
own <- merge(sides[, .(rid, match_id, team_id)], app, by = c("match_id","team_id"))
opp <- merge(sides[, .(rid, match_id, team_id = opp_id)], app, by = c("match_id","team_id"))
X <- sparseMatrix(i = c(own$rid, opp$rid), j = c(own$pcol, opp$pcol + P),
                  x = c(own$tog, opp$tog), dims = c(nrow(sides), 2L*P))
y <- sides$y
cat(sprintf("design: %d rows x %d columns\n", nrow(X), ncol(X)))

# ---- the TORP-informed prior ------------------------------------------------
# A player's own-block prior is his TORP scaled to points; his opp-block prior
# is the negative of it (a better player concedes fewer points to him). The
# scale is fitted once, so the prior is TORP's SHAPE, not an assumed magnitude.
ptorp <- app[, .(m = mean(torp)), by = pcol][order(pcol)]
mvec <- numeric(2L*P)
mvec[ptorp$pcol] <- ptorp$m
mvec[ptorp$pcol + P] <- -ptorp$m
scale_fit <- lm(y ~ as.numeric(X %*% mvec))
k <- coef(scale_fit)[2]
cat(sprintf("prior scale fitted: %.4f points per TORP unit (R2 %.4f)\n",
            k, summary(scale_fit)$r.squared))
m <- mvec * k

fit_arm <- function(use_prior, label, seed = 20260727) {
  set.seed(seed)
  ug <- unique(sides$match_id); ho <- sample(ug, floor(length(ug)*0.25))
  te <- sides$match_id %in% ho; tr_ <- !te
  off_tr <- if (use_prior) as.numeric(X[tr_, ] %*% m) else rep(0, sum(tr_))
  off_te <- if (use_prior) as.numeric(X[te, ] %*% m) else rep(0, sum(te))
  cv <- cv.glmnet(X[tr_, ], y[tr_], alpha = 0, nfolds = 5, standardize = FALSE,
                  offset = off_tr)
  pr <- as.numeric(predict(cv, newx = X[te, ], s = "lambda.min", newoffset = off_te))
  base <- mean(y[tr_])
  r2 <- 1 - sum((y[te] - pr)^2)/sum((y[te] - base)^2)
  # split-half reliability of the coefficients
  h <- sample(ug, floor(length(ug)/2)); a <- sides$match_id %in% h; b <- !a
  cf <- function(sel) {
    o <- if (use_prior) as.numeric(X[sel, ] %*% m) else rep(0, sum(sel))
    z <- as.numeric(coef(glmnet(X[sel, ], y[sel], alpha = 0, lambda = cv$lambda.min,
                                standardize = FALSE, offset = o)))[-1]
    if (use_prior) z + m else z
  }
  ca <- cf(a); cb <- cf(b)
  ok <- is.finite(ca) & is.finite(cb)
  rel <- cor(ca[ok], cb[ok])
  off_all <- if (use_prior) as.numeric(X %*% m) else rep(0, nrow(X))
  full <- as.numeric(coef(glmnet(X, y, alpha = 0, lambda = cv$lambda.min,
                                 standardize = FALSE, offset = off_all)))[-1]
  if (use_prior) full <- full + m
  cat(sprintf("%-28s OOS R2 %+.4f | coef split-half r %+.3f | lambda %.2f\n",
              label, r2, rel, cv$lambda.min))
  list(coef = full, r2 = r2, rel = rel)
}
cat("\n=== ARMS ===\n")
a_plain <- fit_arm(FALSE, "shrink to zero (plain)")
a_prior <- fit_arm(TRUE,  "shrink to TORP prior")

# ---- read out defensive ratings --------------------------------------------
nm <- unique(pg[, .(pid, player_name)], by = "pid")
pos <- pg[, .N, by = .(pid, position_group)][order(-N)][, .SD[1], by = pid]
look <- merge(data.table(pid = players, idx = seq_along(players)), nm, by = "pid")
look <- merge(look, pos[, .(pid, position_group)], by = "pid", all.x = TRUE)
setorder(look, idx)
show <- function(fit, label) {
  d <- copy(look)[, val := -fit$coef[idx + P]][pid != "__REPLACEMENT__"]
  cat(sprintf("\n--- %s: top 10 KEY DEFENDERS (higher = concedes less) ---\n", label))
  print(head(d[position_group == "KEY_DEFENDER"][order(-val),
              .(player_name, val = round(val, 3))], 10))
  d
}
d_plain <- show(a_plain, "shrink to zero")
d_prior <- show(a_prior, "shrink to TORP prior")
cat(sprintf("\ncor(plain, prior) over %d players = %.3f\n",
            nrow(d_plain), cor(d_plain$val, d_prior$val)))

# ---- co-appearance diagnostic ----------------------------------------------
# How separable is a key defender from his most frequent teammate? If a pair
# almost always plays together, ridge cannot tell them apart and whatever it
# assigns is a shared team effect wearing two names.
cat("\n=== SEPARABILITY: co-appearance with most-frequent teammate ===\n")
tm <- pg[pid != "__REPLACEMENT__", .(pid, match_id, team_id, position_group)]
kd <- unique(tm[position_group == "KEY_DEFENDER", .(pid)])
setkey(tm, match_id, team_id)
pairs <- merge(tm[pid %in% kd$pid, .(pid, match_id, team_id)],
               tm[, .(mate = pid, match_id, team_id)],
               by = c("match_id","team_id"), allow.cartesian = TRUE)
pairs <- pairs[pid != mate]
ng <- tm[pid %in% kd$pid, .(games = uniqueN(match_id)), by = pid]
co <- pairs[, .(together = uniqueN(match_id)), by = .(pid, mate)]
co <- merge(co, ng, by = "pid")[, share := together/games]
top <- co[order(-share)][, .SD[1], by = pid]
top <- merge(top, nm, by = "pid")
top <- top[games >= 30]
cat(sprintf("key defenders with >=30 games: %d | median top-teammate share %.2f\n",
            nrow(top), median(top$share)))
cat("  players whose top teammate shares >90%% of their games (least separable):\n")
print(head(top[order(-share), .(player_name, games, top_mate_share = round(share, 3))], 8))
cat("\n  A share near 1.00 means ridge is splitting one joint effect between two\n")
cat("  players on essentially no independent information.\n")
