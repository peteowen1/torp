# Plan §7.22 — a general RAPM for AFL, with flavours.
#
# WHY. §7.21 established that a player effect on defensive prevention is real
# (block-null p < 0.001, ~32% signal, ~2.8 pts/game sd for key defenders) but
# that a one-player-at-a-time WOWY is an UPPER BOUND and must never price
# anything: it hands shared team-phase variance to every co-absent player.
# The fix is to estimate all players SIMULTANEOUSLY with regularisation --
# RAPM. pannaverse already runs this shape (RAPM+SPM), so this is deliberately
# built as a general engine rather than a one-off defensive model.
#
# THE ENGINE. One ridge fit over a sparse design matrix:
#   FLAVOUR "margin"  : row = match. Columns = players, +1 if he played for the
#                       home team, -1 for away. Target = home margin. One
#                       coefficient per player = his margin contribution.
#   FLAVOUR "split"   : row = team-side (2 per match). Columns are DOUBLED --
#                       an own-block and an opp-block. Target = points scored by
#                       that team. The own-block coefficient is offensive value
#                       (ORAPM); the opp-block coefficient is what a player does
#                       to the opposition's scoring, i.e. defensive value
#                       (DRAPM, sign-flipped so higher = better defender).
#   FLAVOUR "prevent" : same design as "split" but the target is the
#                       entry-adjusted PREVENTION residual from §7.20, so the
#                       model prices suppression specifically rather than
#                       general concession.
#
# EXPOSURE. Binary (played / did not) or TOG-weighted. AFL has no stint data,
# so TOG is the only available exposure signal and a player who was subbed out
# at quarter time should not carry the same weight as one who played out the
# game.
#
# EVALUATION. Regularised coefficients always look plausible, so neither face
# validity nor in-sample fit is admissible evidence here. Two gates:
#   1. OUT-OF-SAMPLE on held-out matches, against a sensible baseline.
#   2. SPLIT-HALF reliability of the player coefficients themselves -- a rating
#      that does not replicate on independent halves of the data is not a
#      rating. §7.21 put the ceiling for this quantity around 0.33.
suppressMessages({library(arrow); library(data.table); library(Matrix); library(glmnet)})

DD <- "C:/dev/torpverse/torpdata/data/"
SEASONS <- 2021:2026
MIN_GAMES <- 10L     # below this a player is pooled into a replacement bucket
rdp <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(DD, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names=TRUE, fill=TRUE)

pg <- rdp("player_game_%d.parquet", SEASONS)
pg <- pg[!is.na(team_id) & !is.na(player_id)]
pg[, tog := pmax(pmin(fifelse(is.na(time_on_ground_percentage), 80,
                              time_on_ground_percentage)/100, 1), 0.05)]
res <- rdp("results_%d.parquet", SEASONS)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, season, home_team_id, away_team_id, home_score, away_score)]
mdate <- unique(pg[, .(match_id, mdate = as.Date(utc_start_time))], by="match_id")
res <- merge(res, mdate, by="match_id")
setorder(res, mdate)

# replacement-level pooling: rare players share one column so they do not each
# get a noisy free parameter
gcount <- pg[, .N, by=player_id]
pg[, pid := fifelse(player_id %in% gcount[N >= MIN_GAMES, player_id],
                    player_id, "__REPLACEMENT__")]
players <- sort(unique(pg$pid))
P <- length(players)
cat(sprintf("matches: %s | players with >=%d games: %d (of %d)\n",
            format(nrow(res), big.mark=","), MIN_GAMES, P - 1L, nrow(gcount)))

app <- pg[, .(tog = max(tog)), by = .(match_id, team_id, pid)]
app[, pcol := match(pid, players)]

# ---- design matrices --------------------------------------------------------
build_margin <- function(weighted) {
  m_idx <- match(app$match_id, res$match_id)
  side <- fifelse(app$team_id == res$home_team_id[m_idx], 1, -1)
  w <- if (weighted) app$tog else 1
  keep <- !is.na(m_idx)
  X <- sparseMatrix(i = m_idx[keep], j = app$pcol[keep], x = (side*w)[keep],
                    dims = c(nrow(res), P))
  list(X = X, y = res$home_score - res$away_score, grp = seq_len(nrow(res)))
}

build_split <- function(weighted, target = c("points","prevent")) {
  target <- match.arg(target)
  sides <- rbind(
    res[, .(match_id, season, mdate, team_id = home_team_id, opp_id = away_team_id,
            scored = home_score, conceded = away_score, row = .I)],
    res[, .(match_id, season, mdate, team_id = away_team_id, opp_id = home_team_id,
            scored = away_score, conceded = home_score, row = .I + nrow(res))])
  setorder(sides, row)
  if (target == "prevent") {
    ent <- pg[, .(entries = sum(inside50s, na.rm=TRUE)), by=.(match_id, team_id)]
    sides <- merge(sides, ent[, .(match_id, opp_id=team_id, opp_entries=entries)],
                   by=c("match_id","opp_id"), all.x=TRUE)
    setorder(sides, row)
    sides <- sides[!is.na(opp_entries) & opp_entries > 0]
    oq <- sides[, .(opp_ppe = sum(conceded)/sum(opp_entries)), by=.(opp_id, season)]
    sides <- merge(sides, oq, by=c("opp_id","season")); setorder(sides, row)
    f <- lm(conceded ~ opp_entries + opp_ppe, data = sides)
    sides[, y := predict(f, sides) - conceded]   # higher = prevented more
  } else {
    sides[, y := scored]
  }
  sides[, rid := .I]
  own <- merge(sides[, .(rid, match_id, team_id)], app, by=c("match_id","team_id"))
  opp <- merge(sides[, .(rid, match_id, team_id = opp_id)], app, by=c("match_id","team_id"))
  w_own <- if (weighted) own$tog else 1
  w_opp <- if (weighted) opp$tog else 1
  X <- sparseMatrix(i = c(own$rid, opp$rid),
                    j = c(own$pcol, opp$pcol + P),
                    x = c(w_own, w_opp),
                    dims = c(nrow(sides), 2L*P))
  list(X = X, y = sides$y, grp = sides$match_id, sides = sides)
}

# ---- fit + evaluate ---------------------------------------------------------
eval_flavour <- function(name, D, seed = 20260727) {
  set.seed(seed)
  n <- nrow(D$X)
  # hold out whole MATCHES, not rows -- the two sides of a match share players
  ug <- unique(D$grp); ho <- sample(ug, floor(length(ug)*0.25))
  te <- D$grp %in% ho; tr <- !te
  cv <- cv.glmnet(D$X[tr, ], D$y[tr], alpha = 0, nfolds = 5, standardize = FALSE)
  pr <- as.numeric(predict(cv, newx = D$X[te, ], s = "lambda.min"))
  base <- mean(D$y[tr])
  r2 <- 1 - sum((D$y[te] - pr)^2) / sum((D$y[te] - base)^2)
  mae <- mean(abs(D$y[te] - pr))
  mae_b <- mean(abs(D$y[te] - base))

  # split-half reliability of the COEFFICIENTS themselves
  h <- sample(ug, floor(length(ug)/2)); a <- D$grp %in% h; b <- !a
  ca <- as.numeric(coef(glmnet(D$X[a,], D$y[a], alpha=0, lambda=cv$lambda.min,
                               standardize=FALSE)))[-1]
  cb <- as.numeric(coef(glmnet(D$X[b,], D$y[b], alpha=0, lambda=cv$lambda.min,
                               standardize=FALSE)))[-1]
  ok <- is.finite(ca) & is.finite(cb) & (ca != 0 | cb != 0)
  rel <- suppressWarnings(cor(ca[ok], cb[ok]))

  full <- as.numeric(coef(glmnet(D$X, D$y, alpha=0, lambda=cv$lambda.min,
                                 standardize=FALSE)))[-1]
  cat(sprintf("%-22s lambda %8.2f | OOS R2 %+.4f | MAE %6.2f (base %6.2f) | coef split-half r %+.3f\n",
              name, cv$lambda.min, r2, mae, mae_b, rel))
  list(coef = full, r2 = r2, rel = rel)
}

cat("\n=== RAPM FLAVOURS ===\n")
fits <- list()
fits[["margin (binary)"]]   <- eval_flavour("margin (binary)",   build_margin(FALSE))
fits[["margin (TOG-wt)"]]   <- eval_flavour("margin (TOG-wt)",   build_margin(TRUE))
fits[["split pts (binary)"]] <- eval_flavour("split pts (binary)", build_split(FALSE, "points"))
fits[["split pts (TOG-wt)"]] <- eval_flavour("split pts (TOG-wt)", build_split(TRUE, "points"))
fits[["prevention (TOG-wt)"]] <- eval_flavour("prevention (TOG-wt)", build_split(TRUE, "prevent"))

# ---- read the defensive ratings out -----------------------------------------
nm <- unique(pg[, .(pid, player_name)], by = "pid")
pos <- pg[, .N, by = .(pid, position_group)][order(-N)][, .SD[1], by = pid]
lookup <- merge(data.table(pid = players, idx = seq_along(players)), nm, by="pid")
lookup <- merge(lookup, pos[, .(pid, position_group)], by="pid", all.x=TRUE)
setorder(lookup, idx)

show_def <- function(fitname, block_offset) {
  cf <- fits[[fitname]]$coef
  d <- copy(lookup)
  # opp-block coefficient: effect of this player on the OPPOSITION's target.
  # For points, lower is better, so flip the sign to make "higher = better".
  d[, val := -cf[idx + block_offset]]
  d <- d[pid != "__REPLACEMENT__"]
  cat(sprintf("\n--- %s: top 12 defensive ratings (all positions) ---\n", fitname))
  print(head(d[order(-val), .(player_name, position_group, val = round(val, 3))], 12))
  cat(sprintf("--- %s: top 10 KEY DEFENDERS ---\n", fitname))
  print(head(d[position_group == "KEY_DEFENDER"][order(-val),
              .(player_name, val = round(val, 3))], 10))
  invisible(d)
}
show_def("split pts (TOG-wt)", P)
show_def("prevention (TOG-wt)", P)

cat("\n=== do the two defensive flavours agree? ===\n")
a <- lookup[, .(idx, pid)]; a[, v1 := -fits[["split pts (TOG-wt)"]]$coef[idx + P]]
a[, v2 := -fits[["prevention (TOG-wt)"]]$coef[idx + P]]
a <- a[pid != "__REPLACEMENT__"]
cat(sprintf("  cor(DRAPM points, DRAPM prevention) = %+.3f over %d players\n",
            cor(a$v1, a$v2), nrow(a)))
