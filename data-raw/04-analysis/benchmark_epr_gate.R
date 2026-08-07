# Does the rating causally track a team's points? Fast, and with team fixed effects.
#
# REPLACES the Brownlow check, vetoed 2026-08-05 and rightly: umpire votes are an
# opinion about who looked good, not a measurement of contribution to score, and
# they are midfielder-biased in exactly the direction that would undo the
# defender work.
#
# WHY AN EPR-ONLY MODEL IS BETTER THAN THE PRODUCTION GATE, and it is NOT about
# sample size -- both see the same matches. It is about EFFECT SIZE. The
# production pipeline feeds five GAMs and an XGBoost `epr_diff`, `psr_diff`,
# `torp_diff`, `elo_diff`, `xelo_diff`, weather and venue, then blends. It
# reweights whatever changes, which is precisely why five separate rating
# changes have gated neutral there. Strip it to the rating alone and a rating
# improvement has nowhere to hide: the signal is undiluted, so the same true
# effect produces a much larger measured one against the same noise.
#
# It also runs in seconds rather than 20 minutes an arm, so it can be iterated
# against instead of consulted twice a day.
#
# THE FIXED-EFFECTS PART IS THE CAUSAL CLAIM. Regressing margin on rating
# difference across all teams mostly re-measures "good clubs win", which we
# already know. With team and season fixed effects in, the surviving variation
# is WITHIN club: the same team, same year, fielding different players week to
# week. If the coefficient holds there, the rating is tracking who is playing
# rather than which jumper they wear. That is the nearest thing to a causal read
# available without a randomised lineup.
#
# WHY NOT RAPM. Measured earlier in this program and it is not identifiable on
# AFL data: teammate co-appearance runs ~0.97, so a weak prior returns team
# noise and a strong one returns the prior. The salvageable part of the idea is
# exactly the fixed-effects design here -- use within-team lineup variation
# rather than trying to separate 22 always-together players.
#
# FOUR READS, and the last one is the defender question stated directly:
#   1. pooled          margin ~ rating_diff             -- calibration, target 1.0
#   2. within team     + team and season fixed effects  -- the causal-ish read
#   3. out of sample   fit on seasons < s, score s      -- MAE/RMSE, all seasons
#   4. for vs against  points scored ~ attacking rating,
#                      points conceded ~ defensive rating
#
# Read 4 matters because torp assigns no defensive credit at all. If a key
# defender's rating does not predict points CONCEDED, the metric is not
# measuring defence, and no amount of leaderboard tuning fixes that.

suppressMessages({ library(data.table) })

#' Team-match lineup ratings from who actually played
#'
#' Joins the rating a player carried BEFORE the match to the players who
#' actually took the field. Uses the real 22 rather than a list-based team
#' strength, so a late out is reflected.
.bm_lineup <- function(pgd, ratings, rating_col = "epr") {
  p <- data.table(match_id = as.character(pgd$match_id), player_id = pgd$player_id,
                  team = pgd$team, season = pgd$season, round = pgd$round,
                  pos = pgd$position_group)
  r <- data.table(player_id = ratings$player_id, season = ratings$season,
                  round = ratings$round, v = ratings[[rating_col]])
  r <- r[is.finite(v)]
  # The rating as at the START of the round, i.e. the previous round's value.
  # Joining on the same round would use a rating that already contains the match
  # being predicted -- leakage, and it would flatter every arm equally, which is
  # worse than useless because it hides real differences.
  r[, round := round + 1L]
  m <- merge(p, r, by = c("player_id", "season", "round"))
  m[]
}

bm_epr_gate <- function(pgd, ratings, results, label, rating_col = "epr",
                        def_positions = c("KEY_DEFENDER", "MEDIUM_DEFENDER")) {
  lu <- .bm_lineup(pgd, ratings, rating_col)
  cov <- nrow(lu) / nrow(pgd)
  tm <- lu[, .(rating = sum(v), n_players = .N,
               rating_def = sum(v[pos %chin% def_positions]),
               rating_att = sum(v[!pos %chin% def_positions])),
           by = .(match_id, team, season)]

  r <- as.data.table(results)
  tg <- r[, .(match_id = as.character(match_id), home = home_team_name,
              away = away_team_name, hs = home_score, as_ = away_score)]
  tg <- tg[is.finite(hs) & is.finite(as_)]
  h <- merge(tg, tm, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tg, tm, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, .(match_id, season, margin = hs - as_, hs, as_, home,
                   r_h = rating, def_h = rating_def, att_h = rating_att)],
             a[, .(match_id, away, r_a = rating, def_a = rating_def, att_a = rating_att)],
             by = "match_id")
  m[, d := r_h - r_a]
  m <- m[is.finite(d) & is.finite(margin)]

  fit <- function(f, dat) {
    z <- summary(stats::lm(f, data = dat))
    list(coef = round(stats::coef(z)[2, 1], 4), t = round(stats::coef(z)[2, 3], 2),
         r2 = round(z$r.squared, 4))
  }
  pooled <- fit(margin ~ d, m)
  # Team fixed effects: the home and away club, plus season. What survives is
  # within-club, within-year lineup variation.
  m[, `:=`(fh = factor(home), fa = factor(away), fs = factor(season))]
  fe <- fit(margin ~ d + fh + fa + fs, m)

  # Out of sample, expanding window over every season we have -- not the match
  # gate's two.
  seasons <- sort(unique(m$season)); oos <- NULL
  if (length(seasons) > 2) {
    oos <- rbindlist(lapply(seasons[-1], function(s) {
      tr <- m[season < s]; te <- m[season == s]
      if (nrow(tr) < 100 || nrow(te) < 20) return(NULL)
      f <- stats::lm(margin ~ d, data = tr)
      p <- stats::predict(f, newdata = te)
      # Probability metrics as well as error ones, because they answer a
      # different question and have disagreed before: a change can shorten the
      # average miss while making the model worse-calibrated about who wins.
      # Quoting MAE alone once made a position-split arm look like a winner
      # when it was worst of five on Brier, logloss and bits.
      #
      # The margin model has no probability of its own, so one is derived the
      # standard way: P(home win) = Phi(pred / sigma) with sigma the TRAINING
      # residual sd. Training, not test -- using the test spread would leak the
      # season being scored into its own forecast.
      sigma <- stats::sd(stats::residuals(f))
      pw <- stats::pnorm(p / sigma)
      pw <- pmin(pmax(pw, 1e-6), 1 - 1e-6)
      # Draws are 0.5, matching the production gate's convention rather than
      # dropping them -- a drawn game is not a missing observation.
      hw <- data.table::fifelse(te$margin > 0, 1, data.table::fifelse(te$margin < 0, 0, 0.5))
      data.table(season = s, n = nrow(te), mae = mean(abs(p - te$margin)),
                 rmse = sqrt(mean((p - te$margin)^2)),
                 tips = mean((p > 0) == (te$margin > 0)),
                 brier = mean((pw - hw)^2),
                 logloss = -mean(hw * log(pw) + (1 - hw) * log(1 - pw)),
                 bits = mean(data.table::fifelse(
                   hw == 1, 1 + log2(pw),
                   data.table::fifelse(hw == 0, 1 + log2(1 - pw),
                                       1 + 0.5 * log2(pw * (1 - pw))))))
    }))
  }

  # For and against, separately -- and WITH the same fixed effects, because
  # without them this measures almost nothing. A club with well-rated defenders
  # is usually a good club, so a raw "points conceded ~ opponent defensive
  # rating" coefficient is mostly "good teams concede less", and reading it as
  # evidence that torp measures defence would be reading team quality.
  #
  # Both are kept: the raw pair for continuity, the fixed-effects pair to believe.
  fa_ <- rbind(
    m[, .(pts = hs, own_att = att_h, opp_def = def_a, own = home, opp = away, season)],
    m[, .(pts = as_, own_att = att_a, opp_def = def_h, own = away, opp = home, season)])
  fa_[, `:=`(fo = factor(own), fd = factor(opp), fs = factor(season))]
  att <- fit(pts ~ own_att, fa_)
  def <- fit(pts ~ opp_def, fa_)
  att_fe <- fit(pts ~ own_att + fo + fd + fs, fa_)
  def_fe <- fit(pts ~ opp_def + fo + fd + fs, fa_)

  out <- list(label = label, lineup_coverage = round(cov, 3),
              n_matches = nrow(m), mean_players = round(mean(tm$n_players), 1),
              pooled = pooled, fixed_effects = fe,
              oos = oos,
              oos_mae     = if (!is.null(oos)) round(mean(oos$mae), 4) else NA_real_,
              oos_rmse    = if (!is.null(oos)) round(mean(oos$rmse), 4) else NA_real_,
              oos_tips    = if (!is.null(oos)) round(mean(oos$tips), 4) else NA_real_,
              oos_brier   = if (!is.null(oos)) round(mean(oos$brier), 5) else NA_real_,
              oos_logloss = if (!is.null(oos)) round(mean(oos$logloss), 5) else NA_real_,
              oos_bits    = if (!is.null(oos)) round(mean(oos$bits), 5) else NA_real_,
              attack = att, defence = def,
              attack_fe = att_fe, defence_fe = def_fe)
  class(out) <- c("torp_epr_gate", "list"); out
}

print.torp_epr_gate <- function(x, ...) {
  cat("\n=== EPR gate:", x$label, "===\n")
  cat(sprintf("  %d matches | %.1f rated players per team | lineup coverage %.1f%%\n",
              x$n_matches, x$mean_players, 100 * x$lineup_coverage))
  if (x$lineup_coverage < 0.8)
    cat("  !! coverage below 80% -- many players had no prior-round rating; read with care\n")
  cat("\n  1. POOLED       margin ~ rating diff\n")
  cat(sprintf("     coef %.4f (t %.1f)  R2 %.4f   -- target 1.0 if calibrated\n",
              x$pooled$coef, x$pooled$t, x$pooled$r2))
  cat("\n  2. WITHIN TEAM  + club and season fixed effects\n")
  cat(sprintf("     coef %.4f (t %.1f)  R2 %.4f\n",
              x$fixed_effects$coef, x$fixed_effects$t, x$fixed_effects$r2))
  cat("     This is the causal-ish read: same club, same year, different players.\n")
  cat("     A coefficient that collapses here was measuring which club, not who played.\n")
  if (!is.null(x$oos)) {
    cat("\n  3. OUT OF SAMPLE  fit on earlier seasons, score the next\n")
    cat(sprintf("     across %d seasons: MAE %.3f | RMSE %.3f | tips %.3f | Brier %.5f | logloss %.5f | bits %.5f\n",
                nrow(x$oos), x$oos_mae, x$oos_rmse, x$oos_tips,
                x$oos_brier, x$oos_logloss, x$oos_bits))
    print(x$oos[, .(season, n, mae = round(mae, 2), rmse = round(rmse, 2),
                    tips = round(tips, 3), brier = round(brier, 4),
                    logloss = round(logloss, 4), bits = round(bits, 4))])
    cat("     MAE/RMSE lower better; tips/bits HIGHER better; Brier/logloss lower.\n")
    cat("     Win probability is Phi(pred / training residual sd) -- the margin\n")
    cat("     model has none of its own. Draws count as 0.5, not dropped.\n")
  }
  cat("\n  4. FOR AND AGAINST\n")
  cat(sprintf("     points scored   ~ own attack rating    raw %+.4f (t %5.1f) | +FE %+.4f (t %5.1f)\n",
              x$attack$coef, x$attack$t, x$attack_fe$coef, x$attack_fe$t))
  cat(sprintf("     points conceded ~ opp defence rating   raw %+.4f (t %5.1f) | +FE %+.4f (t %5.1f)\n",
              x$defence$coef, x$defence$t, x$defence_fe$coef, x$defence_fe$t))
  cat("     BELIEVE THE +FE COLUMN. A club with well-rated defenders is usually a\n")
  cat("     good club, so the raw coefficient is largely 'good teams concede less'.\n")
  cat("     With club fixed effects in, what survives is the SAME club conceding\n")
  cat("     more or less depending on which defenders it picked. That is the\n")
  cat("     defender question stated directly, and torp assigns no defensive\n")
  cat("     credit -- so a collapse here is expected, and is the thing to fix.\n\n")
  invisible(x)
}

compare_epr_gates <- function(a, b) {
  # All four OOS metrics, always. Error metrics and probability metrics answer
  # different questions and have disagreed here before -- a change that shortens
  # the average miss can still be worse at saying who wins. `better` states the
  # direction per row so nobody has to remember which way Brier runs.
  rows <- data.table(
    metric = c("pooled coef", "pooled R2", "within-team coef", "within-team t",
               "within-team R2",
               "OOS MAE", "OOS RMSE", "OOS tips", "OOS Brier", "OOS logloss", "OOS bits",
               "points-conceded coef (+FE)"),
    better = c("nearer 1.0", "higher", "nearer 1.0", "higher", "higher",
               "lower", "lower", "higher", "lower", "lower", "higher",
               "more negative"),
    a = c(a$pooled$coef, a$pooled$r2, a$fixed_effects$coef, a$fixed_effects$t,
          a$fixed_effects$r2, a$oos_mae, a$oos_rmse, a$oos_tips,
          a$oos_brier, a$oos_logloss, a$oos_bits, a$defence_fe$coef),
    b = c(b$pooled$coef, b$pooled$r2, b$fixed_effects$coef, b$fixed_effects$t,
          b$fixed_effects$r2, b$oos_mae, b$oos_rmse, b$oos_tips,
          b$oos_brier, b$oos_logloss, b$oos_bits, b$defence_fe$coef))
  setnames(rows, c("a", "b"), c(a$label, b$label))
  rows[, delta := round(get(b$label) - get(a$label), 5)]
  cat("\n=== EPR gate: ", a$label, " vs ", b$label, " ===\n", sep = "")
  print(rows)

  # A one-line verdict across the four OOS metrics, because reading six rows and
  # forming an impression is how "wins on MAE" got quoted as "better" before.
  o <- rows[metric %chin% c("OOS MAE", "OOS RMSE", "OOS Brier", "OOS logloss", "OOS bits")]
  dir <- data.table::fifelse(o$better == "higher", 1, -1)
  imp <- sum(dir * o$delta > 0); wor <- sum(dir * o$delta < 0)
  cat(sprintf("\nOOS metrics: %d better, %d worse, %d unchanged (of %d).\n",
              imp, wor, nrow(o) - imp - wor, nrow(o)))
  cat("A change worth making does not lose on any of them and wins on more than one.\n")
  cat("Within-team coefficient nearer 1.0 with a higher t means the rating tracks\n")
  cat("WHO IS PLAYING rather than which club it is.\n\n")
  invisible(rows)
}
