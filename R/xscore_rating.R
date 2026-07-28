# xScore Team Power Rating
# ========================
# Sequential, pre-match team strength rating used as a match-model feature
# (xelo_diff), replacing the win-based Elo (team_elo.R) that shipped with C6.
#
# HOW IT DIFFERS FROM team_elo.R, AND WHY
# ---------------------------------------
# `build_team_elo()` is a classical Elo: it updates on a binary win/loss with a
# 538-style margin multiplier, producing a rating on an abstract scale that then
# has to be mapped to points. This rating instead lives directly in POINTS space
# and updates on the prediction error of an *expected*-score margin:
#
#     predicted = (rating_home - rating_away) + hga
#     rating_home += k * (xscore_margin - predicted)
#
# Two consequences. First, the rating is already a margin forecast, so nothing
# is lost in a rating-to-points mapping. Second, and the actual point: the
# scoreboard margin is a noisy realisation of team quality — AFL conversion
# variance is large, so a side can dominate territory and shots and still lose.
# Updating on expected score strips that conversion noise out, so the rating
# re-converges faster after genuine form changes and wanders less after flukes.
# **No competitor can build this** — xScore is torp's own.
#
# EVIDENCE (2026-07-28, docs/plans/FABLE-MATCH-FEATURES-PLAN.md §6.1 and §6.4)
# As a standalone margin predictor, identical 695-match set, 2023-2026:
#   win-based Elo (team_elo.R)   MAE 27.15  cor 0.524
#   this rating                  MAE 26.38  cor 0.559
# and this rating renders the Elo redundant (beta(elo) 0.09, p 9e-10), not the
# other way round. In-model (rolling OOS, pooled 2025:2026, n=387, production
# trainers, swapping only this feature) it improved ALL SIX headline metrics:
#   MAE 25.622 -> 25.510, RMSE 32.646 -> 32.525, Brier 0.17891 -> 0.17696,
#   bits 0.23135 -> 0.23701, slope 0.959 -> 0.982, cor 0.610 -> 0.613.
# The MAE bootstrap CI still spans zero ([-0.442, +0.219]) — the effect is
# smaller than the measured XGBoost retraining noise floor -- so this was adopted
# under G7's EXPLORE tier, not a bootstrap-confirmed SHIP. See D-M1.
#
# A fast-k variant was tested and was a wash (MAE 25.643): the chain's
# player-rating features already carry short-term form, so what it wants from a
# team rating is a stable class signal.
#
# Leak-safe by construction: a match's rating is a function only of strictly
# earlier matches, so the whole table is computed once and joined on.

# .xscore_matches_from_team_mdl_df ----

#' Build a one-row-per-match table for xScore-rating construction
#'
#' @param team_mdl_df Complete model dataset from \code{.build_team_mdl_df()}
#' @return data.frame(match_id, date, season, round, home_team, away_team,
#'   home_xmargin), completed matches only, sorted by time
#' @keywords internal
.xscore_matches_from_team_mdl_df <- function(team_mdl_df) {
  # Fail with the actual problem rather than data.frame()'s "arguments imply
  # differing number of rows", which is what a missing column produces here
  # (h$missing_col is NULL, so the column is silently zero-length).
  required <- c("team_type", "win", "match_id", "utc_start_time", "season.x",
                "round_number.x", "team_name.x", "team_name.y",
                "xscore_diff", "score_diff")
  missing <- setdiff(required, names(team_mdl_df))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "team_mdl_df is missing column{?s} required by the xScore rating: {.val {missing}}",
      "i" = "Expected the output of .build_team_mdl_df(); see xscore_rating.R."
    ))
  }

  h <- team_mdl_df[team_mdl_df$team_type == "home" & !is.na(team_mdl_df$win), ]
  out <- data.frame(
    match_id     = h$match_id,
    date         = as.Date(h$utc_start_time),
    season       = h$season.x,
    round        = h$round_number.x,
    home_team    = as.character(h$team_name.x),
    away_team    = as.character(h$team_name.y),
    home_xmargin = h$xscore_diff,
    home_margin  = h$score_diff,
    stringsAsFactors = FALSE
  )

  # xScore is the whole basis of this rating; where the xG join failed, fall
  # back to the actual margin rather than dropping the match (dropping would
  # silently break the sequence for both teams). Warn so a systematic gap is
  # visible rather than quietly degrading the rating to a plain points Elo.
  n_na <- sum(is.na(out$home_xmargin))
  if (n_na > 0) {
    pct <- round(100 * n_na / nrow(out), 1)
    if (pct > 10) {
      cli::cli_warn(c(
        "xScore rating: {n_na} of {nrow(out)} matches ({pct}%) have no xScore margin.",
        "!" = "Above 10% the rating degrades toward a plain points rating -- check the xG join."
      ))
    } else {
      cli::cli_inform("xScore rating: {n_na} match{?es} without xScore margin, using actual margin there")
    }
    out$home_xmargin[is.na(out$home_xmargin)] <- out$home_margin[is.na(out$home_xmargin)]
  }

  out[order(out$season, out$round, out$date, out$match_id), ]
}

# build_xscore_rating ----

#' Sequential xScore-based team power rating, pre-match ratings only
#'
#' @param matches data.frame with match_id, date, season, round, home_team,
#'   away_team, home_xmargin (see \code{.xscore_matches_from_team_mdl_df()})
#' @param k Update rate in points-per-point-of-error. Default
#'   \code{XRATING_K} (fitted on pre-2025 data only).
#' @param hga Home-ground advantage in points, added to the predicted margin
#'   before the error is computed. Default \code{XRATING_HGA}.
#' @param carryover Fraction of rating retained across a season boundary
#'   (0 = full reset to average). Default \code{XRATING_CARRYOVER}.
#' @return A list with:
#'   \item{by_match}{data.frame(match_id, team_name, rating_pre) -- two rows per
#'     match, the rating immediately BEFORE that match.}
#'   \item{current}{data.frame(team_name, rating_current) -- rating after each
#'     team's most recent completed match, for upcoming fixtures.}
#' @keywords internal
build_xscore_rating <- function(matches, k = XRATING_K, hga = XRATING_HGA,
                                carryover = XRATING_CARRYOVER) {
  stopifnot(all(c("match_id", "season", "home_team", "away_team", "home_xmargin") %in%
                  names(matches)))

  teams <- sort(unique(c(matches$home_team, matches$away_team)))
  # Ratings are centred on 0 (a league-average team), not 1500 -- the scale is
  # points of expected margin, so 0 is the natural origin.
  r <- stats::setNames(rep(0, length(teams)), teams)
  last_season <- stats::setNames(rep(NA_integer_, length(teams)), teams)

  n <- nrow(matches)
  pre_home <- pre_away <- numeric(n)

  for (i in seq_len(n)) {
    h <- matches$home_team[i]
    a <- matches$away_team[i]
    s <- matches$season[i]

    # Season-boundary regression to the mean, applied lazily on first sight of
    # each team in a new season (same convention as build_team_elo()).
    if (!is.na(last_season[[h]]) && last_season[[h]] < s) r[[h]] <- carryover * r[[h]]
    if (!is.na(last_season[[a]]) && last_season[[a]] < s) r[[a]] <- carryover * r[[a]]

    pre_home[i] <- r[[h]]
    pre_away[i] <- r[[a]]

    err <- matches$home_xmargin[i] - ((r[[h]] - r[[a]]) + hga)
    r[[h]] <- r[[h]] + k * err
    r[[a]] <- r[[a]] - k * err

    last_season[[h]] <- s
    last_season[[a]] <- s
  }

  list(
    by_match = data.frame(
      match_id   = rep(matches$match_id, 2),
      team_name  = c(matches$home_team, matches$away_team),
      rating_pre = c(pre_home, pre_away),
      stringsAsFactors = FALSE
    ),
    current = data.frame(
      team_name      = names(r),
      rating_current = unname(r),
      stringsAsFactors = FALSE
    )
  )
}

# join_xrating_diff_to_team_mdl_df ----

#' Join per-team xScore ratings onto team_mdl_df as a symmetric \code{xelo_diff}
#'
#' Same conventions as \code{join_elo_diff_to_team_mdl_df()}: this team's rating
#' minus its opponent's, with no HGA baked in (\code{team_type_fac} already
#' carries home advantage). Unplayed fixtures -- which is exactly what
#' \code{run_predictions_pipeline()} needs the feature for -- fall back to each
#' team's current rating; a team with no history at all falls back to 0.
#'
#' @param team_mdl_df Complete model dataset
#' @param rating_result Output of \code{build_xscore_rating()}
#' @return team_mdl_df with an added numeric \code{xelo_diff} column
#' @keywords internal
join_xrating_diff_to_team_mdl_df <- function(team_mdl_df, rating_result) {
  # String keys rather than factor joins: factor level mismatches join silently
  # and wrongly, which is the failure mode elo_lib.R documents.
  key <- stats::setNames(rating_result$by_match$rating_pre,
                          paste(rating_result$by_match$match_id,
                                rating_result$by_match$team_name))
  cur <- stats::setNames(rating_result$current$rating_current,
                          rating_result$current$team_name)

  tx <- as.character(team_mdl_df$team_name.x)
  ty <- as.character(team_mdl_df$team_name.y)

  rx <- unname(key[paste(team_mdl_df$match_id, tx)])
  ry <- unname(key[paste(team_mdl_df$match_id, ty)])

  n_fallback <- sum(is.na(rx) | is.na(ry))
  rx[is.na(rx)] <- unname(cur[tx[is.na(rx)]])
  ry[is.na(ry)] <- unname(cur[ty[is.na(ry)]])

  n_unrated <- sum(is.na(rx) | is.na(ry))
  rx[is.na(rx)] <- 0
  ry[is.na(ry)] <- 0

  team_mdl_df$xelo_diff <- rx - ry

  if (n_fallback > 0) {
    cli::cli_inform("join_xrating_diff_to_team_mdl_df: {n_fallback} row{?s} used the current-rating fallback (upcoming fixtures)")
  }
  if (n_unrated > 0) {
    cli::cli_inform("join_xrating_diff_to_team_mdl_df: {n_unrated} row{?s} ha{?s/ve} a team with no rating history at all -- using neutral 0")
  }
  team_mdl_df
}

# build_xrating_diff ----

#' Convenience wrapper: build the xScore rating and join \code{xelo_diff} on
#'
#' @param team_mdl_df Complete model dataset from \code{.build_team_mdl_df()}
#' @inheritParams build_xscore_rating
#' @return team_mdl_df with an added \code{xelo_diff} column
#' @keywords internal
build_xrating_diff <- function(team_mdl_df, k = XRATING_K, hga = XRATING_HGA,
                               carryover = XRATING_CARRYOVER) {
  matches <- .xscore_matches_from_team_mdl_df(team_mdl_df)
  rating <- build_xscore_rating(matches, k = k, hga = hga, carryover = carryover)
  join_xrating_diff_to_team_mdl_df(team_mdl_df, rating)
}
