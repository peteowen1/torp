# Team Elo Rating
# ================
# Sequential, pre-match team Elo rating used as a match-model feature
# (elo_diff). Leak-safe by construction: each match's elo_pre is a function
# only of strictly-earlier matches (sorted by date), so the whole table can
# be computed once and joined onto team_mdl_df without per-round retraining.
#
# Validated via the leak-safe rolling week-by-week OOS harness (2026-07,
# FABLE-MATCH-MAE-PLAN.md WS2/WS5): adding elo_diff as a GAM/XGB feature
# alongside the existing player-rating diffs (epr/psr/torp_diff) was part of
# the confirmed match-MAE improvement (candidate "C6").

# .matches_from_team_mdl_df ----

#' Build a one-row-per-match table from team_mdl_df for Elo construction
#'
#' @param team_mdl_df Complete model dataset from \code{.build_team_mdl_df()}
#' @return data.frame(match_id, date, season, round, home_team, away_team,
#'   home_margin), completed matches only, sorted by date then match_id
#' @keywords internal
.matches_from_team_mdl_df <- function(team_mdl_df) {
  # Filter on the margin as well as win: a completed row with a win but no
  # score_diff would otherwise reach build_team_elo() and be scored as a draw.
  # (.build_team_mdl_df() derives win from score_diff, so today the two are NA
  # together; this keeps that from being load-bearing.)
  out <- team_mdl_df[team_mdl_df$team_type == "home" &
                       !is.na(team_mdl_df$win) & !is.na(team_mdl_df$score_diff), ]
  data.frame(
    match_id    = out$match_id,
    date        = as.Date(out$utc_start_time),
    season      = out$season.x,
    round       = out$round_number.x,
    home_team   = as.character(out$team_name.x),
    away_team   = as.character(out$team_name.y),
    home_margin = out$score_diff,
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(date, match_id)
}

# .elo_mov_mult ----

#' 538-style margin-of-victory multiplier for one Elo update
#'
#' @param margin Final margin (any sign; only its size is used here).
#' @param elo_diff_winner Winner's pre-match rating minus the loser's. Positive
#'   for a favourite win, negative for an upset.
#' @return Positive multiplier. The denominator is floored at
#'   \code{ELO_MOV_DENOM_MIN} so it cannot reach zero or turn negative.
#' @keywords internal
.elo_mov_mult <- function(margin, elo_diff_winner) {
  log(abs(margin) + 1) * (2.2 / pmax(0.001 * elo_diff_winner + 2.2, ELO_MOV_DENOM_MIN))
}

# .elo_regress ----

#' Regress a rating toward \code{ELO_INIT} across season boundaries
#'
#' One boundary keeps \code{carryover} of the rating; \code{n_seasons}
#' boundaries keep \code{carryover^n_seasons}, which is what applying the
#' single-boundary step once per season would give.
#'
#' @param elo Rating(s) before regression.
#' @param n_seasons Number of season boundaries crossed (>= 1). Vectorised.
#' @param carryover Fraction retained per boundary.
#' @return Regressed rating(s).
#' @keywords internal
.elo_regress <- function(elo, n_seasons, carryover = ELO_CARRYOVER) {
  keep <- carryover^n_seasons
  keep * elo + (1 - keep) * ELO_INIT
}

# build_team_elo ----

#' Sequential team-Elo rating, pre-match ratings only
#'
#' Standard Elo with home-ground advantage and a 538-style, winner-relative
#' margin-of-victory multiplier: it dampens updates when the favourite wins
#' as expected (prevents runaway ratings for dominant teams) and amplifies
#' them on an upset, so ratings correct faster after a form change. Applies
#' partial regression-to-mean at each team's first match of a new season.
#'
#' @param matches data.frame with columns match_id, date, season, home_team,
#'   away_team, home_margin (home_score - away_score), sorted by date
#'   ascending (see \code{.matches_from_team_mdl_df()}).
#' @param k Update rate (higher = faster-moving ratings). Default
#'   \code{ELO_K} (tuned via grid search on pre-2025 data).
#' @param hga Home-ground advantage, in Elo points, added to the home team's
#'   rating before computing expected result. Default \code{ELO_HGA}.
#' @param carryover Fraction of a team's rating retained across a season
#'   boundary (1 = no regression, 0 = full reset to \code{ELO_INIT}). Default
#'   \code{ELO_CARRYOVER}.
#' @param mov_mult Logical; apply the log-margin x anti-runaway multiplier
#'   (538-style) to the update magnitude. FALSE = plain win/loss/draw Elo.
#' @return A list with:
#'   \item{by_match}{data.table(match_id, team_name, elo_pre) -- two rows per
#'     match (home team + away team), elo_pre = rating immediately BEFORE
#'     that match.}
#'   \item{current}{data.table(team_name, elo_current) -- each team's rating
#'     immediately AFTER its most recent match in \code{matches}. Used for
#'     upcoming/unplayed fixtures, where there is no historical elo_pre to
#'     look up (see \code{join_elo_diff_to_team_mdl_df()}).}
#' @keywords internal
build_team_elo <- function(matches, k = ELO_K, hga = ELO_HGA, carryover = ELO_CARRYOVER,
                            mov_mult = TRUE) {
  stopifnot(all(c("match_id", "date", "season", "home_team", "away_team", "home_margin") %in% names(matches)))
  matches <- matches[order(matches$date, matches$match_id), ]

  teams <- sort(unique(c(matches$home_team, matches$away_team)))
  elo <- stats::setNames(rep(ELO_INIT, length(teams)), teams)
  last_season <- stats::setNames(rep(NA_integer_, length(teams)), teams)

  n <- nrow(matches)
  pre_home <- numeric(n)
  pre_away <- numeric(n)

  for (i in seq_len(n)) {
    h <- matches$home_team[i]
    a <- matches$away_team[i]
    s <- matches$season[i]

    # Season-boundary regression-to-mean, applied lazily on first sight of
    # each team in a new season. Ratings are otherwise untouched between
    # matches, so this equals applying one step per team per season boundary
    # -- including boundaries a team sat out entirely (carryover^gap).
    if (!is.na(last_season[[h]]) && last_season[[h]] < s) {
      elo[[h]] <- .elo_regress(elo[[h]], s - last_season[[h]], carryover)
    }
    if (!is.na(last_season[[a]]) && last_season[[a]] < s) {
      elo[[a]] <- .elo_regress(elo[[a]], s - last_season[[a]], carryover)
    }

    elo_h <- elo[[h]]
    elo_a <- elo[[a]]
    pre_home[i] <- elo_h
    pre_away[i] <- elo_a

    exp_home <- 1 / (1 + 10^(-((elo_h + hga) - elo_a) / 400))
    m <- matches$home_margin[i]
    result <- if (is.na(m)) 0.5 else if (m > 0) 1 else if (m < 0) 0 else 0.5

    mov <- if (isTRUE(mov_mult) && !is.na(m) && m != 0) {
      # 538-style: winner-relative, NOT abs() -- (winner_elo - loser_elo) is
      # positive for a favourite win (dampens it) and NEGATIVE for an upset
      # (amplifies the correction), which is the actual autocorrelation-of-
      # era adjustment. abs() collapses that asymmetry and dampens upsets
      # the same as favourite blowouts.
      elo_diff_winner <- if (m > 0) elo_h - elo_a else elo_a - elo_h
      .elo_mov_mult(m, elo_diff_winner)
    } else {
      1
    }

    delta <- k * mov * (result - exp_home)
    elo[[h]] <- elo_h + delta
    elo[[a]] <- elo_a - delta

    last_season[[h]] <- s
    last_season[[a]] <- s
  }

  list(
    by_match = data.table::data.table(
      match_id  = rep(matches$match_id, 2),
      team_name = c(matches$home_team, matches$away_team),
      elo_pre   = c(pre_home, pre_away)
    ),
    current = data.table::data.table(
      team_name   = names(elo),
      elo_current = unname(elo)
    )
  )
}

# join_elo_diff_to_team_mdl_df ----

#' Join per-team elo_pre onto team_mdl_df's long (team-per-match) format and
#' compute a symmetric \code{elo_diff} feature (this team's elo_pre minus
#' opponent's elo_pre -- same convention as epr_diff/torp_diff, no HGA
#' baked in; HGA is already carried by the existing \code{team_type_fac} term).
#'
#' @param team_mdl_df Complete model dataset (has match_id, team_name.x, team_name.y)
#' @param elo_result Output of \code{build_team_elo()}: list(by_match, current)
#' @return team_mdl_df with an added numeric \code{elo_diff} column. Rows
#'   whose match already has a historical elo_pre use that (leak-safe: it
#'   reflects only strictly-prior matches). Rows with no historical elo_pre
#'   -- upcoming/unplayed fixtures, which is exactly what
#'   \code{run_predictions_pipeline()} needs elo_diff for -- fall back to
#'   each team's \emph{current} rating (\code{elo_result$current}, its rating
#'   after its most recent completed match). A team with no rating history
#'   at all (has never appeared in a completed match, e.g. brand new franchise
#'   fixture data glitch) falls back to the neutral starting rating (\code{ELO_INIT}, 1500),
#'   giving \code{elo_diff = 0} for that row only.
#' @keywords internal
join_elo_diff_to_team_mdl_df <- function(team_mdl_df, elo_result) {
  by_match <- elo_result$by_match
  current  <- elo_result$current

  elo_x <- by_match
  names(elo_x) <- c("match_id", "team_name_x_chr", "elo_pre_x")
  elo_y <- by_match
  names(elo_y) <- c("match_id", "team_name_y_chr", "elo_pre_y")

  cur_x <- current
  names(cur_x) <- c("team_name_x_chr", "elo_current_x")
  cur_y <- current
  names(cur_y) <- c("team_name_y_chr", "elo_current_y")

  team_mdl_df$team_name_x_chr <- as.character(team_mdl_df$team_name.x)
  team_mdl_df$team_name_y_chr <- as.character(team_mdl_df$team_name.y)

  out <- team_mdl_df |>
    dplyr::left_join(as.data.frame(elo_x), by = c("match_id", "team_name_x_chr")) |>
    dplyr::left_join(as.data.frame(elo_y), by = c("match_id", "team_name_y_chr")) |>
    dplyr::left_join(as.data.frame(cur_x), by = "team_name_x_chr") |>
    dplyr::left_join(as.data.frame(cur_y), by = "team_name_y_chr") |>
    dplyr::mutate(
      elo_pre_x = dplyr::coalesce(elo_pre_x, elo_current_x, ELO_INIT),
      elo_pre_y = dplyr::coalesce(elo_pre_y, elo_current_y, ELO_INIT),
      elo_diff = elo_pre_x - elo_pre_y
    ) |>
    dplyr::select(-team_name_x_chr, -team_name_y_chr, -elo_pre_x, -elo_pre_y,
                  -elo_current_x, -elo_current_y)

  n_unrated <- sum(is.na(team_mdl_df$team_name.x) |
                    !(as.character(team_mdl_df$team_name.x) %in% current$team_name) |
                    !(as.character(team_mdl_df$team_name.y) %in% current$team_name))
  if (n_unrated > 0) {
    cli::cli_inform("join_elo_diff_to_team_mdl_df: {n_unrated} row{?s} have a team with no Elo history at all -- using neutral 1500 fallback")
  }
  out
}

# build_elo_diff ----

#' Convenience wrapper: build the Elo table from team_mdl_df and join
#' elo_diff back onto it in one call.
#'
#' @param team_mdl_df Complete model dataset from \code{.build_team_mdl_df()}
#' @inheritParams build_team_elo
#' @return team_mdl_df with an added \code{elo_diff} column
#' @keywords internal
build_elo_diff <- function(team_mdl_df, k = ELO_K, hga = ELO_HGA, carryover = ELO_CARRYOVER) {
  matches <- .matches_from_team_mdl_df(team_mdl_df)
  elo_result <- build_team_elo(matches, k = k, hga = hga, carryover = carryover)
  join_elo_diff_to_team_mdl_df(team_mdl_df, elo_result)
}
