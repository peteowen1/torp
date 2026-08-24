# AFLW Match Prediction (v1)
# ===========================
# Leak-free-by-construction rebuild of daisychain's match_prediction.R, which
# had a confirmed leak: team_strength was a single end-of-2025-season snapshot
# joined identically to every training AND test row (see
# docs/plans/AFLW-MIGRATION-PLAN.md Sec 0/3). Every feature here is assembled
# from strictly-prior information only -- see the leak-safety notes on each
# join function below.
#
# Feature set is deliberately just two terms (docs/plans/AFLW-MIGRATION-PLAN.md
# Sec 6.0): aflw_elo_diff + aflw_psr_diff. No epr_diff/torp_diff -- those are
# chain-derived and don't exist for AFLW (Sec 0). Do not brand this "TORP for
# AFLW" -- different method, different (much smaller) data volume, not
# comparable to men's match_model.R.

# .aflw_team_psr_by_round ----

#' Aggregate per-player PSR to team-per-round totals for AFLW
#'
#' Sums \code{calculate_psr()}'s per-player-round PSR across each team's
#' listed players for that round. Leak-safe by construction PROVIDED
#' \code{stat_ratings} itself is the point-in-time history
#' (\code{data-raw/06-stat-ratings/aflw_run_pipeline.R}'s per-(season,round)
#' Bayesian estimation, each round's ratings built from a \code{ref_date} at
#' or before that round's earliest match) -- this function does not itself
#' guarantee that property, it inherits it from its input.
#'
#' @param stat_ratings Per-player-round AFLW stat ratings (same shape
#'   \code{.prepare_aflw_stat_rating_data()} plus \code{.estimate_stat_ratings_batch()}
#'   produce -- one row per player per (season, round), \code{*_rating} columns).
#' @param coef_df PSR coefficient data.frame (\code{stat_name}, \code{beta},
#'   optionally \code{sd}) -- e.g. from
#'   \code{read.csv(.find_psr_coef_path(comp = "AFLW"))}.
#' @param teams AFLW roster/lineup data (\code{load_teams(comp = "AFLW")}
#'   shape: \code{player_id}, \code{team_name}, \code{match_id},
#'   \code{round_number}, \code{season}).
#' @return data.table(match_id, team_name, team_psr, n_rated_players) -- one
#'   row per team per match. \code{n_rated_players} lets a caller see how many
#'   of the listed players actually had a PSR history (debut players
#'   contribute 0, not NA -- see below).
#' @keywords internal
.aflw_team_psr_by_round <- function(stat_ratings, coef_df, teams) {
  psr <- calculate_psr(stat_ratings, coef_df, center = FALSE)
  psr_dt <- data.table::as.data.table(psr)
  psr_dt[, `:=`(player_id = as.character(player_id), season = as.integer(season),
               round = as.integer(round))]

  teams_dt <- data.table::as.data.table(teams)
  teams_dt <- teams_dt[, .(
    player_id = as.character(player_id),
    team_name = as.character(team_name),
    match_id  = as.character(match_id),
    season    = as.integer(season),
    round     = as.integer(round_number)
  )]

  merged <- merge(
    teams_dt,
    psr_dt[, .(player_id, season, round, psr)],
    by = c("player_id", "season", "round"),
    all.x = TRUE
  )
  # A listed player with no PSR history at all (debut, or a stat-ratings gap)
  # contributes 0 -- neutral, not a team-strength penalty for fielding a
  # rookie, and NOT dropped (dropping would silently understate a team
  # missing several such players, which is exactly the kind of silent
  # degradation this repo has been burned by before).
  n_unrated_player_rows <- sum(is.na(merged$psr))
  if (n_unrated_player_rows > 0) {
    cli::cli_inform(".aflw_team_psr_by_round: {n_unrated_player_rows} listed-player-round{?s} have no PSR history -- treated as 0 (neutral)")
  }
  # Count BEFORE replacing NA with 0 -- n_rated_players exists specifically
  # to tell "22 players, all rated" apart from "22 players, 15 unrated", and
  # doing the fifelse() first would make it always equal n_players.
  merged[, .rated := !is.na(psr)]
  merged[, psr := data.table::fifelse(is.na(psr), 0, psr)]

  merged[, .(team_psr = sum(psr), n_rated_players = sum(.rated), n_players = .N),
        by = .(match_id, team_name)]
}

# join_aflw_features_to_matches ----

#' Join leak-safe aflw_elo_diff and aflw_psr_diff onto an AFLW match table
#'
#' Mirrors \code{join_elo_diff_to_team_mdl_df()}'s leak-safety pattern (see
#' that function's docs for the full rationale) adapted for
#' \code{.matches_from_aflw_results()}'s home_team/away_team shape rather than
#' \code{team_mdl_df}'s long team_name.x/team_name.y shape, plus an added
#' \code{aflw_psr_diff} term.
#'
#' \strong{Leak safety, explicit:} \code{elo_pre} (from \code{build_aflw_team_elo()})
#' reflects only strictly-prior matches by construction (see \code{build_team_elo()}).
#' \code{team_psr} (from \code{.aflw_team_psr_by_round()}) is looked up by this
#' match's own \code{round}, which by construction of the stat-ratings pipeline
#' is estimated from data at or before that round's earliest match -- so a
#' round-R prediction never uses a round-R (or later) result. Neither term can
#' see this match's own outcome or any later one.
#'
#' @param matches Output of \code{.matches_from_aflw_results()}: data.frame
#'   with match_id, date, season, home_team, away_team, home_margin.
#' @param elo_result Output of \code{build_aflw_team_elo()}: list(by_match, current).
#' @param team_psr Output of \code{.aflw_team_psr_by_round()}: data.table(match_id,
#'   team_name, team_psr, n_rated_players).
#' @param hga Home-ground advantage added to the home Elo before differencing
#'   (same convention as \code{build_team_elo()}'s own \code{hga} arg -- pass
#'   the SAME value the Elo table was built with). Default \code{AFLW_ELO_HGA}.
#' \strong{On a team's roster not being found at all for a match} (as opposed
#' to being found with a low/zero PSR sum): \code{aflw_psr_diff} is left
#' \code{NA} for that row, NOT coalesced to a neutral 0. A whole-roster miss
#' is a join failure (the team's name in \code{matches} doesn't match any
#' roster row's \code{team_name} for that \code{match_id}), most commonly
#' AFLW's Indigenous-round heritage team names (e.g. "Walyalup" for
#' Fremantle) not yet being in \code{AFL_TEAM_ALIASES} -- see
#' docs/plans/AFLW-MIGRATION-PLAN.md Sec 6.4 item 2. Silently treating that as
#' 0 would compare a real team's PSR sum against a false "team of replacement
#' players" reading for the opponent, which is what produced +-400-point
#' \code{aflw_psr_diff} outliers and wrecked the margin model's R^2 before this
#' was traced and fixed. A single unmatched player within an otherwise-found
#' roster (a debut with no rating history) is NOT this case -- that's handled
#' inside \code{.aflw_team_psr_by_round()}, contributes 0 for that one player,
#' and is not an NA here.
#'
#' @return \code{matches} with three added columns: \code{aflw_elo_diff}
#'   (numeric, never NA -- Elo always has a fallback), \code{aflw_psr_diff}
#'   (numeric, NA when either team's roster wasn't found at all for this
#'   match OR was found but every listed player was unrated -- see
#'   \code{n_rated_players} in \code{.aflw_team_psr_by_round()}'s docs),
#'   \code{aflw_psr_matched} (logical, FALSE on those NA rows -- drop
#'   or otherwise handle explicitly before fitting; do not silently
#'   \code{coalesce()} it to 0). A team whose roster IS found but has zero
#'   rated players (e.g. that round's stat-ratings batch hasn't run yet)
#'   would otherwise compute a confident, fabricated \code{team_psr = 0}
#'   indistinguishable from a real "average team" reading -- gated out here
#'   via the same \code{n_rated_players} count, not just presence of a match.
#' @keywords internal
join_aflw_features_to_matches <- function(matches, elo_result, team_psr, hga = AFLW_ELO_HGA) {
  by_match <- elo_result$by_match
  current  <- elo_result$current

  elo_home <- by_match; names(elo_home) <- c("match_id", "home_team", "elo_pre_home")
  elo_away <- by_match; names(elo_away) <- c("match_id", "away_team", "elo_pre_away")
  cur_home <- current;  names(cur_home) <- c("home_team", "elo_current_home")
  cur_away <- current;  names(cur_away) <- c("away_team", "elo_current_away")

  psr_home <- as.data.frame(team_psr)[, c("match_id", "team_name", "team_psr", "n_rated_players")]
  names(psr_home) <- c("match_id", "home_team", "team_psr_home", "n_rated_home")
  psr_away <- as.data.frame(team_psr)[, c("match_id", "team_name", "team_psr", "n_rated_players")]
  names(psr_away) <- c("match_id", "away_team", "team_psr_away", "n_rated_away")

  out <- matches |>
    dplyr::left_join(elo_home, by = c("match_id", "home_team")) |>
    dplyr::left_join(elo_away, by = c("match_id", "away_team")) |>
    dplyr::left_join(cur_home, by = "home_team") |>
    dplyr::left_join(cur_away, by = "away_team") |>
    dplyr::left_join(psr_home, by = c("match_id", "home_team")) |>
    dplyr::left_join(psr_away, by = c("match_id", "away_team")) |>
    dplyr::mutate(
      elo_pre_home = dplyr::coalesce(elo_pre_home, elo_current_home, 1500),
      elo_pre_away = dplyr::coalesce(elo_pre_away, elo_current_away, 1500),
      aflw_elo_diff = (elo_pre_home + hga) - elo_pre_away,
      # A roster IS found (so team_psr isn't NA) but every listed player is
      # unrated -- e.g. that round's stat-ratings batch hasn't run yet, not
      # a single-debutant case .aflw_team_psr_by_round() already handles.
      # That reads as a confident, fabricated "even match" (0 - 0 = 0)
      # unless coverage is checked too, not just presence of a match.
      aflw_psr_matched = !is.na(team_psr_home) & !is.na(team_psr_away) &
                          !is.na(n_rated_home) & n_rated_home > 0 &
                          !is.na(n_rated_away) & n_rated_away > 0,
      # Belt and braces: NA the DIFF itself when unmatched, not just the flag.
      # The zero-rated-roster case (team_psr_home/away are real 0s, not NA)
      # would otherwise leave aflw_psr_diff as a real, fabricated number for
      # any downstream consumer that forgets to check aflw_psr_matched --
      # exactly the gap predict_aflw_margin()/predict_aflw_win_prob()'s own
      # NA-count warning relies on aflw_psr_diff itself being NA to catch.
      aflw_psr_diff = dplyr::if_else(aflw_psr_matched, team_psr_home - team_psr_away, NA_real_)
    ) |>
    dplyr::select(-elo_pre_home, -elo_pre_away, -elo_current_home, -elo_current_away,
                  -team_psr_home, -team_psr_away, -n_rated_home, -n_rated_away)

  n_unrated_elo <- sum(!(matches$home_team %in% current$team_name) |
                        !(matches$away_team %in% current$team_name))
  if (n_unrated_elo > 0) {
    cli::cli_inform("join_aflw_features_to_matches: {n_unrated_elo} row{?s} have a team with no Elo history at all -- using neutral 1500 fallback")
  }
  n_psr_unmatched <- sum(!out$aflw_psr_matched)
  if (n_psr_unmatched > 0) {
    cli::cli_inform("join_aflw_features_to_matches: {n_psr_unmatched} row{?s} have a team with an unusable PSR reading for that match -- either the roster wasn't found at all (likely an AFL_TEAM_ALIASES gap, e.g. an Indigenous-round team name) or it was found with every listed player unrated (e.g. that round's stat-ratings batch hasn't run yet) -- aflw_psr_diff is NA, not a false 0, for these rows")
  }
  out
}

# fit / predict ----

#' Fit AFLW match-outcome models on aflw_elo_diff + aflw_psr_diff only
#'
#' Two features, and AFLW's full history is ~485 matches -- deliberately a
#' plain logistic/linear fit, not a GAM. A multi-term smooth on this little
#' data would be overfit noise, not signal; see
#' docs/plans/AFLW-MIGRATION-PLAN.md Sec 6.4 item 3 for the OOS comparison
#' that backs this choice.
#'
#' @param train_df Output of \code{join_aflw_features_to_matches()}, training
#'   rows only (completed matches with known \code{home_margin}). Rows where
#'   \code{aflw_psr_matched} is FALSE are dropped explicitly (with a count
#'   reported) rather than left to \code{stats::lm()}/\code{glm()}'s silent
#'   \code{na.omit} default -- see \code{join_aflw_features_to_matches()}'s
#'   docs for why an NA here means a real join failure, not neutral.
#' @return list(win_model = glm object, margin_model = lm object)
#' @keywords internal
fit_aflw_match_model <- function(train_df) {
  # Filter on the flag when present (the normal path, from
  # join_aflw_features_to_matches()) -- but a caller without it must NOT
  # silently fall through to lm()/glm()'s own na.omit default, which is
  # exactly the unreported-drop failure mode this function exists to avoid.
  # Check aflw_psr_diff directly either way.
  keep <- if ("aflw_psr_matched" %in% names(train_df)) {
    train_df$aflw_psr_matched
  } else {
    !is.na(train_df$aflw_psr_diff)
  }
  n_dropped <- sum(!keep)
  if (n_dropped > 0) {
    cli::cli_inform("fit_aflw_match_model: dropping {n_dropped} training row{?s} with an unmatched/NA PSR reading")
  }
  train_df <- train_df[keep, ]
  win_model <- stats::glm(
    I(home_margin > 0) ~ aflw_elo_diff + aflw_psr_diff,
    data = train_df, family = stats::binomial()
  )
  margin_model <- stats::lm(
    home_margin ~ aflw_elo_diff + aflw_psr_diff,
    data = train_df
  )
  list(win_model = win_model, margin_model = margin_model)
}

#' Warn (once per call) how many prediction rows will come back NA because
#' aflw_psr_diff is unavailable for them -- predict()'s own NA propagation
#' is silent otherwise, and a caller reading only the return vector has no
#' way to notice without this.
#' @param newdata data.frame passed to a predict_aflw_*() function.
#' @param label Character, which predict function is calling (for the message).
#' @keywords internal
.warn_aflw_predict_na <- function(newdata, label) {
  if (!"aflw_psr_diff" %in% names(newdata)) return(invisible())
  n_na <- sum(is.na(newdata$aflw_psr_diff))
  if (n_na > 0) {
    cli::cli_warn("{label}: {n_na} of {nrow(newdata)} row{?s} have NA aflw_psr_diff and will predict NA -- check aflw_psr_matched (if present) or the roster/stat-ratings coverage for these rows")
  }
}

#' Predict AFLW home-win probability
#' @param model Output of \code{fit_aflw_match_model()}.
#' @param newdata data.frame with \code{aflw_elo_diff}, \code{aflw_psr_diff}.
#'   A row with NA \code{aflw_psr_diff} predicts NA (not a fabricated value) --
#'   a count is warned, not just silently propagated.
#' @return Numeric vector, P(home win).
#' @keywords internal
predict_aflw_win_prob <- function(model, newdata) {
  .warn_aflw_predict_na(newdata, "predict_aflw_win_prob")
  # na.action = na.pass, not the na.omit default predict.glm() would
  # otherwise inherit from getOption("na.action") -- na.omit DROPS the NA
  # row from the output vector instead of returning NA in place, silently
  # shortening/misaligning the result relative to newdata's own row count.
  as.numeric(stats::predict(model$win_model, newdata = newdata, type = "response", na.action = stats::na.pass))
}

#' Predict AFLW home margin
#' @param model Output of \code{fit_aflw_match_model()}.
#' @param newdata data.frame with \code{aflw_elo_diff}, \code{aflw_psr_diff}.
#'   A row with NA \code{aflw_psr_diff} predicts NA (not a fabricated value) --
#'   a count is warned, not just silently propagated.
#' @return Numeric vector, predicted home_score - away_score.
#' @keywords internal
predict_aflw_margin <- function(model, newdata) {
  .warn_aflw_predict_na(newdata, "predict_aflw_margin")
  # See predict_aflw_win_prob()'s comment -- same na.action fix.
  as.numeric(stats::predict(model$margin_model, newdata = newdata, na.action = stats::na.pass))
}
