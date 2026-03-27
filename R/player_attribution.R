# Player Attribution via Zero-Ablation
#
# Measures individual player impact on match predictions by comparing
# the full prediction (with player ratings) vs a baseline prediction
# (with player ratings zeroed out / set to league average).
#
# Ported from bouncer's cricket implementation. The key insight:
# since prediction models use rating VALUES (not player IDENTITY),
# traditional SHAP doesn't directly give player attribution.
# Zero-ablation gives interpretable per-player contributions.

#' Calculate Player Attribution for a Match
#'
#' Measures how much each player's ratings contribute to the match
#' prediction by comparing full prediction vs prediction with that
#' player's ratings set to league average (zero-ablation).
#'
#' @param match_features Data frame. One row of match features including
#'   player-level rating aggregations (e.g., from `aggregate_lineup_ratings()`).
#' @param predict_fn Function. Takes a feature data frame and returns
#'   a prediction (e.g., win probability or expected goals).
#' @param player_rating_cols Character vector. Column names containing
#'   player-aggregated ratings to ablate.
#' @param baseline_value Numeric. Value to use when ablating (default 0,
#'   representing league average for centered ratings).
#'
#' @return Data frame with columns:
#'   - `feature`: Rating column name
#'   - `full_pred`: Prediction with all features
#'   - `ablated_pred`: Prediction with this feature zeroed
#'   - `contribution`: `full_pred - ablated_pred` (positive = helps team)
#'   - `pct_contribution`: Contribution as percentage of total
#'
#' @export
#'
#' @examples
#' \dontrun{
#' features <- data.frame(
#'   home_panna_sum = 5.2, away_panna_sum = 3.1,
#'   home_offense_mean = 0.8, away_offense_mean = 0.5,
#'   elo_diff = 50
#' )
#' predict_fn <- function(x) 1 / (1 + exp(-x$elo_diff / 100))
#' rating_cols <- c("home_panna_sum", "away_panna_sum",
#'                  "home_offense_mean", "away_offense_mean")
#' attr <- calculate_player_attribution(features, predict_fn, rating_cols)
#' }
calculate_player_attribution <- function(match_features,
                                          predict_fn,
                                          player_rating_cols,
                                          baseline_value = 0) {

  if (nrow(match_features) != 1) {
    cli::cli_abort("match_features must have exactly 1 row")
  }

  # Full prediction (with all player ratings)
  full_pred <- predict_fn(match_features)

  # Ablate each rating column and measure impact
  results <- data.frame(
    feature = character(),
    full_pred = numeric(),
    ablated_pred = numeric(),
    contribution = numeric(),
    stringsAsFactors = FALSE
  )

  available_cols <- intersect(player_rating_cols, names(match_features))

  for (col in available_cols) {
    ablated <- match_features
    ablated[[col]] <- baseline_value
    ablated_pred <- predict_fn(ablated)

    results <- rbind(results, data.frame(
      feature = col,
      full_pred = full_pred,
      ablated_pred = ablated_pred,
      contribution = full_pred - ablated_pred,
      stringsAsFactors = FALSE
    ))
  }

  # Calculate percentage contribution
  total_contribution <- sum(abs(results$contribution))
  if (total_contribution > 0) {
    results$pct_contribution <- round(
      results$contribution / total_contribution * 100, 1
    )
  } else {
    results$pct_contribution <- 0
  }

  results[order(-abs(results$contribution)), ]
}


#' Batch Player Attribution Across Matches
#'
#' Runs zero-ablation attribution for multiple matches and aggregates
#' player-level contributions.
#'
#' @param match_features_list List of single-row data frames (one per match).
#' @param predict_fn Function. Prediction function.
#' @param player_rating_cols Character vector. Rating columns to ablate.
#' @param baseline_value Numeric. Ablation baseline. Default 0.
#'
#' @return Data frame with per-feature average contributions across matches
#' @export
batch_player_attribution <- function(match_features_list,
                                      predict_fn,
                                      player_rating_cols,
                                      baseline_value = 0) {

  all_results <- lapply(match_features_list, function(features) {
    tryCatch(
      calculate_player_attribution(features, predict_fn, player_rating_cols, baseline_value),
      error = function(e) NULL
    )
  })

  all_results <- Filter(Negate(is.null), all_results)

  if (length(all_results) == 0) {
    cli::cli_warn("No successful attributions computed")
    return(data.frame())
  }

  combined <- do.call(rbind, all_results)

  # Aggregate by feature
  agg <- stats::aggregate(
    contribution ~ feature,
    data = combined,
    FUN = function(x) c(mean = mean(x), sd = stats::sd(x), n = length(x))
  )

  result <- data.frame(
    feature = agg$feature,
    mean_contribution = agg$contribution[, "mean"],
    sd_contribution = agg$contribution[, "sd"],
    n_matches = agg$contribution[, "n"],
    stringsAsFactors = FALSE
  )

  result[order(-abs(result$mean_contribution)), ]
}
