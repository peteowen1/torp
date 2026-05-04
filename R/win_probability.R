#' Fit AFL Win Probability Model
#'
#' Fits a logistic regression model for live win probability prediction.
#' The model estimates P(home_win | margin, game_progress) using historical
#' AFL data with quarter-by-quarter scores.
#'
#' @param seasons Numeric vector of seasons to include (default: 2010-2025)
#' @param output_path Path to write the JSON coefficients file (NULL = return only)
#' @param seed Integer seed used to make the synthetic quarter-break noise
#'   reproducible. The fitted coefficients are exported to JSON for browser
#'   inference; without a fixed seed each retraining produces a different model.
#'   Scoped via [withr::local_seed()] so the caller's RNG stream is not affected.
#'
#' @return A list with: coefficients (named list), model (glm object), data (training data)
#' @export
#'
#' @examples
#' \dontrun{
#' result <- fit_win_probability()
#' # Coefficients for browser JS:
#' result$coefficients
#' }
fit_win_probability <- function(seasons = 2010:2025,
                                output_path = NULL,
                                seed = 20250101L) {
  withr::local_seed(seed)
  cli::cli_h1("Fitting AFL Win Probability Model")

  # Fetch historical fixtures from AFL API (all seasons)
  cli::cli_alert_info("Fetching historical fixtures for {length(seasons)} seasons...")
  all_fixtures <- get_afl_fixtures(TRUE)

  if (is.null(all_fixtures) || nrow(all_fixtures) == 0) {
    cli::cli_abort("No fixture data returned from AFL API")
  }

  # Filter to requested seasons and concluded matches
  fixtures <- all_fixtures[
    all_fixtures$season %in% seasons &
    all_fixtures$status == "CONCLUDED",
  ]

  cli::cli_alert_info("Using {nrow(fixtures)} concluded matches across {length(unique(fixtures$season))} seasons")

  # We need to simulate quarter-break states from final scores

  # For each match, create 4 data points (end of Q1, Q2, Q3, Q4)
  # Using league-average scoring distribution by quarter:
  # Q1: ~22%, Q2: ~27%, Q3: ~25%, Q4: ~26% of total scoring
  quarter_pcts <- c(0.22, 0.27, 0.25, 0.26)
  cumulative_pcts <- cumsum(quarter_pcts)  # 0.22, 0.49, 0.74, 1.00

  training_rows <- vector("list", nrow(fixtures) * 4)
  idx <- 1

  for (i in seq_len(nrow(fixtures))) {
    f <- fixtures[i, ]
    hs <- f$home_score
    as <- f$away_score

    if (is.na(hs) || is.na(as)) next

    home_win <- as.integer(hs > as)
    # Skip draws for cleaner binary model
    if (hs == as) next

    for (q in 1:4) {
      pct <- cumulative_pcts[q]
      # Simulate scores at this quarter break (add noise)
      # Use beta distribution to add natural variance to scoring progression
      h_at_q <- round(hs * pct + rnorm(1, 0, hs * 0.05))
      a_at_q <- round(as * pct + rnorm(1, 0, as * 0.05))

      margin <- h_at_q - a_at_q
      complete <- q * 25  # 25%, 50%, 75%, 100%
      time_weight <- sqrt(complete / 100)

      training_rows[[idx]] <- data.frame(
        home_win = home_win,
        margin = margin,
        time_weight = time_weight,
        complete = complete,
        season = f$season,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }

  training_data <- do.call(rbind, training_rows[seq_len(idx - 1)])

  cli::cli_alert_info("Training data: {nrow(training_data)} rows ({nrow(training_data)/4} matches x 4 quarters)")

  # Fit logistic regression
  fit <- stats::glm(
    home_win ~ margin * time_weight,
    family = stats::binomial(link = "logit"),
    data = training_data
  )

  cli::cli_alert_success("Model fitted successfully")
  print(summary(fit))

  # Extract coefficients
  coefs <- stats::coef(fit)
  coefficients <- list(
    intercept = unname(coefs["(Intercept)"]),
    margin = unname(coefs["margin"]),
    timeWeight = unname(coefs["time_weight"]),
    marginTimeWeight = unname(coefs["margin:time_weight"])
  )

  cli::cli_h2("Coefficients for browser JS:")
  cat(jsonlite::toJSON(coefficients, auto_unbox = TRUE, pretty = TRUE), "\n")

  # Validation: check calibration at known states
  cli::cli_h2("Calibration check:")
  test_cases <- data.frame(
    label = c("Kickoff", "Q2 +20", "3QT +30", "3QT -30", "FT +1", "FT +60"),
    margin = c(0, 20, 30, -30, 1, 60),
    complete = c(0, 50, 75, 75, 100, 100),
    stringsAsFactors = FALSE
  )
  test_cases$time_weight <- sqrt(test_cases$complete / 100)
  test_cases$predicted <- stats::predict(fit, newdata = test_cases, type = "response")
  print(test_cases[, c("label", "margin", "complete", "predicted")])

  # Write JSON if output path provided
  if (!is.null(output_path)) {
    jsonlite::write_json(coefficients, output_path, auto_unbox = TRUE, pretty = TRUE)
    cli::cli_alert_success("Coefficients written to {output_path}")
  }

  invisible(list(
    coefficients = coefficients,
    model = fit,
    data = training_data
  ))
}
