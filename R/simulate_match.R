# Match-Level Monte Carlo Simulation
# ====================================
# Scoring-sequence simulation: individual goals/behinds within quarters.
# Produces score distributions, quarter breakdowns, and WP fan charts.

#' Monte Carlo match simulation
#'
#' Simulates a single AFL match N times, producing score distributions,
#' quarter-by-quarter breakdowns, individual scoring events, and a win
#' probability fan chart showing how WP could evolve during the match.
#'
#' @param home_team Character. Home team name (matched via AFL_TEAM_ALIASES).
#' @param away_team Character. Away team name.
#' @param season Numeric. Season year (default: current via get_afl_season()).
#' @param n_sims Integer. Number of Monte Carlo simulations (default 10,000).
#' @param team_ratings Optional data frame with `team` and `torp` columns.
#'   If NULL, loaded via [load_team_ratings()].
#' @param predictions Optional data frame of match predictions with
#'   `home_team`, `away_team`, `pred_xtotal`, `pred_margin`, `pred_win`.
#'   If NULL, loaded via [load_predictions()].
#' @param home_advantage Numeric. Home ground advantage in points.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A `torp_match_sim` S3 object with components:
#'   \item{home_team}{Home team name}
#'   \item{away_team}{Away team name}
#'   \item{n_sims}{Number of simulations run}
#'   \item{estimate}{Pre-game expected margin (home perspective)}
#'   \item{pred_total}{Pre-game expected combined score}
#'   \item{pre_game_wp}{Pre-game win probability}
#'   \item{scores}{data.table: sim_id, home_score, away_score, margin}
#'   \item{quarters}{data.table: sim_id, quarter, home_qtr_score, away_qtr_score}
#'   \item{events}{data.table: sim_id, quarter, event_num, time_pct, team,
#'     score_type, score_value, cum_home, cum_away}
#'   \item{wp_trajectory}{data.table: time_pct, wp_mean, wp_p10, wp_p25,
#'     wp_median, wp_p75, wp_p90}
#'   \item{summary}{List of summary statistics}
#'
#' @export
simulate_match_mc <- function(home_team, away_team,
                              season = get_afl_season(),
                              n_sims = MATCH_SIM_DEFAULT_N,
                              team_ratings = NULL,
                              predictions = NULL,
                              home_advantage = SIM_HOME_ADVANTAGE,
                              seed = NULL) {
  if (!is.null(seed)) withr::local_seed(seed)

  home_team <- torp_replace_teams(home_team)
  away_team <- torp_replace_teams(away_team)

  # Resolve match estimate and predicted total
  match_params <- .resolve_match_params(
    home_team, away_team, season, team_ratings, predictions, home_advantage
  )

  estimate <- match_params$estimate
  pred_total <- match_params$pred_total
  home_conv <- match_params$home_conv
  away_conv <- match_params$away_conv

  # Pre-game WP from logistic
  pre_game_wp <- 1 / (10^(-estimate / SIM_WP_SCALING_FACTOR) + 1)

  cli::cli_inform(
    "Simulating {home_team} vs {away_team} ({n_sims} sims, est={round(estimate, 1)}, WP={round(pre_game_wp * 100, 1)}%)"
  )

  # Run scoring-sequence simulation
  sim_result <- .sim_scoring_sequence(
    estimate = estimate,
    pred_total = pred_total,
    home_conv = home_conv,
    away_conv = away_conv,
    n_sims = n_sims
  )

  # Compute WP trajectory from events
  wp_traj <- .compute_wp_trajectory(sim_result$events, pred_total, n_sims)

  # Build summary
  scores <- sim_result$scores
  sim_summary <- list(
    home_win_pct = mean(scores$margin > 0),
    draw_pct = mean(scores$margin == 0),
    away_win_pct = mean(scores$margin < 0),
    home_score_mean = mean(scores$home_score),
    home_score_sd = stats::sd(scores$home_score),
    home_score_quantiles = stats::quantile(scores$home_score, c(0.1, 0.25, 0.5, 0.75, 0.9)),
    away_score_mean = mean(scores$away_score),
    away_score_sd = stats::sd(scores$away_score),
    away_score_quantiles = stats::quantile(scores$away_score, c(0.1, 0.25, 0.5, 0.75, 0.9)),
    margin_mean = mean(scores$margin),
    margin_sd = stats::sd(scores$margin)
  )

  structure(
    list(
      home_team = home_team,
      away_team = away_team,
      n_sims = n_sims,
      estimate = estimate,
      pred_total = pred_total,
      pre_game_wp = pre_game_wp,
      scores = scores,
      quarters = sim_result$quarters,
      events = sim_result$events,
      wp_trajectory = wp_traj,
      summary = sim_summary
    ),
    class = "torp_match_sim"
  )
}


# Resolve match parameters ----

#' Resolve estimate and predicted total from ratings or predictions
#' @keywords internal
.resolve_match_params <- function(home_team, away_team, season,
                                  team_ratings, predictions, home_advantage) {
  pred_total <- SIM_AVG_TOTAL
  estimate <- NULL
  home_conv <- MATCH_SIM_AVG_CONV_RATE
  away_conv <- MATCH_SIM_AVG_CONV_RATE

  # Try predictions first (most accurate — from GAM pipeline)
  if (!is.null(predictions)) {
    pred_row <- predictions[predictions$home_team == home_team &
                              predictions$away_team == away_team, ]
    if (nrow(pred_row) > 0) {
      pred_row <- pred_row[1, ]
      estimate <- pred_row$pred_margin
      if ("pred_xtotal" %in% names(pred_row)) pred_total <- pred_row$pred_xtotal
      return(list(estimate = estimate, pred_total = pred_total,
                  home_conv = home_conv, away_conv = away_conv))
    }
  }

  # Try loading predictions from torpdata
  if (is.null(estimate)) {
    preds <- tryCatch(load_predictions(season), error = function(e) {
      cli::cli_warn("Could not load predictions: {conditionMessage(e)}. Falling back to team ratings.")
      NULL
    })
    if (!is.null(preds) && nrow(preds) > 0) {
      pred_row <- preds[preds$home_team == home_team & preds$away_team == away_team, ]
      if (nrow(pred_row) > 0) {
        pred_row <- pred_row[1, ]
        estimate <- pred_row$pred_margin
        if ("pred_xtotal" %in% names(pred_row)) pred_total <- pred_row$pred_xtotal
        return(list(estimate = estimate, pred_total = pred_total,
                    home_conv = home_conv, away_conv = away_conv))
      }
    }
  }

  # Fall back to team TORP ratings
  if (is.null(estimate)) {
    if (is.null(team_ratings)) {
      team_ratings <- tryCatch(load_team_ratings(), error = function(e) {
        cli::cli_warn("Could not load team ratings: {conditionMessage(e)}")
        NULL
      })
    }
    if (!is.null(team_ratings) && nrow(team_ratings) > 0) {
      # Use latest rating per team
      tr <- team_ratings
      if ("season" %in% names(tr)) tr <- tr[tr$season == max(tr$season), ]
      if ("round" %in% names(tr)) {
        tr <- tr[tr$round == max(tr$round), ]
      }
      home_r <- tr$torp[tr$team == home_team]
      away_r <- tr$torp[tr$team == away_team]
      if (length(home_r) > 0 && length(away_r) > 0) {
        estimate <- home_advantage + (home_r[1] - away_r[1])
      }
    }
  }

  if (is.null(estimate)) {
    cli::cli_warn("Could not resolve ratings for {home_team} vs {away_team}, using home_advantage only")
    estimate <- home_advantage
  }

  list(estimate = estimate, pred_total = pred_total,
       home_conv = home_conv, away_conv = away_conv)
}


# Core simulation engine ----

#' Simulate scoring sequences for N matches
#'
#' For each simulation: draw margin and total, split into quarters via Dirichlet,
#' then simulate individual scoring events (goals/behinds) within each quarter
#' using calibrated Poisson shot counts and Bernoulli outcomes.
#'
#' @keywords internal
.sim_scoring_sequence <- function(estimate, pred_total, home_conv, away_conv,
                                  n_sims) {
  # Step 1: Draw match totals (vectorized across all sims)
  margins <- as.integer(round(
    stats::rnorm(n_sims, estimate, SIM_NOISE_SD + abs(estimate) / 3)
  ))
  totals <- pmax(stats::rnorm(n_sims, pred_total, SIM_TOTAL_SD), SIM_MIN_TOTAL)
  home_totals <- pmax(round((totals + margins) / 2), 0)
  away_totals <- pmax(round((totals - margins) / 2), 0)

  # Step 2: Split into quarters via Dirichlet
  alpha <- MATCH_SIM_QUARTER_ALPHA
  home_qtr <- .dirichlet_split(home_totals, alpha, n_sims)
  away_qtr <- .dirichlet_split(away_totals, alpha, n_sims)

  # Build quarter-level data.table
  quarters_dt <- data.table::data.table(
    sim_id = rep(seq_len(n_sims), each = 4L),
    quarter = rep(1:4, times = n_sims),
    home_qtr_score = as.vector(t(home_qtr)),
    away_qtr_score = as.vector(t(away_qtr))
  )

  # Step 3: Simulate scoring events within each quarter
  events_list <- vector("list", n_sims * 8L)  # 4 quarters x 2 teams
  idx <- 0L

  for (sim in seq_len(n_sims)) {
    for (qtr in 1:4) {
      h_events <- .simulate_quarter_events(
        qtr_score = home_qtr[sim, qtr], conv_rate = home_conv,
        quarter = qtr, sim_id = sim, team_label = "home"
      )
      if (!is.null(h_events)) {
        idx <- idx + 1L
        events_list[[idx]] <- h_events
      }

      a_events <- .simulate_quarter_events(
        qtr_score = away_qtr[sim, qtr], conv_rate = away_conv,
        quarter = qtr, sim_id = sim, team_label = "away"
      )
      if (!is.null(a_events)) {
        idx <- idx + 1L
        events_list[[idx]] <- a_events
      }
    }
  }

  events_dt <- data.table::rbindlist(events_list[seq_len(idx)])

  # Sort events within each sim by time
  if (nrow(events_dt) > 0) {
    data.table::setorder(events_dt, sim_id, time_pct, -score_value)
    # Recompute cumulative scores after sorting (interleaved home/away)
    events_dt[, c("cum_home", "cum_away") := .recompute_cumulative(
      .SD$team, .SD$score_value
    ), by = sim_id]
  }

  # Match-level scores
  scores_dt <- data.table::data.table(
    sim_id = seq_len(n_sims),
    home_score = as.integer(rowSums(home_qtr)),
    away_score = as.integer(rowSums(away_qtr))
  )
  scores_dt[, margin := home_score - away_score]

  list(scores = scores_dt, quarters = quarters_dt, events = events_dt)
}


#' Recompute cumulative home/away scores from interleaved events
#' @keywords internal
.recompute_cumulative <- function(team, score_value) {
  n <- length(team)
  cum_h <- integer(n)
  cum_a <- integer(n)
  h <- 0L
  a <- 0L
  for (i in seq_len(n)) {
    if (team[i] == "home") {
      h <- h + score_value[i]
    } else {
      a <- a + score_value[i]
    }
    cum_h[i] <- h
    cum_a[i] <- a
  }
  list(cum_home = cum_h, cum_away = cum_a)
}


#' Split totals into quarters via Dirichlet distribution
#'
#' Uses the gamma-distribution trick: draw k independent Gamma(alpha_k, 1)
#' samples, normalize to get Dirichlet fractions, then distribute total score.
#'
#' @param totals Numeric vector of length n_sims with total scores.
#' @param alpha Numeric vector of Dirichlet concentration parameters (length 4).
#' @param n_sims Integer number of simulations.
#' @return Integer matrix (n_sims x 4) of quarter scores.
#' @keywords internal
.dirichlet_split <- function(totals, alpha, n_sims) {
  k <- length(alpha)
  # Draw n_sims x k gamma samples
  gamma_draws <- matrix(
    stats::rgamma(n_sims * k, rep(alpha, each = n_sims)),
    nrow = n_sims, ncol = k
  )
  fractions <- gamma_draws / rowSums(gamma_draws)

  # Distribute totals into quarters
  raw <- fractions * totals
  qtr_scores <- round(raw)

  # Fix rounding errors to preserve total
  adj <- as.integer(totals - rowSums(qtr_scores))
  for (i in which(adj != 0L)) {
    # Distribute adjustment across random quarters
    sign_adj <- sign(adj[i])
    for (j in seq_len(abs(adj[i]))) {
      q <- sample.int(k, 1L)
      qtr_scores[i, q] <- qtr_scores[i, q] + sign_adj
    }
  }

  # Ensure non-negative
  qtr_scores[qtr_scores < 0] <- 0L
  storage.mode(qtr_scores) <- "integer"
  qtr_scores
}


#' Simulate individual scoring events within a quarter
#'
#' Decomposes a quarter score into goals and behinds, then generates
#' time-ordered scoring events. Uses the constraint: 6*goals + behinds = total.
#'
#' @param qtr_score Integer. Total score for this team in this quarter.
#' @param conv_rate Numeric. Goal conversion rate (goals / total shots).
#' @param quarter Integer. Quarter number (1-4).
#' @param sim_id Integer. Simulation ID.
#' @param team_label Character. "home" or "away".
#' @return data.table of scoring events, or NULL if qtr_score == 0.
#' @keywords internal
.simulate_quarter_events <- function(qtr_score, conv_rate, quarter, sim_id,
                                     team_label) {
  if (qtr_score <= 0L) return(NULL)

  # Determine goals and behinds from total score
  # Use iterative approach: try random goal counts until 6*g + b = total
  max_goals <- qtr_score %/% 6L

  if (max_goals == 0L) {
    # All behinds
    n_goals <- 0L
    n_behinds <- qtr_score
  } else {
    # Draw number of goals, then behinds = remainder
    # Expected goals = total * conv_rate / (6*conv_rate + (1-conv_rate))
    # ≈ total * conv_rate / (5*conv_rate + 1)
    expected_goals <- qtr_score * conv_rate / (5 * conv_rate + 1)
    n_goals <- min(
      stats::rpois(1L, lambda = max(expected_goals, 0.5)),
      max_goals
    )
    remainder <- qtr_score - 6L * n_goals
    if (remainder < 0L) {
      n_goals <- max_goals
      remainder <- qtr_score - 6L * n_goals
    }
    n_behinds <- remainder
  }

  n_events <- n_goals + n_behinds
  if (n_events == 0L) return(NULL)

  # Generate event times uniformly within the quarter
  qtr_start <- (quarter - 1) / 4
  qtr_end <- quarter / 4
  event_times <- sort(stats::runif(n_events, qtr_start, qtr_end))

  # Assign event types (shuffle goals and behinds)
  event_types <- sample(c(
    rep("goal", n_goals),
    rep("behind", n_behinds)
  ))
  score_values <- ifelse(event_types == "goal", 6L, 1L)

  data.table::data.table(
    sim_id = sim_id,
    quarter = quarter,
    event_num = seq_len(n_events),
    time_pct = event_times,
    team = team_label,
    score_type = event_types,
    score_value = score_values
  )
}


# WP trajectory ----

#' Compute win probability trajectory from scoring events
#'
#' For each simulation, computes WP at regular time intervals based on
#' cumulative score margin and time remaining. Aggregates across simulations
#' to produce percentile bands.
#'
#' @param events_dt data.table of scoring events with cum_home, cum_away columns.
#' @param pred_total Predicted combined total score.
#' @param n_sims Number of simulations.
#' @return data.table with time_pct and WP percentile columns.
#' @keywords internal
.compute_wp_trajectory <- function(events_dt, pred_total, n_sims) {
  time_points <- seq(0, 1, by = 0.05)
  n_times <- length(time_points)

  if (nrow(events_dt) == 0) {
    # No events: WP stays at 0.5 throughout
    return(data.table::data.table(
      time_pct = time_points,
      wp_mean = 0.5, wp_p10 = 0.5, wp_p25 = 0.5,
      wp_median = 0.5, wp_p75 = 0.5, wp_p90 = 0.5
    ))
  }

  # Vectorized approach: for each time point, find margin at that time
  # across all sims simultaneously using data.table rolling joins

  # Pre-compute final margin per sim (for blending with pre-game expectation)
  final_margins <- events_dt[, .(final_margin = cum_home[.N] - cum_away[.N]),
                              by = sim_id]
  # Fill in sims with no events
  all_sims <- data.table::data.table(sim_id = seq_len(n_sims))
  final_margins <- merge(all_sims, final_margins, by = "sim_id", all.x = TRUE)
  final_margins[is.na(final_margin), final_margin := 0L]

  # Add margin column to events
  events_dt[, margin := cum_home - cum_away]

  # For each time point, find the last event <= t for each sim
  # using data.table rolling join
  data.table::setkey(events_dt, sim_id, time_pct)

  wp_matrix <- matrix(NA_real_, nrow = n_sims, ncol = n_times)

  for (t_idx in seq_len(n_times)) {
    t <- time_points[t_idx]

    if (t == 0) {
      current_margins <- rep(0, n_sims)
    } else {
      # Rolling join: for each sim, find last event at or before time t
      lookup <- data.table::data.table(sim_id = seq_len(n_sims), time_pct = t)
      rolled <- events_dt[lookup, .(sim_id, margin = x.margin),
                           on = .(sim_id, time_pct), roll = TRUE]
      current_margins <- rolled$margin
      current_margins[is.na(current_margins)] <- 0
    }

    fm <- final_margins$final_margin
    blended <- t * current_margins + (1 - t) * (fm * 0.3)
    effective <- blended * (1 + sqrt(t))
    wp_matrix[, t_idx] <- 1 / (10^(-effective / SIM_WP_SCALING_FACTOR) + 1)
  }

  data.table::data.table(
    time_pct = time_points,
    wp_mean = colMeans(wp_matrix),
    wp_p10 = apply(wp_matrix, 2, stats::quantile, probs = 0.10),
    wp_p25 = apply(wp_matrix, 2, stats::quantile, probs = 0.25),
    wp_median = apply(wp_matrix, 2, stats::quantile, probs = 0.50),
    wp_p75 = apply(wp_matrix, 2, stats::quantile, probs = 0.75),
    wp_p90 = apply(wp_matrix, 2, stats::quantile, probs = 0.90)
  )
}


# S3 methods ----

#' @export
print.torp_match_sim <- function(x, ...) {
  s <- x$summary
  cli::cli_h2("{x$home_team} vs {x$away_team}")
  cli::cli_text("{x$n_sims} simulations")
  cli::cli_text("")

  cli::cli_h3("Win Probability")
  cli::cli_bullets(c(
    " " = "{x$home_team}: {round(s$home_win_pct * 100, 1)}%",
    " " = "Draw: {round(s$draw_pct * 100, 1)}%",
    " " = "{x$away_team}: {round(s$away_win_pct * 100, 1)}%"
  ))

  cli::cli_text("")
  cli::cli_h3("Score Distribution")
  hq <- round(s$home_score_quantiles)
  aq <- round(s$away_score_quantiles)
  cli::cli_text(
    "{x$home_team}: {round(s$home_score_mean, 1)} \\
    (SD {round(s$home_score_sd, 1)}, \\
    Q10-Q90: {hq[1]}-{hq[5]})"
  )
  cli::cli_text(
    "{x$away_team}: {round(s$away_score_mean, 1)} \\
    (SD {round(s$away_score_sd, 1)}, \\
    Q10-Q90: {aq[1]}-{aq[5]})"
  )
  cli::cli_text(
    "Margin: {round(s$margin_mean, 1)} (SD {round(s$margin_sd, 1)})"
  )

  invisible(x)
}


#' @export
summary.torp_match_sim <- function(object, ...) {
  s <- object$summary

  cat("=== Match Simulation Summary ===\n")
  cat(object$home_team, "vs", object$away_team, "\n")
  cat(object$n_sims, "simulations\n\n")

  cat("Pre-game estimate:", round(object$estimate, 1), "points\n")
  cat("Pre-game WP:", round(object$pre_game_wp * 100, 1), "%\n\n")

  cat("Win Probabilities:\n")
  cat("  ", object$home_team, ":", round(s$home_win_pct * 100, 1), "%\n")
  cat("  Draw:", round(s$draw_pct * 100, 1), "%\n")
  cat("  ", object$away_team, ":", round(s$away_win_pct * 100, 1), "%\n\n")

  cat("Score Distribution:\n")
  cat("       Mean    SD   Q10   Q25   Q50   Q75   Q90\n")
  hq <- round(s$home_score_quantiles)
  aq <- round(s$away_score_quantiles)
  cat(sprintf("  %-4s %5.1f %5.1f %5d %5d %5d %5d %5d\n",
              substr(object$home_team, 1, 4),
              s$home_score_mean, s$home_score_sd,
              hq[1], hq[2], hq[3], hq[4], hq[5]))
  cat(sprintf("  %-4s %5.1f %5.1f %5d %5d %5d %5d %5d\n",
              substr(object$away_team, 1, 4),
              s$away_score_mean, s$away_score_sd,
              aq[1], aq[2], aq[3], aq[4], aq[5]))
  cat(sprintf("\n  Margin: %5.1f (SD %.1f)\n", s$margin_mean, s$margin_sd))

  # Quarter breakdown
  qtrs <- object$quarters
  if (nrow(qtrs) > 0) {
    cat("\nQuarter Averages:\n")
    cat("       Q1    Q2    Q3    Q4\n")
    qtr_means_h <- tapply(qtrs$home_qtr_score, qtrs$quarter, mean)
    qtr_means_a <- tapply(qtrs$away_qtr_score, qtrs$quarter, mean)
    cat(sprintf("  %-4s %5.1f %5.1f %5.1f %5.1f\n",
                substr(object$home_team, 1, 4),
                qtr_means_h[1], qtr_means_h[2], qtr_means_h[3], qtr_means_h[4]))
    cat(sprintf("  %-4s %5.1f %5.1f %5.1f %5.1f\n",
                substr(object$away_team, 1, 4),
                qtr_means_a[1], qtr_means_a[2], qtr_means_a[3], qtr_means_a[4]))
  }

  invisible(object)
}
