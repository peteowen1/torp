#' Plot season simulation results
#'
#' Visualizes the output of [simulate_afl_season()] as a ladder probability
#' chart, position heatmap, or finals probability chart.
#'
#' @param sim_results A `torp_sim_results` object from [simulate_afl_season()].
#' @param type One of `"ladder"` (default), `"position"`, or `"finals"`.
#' @param teams Optional character vector of team names to include. NULL for all.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_tile geom_point geom_segment
#'   coord_flip labs scale_fill_gradient scale_fill_manual facet_wrap
#' @importFrom rlang .data
plot_simulation <- function(sim_results, type = c("ladder", "position", "finals"),
                            teams = NULL) {
  type <- match.arg(type)

  if (!inherits(sim_results, "torp_sim_results")) {
    cli::cli_abort("{.arg sim_results} must be a {.cls torp_sim_results} object")
  }

  summary <- summarise_simulations(sim_results)

  if (!is.null(teams)) {
    summary <- summary[summary$team %in% teams, ]
    if (nrow(summary) == 0) {
      all_teams <- summarise_simulations(sim_results)$team
      cli::cli_abort(c(
        "No matching teams found in simulation results.",
        "i" = "Available: {.val {all_teams}}"
      ))
    }
  }

  if (type == "ladder") {
    # Ladder probability chart: top 8/4/1 probabilities
    summary <- summary[order(summary$avg_wins, decreasing = TRUE), ]
    summary$team <- factor(summary$team, levels = rev(summary$team))

    p <- ggplot2::ggplot(summary, ggplot2::aes(x = .data$team, y = .data$avg_wins)) +
      ggplot2::geom_col(ggplot2::aes(fill = .data$team), width = 0.7, show.legend = FALSE) +
      ggplot2::coord_flip() +
      team_fill_scale(guide = "none") +
      ggplot2::labs(
        title = paste("AFL", sim_results$season, "Season Simulation"),
        subtitle = paste0(sim_results$n_sims, " simulations \u2014 ordered by expected wins"),
        x = NULL, y = "Expected Wins"
      ) +
      theme_torp()

  } else if (type == "position") {
    # Position heatmap: team x ladder position probability
    ladders <- data.table::as.data.table(sim_results$ladders)
    n <- sim_results$n_sims

    # Compute position probabilities
    pos_probs <- ladders[, .(prob = .N / n), by = .(team, rank)]

    if (!is.null(teams)) {
      pos_probs <- pos_probs[pos_probs$team %in% teams, ]
    }

    # Order teams by avg rank
    team_order <- summary$team[order(summary$avg_rank)]
    pos_probs$team <- factor(pos_probs$team, levels = rev(team_order))

    p <- ggplot2::ggplot(pos_probs, ggplot2::aes(
      x = .data$rank, y = .data$team, fill = .data$prob
    )) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.2) +
      ggplot2::scale_fill_gradient(
        low = "white", high = "#2166ac",
        labels = function(x) paste0(round(x * 100), "%"),
        name = "Probability"
      ) +
      ggplot2::scale_x_continuous(breaks = seq(1, 18)) +
      ggplot2::labs(
        title = paste("AFL", sim_results$season, "Ladder Position Probabilities"),
        subtitle = paste0(sim_results$n_sims, " simulations"),
        x = "Ladder Position", y = NULL
      ) +
      theme_torp() +
      ggplot2::theme(legend.position = "right")

  } else {
    # Finals chart: finals/GF/premiership probabilities
    summary <- summary[order(summary$made_finals_pct, decreasing = TRUE), ]
    summary <- summary[summary$made_finals_pct > 0, ]

    if (nrow(summary) == 0) {
      cli::cli_abort("No teams have finals probability > 0")
    }

    summary$team <- factor(summary$team, levels = rev(summary$team))

    finals_long <- data.frame(
      team = rep(summary$team, 3),
      stage = rep(c("Made Finals", "Made Grand Final", "Won Premiership"), each = nrow(summary)),
      pct = c(summary$made_finals_pct * 100, summary$made_gf_pct * 100, summary$won_gf_pct * 100),
      stringsAsFactors = FALSE
    )
    finals_long$stage <- factor(finals_long$stage,
      levels = c("Made Finals", "Made Grand Final", "Won Premiership"))

    p <- ggplot2::ggplot(finals_long, ggplot2::aes(
      x = .data$team, y = .data$pct, fill = .data$stage
    )) +
      ggplot2::geom_col(position = "dodge", width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("Made Finals" = "#4292c6", "Made Grand Final" = "#2171b5",
                    "Won Premiership" = "#084594"),
        name = NULL
      ) +
      ggplot2::labs(
        title = paste("AFL", sim_results$season, "Finals Probabilities"),
        subtitle = paste0(sim_results$n_sims, " simulations"),
        x = NULL, y = "Probability (%)"
      ) +
      theme_torp()
  }

  p
}


#' Plot match simulation results
#'
#' Visualizes the output of [simulate_match_mc()] as score distributions,
#' win probability fan charts, margin density, or quarter breakdowns.
#'
#' @param match_sim A `torp_match_sim` object from [simulate_match_mc()].
#' @param type One of `"scores"`, `"wp_trajectory"`, `"margin"`, or `"quarters"`.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_density geom_ribbon geom_line geom_vline
#'   geom_violin labs scale_x_continuous scale_y_continuous scale_fill_manual
#'   scale_colour_manual
plot_match_simulation <- function(match_sim,
                                  type = c("scores", "wp_trajectory", "margin", "quarters")) {
  type <- match.arg(type)

  if (!inherits(match_sim, "torp_match_sim")) {
    cli::cli_abort("{.arg match_sim} must be a {.cls torp_match_sim} object")
  }

  home <- match_sim$home_team
  away <- match_sim$away_team
  home_short <- sub(" .*", "", home)
  away_short <- sub(" .*", "", away)

  if (type == "scores") {
    # Score distribution: overlapping density for home and away
    scores_long <- data.table::data.table(
      score = c(match_sim$scores$home_score, match_sim$scores$away_score),
      team = factor(
        rep(c(home, away), each = match_sim$n_sims),
        levels = c(home, away)
      )
    )

    p <- ggplot2::ggplot(scores_long, ggplot2::aes(
      x = .data$score, fill = .data$team, colour = .data$team
    )) +
      ggplot2::geom_density(alpha = 0.35, linewidth = 0.8) +
      team_fill_scale(name = NULL) +
      team_color_scale(name = NULL) +
      ggplot2::geom_vline(
        xintercept = match_sim$summary$home_score_mean,
        linetype = "dashed", alpha = 0.6
      ) +
      ggplot2::geom_vline(
        xintercept = match_sim$summary$away_score_mean,
        linetype = "dashed", alpha = 0.6
      ) +
      ggplot2::labs(
        title = paste(home, "vs", away, "- Score Distributions"),
        subtitle = paste0(match_sim$n_sims, " simulations"),
        x = "Score", y = "Density"
      ) +
      theme_torp()

  } else if (type == "wp_trajectory") {
    # WP fan chart with percentile bands
    wp <- match_sim$wp_trajectory

    p <- ggplot2::ggplot(wp, ggplot2::aes(x = .data$time_pct)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$wp_p10, ymax = .data$wp_p90),
        fill = "#2166ac", alpha = 0.15
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$wp_p25, ymax = .data$wp_p75),
        fill = "#2166ac", alpha = 0.25
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$wp_median),
        colour = "#2166ac", linewidth = 1.2
      ) +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey50") +
      ggplot2::scale_x_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = c("Start", "Q1", "Half", "Q3", "Full")
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        labels = function(x) paste0(round(x * 100), "%")
      ) +
      ggplot2::annotate(
        "text", x = 0.02, y = 0.95, label = home_short,
        hjust = 0, fontface = "bold", size = 3.5, colour = "grey30"
      ) +
      ggplot2::annotate(
        "text", x = 0.02, y = 0.05, label = away_short,
        hjust = 0, fontface = "bold", size = 3.5, colour = "grey30"
      ) +
      ggplot2::labs(
        title = paste(home, "vs", away, "- Win Probability Fan Chart"),
        subtitle = paste0(
          match_sim$n_sims, " simulations | ",
          "Pre-game: ", home_short, " ", round(match_sim$pre_game_wp * 100, 1), "%"
        ),
        x = "Match Progress", y = paste(home_short, "Win Probability")
      ) +
      theme_torp()

  } else if (type == "margin") {
    # Margin density
    p <- ggplot2::ggplot(
      match_sim$scores,
      ggplot2::aes(x = .data$margin)
    ) +
      ggplot2::geom_density(fill = "#2166ac", alpha = 0.4, colour = "#2166ac") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
      ggplot2::geom_vline(
        xintercept = match_sim$summary$margin_mean,
        linetype = "solid", colour = "#d6604d", linewidth = 0.8
      ) +
      ggplot2::annotate(
        "text",
        x = match_sim$summary$margin_mean + 2,
        y = Inf, vjust = 2,
        label = paste0("Mean: ", round(match_sim$summary$margin_mean, 1)),
        colour = "#d6604d", size = 3.5, hjust = 0
      ) +
      ggplot2::annotate(
        "text", x = -Inf, y = Inf, vjust = 2, hjust = -0.1,
        label = paste0("\u2190 ", away_short, " wins"),
        colour = "grey50", size = 3
      ) +
      ggplot2::annotate(
        "text", x = Inf, y = Inf, vjust = 2, hjust = 1.1,
        label = paste0(home_short, " wins \u2192"),
        colour = "grey50", size = 3
      ) +
      ggplot2::labs(
        title = paste(home, "vs", away, "- Margin Distribution"),
        subtitle = paste0(
          match_sim$n_sims, " simulations | ",
          home_short, " win ", round(match_sim$summary$home_win_pct * 100, 1), "%"
        ),
        x = "Margin (positive = home win)", y = "Density"
      ) +
      theme_torp()

  } else {
    # Quarter breakdown: violin plots
    qtrs <- match_sim$quarters
    qtrs_long <- data.table::data.table(
      score = c(qtrs$home_qtr_score, qtrs$away_qtr_score),
      quarter = factor(rep(qtrs$quarter, 2), labels = paste0("Q", 1:4)),
      team = factor(
        c(rep(home, nrow(qtrs)), rep(away, nrow(qtrs))),
        levels = c(home, away)
      )
    )

    p <- ggplot2::ggplot(qtrs_long, ggplot2::aes(
      x = .data$quarter, y = .data$score, fill = .data$team
    )) +
      ggplot2::geom_violin(
        position = ggplot2::position_dodge(width = 0.8),
        alpha = 0.6, draw_quantiles = 0.5
      ) +
      team_fill_scale(name = NULL) +
      ggplot2::labs(
        title = paste(home, "vs", away, "- Quarter Scoring"),
        subtitle = paste0(match_sim$n_sims, " simulations"),
        x = "Quarter", y = "Score"
      ) +
      theme_torp()
  }

  p
}
