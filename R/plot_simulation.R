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
  }

  if (type == "ladder") {
    # Ladder probability chart: top 8/4/1 probabilities
    summary <- summary[order(summary$avg_wins, decreasing = TRUE), ]
    summary$team <- factor(summary$team, levels = rev(summary$team))

    # Build long-form data for stacked display
    ladder_long <- data.frame(
      team = rep(summary$team, 3),
      metric = rep(c("Top 8", "Top 4", "Premiers"), each = nrow(summary)),
      pct = c(summary$top_8_pct * 100, summary$top_4_pct * 100, summary$top_1_pct * 100),
      stringsAsFactors = FALSE
    )
    ladder_long$metric <- factor(ladder_long$metric, levels = c("Top 8", "Top 4", "Premiers"))

    # Get team colours for fill
    team_cols <- AFL_TEAM_COLORS[as.character(summary$team)]
    names(team_cols) <- as.character(summary$team)

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
    ladders <- sim_results$ladders
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
