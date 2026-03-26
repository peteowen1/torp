#' Plot EP/WP game flow chart
#'
#' Visualizes the expected points or win probability trajectory of a single
#' AFL match. The most common visualization in sports analytics.
#'
#' @param season Season year (default: current season).
#' @param round Round number. If NULL and `match_id` is NULL, uses latest round.
#' @param match_id Optional specific match ID. If NULL, the round must contain
#'   exactly one match or an error is raised with available matches.
#' @param metric One of `"wp"` (win probability, default) or `"ep"` (expected points).
#' @param show_plays Logical. If TRUE (default), overlay key play markers (goals).
#' @param home_color Override hex colour for home team.
#' @param away_color Override hex colour for away team.
#' @param data Optional pre-loaded EP/WP chart data. If NULL, loads via
#'   `load_ep_wp_charts()`.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_vline geom_point
#'   geom_ribbon labs scale_x_continuous scale_y_continuous annotate
#' @importFrom rlang .data
plot_ep_wp <- function(season = get_afl_season(), round = NULL, match_id = NULL,
                       metric = c("wp", "ep"), show_plays = TRUE,
                       home_color = NULL, away_color = NULL,
                       data = NULL) {
  metric <- match.arg(metric)

  # Load data
  if (is.null(data)) {
    if (is.null(round) && is.null(match_id)) {
      cli::cli_abort("Must provide either {.arg round} or {.arg match_id}")
    }
    data <- load_ep_wp_charts(season, rounds = if (!is.null(round)) round else TRUE)
  }

  # Filter to match
  if (!is.null(match_id)) {
    match_data <- data[data$match_id == match_id, ]
    if (nrow(match_data) == 0) {
      cli::cli_abort("No data found for match_id {.val {match_id}}")
    }
  } else {
    matches <- unique(data$match_id)
    if (length(matches) == 0) {
      cli::cli_abort("No matches found in the data for Round {round}")
    }
    if (length(matches) > 1) {
      match_labels <- vapply(matches, function(mid) {
        row1 <- data[data$match_id == mid, ][1, ]
        paste0(row1$home_team_name, " vs ", row1$away_team_name, " (", mid, ")")
      }, character(1))
      cli::cli_abort(c(
        "Multiple matches found for Round {round}. Specify {.arg match_id}:",
        stats::setNames(match_labels, rep("*", length(match_labels)))
      ))
    }
    match_data <- data
  }

  # Extract team names (already canonical from load_ep_wp_charts normalization)
  home_team <- match_data$home_team_name[1]
  away_team <- match_data$away_team_name[1]
  rd <- match_data$round_number[1]
  szn <- match_data$season[1]

  # Resolve colours (NA-safe)
  if (is.null(home_color)) home_color <- team_color_lookup(home_team, "#1f77b4")
  if (is.null(away_color)) away_color <- team_color_lookup(away_team, "#ff7f0e")

  qtr_breaks <- quarter_breaks()

  if (metric == "wp") {
    # WP chart: area fill around 50%
    p <- ggplot2::ggplot(match_data, ggplot2::aes(x = .data$total_seconds, y = .data$wp * 100)) +
      ggplot2::geom_hline(yintercept = 50, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = 50, ymax = pmax(.data$wp * 100, 50)),
        fill = home_color, alpha = 0.3
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = pmin(.data$wp * 100, 50), ymax = 50),
        fill = away_color, alpha = 0.3
      ) +
      ggplot2::geom_line(colour = "grey20", linewidth = 0.5) +
      ggplot2::scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      ggplot2::labs(
        title = paste(home_team, "vs", away_team),
        subtitle = paste0("Win Probability \u2014 Round ", rd, ", ", szn),
        x = NULL, y = "Home Win Probability"
      )
  } else {
    # EP chart: expected points differential
    p <- ggplot2::ggplot(match_data, ggplot2::aes(x = .data$total_seconds, y = .data$exp_pts)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = 0, ymax = pmax(.data$exp_pts, 0)),
        fill = home_color, alpha = 0.3
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = pmin(.data$exp_pts, 0), ymax = 0),
        fill = away_color, alpha = 0.3
      ) +
      ggplot2::geom_line(colour = "grey20", linewidth = 0.5) +
      ggplot2::labs(
        title = paste(home_team, "vs", away_team),
        subtitle = paste0("Expected Points \u2014 Round ", rd, ", ", szn),
        x = NULL, y = "Expected Points (Home)"
      )
  }

  # Quarter breaks
  p <- p +
    ggplot2::geom_vline(xintercept = qtr_breaks[-1], linetype = "dotted", colour = "grey70") +
    ggplot2::scale_x_continuous(
      breaks = qtr_breaks,
      labels = names(qtr_breaks)
    )

  # Key play markers (goals)
  if (show_plays && "shot_row" %in% names(match_data) && "points_shot" %in% names(match_data)) {
    goals <- match_data[!is.na(match_data$points_shot) & match_data$points_shot == 6, ]
    if (nrow(goals) > 0) {
      p <- p + ggplot2::geom_point(
        data = goals,
        ggplot2::aes(
          x = .data$total_seconds,
          y = if (metric == "wp") .data$wp * 100 else .data$exp_pts
        ),
        size = 1.5, alpha = 0.5, colour = "grey30"
      )
    }
  }

  p + theme_torp()
}
