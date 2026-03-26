#' Plot team ratings comparison
#'
#' Displays team-level TORP ratings as a horizontal bar chart with team colours.
#'
#' @param team_ratings Optional data frame of team ratings. If NULL (default),
#'   loads via `load_team_ratings()`.
#' @param metric Column name to plot. One of `"torp"` (default), `"recv_epr"`,
#'   `"disp_epr"`, `"spoil_epr"`, `"hitout_epr"`, or `"epr"`.
#' @param season Season year for title. Default: current season.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_vline coord_flip labs
#' @importFrom rlang .data
plot_team_ratings <- function(team_ratings = NULL,
                              metric = c("torp", "epr", "recv_epr", "disp_epr",
                                          "spoil_epr", "hitout_epr"),
                              season = get_afl_season()) {
  metric <- match.arg(metric)

  if (is.null(team_ratings)) {
    team_ratings <- load_team_ratings()
  }

  if (!metric %in% names(team_ratings)) {
    available <- intersect(c("torp", "epr", "recv_epr", "disp_epr", "spoil_epr", "hitout_epr"),
                            names(team_ratings))
    cli::cli_abort(c(
      "Column {.val {metric}} not found in team ratings.",
      "i" = "Available: {.val {available}}"
    ))
  }

  # Sort by metric
  team_ratings <- team_ratings[order(team_ratings[[metric]]), ]
  team_ratings$team <- factor(team_ratings$team, levels = team_ratings$team)

  metric_label <- switch(metric,
    torp = "TORP Rating",
    epr = "EPR",
    recv_epr = "Receiving EPR",
    disp_epr = "Disposal EPR",
    spoil_epr = "Spoil EPR",
    hitout_epr = "Hitout EPR",
    metric
  )

  p <- ggplot2::ggplot(team_ratings, ggplot2::aes(
    x = .data$team,
    y = .data[[metric]],
    fill = .data$team
  )) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
    ggplot2::coord_flip() +
    team_fill_scale(guide = "none") +
    ggplot2::labs(
      title = paste("AFL", season, "Team Ratings"),
      subtitle = metric_label,
      x = NULL, y = metric_label
    ) +
    theme_torp()

  p
}
