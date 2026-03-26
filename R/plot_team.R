#' Plot team ratings comparison
#'
#' Displays team-level TORP ratings as a horizontal bar chart with team colours.
#' Automatically uses the latest round from `load_team_ratings()` and maps
#' column names (`team_epr` -> `epr`, etc.).
#'
#' @param team_ratings Optional data frame of team ratings. If NULL (default),
#'   loads via `load_team_ratings()` and filters to the latest round.
#' @param metric One of `"epr"` (default), `"recv"`, `"disp"`, `"spoil"`,
#'   or `"hitout"`. Mapped to `team_epr`, `team_recv`, etc.
#' @param season Season year for title. Default: current season.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_vline coord_flip labs
#' @importFrom rlang .data
plot_team_ratings <- function(team_ratings = NULL,
                              metric = c("epr", "recv", "disp", "spoil", "hitout"),
                              season = get_afl_season()) {
  metric <- match.arg(metric)

  if (is.null(team_ratings)) {
    team_ratings <- load_team_ratings()
  }

  # Filter to latest round per season if multiple rounds exist
  if ("round" %in% names(team_ratings) && "season" %in% names(team_ratings)) {
    latest <- team_ratings$season == max(team_ratings$season, na.rm = TRUE)
    team_ratings <- team_ratings[latest, ]
    latest_round <- max(team_ratings$round, na.rm = TRUE)
    team_ratings <- team_ratings[team_ratings$round == latest_round, ]
  }

  # Normalise team names to canonical full names for colour matching
  team_ratings$team <- torp_replace_teams(team_ratings$team)

  # Map metric names: epr -> team_epr, recv -> team_recv, etc.
  col_name <- paste0("team_", metric)
  if (!col_name %in% names(team_ratings)) {
    # Try without prefix
    if (metric %in% names(team_ratings)) {
      col_name <- metric
    } else {
      available <- grep("^team_", names(team_ratings), value = TRUE)
      cli::cli_abort(c(
        "Column {.val {col_name}} not found in team ratings.",
        "i" = "Available: {.val {available}}"
      ))
    }
  }

  # Sort by metric
  team_ratings <- team_ratings[order(team_ratings[[col_name]]), ]
  team_ratings$team <- factor(team_ratings$team, levels = team_ratings$team)

  metric_label <- switch(metric,
    epr = "Team EPR",
    recv = "Receiving EPR",
    disp = "Disposal EPR",
    spoil = "Spoil EPR",
    hitout = "Hitout EPR",
    metric
  )

  p <- ggplot2::ggplot(team_ratings, ggplot2::aes(
    x = .data$team,
    y = .data[[col_name]],
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
