#' Plot shot map for a match or team
#'
#' Displays shot locations on a half-field layout with colour indicating
#' expected goal probability (xG). Shots are positioned by their field
#' coordinates (`goal_x` for distance to goal, `y` for lateral position).
#'
#' @param season Season year (default: current season).
#' @param round Round number. Required if `match_id` is NULL.
#' @param match_id Optional specific match ID. If NULL, uses `season` + `round`.
#' @param team Optional team name to filter to (shows only that team's shots).
#' @param show_outcome Logical. If TRUE (default), shapes indicate actual
#'   outcome (goal/behind/miss). If FALSE, all shots shown as circles.
#' @param data Optional pre-loaded PBP data with shot columns (`goal_x`, `y`,
#'   `goal_prob`, `shot_row`, `points_shot`). If NULL, loads via `load_pbp()`.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_vline geom_segment
#'   scale_colour_gradient scale_shape_manual labs coord_fixed annotate
#' @importFrom rlang .data
plot_shot_map <- function(season = get_afl_season(), round = NULL, match_id = NULL,
                          team = NULL, show_outcome = TRUE,
                          data = NULL) {
  # Load data
  if (is.null(data)) {
    if (is.null(round) && is.null(match_id)) {
      cli::cli_abort("Must provide either {.arg round} or {.arg match_id}")
    }
    data <- load_pbp(season, rounds = if (!is.null(round)) round else TRUE)
  }

  # Filter to match
  if (!is.null(match_id)) {
    data <- data[data$match_id == match_id, ]
    if (nrow(data) == 0) {
      cli::cli_abort("No data found for match_id {.val {match_id}}")
    }
  }

  # Ensure shot columns exist
  required_cols <- c("goal_x", "y", "shot_row")
  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required columns: {.val {missing}}. Ensure PBP data has been processed through the shot pipeline.")
  }

  # Filter to shots only
  shots <- data[!is.na(data$shot_row) & data$shot_row == 1, ]

  if (nrow(shots) == 0) {
    cli::cli_abort("No shots found in the data")
  }

  # Filter to team if specified
  if (!is.null(team)) {
    team_resolved <- torp_replace_teams(team)
    # Try matching against team column
    if ("team" %in% names(shots)) {
      team_match <- shots$team == team_resolved | shots$team == team
      if (sum(team_match) == 0) {
        available <- unique(shots$team)
        cli::cli_abort(c(
          "No shots found for team {.val {team}}.",
          "i" = "Available teams: {.val {available}}"
        ))
      }
      shots <- shots[team_match, ]
    }
  }

  # Determine outcome for shape mapping
  if (show_outcome && "points_shot" %in% names(shots)) {
    shots$outcome <- ifelse(is.na(shots$points_shot), "Miss",
                     ifelse(shots$points_shot == 6, "Goal",
                     ifelse(shots$points_shot == 1, "Behind", "Miss")))
    shots$outcome <- factor(shots$outcome, levels = c("Goal", "Behind", "Miss"))
  }

  # Determine colour variable (xG if available, otherwise distance)
  has_xg <- "goal_prob" %in% names(shots)

  # Build title
  if (!is.null(match_id) || (!is.null(round) && length(unique(shots$match_id)) == 1)) {
    home <- if ("home_team_name" %in% names(shots)) shots$home_team_name[1] else "Home"
    away <- if ("away_team_name" %in% names(shots)) shots$away_team_name[1] else "Away"
    rd <- if ("round_number" %in% names(shots)) shots$round_number[1] else round
    title <- paste(home, "vs", away)
    subtitle_parts <- paste0("Round ", rd, ", ", season)
  } else {
    title <- if (!is.null(team)) paste(team, "Shot Map") else "Shot Map"
    subtitle_parts <- paste0("Round ", round, ", ", season)
  }

  if (!is.null(team)) subtitle_parts <- paste0(subtitle_parts, " \u2014 ", team, " shots only")

  # Build the plot
  # Half-field: goal_x runs from 0 (at goal) to ~80+ (midfield)
  # y runs from about -40 to +40
  p <- ggplot2::ggplot(shots, ggplot2::aes(x = .data$goal_x, y = .data$y))

  # Field reference lines
  p <- p +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 50, colour = "grey70", linetype = "dashed", linewidth = 0.3) +
    ggplot2::annotate("text", x = 50, y = -38, label = "50m arc", colour = "grey50",
                       size = 2.5, hjust = 0) +
    ggplot2::annotate("text", x = 1, y = -38, label = "Goal line", colour = "grey50",
                       size = 2.5, hjust = 0)

  # Points: colour by xG, shape by outcome
  if (has_xg && show_outcome && "outcome" %in% names(shots)) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(colour = .data$goal_prob, shape = .data$outcome),
      size = 3, alpha = 0.8
    ) +
    ggplot2::scale_shape_manual(
      values = c("Goal" = 16, "Behind" = 17, "Miss" = 4),
      name = "Outcome"
    )
  } else if (has_xg) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(colour = .data$goal_prob),
      size = 3, alpha = 0.8
    )
  } else {
    p <- p + ggplot2::geom_point(size = 3, alpha = 0.8, colour = "steelblue")
  }

  # Colour scale for xG
  if (has_xg) {
    p <- p + ggplot2::scale_colour_gradient(
      low = "#d73027", high = "#1a9850",
      name = "Goal Prob",
      limits = c(0, 1),
      labels = function(x) paste0(round(x * 100), "%")
    )
  }

  p <- p +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle_parts,
      x = "Distance to Goal (m)",
      y = "Lateral Position (m)"
    ) +
    theme_torp() +
    ggplot2::theme(legend.position = "right")

  p
}
