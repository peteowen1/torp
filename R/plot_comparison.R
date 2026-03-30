#' Plot multi-player rating comparison
#'
#' Overlays per-game rating trends for 2-5 players on a single chart,
#' showing rolling averages for easy comparison.
#'
#' @param player_names Character vector of 2-5 player names (partial match OK).
#' @param seasons Seasons to include. Numeric vector or TRUE for all. Default TRUE.
#' @param metric One of `"torp_value"` (default), `"epv"`, `"psv"`.
#' @param rolling Rolling average window (number of games). Default 10.
#' @param show_points Logical. If TRUE (default), show individual game points
#'   in addition to the rolling average line.
#' @param data Optional pre-loaded data from `load_player_game_ratings()`.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_hline labs
#'   scale_colour_manual
#' @importFrom rlang .data
plot_player_comparison <- function(player_names, seasons = TRUE,
                                   metric = c("torp_value", "epv", "psv"),
                                   rolling = 10, show_points = TRUE,
                                   data = NULL) {
  metric <- match.arg(metric)

  if (length(player_names) < 2 || length(player_names) > 5) {
    cli::cli_abort("{.arg player_names} must contain 2-5 player names, got {length(player_names)}")
  }

  # Load data
  if (is.null(data)) {
    data <- load_player_game_ratings(seasons)
  }

  # Check metric column exists
  if (!metric %in% names(data)) {
    available <- intersect(c("torp_value", "epv", "psv", "osv", "dsv"), names(data))
    cli::cli_abort(c(
      "Column {.val {metric}} not found in game ratings data.",
      "i" = "Available: {.val {available}}"
    ))
  }

  # Resolve each player and build combined data
  all_players <- list()
  resolved_names <- character()

  for (pname in player_names) {
    player <- tryCatch(
      resolve_player(pname, seasons = if (isTRUE(seasons)) TRUE else seasons),
      error = function(e) {
        cli::cli_warn("Could not resolve player {.val {pname}}: {e$message}")
        NULL
      }
    )
    if (is.null(player)) next
    pid <- player$player_id

    player_df <- data[data$player_id == pid, ]
    if (nrow(player_df) == 0) {
      cli::cli_warn("No game ratings found for {.val {pname}}, skipping")
      next
    }

    player_df <- player_df[order(player_df$season, player_df$round_number), ]
    player_df$game_number <- seq_len(nrow(player_df))
    player_df$player_label <- player$player_name

    # Rolling average
    player_df$rolling_avg <- .rolling_mean(player_df[[metric]], rolling)

    all_players[[length(all_players) + 1]] <- player_df
    resolved_names <- c(resolved_names, player$player_name)
  }

  if (length(all_players) == 0) {
    cli::cli_abort("No game ratings found for any of the specified players")
  }

  combined <- do.call(rbind, all_players)

  # Assign colours: use team colours if available, otherwise use a palette
  unique_players <- unique(combined$player_label)
  # Try to get team colours for each player
  player_colours <- character()
  for (plabel in unique_players) {
    player_team <- torp_replace_teams(combined$team[combined$player_label == plabel][1])
    player_colours[plabel] <- team_color_lookup(player_team, NA_character_)
  }

  # If there are colour collisions (same team) or NAs, use a fallback palette
  if (anyNA(player_colours) || length(unique(player_colours)) < length(player_colours)) {
    fallback <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
    player_colours <- stats::setNames(fallback[seq_along(unique_players)], unique_players)
  }

  metric_label <- switch(metric,
    torp_value = "TORP Value",
    epv = "EPV",
    psv = "PSV",
    metric
  )

  p <- ggplot2::ggplot(combined, ggplot2::aes(
    x = .data$game_number,
    colour = .data$player_label
  ))

  if (show_points) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(y = .data[[metric]]),
      size = 1, alpha = 0.2
    )
  }

  p <- p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$rolling_avg),
      linewidth = 0.9
    ) +
    ggplot2::scale_colour_manual(values = player_colours, name = NULL) +
    ggplot2::labs(
      title = "Player Comparison",
      subtitle = paste0(metric_label, " (", rolling, "-game rolling avg)"),
      x = "Game #",
      y = metric_label
    ) +
    theme_torp()

  p
}
