#' Plot player rating trend over time
#'
#' Shows per-game TORP (or component) ratings as points with a rolling average
#' smoothed line. Useful for tracking player form and development.
#'
#' @param player_name Player name (partial match OK, resolved via `resolve_player()`).
#' @param seasons Seasons to include. Numeric vector or TRUE for all. Default TRUE.
#' @param metric One of `"torp_value"` (default), `"epv"`, `"psv"`. The per-game
#'   value column to plot.
#' @param rolling Rolling average window (number of games). Default 5.
#' @param show_season_avg Logical. If TRUE (default), show horizontal segments
#'   for each season's average.
#' @param data Optional pre-loaded data from `load_player_game_ratings()`.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_hline geom_segment
#'   labs scale_colour_manual
#' @importFrom rlang .data
plot_player_rating <- function(player_name, seasons = TRUE,
                               metric = c("torp_value", "epv", "psv"),
                               rolling = 5, show_season_avg = TRUE,
                               data = NULL) {
  metric <- match.arg(metric)

  # Load data
  if (is.null(data)) {
    data <- load_player_game_ratings(seasons)
  }

  # Resolve player
  player <- resolve_player(player_name, seasons = if (isTRUE(seasons)) TRUE else seasons)
  pid <- player$player_id

  # Filter to player
  pdf <- data[data$player_id == pid, ]
  if (nrow(pdf) == 0) {
    cli::cli_abort("No game ratings found for {.val {player_name}}")
  }

  # Check metric column exists
  if (!metric %in% names(pdf)) {
    available <- intersect(c("torp_value", "epv", "psv", "osv", "dsv"), names(pdf))
    cli::cli_abort(c(
      "Column {.val {metric}} not found in game ratings data.",
      "i" = "Available: {.val {available}}"
    ))
  }

  # Sort by game order and compute rolling average
  pdf <- pdf[order(pdf$season, pdf$round_number), ]
  pdf$game_number <- seq_len(nrow(pdf))
  pdf$season_fac <- factor(pdf$season)

  # Rolling average
  vals <- pdf[[metric]]
  if (all(is.na(vals))) {
    cli::cli_abort("Column {.val {metric}} contains only NA values for {.val {player_name}}")
  }
  pdf$rolling_avg <- NA_real_
  for (i in seq_along(vals)) {
    start <- max(1, i - rolling + 1)
    pdf$rolling_avg[i] <- mean(vals[start:i], na.rm = TRUE)
  }

  metric_label <- switch(metric,
    torp_value = "TORP Value",
    epv = "EPV",
    psv = "PSV",
    metric
  )

  p <- ggplot2::ggplot(pdf, ggplot2::aes(x = .data$game_number)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data[[metric]], colour = .data$season_fac),
      size = 1.5, alpha = 0.4
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$rolling_avg),
      colour = "grey20", linewidth = 0.8
    ) +
    ggplot2::labs(
      title = player$player_name,
      subtitle = paste0(metric_label, " per game (", rolling, "-game rolling avg)"),
      x = "Game #",
      y = metric_label,
      colour = "Season"
    )

  # Season average segments
  if (show_season_avg) {
    season_avgs <- stats::aggregate(
      pdf[[metric]],
      by = list(season = pdf$season),
      FUN = mean, na.rm = TRUE
    )
    names(season_avgs) <- c("season", "avg")

    for (i in seq_len(nrow(season_avgs))) {
      szn <- season_avgs$season[i]
      szn_rows <- pdf[pdf$season == szn, ]
      if (nrow(szn_rows) > 0) {
        p <- p + ggplot2::annotate(
          "segment",
          x = min(szn_rows$game_number), xend = max(szn_rows$game_number),
          y = season_avgs$avg[i], yend = season_avgs$avg[i],
          linetype = "solid", colour = "grey40", linewidth = 0.4, alpha = 0.6
        )
      }
    }
  }

  p + theme_torp()
}


#' Plot stat rating profile as bar chart or radar
#'
#' Visualizes a player's stat rating percentile ranks grouped by category.
#'
#' @param x A `torp_stat_rating_profile` object from [player_stat_rating_profile()].
#' @param type One of `"bar"` (default) or `"radar"`.
#' @param categories Optional character vector of categories to include (e.g.
#'   `c("scoring", "disposal")`). NULL for all.
#' @param top_n For bar chart, number of stats to show (sorted by percentile).
#'   Default 15. Use NULL for all.
#' @param comparison One of `"position"` (default, uses `pos_pct`) or `"league"`
#'   (uses `league_pct`).
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_vline coord_flip coord_polar
#'   scale_fill_gradient2 labs facet_wrap
#' @importFrom rlang .data
plot_stat_rating_profile <- function(x, type = c("bar", "radar"),
                                     categories = NULL, top_n = 15,
                                     comparison = c("position", "league")) {
  type <- match.arg(type)
  comparison <- match.arg(comparison)

  if (!inherits(x, "torp_stat_rating_profile")) {
    cli::cli_abort("{.arg x} must be a {.cls torp_stat_rating_profile} object")
  }

  sk <- x$stat_ratings
  info <- x$player_info
  pct_col <- if (comparison == "position") "pos_pct" else "league_pct"

  if (!pct_col %in% names(sk)) {
    cli::cli_abort("Column {.val {pct_col}} not found in stat ratings")
  }

  # Filter categories
  if (!is.null(categories) && "category" %in% names(sk)) {
    sk <- sk[sk$category %in% categories, ]
  }

  # Remove NAs
  sk <- sk[!is.na(sk[[pct_col]]), ]

  if (nrow(sk) == 0) {
    cli::cli_abort("No stats remaining after filtering")
  }

  if (type == "bar") {
    # Sort by percentile, take top_n
    sk <- sk[order(-sk[[pct_col]]), ]
    if (!is.null(top_n) && nrow(sk) > top_n) {
      sk <- sk[seq_len(top_n), ]
    }

    # Order factor for display
    sk$stat <- factor(sk$stat, levels = rev(sk$stat))

    p <- ggplot2::ggplot(sk, ggplot2::aes(
      x = .data$stat,
      y = .data[[pct_col]],
      fill = .data[[pct_col]]
    )) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_vline(xintercept = 0, colour = "grey30") +
      ggplot2::geom_hline(yintercept = 50, linetype = "dashed", colour = "grey50") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient2(
        low = "#d73027", mid = "#ffffbf", high = "#1a9850",
        midpoint = 50, limits = c(0, 100), guide = "none"
      ) +
      ggplot2::scale_y_continuous(limits = c(0, 100)) +
      ggplot2::labs(
        title = info$name,
        subtitle = paste0(
          "Stat Rating Percentiles vs ",
          if (comparison == "position") paste0(info$pos_group, " position group") else "league",
          " \u2014 ", x$ref_date
        ),
        x = NULL, y = "Percentile"
      ) +
      theme_torp()

  } else {
    # Radar: category-level averages
    if (!"category" %in% names(sk)) {
      cli::cli_abort("Radar chart requires a {.val category} column in stat ratings")
    }

    cat_avgs <- stats::aggregate(
      sk[[pct_col]],
      by = list(category = sk$category),
      FUN = mean, na.rm = TRUE
    )
    names(cat_avgs) <- c("category", "avg_pct")
    cat_avgs <- cat_avgs[order(cat_avgs$category), ]
    # Close the polygon by repeating first row
    cat_avgs_closed <- rbind(cat_avgs, cat_avgs[1, ])
    cat_avgs_closed$idx <- seq_len(nrow(cat_avgs_closed))

    p <- ggplot2::ggplot(cat_avgs, ggplot2::aes(x = .data$category, y = .data$avg_pct)) +
      ggplot2::geom_col(fill = team_color_lookup(info$team, "#4292c6"), alpha = 0.6, width = 0.8) +
      ggplot2::geom_hline(yintercept = 50, linetype = "dashed", colour = "grey50") +
      ggplot2::coord_polar() +
      ggplot2::scale_y_continuous(limits = c(0, 100)) +
      ggplot2::labs(
        title = info$name,
        subtitle = paste0("Skill Categories vs ",
          if (comparison == "position") info$pos_group else "league"),
        x = NULL, y = NULL
      ) +
      theme_torp() +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

  p
}
