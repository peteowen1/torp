#' torp ggplot2 theme
#'
#' A clean theme for torp visualizations based on `theme_minimal()`.
#'
#' @param base_size Base font size (default 12).
#' @return A ggplot2 theme object.
#' @export
#' @importFrom ggplot2 theme_minimal theme element_text element_blank
theme_torp <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey40"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Team colour scale (colour aesthetic)
#'
#' A convenience wrapper for `scale_colour_manual()` using `AFL_TEAM_COLORS`.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_colour_manual()].
#' @return A ggplot2 colour scale.
#' @export
#' @importFrom ggplot2 scale_colour_manual
team_color_scale <- function(...) {
  ggplot2::scale_colour_manual(values = AFL_TEAM_COLORS, ...)
}

#' Team colour scale (fill aesthetic)
#'
#' A convenience wrapper for `scale_fill_manual()` using `AFL_TEAM_COLORS`.
#'
#' @param ... Additional arguments passed to [ggplot2::scale_fill_manual()].
#' @return A ggplot2 fill scale.
#' @export
#' @importFrom ggplot2 scale_fill_manual
team_fill_scale <- function(...) {
  ggplot2::scale_fill_manual(values = AFL_TEAM_COLORS, ...)
}

#' Quarter break positions for game flow charts
#'
#' Returns a named numeric vector of quarter boundary seconds for x-axis annotation.
#'
#' @return Named numeric vector with quarter boundaries.
#' @keywords internal
quarter_breaks <- function() {
  c(Q1 = 0, Q2 = 2000, Q3 = 4000, Q4 = 6000, End = 8000)
}
