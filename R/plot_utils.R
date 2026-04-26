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

#' Look up a team colour with NA-safe fallback
#'
#' @param team Canonical team name.
#' @param default Fallback hex colour if team not found.
#' @param colors Named colour vector (default `AFL_TEAM_COLORS`).
#' @return A hex colour string.
#' @keywords internal
team_color_lookup <- function(team, default = "#808080", colors = AFL_TEAM_COLORS) {
  col <- colors[team]
  if (is.na(col) || is.null(col)) default else unname(col)
}

#' Default visual constants for torp plots
#'
#' Returns a named list of recurring plot constants (line weights, point
#' sizes, reference colours) used across the `plot_*.R` files. Centralising
#' these here means changing torp's visual language is a one-file edit
#' instead of a grep-and-replace across every plot function.
#'
#' Use as `td <- plot_defaults(); td$gridline_lwd` or
#' `with(plot_defaults(), { ... })`. Existing plot functions still hardcode
#' these values inline; new plots and incremental migrations should reference
#' this helper instead.
#'
#' @return A named list with the following slots:
#' \describe{
#'   \item{`gridline_lwd`}{Subtle reference line weight (e.g. zero hline)}
#'   \item{`primary_lwd`}{Standard data line weight (e.g. main trace)}
#'   \item{`emphasis_lwd`}{Heavier weight for axis-zero / important refs}
#'   \item{`heavy_lwd`}{Thickest weight for callout lines}
#'   \item{`point_sm`}{Small point/jitter size}
#'   \item{`point_md`}{Default geom_point size}
#'   \item{`point_lg`}{Large point size for shot/highlighted markers}
#'   \item{`grey_dark`}{Primary text/line grey (`grey20`)}
#'   \item{`grey_mid`}{De-emphasised text grey (`grey30`)}
#'   \item{`grey_axis`}{Axis line grey (`grey40`)}
#'   \item{`grey_ref`}{Soft reference grey (`grey50`)}
#' }
#' @export
plot_defaults <- function() {
  list(
    gridline_lwd = 0.3,
    primary_lwd  = 0.5,
    emphasis_lwd = 0.8,
    heavy_lwd    = 1.2,
    point_sm     = 1.5,
    point_md     = 2.5,
    point_lg     = 3,
    grey_dark    = "grey20",
    grey_mid     = "grey30",
    grey_axis    = "grey40",
    grey_ref     = "grey50"
  )
}

#' Trailing rolling mean
#'
#' Computes a trailing (right-aligned) rolling mean over a numeric vector.
#'
#' @param x Numeric vector.
#' @param window Integer window size.
#' @return Numeric vector of same length as `x`.
#' @keywords internal
.rolling_mean <- function(x, window) {
  n <- length(x)
  out <- numeric(n)
  for (i in seq_len(n)) {
    start <- max(1L, i - window + 1L)
    out[i] <- mean(x[start:i], na.rm = TRUE)
  }
  out
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
