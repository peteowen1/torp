#' Plot team ratings comparison
#'
#' Displays team-level TORP ratings as a horizontal bar chart with team colours.
#' Automatically uses the latest round from `load_team_ratings()` and maps
#' column names (`team_epr` -> `epr`, etc.).
#'
#' @param team_ratings Optional data frame of team ratings. If NULL (default),
#'   loads via `load_team_ratings()` and filters to the latest round.
#' @param metric One of `"epr"` (default), `"recv"`, `"disp"`, `"contest"`,
#'   `"spoil"` or `"hitout"`. Mapped to `team_epr`, `team_epr_recv`, etc.
#'
#'   `"contest"` is a display channel with no published column behind it: it is
#'   `team_epr_spoil + team_epr_hitout`, computed here. Neither part is what its
#'   name suggests — the "spoil" channel is spoils plus tackles plus pressure,
#'   the "hitout" channel is hitouts plus ruck contests — so both are contest
#'   value and the sum is the honest unit. `"spoil"` and `"hitout"` still work
#'   and are labelled as the aerial and stoppage halves of it.
#'
#'   Contest is a **small** channel: sd 0.266 against disposal's 1.693, roughly
#'   2% of EPR's spread. It will look flat beside the others, correctly.
#' @param season Season year for title. Default: current season.
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_vline coord_flip labs
#' @importFrom rlang .data
plot_team_ratings <- function(team_ratings = NULL,
                              metric = c("epr", "recv", "disp", "contest",
                                         "spoil", "hitout"),
                              season = get_afl_season()) {
  metric <- match.arg(metric)

  if (is.null(team_ratings)) {
    team_ratings <- load_team_ratings()
  }

  if (nrow(team_ratings) == 0) {
    cli::cli_abort("No team ratings data available. Ratings may not have been computed yet.")
  }

  # Filter to latest round per season if multiple rounds exist
  if ("round" %in% names(team_ratings) && "season" %in% names(team_ratings)) {
    latest <- team_ratings$season == max(team_ratings$season, na.rm = TRUE)
    team_ratings <- team_ratings[latest, ]
    latest_round <- max(team_ratings$round, na.rm = TRUE)
    team_ratings <- team_ratings[team_ratings$round == latest_round, ]
  }

  # "contest" is a DISPLAY channel, not a published column. `epv_spoil` is
  # spoils + tackles + pressure and `epv_hitout` is hitouts + ruck contests
  # (player_credit.R:857-859), so neither is what its name says and both are
  # contest value. The two correlate only +0.17, so the sum is not cancelling a
  # real ruck-versus-defender opposition.
  #
  # On accuracy, being exact rather than reassuring:
  #   * the merge itself adds ~1e-16. It reassociates the sum, and floating-point
  #     addition is not associative, so the last bit can move. Measured, not
  #     assumed -- an expect_identical() on it fails and expect_equal() passes.
  #   * PLAYER ratings reconcile exactly: epr = recv + disp + spoil + hitout to
  #     0.000000 over all 721 rated players at 2026 R23.
  #   * TEAM ratings reconcile only to ~0.02, because run_ratings_pipeline.R
  #     round()s each team_epr_* to 2dp independently. That gap is PRE-EXISTING
  #     -- the four-way split carries it too -- and merging does not widen it.
  #
  # Computed here rather than published: the release schema is a consumer
  # contract, and this changes nothing about the ratings.
  #
  # Expect it to look FLAT next to the others. Contest sd is 0.266 against
  # disposal's 1.693 and receiving's 1.134 -- about 2% of EPR's variance. That is
  # the honest size of it, not a plotting problem.
  if (metric == "contest") {
    parts <- c("team_epr_spoil", "team_epr_hitout")
    missing <- setdiff(parts, names(team_ratings))
    if (length(missing) > 0) {
      cli::cli_abort(c(
        "Cannot build the {.val contest} metric: {.field {missing}} absent from team ratings.",
        "i" = "It is the sum of {.field team_epr_spoil} and {.field team_epr_hitout}, so both must be present."
      ))
    }
    team_ratings$team_epr_contest <-
      team_ratings$team_epr_spoil + team_ratings$team_epr_hitout
  }

  # Map metric names: epr -> team_epr, recv -> team_epr_recv, etc.
  col_name <- if (metric == "epr") "team_epr" else paste0("team_epr_", metric)
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
    contest = "Contest EPR",
    # Retained as the two halves of Contest. "Spoil" and "Hitout" are the
    # historical names and they understate what the channels hold, so they are
    # labelled by what they measure rather than by the column name.
    spoil = "Contest EPR (aerial, incl. tackles and pressure)",
    hitout = "Contest EPR (stoppage, incl. ruck contests)",
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
