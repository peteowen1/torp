#' Play type for a Net Points payment
#'
#' Labels one payment row by the role it was paid in and the play-by-play
#' description of the row. These are the columns of "Where Net Points Come
#' From" (`data-raw/04-analysis/build_np_categories_artifact.R`) and of the
#' published breakdown; both read this one function.
#'
#' @param role Payment role from `attr(np, "np_team_margin_payments")`.
#' @param d Play-by-play `description` of the row.
#' @return Character vector of category labels.
#' @keywords internal
.np_play_type <- function(role, d) {
  data.table::fcase(
    role == "actor" & d == "Kick",                    "Kick",
    role == "actor" & d == "Handball",                "Handball",
    role == "actor" & d == "Ground Kick",             "Ground kick",
    role == "actor" & d == "Bounce",                  "Bounce",
    role == "actor" & grepl("^Free For", d),          "Free kick (own)",
    role == "actor" & d == "Handball Received",       "Taking a handball",
    role == "actor" & grepl("Gather", d),             "Gathering the ball",
    role == "actor" & d == "Uncontested Mark",        "Marking (uncontested)",
    role == "actor" & grepl("Contested Mark|Pack Mark", d), "Marking (contested)",
    role == "actor",                                  "Other own act",
    role == "receiver" & d == "Kick",                 "Receiving a kick",
    role == "receiver",                               "Receiving a handball",
    role == "contest_winner",                         "Winning a contest (mark/spoil)",
    role == "ball_winner" & grepl("Loose Ball|Hard Ball|Crumb", d), "Winning a loose/ground ball",
    role == "ball_winner",                            "Winning the ball off a disposal",
    role == "error_blame",                            "Charged for an error",
    # the blame side of a turnover sent back to the teammate who set it up
    role == "pressure_back",                          "Blame passed back (turnover)",
    role == "stoppage_ruck" & d == "Centre Bounce",   "Ruck: centre bounce",
    role == "stoppage_ruck" & d == "Ball Up Call",    "Ruck: ball-up",
    role == "stoppage_ruck",                          "Ruck: boundary throw-in",
    role == "stoppage_player" & d == "Centre Bounce", "First possession: centre bounce",
    role == "stoppage_player" & d == "Ball Up Call",  "First possession: ball-up",
    role == "stoppage_player",                        "First possession: boundary throw-in",
    default = "Other own act")
}

#' Split each player-match's Net Points by play type
#'
#' Every player-match's `net_points` is three parts that add up exactly: the
#' payments made to him by name, his share of the team pool, and his share of
#' the anchor to the real margin (`attr(np, "np_team_margin_parts")`). This
#' splits the named part by play type and role, using the payment rows at
#' their final value (`attr(np, "np_team_margin_payments")`), so the rows for a
#' player-match sum to its `net_points` with nothing left over.
#'
#' @param np Output of `.np_team_margin()`, carrying both attributes.
#' @param pbp_data Play-by-play for the same matches (for `description`).
#' @param tol Largest allowed gap between a player-match's categories and its
#'   `net_points`. The default is rounding level: anything bigger means a
#'   payment was dropped, double-counted or mislabelled.
#' @return data.table with `match_id`, `player_id`, `category`, `value`
#'   (points, positive is good for the player).
#' @keywords internal
.np_breakdown <- function(np, pbp_data, tol = 1e-9) {
  pay   <- attr(np, "np_team_margin_payments")
  parts <- attr(np, "np_team_margin_parts")
  if (is.null(pay) || is.null(parts)) {
    cli::cli_abort(c(
      "The Net Points frame carries no final-value payments.",
      "x" = "The breakdown needs {.fn .np_team_margin}'s output (NP_TEAM_MARGIN_CONVENTION = TRUE)."
    ))
  }
  pay <- data.table::as.data.table(pay)[, `:=`(match_id = as.character(match_id),
                                                player_id = as.character(player_id))]
  parts <- data.table::as.data.table(parts)[, `:=`(match_id = as.character(match_id),
                                                    player_id = as.character(player_id))]
  desc <- unique(data.table::as.data.table(pbp_data)[
    , .(match_id = as.character(match_id), display_order, description)])
  if (anyDuplicated(desc, by = c("match_id", "display_order"))) {
    cli::cli_abort("Play-by-play has more than one description for a (match_id, display_order).")
  }
  pay <- merge(pay, desc, by = c("match_id", "display_order"), all.x = TRUE)
  pay[, category := .np_play_type(role, description)]
  named <- pay[, .(value = sum(paid)), by = .(match_id, player_id, category)]
  pools <- rbind(parts[, .(match_id, player_id, category = "Team pool share", value = share)],
                 parts[, .(match_id, player_id, category = "Anchor to the real margin", value = recon)])
  out <- rbind(named, pools)[value != 0]

  # the gate: categories rebuild net_points exactly, for every player-match
  chk <- merge(out[, .(tot = sum(value)), by = .(match_id, player_id)],
               data.table::as.data.table(np)[, .(match_id = as.character(match_id),
                                                 player_id = as.character(player_id), net_points)],
               by = c("match_id", "player_id"), all = TRUE)
  chk[is.na(tot), tot := 0][is.na(net_points), net_points := 0]
  gap <- max(abs(chk$tot - chk$net_points))
  if (!is.finite(gap) || gap > tol) {
    cli::cli_abort(c(
      "Net Points play types do not add up to net_points (worst gap {signif(gap, 3)} points).",
      "x" = "A payment was dropped, double-counted or labelled into the wrong row."
    ))
  }
  out[]
}
