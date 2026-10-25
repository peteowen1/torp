#' Play type for a Net Points payment
#'
#' Labels one payment row by the role it was paid in and the play-by-play
#' description of the row. These are the columns of "Where Net Points Come
#' From" (`data-raw/04-analysis/build_np_categories_artifact.R`, which reads
#' it through `.np_breakdown()`) and of the published breakdown
#' (`R/player_credit.R`, `data-raw/01-data/daily_release.R`).
#'
#' @param role Payment role from `attr(np, "np_team_margin_payments")`.
#' @param d Play-by-play `description` of the row.
#' @return Character vector of category labels. Aborts on a role this function
#'   does not know, rather than filing it under an own-act label.
#' @keywords internal
.np_play_type <- function(role, d) {
  known_roles <- c("actor", "receiver", "contest_winner", "ball_winner",
                   "error_blame", "pressure_back", "stoppage_ruck",
                   "stoppage_player")
  bad <- setdiff(unique(role), known_roles)
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Unknown Net Points payment role{?s}: {.val {bad}}.",
      "x" = "Add {cli::qty(length(bad))}{?it/them} to {.fn .np_play_type} so the play type is not silently mislabelled."
    ))
  }
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

#' Which game-logs column each Net Points play type adds into
#'
#' Every category the breakdown can carry, mapped to one of four columns of
#' `player_game_data` / `player_game_ratings` (Pete's grouping, 2026-10-25,
#' torp#263). The four sum, per player-match, to `net_points`, and each one is
#' the exact sum of its categories in the published `np_breakdown_<season>`:
#'
#' * `np_disp`: what he did with the ball, including the blame passed back to
#'   him for a turnover he set up.
#' * `np_recv`: getting the ball (receiving, taking, gathering, marking), and
#'   errors charged to him.
#' * `np_contest`: contests, loose balls, balls won off a disposal, stoppages.
#' * `np_team`: the team pool share, the anchor to the real margin, and the
#'   re-spread from a team-mate with no act.
#'
#' Covers every label `.np_play_type()` can return plus the three pool rows
#' (`.np_breakdown()` and the v4 branch of `create_player_game_data()`). A new
#' label must be added here: `.np_family_sums()` aborts on any it does not map.
#' @keywords internal
.NP_PLAY_TYPE_FAMILY <- c(
  "Kick"                                   = "np_disp",
  "Handball"                               = "np_disp",
  "Ground kick"                            = "np_disp",
  "Bounce"                                 = "np_disp",
  "Free kick (own)"                        = "np_disp",
  "Blame passed back (turnover)"           = "np_disp",
  "Other own act"                          = "np_disp",
  "Receiving a kick"                       = "np_recv",
  "Receiving a handball"                   = "np_recv",
  "Taking a handball"                      = "np_recv",
  "Gathering the ball"                     = "np_recv",
  "Marking (uncontested)"                  = "np_recv",
  "Marking (contested)"                    = "np_recv",
  "Charged for an error"                   = "np_recv",
  "Winning a contest (mark/spoil)"         = "np_contest",
  "Winning a loose/ground ball"            = "np_contest",
  "Winning the ball off a disposal"        = "np_contest",
  "Ruck: centre bounce"                    = "np_contest",
  "Ruck: ball-up"                          = "np_contest",
  "Ruck: boundary throw-in"                = "np_contest",
  "First possession: centre bounce"        = "np_contest",
  "First possession: ball-up"              = "np_contest",
  "First possession: boundary throw-in"    = "np_contest",
  "Team pool share"                        = "np_team",
  "Anchor to the real margin"              = "np_team",
  "Re-spread from a team-mate with no act" = "np_team"
)

#' The four Net Points family columns, in publishing order
#' @keywords internal
.NP_FAMILY_COLS <- c("np_disp", "np_recv", "np_contest", "np_team")

#' Sum a Net Points breakdown into its four families
#'
#' @param np_bd A breakdown: `match_id`, `player_id`, `category`, `value`
#'   (`.np_breakdown()`, or the `np_breakdown` attribute of
#'   `create_player_game_data()`).
#' @return data.table, one row per player-match in `np_bd`: `match_id`,
#'   `player_id` (both character) and the four `.NP_FAMILY_COLS`, 0 where he
#'   has no category in that family. Aborts on a category
#'   `.NP_PLAY_TYPE_FAMILY` does not map, rather than leaving its points out.
#' @keywords internal
.np_family_sums <- function(np_bd) {
  fam <- unname(.NP_PLAY_TYPE_FAMILY[as.character(np_bd$category)])
  bad <- unique(as.character(np_bd$category)[is.na(fam)])
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Net Points play type{?s} with no family: {.val {bad}}.",
      "x" = "Add {cli::qty(length(bad))}{?it/them} to {.code .NP_PLAY_TYPE_FAMILY} (R/np_breakdown.R) so np_disp + np_recv + np_contest + np_team still add up to net_points."
    ))
  }
  bd <- data.table::data.table(match_id = as.character(np_bd$match_id),
                               player_id = as.character(np_bd$player_id),
                               family = fam, value = np_bd$value)
  bd[, .(np_disp    = sum(value[family == "np_disp"]),
         np_recv    = sum(value[family == "np_recv"]),
         np_contest = sum(value[family == "np_contest"]),
         np_team    = sum(value[family == "np_team"])),
     by = .(match_id, player_id)]
}

#' Gate: the four family columns on a frame equal the breakdown's family sums
#'
#' For every player-match on either side, each of `np_disp`, `np_recv`,
#' `np_contest`, `np_team` must equal the sum of its categories in `np_bd`, and
#' the four must add up to `net_points`. A player-match missing from one side
#' counts as zeros there, so a dropped or duplicated row is caught even when
#' its categories happen to net to zero.
#'
#' @param frame Data frame with `match_id`, `player_id`, `net_points` and the
#'   four family columns.
#' @param np_bd The breakdown the columns were built from.
#' @param tol Largest allowed gap, in points.
#' @return `frame`, invisibly. Aborts on any gap above `tol`.
#' @keywords internal
.np_family_gate <- function(frame, np_bd, tol = 1e-9) {
  miss <- setdiff(c("match_id", "player_id", "net_points", .NP_FAMILY_COLS), names(frame))
  if (length(miss) > 0L) {
    cli::cli_abort("Net Points family gate: the frame has no {.field {miss}}.")
  }
  want <- .np_family_sums(np_bd)
  got <- data.table::data.table(match_id = as.character(frame$match_id),
                                player_id = as.character(frame$player_id))
  for (v in c("net_points", .NP_FAMILY_COLS)) data.table::set(got, j = v, value = frame[[v]])
  if (anyDuplicated(got, by = c("match_id", "player_id"))) {
    cli::cli_abort("Net Points family gate: the frame has more than one row for a player-match.")
  }
  chk <- merge(got, want, by = c("match_id", "player_id"), all = TRUE, suffixes = c("", ".bd"))
  for (v in names(chk)[-(1:2)]) data.table::set(chk, i = which(is.na(chk[[v]])), j = v, value = 0)
  gaps <- vapply(.NP_FAMILY_COLS, function(v) max(abs(chk[[v]] - chk[[paste0(v, ".bd")]])), numeric(1))
  sum_gap <- max(abs(chk$np_disp + chk$np_recv + chk$np_contest + chk$np_team - chk$net_points))
  if (!all(is.finite(gaps)) || any(gaps > tol)) {
    worst <- names(gaps)[which.max(gaps)]
    cli::cli_abort(c(
      "Net Points family columns do not match the play-type breakdown (worst: {.field {worst}}, {signif(max(gaps), 3)} points).",
      "x" = "Each of np_disp / np_recv / np_contest / np_team must be the exact sum of its breakdown categories."
    ))
  }
  if (!is.finite(sum_gap) || sum_gap > tol) {
    cli::cli_abort(c(
      "np_disp + np_recv + np_contest + np_team miss net_points by up to {signif(sum_gap, 3)} points.",
      "x" = "They would sit beside net_points on the site and not sum to it."
    ))
  }
  invisible(frame)
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
  # as.data.table() deep-copies even a data.table, so copy exactly once and
  # only the narrow tables; pbp is projected to three columns before any copy.
  pay   <- data.table::copy(if (data.table::is.data.table(pay)) pay else data.table::as.data.table(pay))
  parts <- data.table::copy(if (data.table::is.data.table(parts)) parts else data.table::as.data.table(parts))
  pay[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  parts[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  desc <- unique(
    if (data.table::is.data.table(pbp_data)) {
      pbp_data[, .(match_id = as.character(match_id), display_order, description)]
    } else {
      data.table::as.data.table(pbp_data[, c("match_id", "display_order", "description")])[
        , .(match_id = as.character(match_id), display_order, description)]
    })
  if (anyDuplicated(desc, by = c("match_id", "display_order"))) {
    cli::cli_abort("Play-by-play has more than one description for a (match_id, display_order).")
  }
  pay <- merge(pay, desc, by = c("match_id", "display_order"), all.x = TRUE)
  # A payment with no play-by-play description falls into "Other own act". The
  # sum gate below cannot see that (relabelling keeps totals), so say how many.
  unk <- pay[is.na(description), .N]
  if (unk > 0) {
    cli::cli_alert_warning(
      "Net Points breakdown: {unk} of {nrow(pay)} payment rows have no play-by-play description; labelled 'Other own act'.")
  }
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
  if (nrow(chk) == 0L) cli::cli_abort("No player-matches to break down.")
  gap <- max(abs(chk$tot - chk$net_points))
  if (!is.finite(gap) || gap > tol) {
    cli::cli_abort(c(
      "Net Points play types do not add up to net_points (worst gap {signif(gap, 3)} points).",
      "x" = "A payment was dropped, double-counted or labelled into the wrong row."
    ))
  }
  out[]
}
