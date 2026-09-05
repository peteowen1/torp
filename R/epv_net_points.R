# EPV as a Net Points ledger: allocate the actual margin, do not fit a scale.
# ===========================================================================
# Design and measurements: ../docs/plans/EPV-NET-POINTS.md
#
# THE FRAME. Everything internal to this file works in the HOME-MARGIN frame: a
# player's value is his contribution to (home score - away score). For a home
# player positive is good; for an away player negative is good. The identity
# being enforced is therefore a single sum over all 44 players:
#
#     sum(net_points_home_margin_frame) == home_score - away_score
#
# which is Oliver's Net Points identity stated for AFL. Only at the very end is
# the sign flipped for away players so that "good" reads positive for everyone.
#
# WHY THE FRAME MATTERS, because getting it wrong looks like a conservation bug
# and is actually a sign error. A turnover is not paid twice. In the home-margin
# frame the loser's failure and the winner's success point the SAME direction --
# an away turnover pushes the margin toward home whether you describe it as the
# away team failing or the home team succeeding -- so the two SHARE one quantity
# rather than each receiving it. That is exactly why a cross-team transfer here
# preserves the total, where in a naive "each team's own frame" ledger it would
# not.
#
# WHAT THIS FILE DELIBERATELY DOES NOT DO. No centring, no standardisation, no
# TOG scaling of the output, no opponent adjustment. Each of those compares a
# player against an expectation, and the moment an expectation is subtracted the
# team totals stop summing to the margin. They belong at the EPR layer, whose
# job is prediction and which has no conservation requirement. This split is the
# whole point: EPV calibrates to what happened, EPR to what will happen.

#' Is this row's value phantom rather than football?
#'
#' @param desc Character vector of PBP descriptions.
#' @return Logical vector, TRUE where the row must not be allocated.
#' @keywords internal
.np_is_excluded <- function(desc) {
  !is.na(desc) & desc %in% NP_EXCLUDED_DESCS
}

#' Look up the mirror position, warning once on anything unmapped
#'
#' Unknown slots mirror to themselves rather than dropping out: a slot with no
#' mirror must still receive its pool, or the allocation silently leaks.
#'
#' @param pos Character vector of lineup positions.
#' @return Character vector of mirror positions, same length.
#' @keywords internal
.np_mirror_of <- function(pos) {
  out <- unname(NP_POSITION_MIRROR[pos])
  unmapped <- is.na(out) & !is.na(pos)
  if (any(unmapped)) {
    bad <- sort(unique(pos[unmapped]))
    cli::cli_warn(c(
      "{length(bad)} lineup position{?s} have no mirror and will mirror to themselves: {.val {bad}}",
      "i" = "Add them to {.var NP_POSITION_MIRROR} if they are real slots."
    ))
    out[unmapped] <- pos[unmapped]
  }
  out
}

#' Build the per-act ledger in the home-margin frame
#'
#' @param pbp_data Play-by-play carrying `delta_epv`, `home_away`, `team`,
#'   `player_id`, `description`, `match_id`, `display_order`.
#' @return A data.table of ledger rows with `hm` (home-margin-frame value) plus
#'   the next row's team and player, used to detect turnovers.
#' @keywords internal
.np_build_ledger <- function(pbp_data) {
  d <- data.table::as.data.table(pbp_data)
  need <- c("match_id", "display_order", "delta_epv", "home_away", "team",
            "player_id", "description")
  missing <- setdiff(need, names(d))
  if (length(missing)) {
    cli::cli_abort(c(
      "Play-by-play is missing {length(missing)} column{?s} the ledger needs: {.val {missing}}",
      "i" = "The net-points ledger reads raw PBP, not a derived credit frame."
    ))
  }

  # Adjacency FIRST, on the unfiltered sequence -- see .np_adjacency(). Doing
  # this after the filters below is what made half of all goals read as
  # turnovers.
  adj <- .np_adjacency(d)

  # EVERY filter below reports what it removed, and the four counts must add up
  # to n_all. They did not before 2026-09-05: the NA-delta_epv drop was silent,
  # and the team/player/orientation message was computed across the exclusion
  # filter as well, so it double-labelled the centre-bounce rows under a second,
  # wrong reason. Two log lines that do not reconcile to the input are worse than
  # one, because they read as if they do.
  n_all <- nrow(d)
  d <- d[!is.na(delta_epv)]
  n_val <- nrow(d)
  if (n_all > n_val) {
    cli::cli_alert_info(
      "Net points: {format(n_all - n_val, big.mark = ',')} PBP row{?s} ({round(100 * (n_all - n_val) / n_all, 1)}%) have no {.field delta_epv} and carry no value to allocate")
  }

  # The centre-bounce artifact is +4,461 points in 2026 and was previously
  # dropped only as a side effect of requiring a non-NA team.
  excl <- d[.np_is_excluded(description)]
  if (nrow(excl)) {
    cli::cli_alert_info(
      "Net points: excluding {format(nrow(excl), big.mark = ',')} phantom row{?s} worth {round(sum(excl$delta_epv), 1)} points ({paste(NP_EXCLUDED_DESCS, collapse = ', ')})")
  }
  d <- d[!.np_is_excluded(description)]
  n_post_excl <- nrow(d)

  d <- d[!is.na(team) & !is.na(player_id) & !is.na(home_away)]
  n_keep <- nrow(d)
  cli::cli_alert_info(
    "Net points ledger: {format(n_keep, big.mark = ',')} of {format(n_all, big.mark = ',')} PBP rows ({round(100 * n_keep / n_all, 1)}%); {format(n_post_excl - n_keep, big.mark = ',')} dropped for a missing team, player or orientation")
  stopifnot((n_all - n_val) + nrow(excl) + (n_post_excl - n_keep) + n_keep == n_all)
  if (n_keep == 0) {
    cli::cli_abort("Net points ledger is empty after filtering -- nothing to allocate.")
  }

  data.table::setorder(d, match_id, display_order)
  # Home-margin frame: an away act's value flips sign, because a good away act
  # pushes the margin down.
  d[, hm := delta_epv * data.table::fifelse(home_away == "Home", 1, -1)]
  d[adj, on = .(match_id, display_order),
    `:=`(next_team = i.next_team, next_player = i.next_player)]
  d[, .(match_id, display_order, description, team, home_away, player_id,
        hm, next_team, next_player)]
}

#' Who genuinely acted next, computed on the UNFILTERED sequence
#'
#' \strong{Adjacency and value-exclusion are independent operations, and running
#' them in the wrong order silently rewrites who did what.} The first version of
#' this module filtered rows out and then took `shift(-1L)` on what remained, so
#' any dropped row was stepped over and the "next" act became whatever happened
#' to follow the gap. Measured on 2026 before the fix:
#'
#' \itemize{
#'   \item 15,556 disposals (10.2%) had the wrong next team;
#'   \item 7,847 of 40,785 detected turnovers (19.2%) were not turnovers at all
#'     but restarts -- out of bounds, ball-ups, centre bounces;
#'   \item \strong{2,586 goals were classified as turnovers} -- half of the 5,143
#'     kicks followed by a centre bounce. A goal was firing the defensive pool
#'     and paying the opposition for conceding it.
#' }
#'
#' None of that breaks conservation, which is exactly why it survived a suite
#' whose every assertion was about the total. It is a pure attribution error.
#'
#' \strong{A restart is chain-terminal, not a turnover.} Possession legitimately
#' ended; nobody took the ball off anyone. Centre bounces, ball-ups and
#' out-of-bounds all carry no `team`, so a missing team on the following row is
#' the test for "there is no next actor" -- such rows get `NA` and are therefore
#' neither retained disposals nor turnovers.
#'
#' @param pbp_data The full play-by-play, before any filtering.
#' @return A data.table of `match_id`, `display_order`, `next_team`,
#'   `next_player`.
#' @keywords internal
.np_adjacency <- function(pbp_data) {
  a <- data.table::as.data.table(pbp_data)[, .(match_id, display_order, team,
                                               player_id, description)]
  data.table::setorder(a, match_id, display_order)
  a[, `:=`(nt = data.table::shift(team, -1L),
           npl = data.table::shift(player_id, -1L),
           nd = data.table::shift(description, -1L)), by = match_id]
  # Chain-terminal: the next event has no acting team (a restart), or is one we
  # exclude as phantom. Either way there is no next actor to credit.
  a[, terminal := is.na(nt) | .np_is_excluded(nd)]
  a[, .(match_id, display_order,
        next_team = data.table::fifelse(terminal, NA_character_, nt),
        next_player = data.table::fifelse(terminal, NA_character_, npl))]
}

#' Move a share of each retained disposal from the disposer to the receiver
#'
#' Conservation is untouched by construction: value moves between two players in
#' the same team-match, so no team total changes.
#'
#' @param led Ledger from `.np_build_ledger()`.
#' @param alpha Receiver's share in [0, 1].
#' @return A data.table of `match_id`, `team`, `player_id`, `np_direct`.
#' @keywords internal
.np_direct_credit <- function(led, alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg receiver_share} must be one number in [0, 1], not {.val {alpha}}.")
  }
  l <- data.table::copy(led)
  l[, retained := description %in% NP_DISPOSAL_DESCS &
      !is.na(next_team) & next_team == team & !is.na(next_player)]

  actor <- l[, .(match_id, team, player_id,
                 v = data.table::fifelse(retained, hm * (1 - alpha), hm))]
  recv <- l[retained == TRUE,
            .(match_id, team, player_id = next_player, v = hm * alpha)]
  out <- data.table::rbindlist(list(actor, recv))[
    , .(np_direct = sum(v)), by = .(match_id, team, player_id)]

  moved <- l[retained == TRUE, sum(abs(hm)) * alpha]
  cli::cli_alert_info(
    "Receiver split: {round(100 * alpha)}% of {format(l[retained == TRUE, .N], big.mark = ',')} retained disposals reallocated ({round(moved, 1)} points of |value|)")
  out
}

#' Take the defensive share off each turnover and pool it by opposing position
#'
#' The pool is keyed on the DISPOSER's position, because that is what the mirror
#' map is indexed by: the player who lost the ball tells us which opposing slot
#' was most likely responsible for winning it.
#'
#' @param led Ledger from `.np_build_ledger()`.
#' @param lineup Per-match roster: `match_id`, `team`, `player_id`, `position`,
#'   `tog`, `def_acts`.
#' @param phi Defensive share in [0, 1].
#' @param psi Share of the pool paid straight to the OBSERVED ball-winner (the
#'   actor on the next row). The remainder is spread by `.np_spread_pool()`.
#' @return A list of `debits` (per disposer, negative of what they keep),
#'   `won` (paid directly to identified ball-winners) and `pool` (the remainder,
#'   per `match_id`, `def_team`, `winner_slot`).
#' @keywords internal
.np_defensive_pool <- function(led, lineup, phi, psi = 0) {
  if (!is.numeric(phi) || length(phi) != 1 || is.na(phi) || phi < 0 || phi > 1) {
    cli::cli_abort("{.arg defensive_share} must be one number in [0, 1], not {.val {phi}}.")
  }
  to <- led[description %in% NP_DISPOSAL_DESCS &
              !is.na(next_team) & next_team != team]
  if (!is.numeric(psi) || length(psi) != 1 || is.na(psi) || psi < 0 || psi > 1) {
    cli::cli_abort("{.arg ball_winner_share} must be one number in [0, 1], not {.val {psi}}.")
  }
  if (nrow(to) == 0) {
    cli::cli_warn("No turnovers found -- the defensive pool is empty.")
    return(list(debits = NULL, won = NULL, pool = NULL))
  }

  pos <- lineup[, .(match_id, team, player_id, position)]
  to <- merge(to, pos, by = c("match_id", "team", "player_id"), all.x = TRUE)
  # A disposer with no lineup row cannot be mirrored. Spread his pool flatly by
  # sending it to the catch-all slot rather than dropping it, and SAY how much.
  unmapped <- to[is.na(position)]
  if (nrow(unmapped)) {
    cli::cli_warn(c(
      "{format(nrow(unmapped), big.mark = ',')} turnover{?s} ({round(100 * nrow(unmapped) / nrow(to), 1)}%) have no lineup position for the disposer.",
      "i" = "Their pool is spread flatly across the opposing team instead of by matchup."
    ))
    to[is.na(position), position := NA_character_]
  }

  to[, winner_slot := data.table::fifelse(is.na(position), NA_character_,
                                          .np_mirror_of(position))]
  debits <- to[, .(np_ceded = -sum(hm) * phi),
               by = .(match_id, team, player_id)]

  # SPLIT THE POOL. `psi` goes to the player we can SEE won the ball -- the
  # actor on the next row -- and the rest is spread for the pressure that forced
  # the turnover, which is usually not the same man.
  #
  # This split exists because routing the whole pool by positional mirror was
  # measurably wrong. Defenders win 47.3% of turnovers and lose 22.8%, so they
  # are net ball-winners; but the mirror of a midfielder who coughs it up is
  # another midfielder, so the credit defenders earned was paid to midfielders.
  # Under mirror-only routing, raising phi from 0 to 0.9 moved defenders from
  # -0.84 to -2.19 points per game and midfielders from -0.20 to +2.55 -- the
  # opposite of what a defensive-credit mechanism is for.
  to[, has_winner := !is.na(next_player)]
  won <- to[has_winner == TRUE,
            .(np_defensive_won = sum(hm) * phi * psi),
            by = .(match_id, team = next_team, player_id = next_player)]
  # Any pool with no identifiable winner keeps its full value for the spread,
  # rather than quietly losing `psi` of it.
  to[, spread_hm := hm * phi * data.table::fifelse(has_winner, 1 - psi, 1)]
  pool <- to[, .(pool_hm = sum(spread_hm)),
             by = .(match_id, def_team = next_team, winner_slot)]

  # Report the GROSS moved, not the net. The net is near zero by construction --
  # home and away turnover debits carry opposite signs in the home-margin frame
  # and cancel across the season -- so logging `sum(pool_hm)` reads as "almost
  # nothing happened" when in fact tens of thousands of points changed hands.
  cli::cli_alert_info(
    "Defensive pool: {round(100 * phi)}% of {format(nrow(to), big.mark = ',')} turnovers = {round(sum(abs(to$hm)) * phi, 1)} points gross; {round(100 * psi)}% to the observed ball-winner ({round(100 * mean(to$has_winner), 1)}% identified), rest spread")
  list(debits = debits, won = won, pool = pool)
}

#' Spread each pool across the winning team's on-field players
#'
#' Conservation holds for ANY non-negative weighting -- the weights are
#' normalised within each pool group -- so the choice of `spread` changes who is
#' paid and never whether the ledger balances. That is what makes it safe to
#' swap the rule later without re-proving the identity.
#'
#' @param pool From `.np_defensive_pool()`.
#' @param lineup Per-match roster with `position`, `tog`, `def_acts`.
#' @param spread One of "matchup", "defensive_acts", "tog".
#' @param mirror_share Share the mirror slot takes under "matchup".
#' @return A data.table of `match_id`, `team`, `player_id`, `np_defensive`.
#' @keywords internal
.np_spread_pool <- function(pool, lineup, spread, mirror_share) {
  if (is.null(pool) || nrow(pool) == 0) return(NULL)

  a <- merge(pool, lineup, by.x = c("match_id", "def_team"),
             by.y = c("match_id", "team"), allow.cartesian = TRUE)
  if (nrow(a) == 0) {
    cli::cli_abort(c(
      "No lineup rows matched the defensive pool -- every point would be lost.",
      "i" = "Check that {.arg player_stats} covers the same matches as {.arg pbp_data}."
    ))
  }

  a[, w := switch(spread,
    tog = tog,
    defensive_acts = def_acts,
    matchup = 0,  # filled below
    cli::cli_abort("Unknown {.arg spread}: {.val {spread}}")
  )]

  if (identical(spread, "matchup")) {
    # The mirror slot takes `mirror_share` of the pool; the rest of the team
    # shares the remainder by time on ground. A pool whose winner_slot is
    # unknown (disposer had no lineup row) falls through to a flat TOG spread,
    # which is what the warning in .np_defensive_pool() promised.
    a[, is_mirror := !is.na(winner_slot) & position == winner_slot]
    a[, n_mirror := sum(is_mirror), by = .(match_id, def_team, winner_slot)]
    a[, tog_mirror := sum(tog * is_mirror), by = .(match_id, def_team, winner_slot)]
    a[, tog_other := sum(tog * !is_mirror), by = .(match_id, def_team, winner_slot)]
    a[, w := data.table::fcase(
      n_mirror == 0,  tog,                                    # no mirror on park
      is_mirror,      mirror_share * tog / pmax(tog_mirror, 1e-9),
      default =       (1 - mirror_share) * tog / pmax(tog_other, 1e-9)
    )]
  }

  a[!is.finite(w) | w < 0, w := 0]
  a[, wsum := sum(w), by = .(match_id, def_team, winner_slot)]
  # A group with no weight anywhere still has to be paid: fall back to flat --
  # and SAY SO. Silently degrading a targeted spread rule to flat is exactly the
  # failure this module logs everywhere else. Under `defensive_acts` this fires
  # when a whole team-match recorded no tackles/pressure/spoils/intercepts, which
  # in practice means their player_stats rows failed to join.
  flat_groups <- unique(a[wsum <= 0, .(match_id, def_team, winner_slot)])
  if (nrow(flat_groups)) {
    flat_pts <- sum(abs(unique(a[wsum <= 0, .(match_id, def_team, winner_slot, pool_hm)])$pool_hm))
    cli::cli_warn(c(
      "{nrow(flat_groups)} pool group{?s} had zero weight under {.val {spread}} and fell back to a FLAT spread ({round(flat_pts, 1)} points).",
      "i" = "Check that {.arg player_stats} joined for those teams."
    ))
  }
  a[wsum <= 0, w := 1]
  a[, wsum := sum(w), by = .(match_id, def_team, winner_slot)]
  a[, alloc := pool_hm * w / wsum]

  out <- a[, .(np_defensive = sum(alloc)), by = .(match_id, team = def_team, player_id)]

  owed <- sum(pool$pool_hm)
  got <- sum(out$np_defensive)
  if (abs(got - owed) > max(1e-6, 1e-9 * abs(owed))) {
    cli::cli_abort(c(
      "Defensive pool did not conserve: owed {round(owed, 4)}, allocated {round(got, 4)}.",
      "x" = "A spread rule may move value between players; it may never change the total."
    ))
  }
  out
}

#' Force each match to sum exactly to its margin
#'
#' After allocation the ledger is close but not exact (median 4.9 points in
#' 2026). Oliver's system is exact by construction, so the remainder is booked
#' explicitly: half to each team, spread by time on ground, in the home-margin
#' frame. A positive residual means the home team outperformed what the ledger
#' explained, so home players are credited and away players debited -- which is
#' the right direction and is why the split is signed rather than absolute.
#'
#' This term is where home-ground advantage and model error land. It is
#' deliberately flat so it cannot reorder players within a team.
#'
#' @section Two constraints, not one:
#' Pinning only the per-match SUM leaves the LEVEL free, and the raw ledger's
#' level is badly behaved: it measures each team's absolute scoring performance
#' against expectation, so in a high-scoring game both teams can be strongly
#' positive at once. Measured 2026, one match had home +95.5 and away +32.5 in
#' their own frames -- a correct 63-point difference sitting on a meaningless
#' level. `level = "half_margin"` adds the second constraint, turning an
#' absolute-performance ledger into a margin ledger:
#'
#'     home total = +margin/2       away total = -margin/2
#'
#' Two constraints, two team totals, fully determined. The correction is flat
#' within a team (TOG-weighted), so it shifts the level without reordering
#' anyone; what it cannot do is hide that the underlying level was off, which
#' is why `np_residual` is reported as its own column rather than folded in.
#'
#' @param np Per-player frame already carrying `np_raw` (home-margin frame),
#'   `margin`, `team`, `home_away` and `tog`.
#' @param level `"half_margin"` pins both team totals; `"sum"` pins only the
#'   match total and lets the level float.
#' @return `np` with an `np_residual` column added, by reference.
#' @keywords internal
.np_reconcile <- function(np, level = c("half_margin", "sum")) {
  level <- match.arg(level)
  need <- c("np_raw", "margin", "team", "home_away", "tog", "match_id")
  missing <- setdiff(need, names(np))
  if (length(missing)) {
    cli::cli_abort("{.fn .np_reconcile} needs {.val {missing}}.")
  }
  np[, .tog_side := sum(tog), by = .(match_id, team)]
  # BOTH targets are +margin/2 because this is the home-margin frame. An away
  # player who pushes the margin toward home scores POSITIVE here; the sign flip
  # to his own frame happens once, at the end of build_net_points(). Targeting
  # -margin/2 for the away side is the natural-looking mistake and it makes the
  # match total sum to zero instead of the margin.
  np[, .target := margin / 2]
  if (identical(level, "half_margin")) {
    np[, .got := sum(np_raw), by = .(match_id, team)]
  } else {
    # Pin only the match total; split what is left over evenly so neither side
    # absorbs all of it, and let the level float.
    np[, .got := sum(np_raw) / 2, by = match_id]
  }
  np[, np_residual := (.target - .got) * tog / pmax(.tog_side, 1e-9)]
  np[, c(".got", ".target", ".tog_side") := NULL]
  np
}

#' Allocate the actual match margin across players (Net Points)
#'
#' Rebuilds EPV as a conservation ledger rather than a fitted score: the margin
#' is the fixed total and every point of it is credited or debited to a player.
#' Summed per match, `net_points` differs between the two teams by exactly the
#' final margin.
#'
#' This is Dean Oliver's Net Points model stated for AFL. It replaces nothing in
#' the published pipeline -- `get_player_game_ratings()` and EPR are untouched --
#' and exists so the allocation can be measured before anything is switched over.
#'
#' @section Do not add a home player's value to an away player's:
#' `net_points` and every component are reported in each player's OWN team
#' frame, so positive always means "helped my team". That frame is not shared
#' between the two teams, so summing across them is meaningless -- the identity
#' is a DIFFERENCE, `sum(home) - sum(away) == margin`. Use `net_points_hm` if
#' you need a single additive frame. The visible symptom of getting this wrong
#' is that both sides of a zero-sum transfer read positive: a turnover shows up
#' as a positive `np_ceded` for the disposer and a positive `np_defensive` for
#' the opponent, which is correct in own frames and looks like double-counting.
#'
#' @section What this does not contain:
#' No centring, standardisation, time-on-ground scaling or opponent adjustment.
#' Each compares a player against an expectation, and subtracting an expectation
#' breaks the identity this function exists to hold. They belong at the EPR
#' layer. See `docs/plans/EPV-NET-POINTS.md` §3 A4.
#'
#' @param pbp_data Play-by-play. Defaults to `load_pbp(TRUE)`.
#' @param player_stats Box-score stats supplying the lineup, positions, time on
#'   ground and defensive acts. Defaults to `load_player_stats(TRUE)`.
#' @param results Match results supplying the margin. Defaults to
#'   `load_results(TRUE)`.
#' @param defensive_share Fraction of each turnover paid to the team that won
#'   the ball. Not identifiable from conservation -- see `NP_DEFENSIVE_SHARE`.
#' @param receiver_share Fraction of a retained disposal paid to the receiver.
#' @param ball_winner_share Fraction of each defensive pool paid straight to the
#'   player observed to win the ball (the actor on the next row). The remainder
#'   goes through `spread`, for the pressure that forced the turnover. Routing
#'   the whole pool through `spread` was measurably wrong -- see
#'   `NP_BALL_WINNER_SHARE`.
#' @param spread How the defensive pool is divided: `"matchup"` (the positional
#'   mirror takes `mirror_share`, rest by TOG), `"defensive_acts"` (by box-score
#'   defensive work) or `"tog"` (flat by time on ground).
#' @param mirror_share Share the mirror slot takes when `spread = "matchup"`.
#' @param level `"half_margin"` pins each team's total to half the margin, which
#'   is the Oliver shape; `"sum"` pins only the match total and leaves the level
#'   as the raw ledger produced it. See `.np_reconcile()` for why the level
#'   needs its own constraint.
#' @param reconcile Whether to book the residual so each match sums exactly to
#'   its margin. `FALSE` leaves the raw allocation, which is what you want when
#'   measuring how close the ledger gets on its own.
#'
#' @return A data.table with one row per player-match **that had at least one
#'   allocatable act**. A rostered player whose every touch fell into
#'   `NP_EXCLUDED_DESCS` or a missing-field filter is absent rather than present
#'   at zero; the count is logged. This does not affect conservation (a zero row
#'   contributes nothing either way) but a consumer expecting the full team sheet
#'   must join back to the lineup itself. Columns:
#'   \describe{
#'     \item{`np_direct`}{value from the player's own acts}
#'     \item{`np_defensive_won`}{paid for turnovers he was observed to win}
#'     \item{`np_defensive`}{his share of the pressure pools his team won}
#'     \item{`np_ceded`}{the part of his own turnover debit that was paid to the
#'       opposition instead of to him. Read the SIGN carefully: this is a
#'       positive number that REDUCES his debit, because the debit itself is
#'       already in `np_direct` at full size and this cancels `defensive_share`
#'       of it. It is not a penalty column.}
#'     \item{`np_residual`}{his share of the unexplained margin}
#'     \item{`net_points`}{the total, in the player's OWN team frame, so
#'       positive is always good}
#'   }
#'   Team totals differ by the match margin.
#'
#' @examples
#' \dontrun{
#' np <- build_net_points()
#' # the identity, per match:
#' np[, .(diff = sum(net_points * ifelse(home_away == "Home", 1, -1))), by = match_id]
#' }
#' @export
build_net_points <- function(pbp_data = NULL,
                             player_stats = NULL,
                             results = NULL,
                             defensive_share = NP_DEFENSIVE_SHARE,
                             receiver_share = NP_RECEIVER_SHARE,
                             ball_winner_share = NP_BALL_WINNER_SHARE,
                             spread = c("matchup", "defensive_acts", "tog"),
                             mirror_share = NP_MIRROR_SHARE,
                             level = c("half_margin", "sum"),
                             reconcile = TRUE) {
  spread <- match.arg(spread)
  level <- match.arg(level)
  if (is.null(pbp_data)) pbp_data <- load_pbp(TRUE)
  if (is.null(player_stats)) player_stats <- load_player_stats(TRUE)
  if (is.null(results)) results <- load_results(TRUE)

  led <- .np_build_ledger(pbp_data)

  # --- lineup: roster, positions, TOG and defensive box-score work ----------
  ps <- data.table::as.data.table(player_stats)
  need <- c("match_id", "player_id", "position", "time_on_ground_percentage")
  missing <- setdiff(need, names(ps))
  if (length(missing)) {
    cli::cli_abort("{.arg player_stats} is missing {.val {missing}}.")
  }
  def_cols <- intersect(c("tackles", "pressure_acts", "spoils", "intercepts",
                          "one_percenters"), names(ps))
  if (length(def_cols) == 0) {
    cli::cli_abort(c(
      "{.arg player_stats} has none of the defensive columns the spread needs.",
      "i" = "Expected some of: tackles, pressure_acts, spoils, intercepts, one_percenters."
    ))
  }
  ps[, .def_acts := rowSums(as.matrix(.SD), na.rm = TRUE), .SDcols = def_cols]

  # `team` on the ledger is a team NAME; player_stats carries a team_id. Join
  # through the ledger's own roster so the two vocabularies never have to agree.
  roster <- unique(led[, .(match_id, team, player_id)])
  lineup <- merge(
    roster,
    ps[, .(match_id, player_id, position,
           tog = pmax(time_on_ground_percentage / 100, 0.01),
           def_acts = .def_acts)],
    by = c("match_id", "player_id"), all.x = TRUE)

  # COVERAGE, not presence: a position column that is present and 100% NA would
  # otherwise turn the matchup spread into a silent flat spread.
  cov_pos <- mean(!is.na(lineup$position))
  cov_tog <- mean(!is.na(lineup$tog))
  cov_def <- mean(!is.na(lineup$def_acts) & lineup$def_acts > 0)
  cli::cli_alert_info(
    "Lineup coverage: position {round(100 * cov_pos, 1)}%, TOG {round(100 * cov_tog, 1)}%, defensive acts {round(100 * cov_def, 1)}% over {format(nrow(lineup), big.mark = ',')} player-matches")
  if (identical(spread, "matchup") && cov_pos < 0.5) {
    cli::cli_abort(c(
      "Only {round(100 * cov_pos, 1)}% of players have a lineup position.",
      "x" = "{.arg spread = \"matchup\"} would degrade to a flat spread without saying so."
    ))
  }
  # TOG is load-bearing on EVERY path, not just one spread rule: the "tog" mode,
  # the flat-fallback branch of "matchup", and .np_reconcile()'s residual split
  # (which runs whenever reconcile = TRUE, the default). So it needs an
  # unconditional floor -- a failed join would otherwise impute a flat 0.75 for
  # everyone and quietly degrade all three to "flat" with only an FYI percentage
  # in the log.
  if (cov_tog < 0.5) {
    cli::cli_abort(c(
      "Only {round(100 * cov_tog, 1)}% of players have a time on ground.",
      "x" = "TOG drives the spread AND the reconciliation split; imputing 0.75 for the rest would flatten both silently."
    ))
  }
  if (identical(spread, "defensive_acts") && cov_def < 0.5) {
    cli::cli_abort(c(
      "Only {round(100 * cov_def, 1)}% of players have any defensive box-score acts.",
      "x" = "{.arg spread = \"defensive_acts\"} would fall through to its flat fallback for most groups."
    ))
  }
  lineup[is.na(tog), tog := 0.75]
  lineup[is.na(def_acts), def_acts := 0]

  # --- allocate -------------------------------------------------------------
  direct <- .np_direct_credit(led, receiver_share)
  dp <- .np_defensive_pool(led, lineup, defensive_share, ball_winner_share)
  alloc <- .np_spread_pool(dp$pool, lineup, spread, mirror_share)

  # dp$debits needs the same NULL check as alloc and dp$won -- all three come
  # from the same "no turnovers" return, but merge(x, NULL, by = ...) errors
  # rather than behaving like an empty join, so the warn-and-continue branch
  # crashed with a low-level merge message instead of its own diagnostics.
  np <- if (is.null(dp$debits)) {
    data.table::copy(direct)[, np_ceded := 0]
  } else {
    merge(direct, dp$debits, by = c("match_id", "team", "player_id"), all = TRUE)
  }
  if (!is.null(alloc)) {
    np <- merge(np, alloc, by = c("match_id", "team", "player_id"), all = TRUE)
  } else {
    np[, np_defensive := 0]
  }
  if (!is.null(dp$won)) {
    np <- merge(np, dp$won, by = c("match_id", "team", "player_id"), all = TRUE)
  } else {
    np[, np_defensive_won := 0]
  }
  for (v in c("np_direct", "np_ceded", "np_defensive", "np_defensive_won")) {
    data.table::set(np, i = which(is.na(np[[v]])), j = v, value = 0)
  }
  np[, np_raw := np_direct + np_ceded + np_defensive + np_defensive_won]

  np <- merge(np, lineup[, .(match_id, player_id, tog)],
              by = c("match_id", "player_id"), all.x = TRUE)
  np[is.na(tog), tog := 0.75]

  # `np` is built from the ledger, so a rostered player with no surviving PBP row
  # is ABSENT rather than present at zero. Report it rather than let the output
  # quietly carry fewer players than the team sheet -- see the @return note.
  idle <- nrow(lineup) - nrow(unique(np[, .(match_id, player_id)]))
  if (idle > 0) {
    cli::cli_alert_info(
      "{format(idle, big.mark = ',')} rostered player-match{?es} had no allocatable act and are absent from the output (not zero rows)")
  }

  # --- reconcile to the exact margin ---------------------------------------
  res <- data.table::as.data.table(results)
  res <- res[!is.na(home_score) & !is.na(away_score)]
  margins <- res[, .(match_id = as.character(match_id),
                     margin = home_score - away_score,
                     home_team_name, away_team_name)]
  # An INNER join, so any match absent from `results` -- unplayed, incomplete,
  # or simply a match_id that does not match in type or format -- takes every
  # player in it out of the output. Nothing downstream can see this: the
  # component-sum check only looks at rows that survived, so filtered-to-filtered
  # always balances. Account for it here or it is invisible.
  before_matches <- data.table::uniqueN(np$match_id)
  before_rows <- nrow(np)
  np <- merge(np, margins, by = "match_id")
  lost_matches <- before_matches - data.table::uniqueN(np$match_id)
  if (lost_matches > 0) {
    cli::cli_warn(c(
      "{lost_matches} of {before_matches} match{?es} ({format(before_rows - nrow(np), big.mark = ',')} player-rows) are NOT in {.arg results} and have been dropped.",
      "i" = "Usually an unfinished match or a {.field match_id} type/format mismatch. Their allocated value is discarded."
    ))
  }
  if (nrow(np) == 0) {
    cli::cli_abort(c(
      "No ledger match survived the join to {.arg results}.",
      "x" = "Every allocated point would be silently discarded."
    ))
  }
  np[, home_away := data.table::fifelse(team == home_team_name, "Home", "Away")]

  if (isTRUE(reconcile)) {
    .np_reconcile(np, level)
  } else {
    np[, np_residual := 0]
  }

  np[, net_points_hm := np_raw + np_residual]
  # Flip to each player's OWN frame so positive is good for everyone -- and flip
  # every component by the same sign, or the parts stop summing to the whole and
  # a per-position summary of `np_defensive` averages to ~0 because home and away
  # defenders are being reported in opposite frames.
  np[, .sgn := data.table::fifelse(home_away == "Home", 1, -1)]
  parts <- c("np_direct", "np_defensive", "np_defensive_won", "np_ceded",
             "np_residual")
  for (v in parts) {
    data.table::set(np, j = v, value = np[[v]] * np$.sgn)
  }
  np[, net_points := net_points_hm * .sgn]

  out <- np[, .(match_id, team, player_id, home_away,
                np_direct, np_defensive_won, np_defensive, np_ceded,
                np_residual, net_points_hm, net_points, margin)]
  # The parts must sum to the whole, in whichever frame they are read.
  gap <- max(abs(rowSums(as.matrix(out[, ..parts])) - out$net_points))
  if (gap > 1e-8) {
    cli::cli_abort("Net points components do not sum to the total (max gap {signif(gap, 3)}).")
  }
  data.table::setattr(out, "np_params",
                      list(defensive_share = defensive_share,
                           receiver_share = receiver_share,
                           ball_winner_share = ball_winner_share,
                           spread = spread, mirror_share = mirror_share,
                           level = level, reconciled = isTRUE(reconcile)))
  out[]
}

#' Check that a net-points frame sums to the margin in every match
#'
#' The one property this system can assert exactly, so it is worth asserting
#' rather than eyeballing. Note what it CANNOT see: conservation holds no matter
#' which players were paid, so a green result says the ledger balances and says
#' nothing about whether the right people were credited.
#'
#' @param np Output of `build_net_points()`.
#' @param tol Absolute tolerance in points.
#' @return Invisibly, a data.table of per-match sums. Aborts on a violation.
#' @export
check_net_points_conservation <- function(np, tol = 1e-6) {
  x <- data.table::as.data.table(np)
  # THREE WAYS THIS CHECK USED TO PASS ON EXACTLY WHAT IT EXISTS TO CATCH, all
  # found by review on 2026-09-05 and all reproduced before being fixed:
  #
  #   1. Empty input. `bad` has 0 rows, so the abort never fires and the success
  #      banner reads "conserves in all 0 matches (max error -Inf points)",
  #      because max(numeric(0)) is -Inf rather than an error.
  #   2. An NA margin. `chk[abs(err) > tol]` DROPS NA rather than matching it, so
  #      a match allocating 1000 points against an unknown margin came back in
  #      zero bad rows -- verified with a fixture, not reasoned about.
  #   3. An NA allocation, for the same reason.
  #
  # This is the repo's documented "count the violations, pass if zero" trap
  # (see r-datatable-gotchas.md), and a conservation checker is the last place
  # that should fall into it. Unverifiable rows are now their own failure, not a
  # silent skip.
  if (nrow(x) == 0) {
    cli::cli_abort(c(
      "Net points frame is empty -- nothing to check.",
      "x" = "An empty frame is not a passing conservation check."
    ))
  }
  chk <- x[, .(alloc = sum(net_points_hm), margin = data.table::first(margin)),
           by = match_id]
  unverifiable <- chk[is.na(alloc) | is.na(margin)]
  if (nrow(unverifiable)) {
    cli::cli_abort(c(
      "{nrow(unverifiable)} of {nrow(chk)} match{?es} cannot be checked: allocation or margin is NA.",
      "x" = "First: {unverifiable$match_id[1]}.",
      "i" = "An unverifiable match is a failure, not a pass -- a filter would drop it silently."
    ))
  }
  chk[, err := alloc - margin]
  bad <- chk[abs(err) > tol]
  if (nrow(bad)) {
    cli::cli_abort(c(
      "Net points does not conserve in {nrow(bad)} of {nrow(chk)} match{?es}.",
      "x" = "Worst: {bad[which.max(abs(err))]$match_id} off by {round(bad[which.max(abs(err))]$err, 4)} points.",
      "i" = "Max allowed {tol}."
    ))
  }
  cli::cli_alert_success(
    "Net points conserves in all {nrow(chk)} matches (max error {format(max(abs(chk$err)), digits = 3)} points)")
  invisible(chk)
}
