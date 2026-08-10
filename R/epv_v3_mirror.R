# Allocating an unnamed contest debit by POSITIONAL MATCHUP rather than flatly.
#
# WHY. The flat rule -- spread each unnamed debit equally across all ~22 players
# on the losing team -- is the single most damaging thing measured in the metric.
# It costs the contest channel 0.924 -> 0.384 in conversion to margin, i.e. it
# takes a channel that measures expected points correctly and more than halves
# what it is worth. It was also never a deliberate design choice put to Pete; it
# won a five-way comparison in an earlier session on repeatability grounds.
#
# THE IDEA. Chains names the beaten player in 31.3% of genuine duels, so the
# conditional P(loser position | winner position) is directly OBSERVABLE and the
# shares can be measured instead of assumed. Pete's framing: the mirror should
# take a high share but not all, with shares over positions summing to 100% of
# the team's debit for that event. That is exactly a conditional distribution.
#
# WHAT THE DATA SUPPORTS, AND WHAT IT DOES NOT.
#
#   Supports: the matchup is strongly non-flat. Against a flat baseline, a key
#   defender's duels are lost by key forwards at 6.6x the flat share (71.6%),
#   ruck-on-ruck runs 10.1x (59.4%), medium defender on medium forward 2.2x
#   (52.8%). And it is stable across zones -- KEY_DEF -> KEY_FWD reads 71.4% in
#   the forward 50 and 72.0% in the attacking midfield.
#
#   Does NOT support using the raw matrix. Every named loser comes from a
#   DEFENCE win: 28,758 of them, against fewer than 200 from attack-retained
#   duels. So we observe "who a defender beat" and never "who a forward beat".
#   Worse, naming depends on a `Contest Target` row, which mostly fires on
#   inside-50 entries aimed at a key forward -- so KEY_FORWARD is the top loser
#   column in EVERY row, including when a key forward is the winner (46.1%,
#   n = 471). That cell is selection, not football.
#
# SO: estimate from the reliable defence-win cells and impose SYMMETRY for the
# other direction. If key defenders contest key forwards, key forwards contest
# key defenders. That is an assumption -- stated here rather than buried -- and
# it is the weakest link in this rule. It is testable only indirectly, by
# whether the resulting channel converts nearer to 1.0 than the flat rule.
#
# Thin cells shrink toward the flat share so a matchup seen twice does not get
# a confident weight.

#' Prior weight, in observations, for shrinking a matchup cell to the flat share
#'
#' A cell backed by this many observed duels is weighted half on its own
#' evidence and half on the flat share. The matrix has cells ranging from 11,238
#' observations (key defender beating key forward) to single digits, so a
#' shrinkage is not optional.
#' @keywords internal
EPV3_MIRROR_PRIOR <- 200

#' Build the positional matchup matrix from duels whose loser chains named
#'
#' @param scored Output of \code{score_contests()}, carrying \code{winner_pid},
#'   \code{loser_pid} and \code{loser_tid}.
#' @param pos A data.table of \code{match_id}, \code{player_id}, \code{pos}.
#' @return A data.table of \code{winner_pos}, \code{loser_pos}, \code{w}, where
#'   \code{w} sums to 1 within \code{winner_pos}.
#' @keywords internal
build_mirror_weights <- function(scored, pos) {
  s <- data.table::as.data.table(scored)
  named <- s[!is.na(loser_pid) & !is.na(winner_pid)]
  if (nrow(named) < 500) {
    cli::cli_warn(c(
      "Only {nrow(named)} named-loser duels: too few to estimate matchup weights.",
      "i" = "Falling back to the flat share, which is what {.val team} does."
    ))
    return(NULL)
  }
  named <- merge(named, pos, by.x = c("match_id", "winner_pid"),
                 by.y = c("match_id", "player_id"), all.x = TRUE)
  data.table::setnames(named, "pos", "winner_pos")
  named <- merge(named, pos, by.x = c("match_id", "loser_pid"),
                 by.y = c("match_id", "player_id"), all.x = TRUE)
  data.table::setnames(named, "pos", "loser_pos")
  named <- named[!is.na(winner_pos) & !is.na(loser_pos)]
  if (nrow(named) < 500) return(NULL)

  # Only defence-wins are usable: the attack-retained direction is essentially
  # never logged, so including it would add noise from a handful of rows rather
  # than information.
  d <- named[def_win == TRUE, .N, by = .(winner_pos, loser_pos)]
  if (nrow(d) == 0) return(NULL)

  # Impose symmetry: a duel between positions A and B is the same matchup
  # whichever way it resolved. Adding the transpose gives the attack-win
  # direction we cannot observe, and doubles the evidence on every cell.
  tr <- data.table::copy(d)
  data.table::setnames(tr, c("winner_pos", "loser_pos"), c("loser_pos", "winner_pos"))
  both <- rbind(d, tr)[, .(N = sum(N)), by = .(winner_pos, loser_pos)]

  flat <- pos[, .N, by = pos][, .(loser_pos = pos, flat = N / sum(N))]
  both <- merge(both, flat, by = "loser_pos", all.x = TRUE)
  both[is.na(flat), flat := 0]
  both[, tot := sum(N), by = winner_pos]
  both[, raw := N / tot]
  # Shrink toward the flat share on thin evidence.
  both[, lam := tot / (tot + EPV3_MIRROR_PRIOR)]
  both[, w := lam * raw + (1 - lam) * flat]
  both[, w := w / sum(w), by = winner_pos]
  both[, .(winner_pos, loser_pos, w)]
}

#' Spread an unnamed contest debit across the losing team by positional matchup
#'
#' Every player on the losing team receives a share proportional to his
#' position's weight for the winner's position, normalised within the team so
#' the debit is conserved exactly. Players in the mirror position take the
#' largest share; nobody takes all of it and nobody takes none of it.
#'
#' @param scored Output of \code{score_contests()}.
#' @param chains Raw chains, used for the roster and positions.
#' @param weights From \code{build_mirror_weights()}; \code{NULL} falls back to
#'   the flat share.
#' @return A data.table of \code{player_id}, \code{match_id}, \code{cont_alloc}.
#' @keywords internal
allocate_by_mirror <- function(scored, chains, weights) {
  ch <- data.table::as.data.table(chains)
  s <- data.table::as.data.table(scored)
  pos <- unique(ch[!is.na(player_id) & !is.na(player_position),
                   .(match_id, player_id, pos = player_position)])
  roster <- unique(ch[!is.na(player_id) & !is.na(team_id),
                      .(match_id, team_id, player_id)])
  roster <- merge(roster, pos, by = c("match_id", "player_id"), all.x = TRUE)

  un <- s[is.na(loser_pid) & !is.na(winner_pid)]
  if (nrow(un) == 0 || is.null(weights)) {
    return(data.table::data.table(player_id = character(), match_id = character(),
                                  cont_alloc = numeric()))
  }
  un <- merge(un, pos, by.x = c("match_id", "winner_pid"),
              by.y = c("match_id", "player_id"), all.x = TRUE)
  data.table::setnames(un, "pos", "winner_pos")
  agg <- un[!is.na(winner_pos), .(debit = sum(loser_credit)),
            by = .(match_id, team_id = loser_tid, winner_pos)]
  if (nrow(agg) == 0) {
    return(data.table::data.table(player_id = character(), match_id = character(),
                                  cont_alloc = numeric()))
  }

  a <- merge(agg, roster, by = c("match_id", "team_id"), allow.cartesian = TRUE)
  a <- merge(a, weights, by.x = c("winner_pos", "pos"),
             by.y = c("winner_pos", "loser_pos"), all.x = TRUE)
  # Unmapped positions are all-or-nothing, NOT "shared at the smallest weight".
  # An earlier version of this comment claimed the latter ("nobody is exempt
  # from a team failure"); the code does the opposite in the common case. Read
  # the two lines together:
  #   - a player whose position has no weight for this winner gets w = 0, i.e.
  #     genuinely exempt, whenever ANY teammate in the group does have weight;
  #   - only if the WHOLE (match, team, winner_pos) group has zero weight does
  #     the fallback fire, and it is flat (w = 1 each), not "smallest present".
  # Conservation still holds either way — the debit is redistributed across
  # whoever retains weight rather than lost — so this is a description bug, not
  # a leak. Fix the comment or the behaviour deliberately; do not assume the
  # collective-outcome property holds today, because it does not.
  a[is.na(w), w := 0]
  a[, wsum := sum(w), by = .(match_id, team_id, winner_pos)]
  a[wsum <= 0, w := 1]
  a[, wsum := sum(w), by = .(match_id, team_id, winner_pos)]
  a[, cont_alloc := debit * w / wsum]

  out <- a[, .(cont_alloc = sum(cont_alloc)), by = .(player_id, match_id)]
  owed <- sum(agg$debit)
  got <- sum(out$cont_alloc)
  if (abs(got - owed) > max(1e-6, 0.001 * abs(owed))) {
    cli::cli_abort(c(
      "Mirror allocation did not conserve: owed {round(owed, 2)}, allocated {round(got, 2)}.",
      "x" = "The point of allocating at all is that the debit lands somewhere."
    ))
  }
  cli::cli_alert_info(
    "Contest debits allocated by positional matchup: {round(owed, 1)} points across {format(nrow(out), big.mark = ',')} player-games")
  out
}
