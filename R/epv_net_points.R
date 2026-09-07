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

# NOTE ON @param RANGES. Write "between 0 and 1 inclusive", never the interval
# notation with square brackets: roxygen reads square brackets as a markdown
# link, emits an unresolvable cross-reference into the .Rd, and R CMD check
# fails it as a WARNING -- which this repo's CI treats as an error
# (error_on: "warning"). Cost one red CI run on PR #189. The same text inside a
# cli:: string is fine; only roxygen is parsed as markdown.

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

#' The full act sequence: PBP rows, plus the chains rows PBP drops
#'
#' PBP is an exact subset of chains on `(match_id, display_order)` -- measured
#' on 2026: 355,399 PBP rows, every one present in chains with the same
#' description, team and player, and 82,718 chains-only rows. Those extra rows
#' are precisely the ones the credit rules need to see and PBP cannot show:
#' `Spoil` (13,096, names the spoiler), `Contest Target` (4,423, names the
#' intended target), `Goal` / `Behind`, `Mark Fumbled`, `Tackle`. They carry no
#' `delta_epv` -- the value stays on the PBP row that precedes them -- so they
#' are visible for **resolution** (what the disposal turned into) without ever
#' entering the ledger's value.
#'
#' Without `chains` the sequence is PBP alone and every row is `in_pbp`.
#'
#' @param pbp_data Play-by-play.
#' @param chains Raw chains for the same matches, or `NULL`.
#' @return A data.table ordered by `match_id, display_order` with
#'   `description`, `team`, `home_away`, `player_id`, `delta_epv` and
#'   `in_pbp` (does this row carry a PBP state).
#' @keywords internal
.np_sequence <- function(pbp_data, chains = NULL) {
  p <- data.table::as.data.table(pbp_data)
  if (is.null(chains)) {
    s <- p[, .(match_id, display_order, description, team,
               home_away = as.character(home_away), player_id, delta_epv,
               home_points, away_points, home, x, exp_pts)]
    s[, in_pbp := TRUE]
    data.table::setorder(s, match_id, display_order)
    return(s)
  }
  if (!"team_id" %in% names(p)) {
    cli::cli_abort("Play-by-play needs {.field team_id} to be aligned with chains.")
  }
  ch <- data.table::as.data.table(chains)
  detect_chains_columns(ch)
  cs <- ch[, .(match_id = as.character(match_id), display_order,
               description, team_id, player_id)]
  rm(ch)
  key <- c("match_id", "display_order")
  ndup <- sum(duplicated(cs, by = key))
  if (ndup > 0) {
    cli::cli_abort(c(
      "{format(ndup, big.mark = ',')} duplicated (match_id, display_order) key{?s} in {.arg chains}.",
      "x" = "Every join below would multiply value on those rows."
    ))
  }

  # Every PBP row must be a chains row, or the two sequences are from
  # different vintages and "the next row" would mean different things in each.
  pk <- p[, .(match_id = as.character(match_id), display_order, description,
              team_id, player_id, delta_epv, home_points, away_points, home, x,
              exp_pts)]
  miss <- pk[!cs, on = key]
  if (nrow(miss) > 0) {
    cli::cli_abort(c(
      "{format(nrow(miss), big.mark = ',')} PBP row{?s} ({round(100 * nrow(miss) / nrow(pk), 2)}%) are not in {.arg chains}.",
      "x" = "PBP and chains must be the same matches and vintage; refusing to build a sequence with holes in it.",
      "i" = "First missing: match {.val {miss$match_id[1]}}, display_order {.val {miss$display_order[1]}}."
    ))
  }
  s <- merge(cs, pk[, .(match_id, display_order, pbp_desc = description,
                       pbp_tid = team_id, pbp_pid = player_id,
                       delta_epv, home_points, away_points, home, x, exp_pts,
                       in_pbp = TRUE)],
             by = key, all.x = TRUE)
  s[is.na(in_pbp), in_pbp := FALSE]
  # Same key must mean the same act by the same player for the same team. The
  # VALUE comes from PBP, so a PBP row keeps PBP's own player and team; chains
  # only supplies the rows PBP does not have. Any disagreement is a vintage
  # mismatch and aborts -- a silent difference here would move credit between
  # players while every conservation check stayed green.
  same <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  chk <- s[in_pbp == TRUE]
  n_desc <- sum(!same(chk$pbp_desc, chk$description))
  n_pid <- sum(!same(chk$pbp_pid, chk$player_id))
  n_tid <- sum(!same(chk$pbp_tid, chk$team_id))
  if (n_desc + n_pid + n_tid > 0) {
    cli::cli_abort(c(
      "PBP and chains disagree on {n_desc} description{?s}, {n_pid} player{?s} and {n_tid} team{?s} at the same (match_id, display_order).",
      "x" = "Same key, different act or actor: the two inputs are not the same data."
    ))
  }
  s[in_pbp == TRUE, `:=`(player_id = pbp_pid, team_id = pbp_tid)]
  s[, c("pbp_desc", "pbp_pid", "pbp_tid") := NULL]

  # Team name and home/away frame come from PBP's own per-match map, so a
  # chains-only row is oriented exactly as the PBP rows around it.
  tm <- unique(p[!is.na(team_id) & !is.na(team) & !is.na(home_away),
                 .(match_id = as.character(match_id), team_id, team,
                   home_away = as.character(home_away))])
  dup <- tm[, .N, by = .(match_id, team_id)][N > 1]
  if (nrow(dup) > 0) {
    cli::cli_abort("{nrow(dup)} (match, team_id) pair{?s} map to more than one team name or frame in PBP.")
  }
  s <- merge(s, tm, by = c("match_id", "team_id"), all.x = TRUE)
  data.table::setorder(s, match_id, display_order)

  extra <- s[in_pbp == FALSE]
  top <- head(extra[, .N, by = description][order(-N)], 4)
  top_txt <- if (nrow(top)) paste0(top$description, " ", format(top$N, big.mark = ","), collapse = ", ") else "none"
  cli::cli_alert_info(
    "Net points sequence: {format(nrow(s), big.mark = ',')} chains rows, {format(sum(s$in_pbp), big.mark = ',')} carry PBP value; {format(nrow(extra), big.mark = ',')} chains-only rows visible for resolution ({top_txt})")
  s[, .(match_id, display_order, description, team, home_away, player_id,
        delta_epv, home_points, away_points, home, x, exp_pts, in_pbp)]
}

#' What each disposal turned into: the first row after it that is not in flight
#'
#' The same rule `build_disposal_events()` uses: skip the annotation rows that
#' describe the ball mid-air (`CHAINS_INFLIGHT_DESCS`) and take the first row
#' that says what happened -- `Spoil`, `Contested Mark`, `Loose Ball Get`,
#' `Goal`. This is a different question from adjacency: adjacency asks who
#' next held a PBP state (the ground-ball winner after a spoil), resolution
#' asks who ended the contest (the spoiler). Both are needed and they are
#' allowed to name different people.
#'
#' Looks at most six rows ahead. A disposal with nothing but in-flight rows in
#' that window resolves to `NA`.
#'
#' @param seq Output of `.np_sequence()`.
#' @return `match_id`, `display_order`, `resolve_desc`, `resolve_team`,
#'   `resolve_player`, `resolve_lag`, for disposal rows only.
#' @keywords internal
.np_resolution <- function(seq) {
  s <- seq[, .(match_id, display_order, description, team, player_id)]
  data.table::setorder(s, match_id, display_order)
  K <- 6L
  for (k in seq_len(K)) {
    for (v in c("description", "team", "player_id")) {
      s[, (paste0("f", k, "_", v)) := data.table::shift(get(v), k, type = "lead"),
        by = match_id]
    }
  }
  d <- s[description %chin% NP_DISPOSAL_DESCS]
  inflight <- CHAINS_INFLIGHT_DESCS
  d[, resolve_lag := NA_integer_]
  for (k in rev(seq_len(K))) {
    fd <- d[[paste0("f", k, "_description")]]
    d[!is.na(fd) & !(fd %chin% inflight), resolve_lag := k]
  }
  pick <- function(v) {
    out <- rep(NA_character_, nrow(d))
    for (k in seq_len(K)) {
      idx <- which(d$resolve_lag == k)
      out[idx] <- d[[paste0("f", k, "_", v)]][idx]
    }
    out
  }
  d[, `:=`(resolve_desc = pick("description"), resolve_team = pick("team"),
           resolve_player = pick("player_id"))]
  d[, .(match_id, display_order, resolve_desc, resolve_team, resolve_player,
        resolve_lag)]
}

#' Neutral baseline for a stoppage, by type and location (D15)
#'
#' A stoppage row's own state value is filled from the side that ends up
#' winning the first possession, so ball-ups and throw-ins read near zero and
#' the swing leaks into the row before them. The baseline is the average
#' first-possession value, in the home-margin frame, over both winners, for
#' each stoppage type and 20m band of the ground (oriented to the home side's
#' attacking end). Valuing the stoppage there makes the row before it worth
#' "forcing the stoppage" and the stoppage row worth "winning it", and the two
#' still sum to what they summed to, so conservation is untouched.
#'
#' The centre-bounce baseline must sit near zero; anything else means the
#' frame is wrong, and the function aborts rather than reprice every clearance.
#'
#' @param seq Output of `.np_sequence()`.
#' @return `description`, `band`, `baseline`, `n`.
#' @keywords internal
.np_stoppage_baseline <- function(seq) {
  s <- seq[in_pbp == TRUE, .(match_id, display_order, description, home, x, exp_pts)]
  data.table::setorder(s, match_id, display_order)
  s[, `:=`(n_home = data.table::shift(home, -1L), n_exp = data.table::shift(exp_pts, -1L)),
    by = match_id]
  st <- s[description %chin% NP_STOPPAGE_DESCS & !is.na(home) & is.finite(x) &
            !is.na(n_home) & is.finite(n_exp)]
  if (nrow(st) == 0) {
    cli::cli_abort("No stoppage rows with a following possession -- cannot estimate a baseline.")
  }
  st[, `:=`(x_home = x * data.table::fifelse(home == 1L, 1, -1),
            v_next = n_exp * data.table::fifelse(n_home == 1L, 1, -1))]
  st[, band := floor(x_home / NP_STOPPAGE_BAND_M) * NP_STOPPAGE_BAND_M]
  out <- st[, .(baseline = mean(v_next), n = .N), by = .(description, band)]
  cb <- out[description == "Centre Bounce", sum(baseline * n) / sum(n)]
  if (is.finite(cb) && abs(cb) > 0.5) {
    cli::cli_abort(c(
      "Centre-bounce baseline is {round(cb, 3)}; it should sit near zero.",
      "x" = "The stoppage frame is wrong; refusing to reprice every clearance on it."
    ))
  }
  cli::cli_alert_info(
    "Stoppage baseline: {nrow(out)} type x band cells from {format(nrow(st), big.mark = ',')} stoppages; centre bounce {round(cb, 3)}, ball-up range {round(min(out[description == 'Ball Up Call']$baseline), 2)} to {round(max(out[description == 'Ball Up Call']$baseline), 2)}")
  out
}

#' Build the per-act ledger in the home-margin frame
#'
#' @param pbp_data Play-by-play carrying `delta_epv`, `home_away`, `team`,
#'   `player_id`, `description`, `match_id`, `display_order`.
#' @param chains Raw chains for the same matches, or `NULL`. With chains the
#'   ledger's VALUE is unchanged -- every point still comes from a PBP row --
#'   but each disposal also carries `resolve_*`: the chains row that ended it
#'   (a spoil, a contested mark, a goal), which PBP never shows.
#' @param stoppages `"exclude"` (the default) drops stoppage rows as before;
#'   `"allocate"` keeps them as ledger rows with `is_stoppage = TRUE`, valued at
#'   the neutral baseline (D15), and reprices the row before each one.
#' @param stoppage_baseline Precomputed `.np_stoppage_baseline()` table, or
#'   `NULL` to estimate it from the data.
#' @return A data.table of ledger rows with `hm` (home-margin-frame value) plus
#'   the next row's team and player, used to detect turnovers, and the
#'   resolution columns (`NA` without chains).
#' @keywords internal
.np_build_ledger <- function(pbp_data, chains = NULL,
                             stoppages = c("exclude", "allocate"),
                             stoppage_baseline = NULL) {
  stoppages <- match.arg(stoppages)
  d0 <- data.table::as.data.table(pbp_data)
  need <- c("match_id", "display_order", "delta_epv", "home_away", "team",
            "player_id", "description", "home_points", "away_points", "home",
            "x", "exp_pts")
  missing <- setdiff(need, names(d0))
  if (length(missing)) {
    cli::cli_abort(c(
      "Play-by-play is missing {length(missing)} column{?s} the ledger needs: {.val {missing}}",
      "i" = "The net-points ledger reads raw PBP, not a derived credit frame."
    ))
  }

  seq <- .np_sequence(d0, chains)
  # Adjacency FIRST, on the unfiltered sequence -- see .np_adjacency(). Doing
  # this after the filters below is what made half of all goals read as
  # turnovers. Adjacency is taken over the PBP rows only: a chains-only row
  # holds no state, so "who acted next" must skip it or a spoil would read as
  # the spoiler winning possession. Resolution is where the spoiler is named.
  d <- seq[in_pbp == TRUE]
  adj <- .np_adjacency(d)
  res <- if (is.null(chains)) NULL else .np_resolution(seq)

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
  # Under "allocate" a stoppage row is a ledger row: it needs a frame (`home`)
  # and a value, nothing else. Under "exclude" centre bounces are dropped by
  # rule and the rest fall out for having no team, as before.
  d[, is_stoppage := identical(stoppages, "allocate") &
        description %chin% NP_STOPPAGE_DESCS & !is.na(home)]
  excl <- d[.np_is_excluded(description) & !is_stoppage]
  if (nrow(excl)) {
    cli::cli_alert_info(
      "Net points: excluding {format(nrow(excl), big.mark = ',')} phantom row{?s} worth {round(sum(excl$delta_epv), 1)} points ({paste(NP_EXCLUDED_DESCS, collapse = ', ')})")
  }
  d <- d[!(.np_is_excluded(description) & !is_stoppage)]
  n_post_excl <- nrow(d)

  d <- d[is_stoppage | (!is.na(team) & !is.na(player_id) & !is.na(home_away))]
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
  d[, hm := delta_epv * data.table::fifelse(
    is_stoppage, data.table::fifelse(home == 1L, 1, -1),
    data.table::fifelse(home_away == "Home", 1, -1))]

  if (identical(stoppages, "allocate")) {
    # Reprice each stoppage to its neutral baseline (D15). The row before it is
    # paid up to the baseline; the stoppage row is paid from the baseline to
    # the first possession. adj = baseline - state value the row carried, so
    # (hm_prev + adj) + (hm_stop - adj) is what the pair summed to before.
    bl <- if (is.null(stoppage_baseline)) .np_stoppage_baseline(seq) else
      data.table::as.data.table(stoppage_baseline)
    d[, x_home := x * data.table::fifelse(home == 1L, 1, -1)]
    d[, band := floor(x_home / NP_STOPPAGE_BAND_M) * NP_STOPPAGE_BAND_M]
    d[bl, on = .(description, band), baseline := i.baseline]
    d[, adj := 0]
    d[is_stoppage == TRUE & is.finite(baseline) & is.finite(exp_pts),
      adj := baseline - exp_pts * data.table::fifelse(home == 1L, 1, -1)]
    n_nobase <- d[is_stoppage == TRUE & !is.finite(baseline), .N]
    if (n_nobase > 0) {
      cli::cli_alert_warning(
        "{n_nobase} stoppage{?s} had no baseline cell (location out of range) and keep{?s/} the row's own value.")
    }
    # the previous ledger row in the same match receives +adj; the first row
    # of a match has none, so that stoppage keeps its full swing
    d[, prev_same := data.table::shift(match_id, 1L) == match_id]
    d[is.na(prev_same), prev_same := FALSE]
    d[, adj_here := data.table::fifelse(is_stoppage & prev_same, adj, 0)]
    d[, adj_from_next := data.table::shift(adj_here, -1L, fill = 0), by = match_id]
    d[, hm := hm - adj_here + adj_from_next]
    # the row before a stoppage keeps what was added to it, so its difficulty
    # terms can be moved by the same amount (the surprise absorbs it: the
    # kick's "after" is now the baseline, not the leaked winner's state)
    d[, reprice_hm := adj_from_next]
    d[, `:=`(prev_same = NULL, adj_here = NULL, adj_from_next = NULL,
             x_home = NULL, band = NULL, baseline = NULL, adj = NULL)]
    st <- d[is_stoppage == TRUE]
    cli::cli_alert_info(
      "Stoppages allocated: {format(nrow(st), big.mark = ',')} rows carrying {round(sum(abs(st$hm)), 1)} points gross ({round(sum(abs(st$hm)) / data.table::uniqueN(st$match_id), 1)} a match) after repricing to the baseline")
  }
  d[adj, on = .(match_id, display_order),
    `:=`(next_team = i.next_team, next_player = i.next_player,
         next_desc = i.next_desc)]
  d[, `:=`(resolve_desc = NA_character_, resolve_team = NA_character_,
           resolve_player = NA_character_, resolve_lag = NA_integer_)]
  if (!is.null(res)) {
    d[res, on = .(match_id, display_order),
      `:=`(resolve_desc = i.resolve_desc, resolve_team = i.resolve_team,
           resolve_player = i.resolve_player, resolve_lag = i.resolve_lag)]
    disp <- d[description %chin% NP_DISPOSAL_DESCS]
    cli::cli_alert_info(
      "Net points resolution: {round(100 * mean(!is.na(disp$resolve_desc)), 1)}% of {format(nrow(disp), big.mark = ',')} disposals resolve within 6 rows; {round(100 * mean(disp$resolve_desc %chin% c('Spoil', 'Contest Target', 'Contested Mark'), na.rm = TRUE), 1)}% at a named contest")
  }
  if (!"reprice_hm" %in% names(d)) d[, reprice_hm := 0]
  out <- d[, .(match_id, display_order, description, team, home_away, player_id,
               hm, is_stoppage, reprice_hm, next_team, next_player, next_desc,
               resolve_desc, resolve_team, resolve_player, resolve_lag)]
  # Every named actor in the sequence, including chains-only ones (a spoiler
  # who never touched the ball in PBP), so a contest winner always has a roster
  # row to be paid on.
  data.table::setattr(out, "np_roster",
                      unique(seq[!is.na(team) & !is.na(player_id),
                                 .(match_id, team, player_id)]))
  out
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
#' \strong{So is a score.} A behind is followed by the opposition's kick-in,
#' which HAS a team, so by the rule above it read as a turnover: the defence
#' was paid 30% of every behind conceded (1,981 points in 2026, 9.3 a match)
#' and the kick-in taker took 60% of that -- almost all of it to half-backs.
#' Found on 2026-09-06 by the chains resolution column. The general form: the
#' running score changes between this row and the next only when this row
#' scored (goal, behind, rushed behind, a dribbled ground kick), and a score is
#' always followed by a restart. PBP books the points on the FOLLOWING row,
#' which is why the comparison is this row against the next.
#'
#' @param pbp_data The full play-by-play, before any filtering.
#' @return A data.table of `match_id`, `display_order`, `next_team`,
#'   `next_player`.
#' @keywords internal
.np_adjacency <- function(pbp_data) {
  a <- data.table::as.data.table(pbp_data)[, .(match_id, display_order, team,
                                               player_id, description,
                                               home_points, away_points)]
  data.table::setorder(a, match_id, display_order)
  a[, tot := home_points + away_points]
  a[, `:=`(nt = data.table::shift(team, -1L),
           npl = data.table::shift(player_id, -1L),
           nd = data.table::shift(description, -1L),
           ntot = data.table::shift(tot, -1L)), by = match_id]
  # Chain-terminal: the next event has no acting team (a restart), is one we
  # exclude as phantom, or the score moved (this row scored, and a restart
  # follows). Either way there is no next actor to credit.
  a[, scored := !is.na(ntot) & !is.na(tot) & ntot != tot]
  a[, terminal := is.na(nt) | .np_is_excluded(nd) | scored]
  a[, .(match_id, display_order,
        next_team = data.table::fifelse(terminal, NA_character_, nt),
        next_player = data.table::fifelse(terminal, NA_character_, npl),
        next_desc = data.table::fifelse(terminal, NA_character_, nd))]
}

#' Difficulty terms for every scorable disposal: p, decision, surprise
#'
#' The identity each disposal follows (docs/plans/EPV-V4-CREDIT-RULES.md, D5-D7):
#'
#' ```
#' delta_epv = (EV - before)    the DECISION  -> the disposer
#'           + (after - EV)     the SURPRISE  -> whoever resolved it
#' ```
#'
#' with `EV = (1 - p) * V_keep + p * V_lose` and `p` the modelled chance of
#' losing the ball. Everything here comes from `torp/R/epv_difficulty.R`, built
#' in 2026-08 and never switched on; this is the first consumer. The models are
#' fitted **leak-safe by season** -- each season scored on earlier seasons --
#' and fall back to an in-sample fit, loudly, when no earlier season is present.
#'
#' Terms are in the ACTING team's frame, like `delta_epv`; the ledger flips
#' them to the home-margin frame when it joins them.
#'
#' @param pbp_data Play-by-play carrying `exp_pts` and `delta_epv`.
#' @param chains Raw chains for the same matches.
#' @param leak_safe Fit each season on strictly earlier seasons.
#' @return `match_id`, `display_order`, `p_hat`, `decision`, `surprise`.
#' @keywords internal
.np_difficulty_terms <- function(pbp_data, chains, leak_safe = TRUE,
                                 train_pbp = NULL, train_chains = NULL) {
  de <- build_disposal_events(chains, pbp_data)
  if (nrow(de) == 0) {
    cli::cli_abort("No disposal could be scored for difficulty -- check that {.arg chains} and {.arg pbp_data} overlap.")
  }
  de[, .season := as.integer(substr(match_id, 5, 8))]
  n_bad <- sum(is.na(de$.season) | de$.season < 2000 | de$.season > 2100)
  if (isTRUE(leak_safe) && n_bad > 0) {
    cli::cli_abort(c(
      "Could not parse a plausible season from {n_bad} of {nrow(de)} match_id{?s}.",
      "x" = "Refusing to run leak-safe fitting on an unparsed season."
    ))
  }
  seasons <- sort(unique(de$.season))
  # An explicit training set (the previous season, loaded by the caller) wins
  # over the within-data season loop: the pipeline scores one season at a time.
  have_train <- !is.null(train_pbp) && !is.null(train_chains)
  scored <- if (have_train) {
    de_tr <- build_disposal_events(train_chains, train_pbp)
    if (nrow(de_tr) < 20000) {
      cli::cli_abort("Training data has only {nrow(de_tr)} disposals; refusing to fit the difficulty models on it.")
    }
    cli::cli_alert_info(
      "Difficulty models fitted on {format(nrow(de_tr), big.mark = ',')} training disposals from {paste(sort(unique(substr(de_tr$match_id, 5, 8))), collapse = ', ')}, scoring {paste(seasons, collapse = ', ')}")
    score_disposals(de, fit_disposal_models(de_tr))
  } else if (isTRUE(leak_safe) && length(seasons) > 1) {
    data.table::rbindlist(lapply(seasons, function(s) {
      idx <- de$.season < s
      if (sum(idx) < 20000) {
        idx <- de$.season == s
        cli::cli_alert_warning(
          "Season {s}: difficulty models fitted IN-SAMPLE (no earlier season available).")
      }
      score_disposals(de[.season == s], fit_disposal_models(de, idx))
    }))
  } else {
    if (isTRUE(leak_safe)) {
      cli::cli_alert_warning(
        "Difficulty models fitted IN-SAMPLE on {seasons}: only one season supplied. Fine for measurement, not for a published rating.")
    }
    score_disposals(de, fit_disposal_models(de))
  }
  scored[, decision := V_pre - exp_pts]
  cli::cli_alert_info(
    "Difficulty terms: {format(nrow(scored), big.mark = ',')} disposals scored; mean p(lose) {round(mean(scored$p_hat), 3)}, mean |decision| {round(mean(abs(scored$decision)), 3)}, mean |surprise| {round(mean(abs(scored$surprise)), 3)}")
  out <- scored[, .(match_id = as.character(match_id), display_order, p_hat,
                    decision, surprise)]
  out[, `:=`(contested = FALSE, cont_desc = NA_character_,
             cont_surprise = NA_real_, ground_surprise = NA_real_,
             def_win = NA, winner_pid = NA_character_)]

  # --- the contest branch (D8) ------------------------------------------------
  # A kick that resolves at a contest is split once more: the contest surprise
  # (branch value minus EV) belongs to whoever won the contest, and the
  # ground-ball surprise (what happened after the fall of the ball, minus the
  # branch value) to whoever possessed next. The branch models are v3's aerial
  # models, fitted on every kick that resolves at a mark or spoil; the split is
  # applied only where a contest was actually fought -- a duel outcome, or any
  # mark the defence took -- so a teammate's uncontested mark stays under D6.
  cst <- build_aerial_contests(chains, pbp_data)
  if (nrow(cst) > 0) {
    cst[, .season := as.integer(substr(match_id, 5, 8))]
    cseasons <- sort(unique(cst$.season))
    csc <- if (have_train) {
      cst_tr <- build_aerial_contests(train_chains, train_pbp)
      if (nrow(cst_tr) < 5000) {
        cli::cli_abort("Training data has only {nrow(cst_tr)} aerial contests; refusing to fit the contest models on it.")
      }
      score_contests(cst, fit_contest_models(cst_tr))
    } else if (isTRUE(leak_safe) && length(cseasons) > 1) {
      data.table::rbindlist(lapply(cseasons, function(s) {
        idx <- cst$.season < s
        if (sum(idx) < 5000) {
          idx <- cst$.season == s
          cli::cli_alert_warning(
            "Season {s}: contest models fitted IN-SAMPLE (no earlier season available).")
        }
        score_contests(cst[.season == s], fit_contest_models(cst, idx))
      }))
    } else {
      score_contests(cst, fit_contest_models(cst))
    }
    csc <- csc[def_win == TRUE | out_desc %chin% EPV3_DUEL_OUT]
    csc[, V_branch := data.table::fifelse(def_win, V_def_hat, V_att_hat)]
    ct <- csc[, .(match_id = as.character(match_id), display_order = kick_do,
                  c_p = p_hat, c_decision = V_pre - exp_pts,
                  c_cont = V_branch - V_pre, c_ground = V_after - V_branch,
                  c_desc = out_desc, c_def_win = def_win, c_winner = out_pid)]
    n_new <- nrow(ct[!out, on = .(match_id, display_order)])
    out <- merge(out, ct, by = c("match_id", "display_order"), all = TRUE)
    hit <- !is.na(out$c_p)
    out[hit, `:=`(p_hat = c_p, decision = c_decision,
                  surprise = c_cont + c_ground, contested = TRUE,
                  cont_desc = c_desc, cont_surprise = c_cont,
                  ground_surprise = c_ground, def_win = c_def_win,
                  winner_pid = c_winner)]
    out[, c("c_p", "c_decision", "c_cont", "c_ground", "c_desc", "c_def_win",
            "c_winner") := NULL]
    cli::cli_alert_info(
      "Contest terms: {format(sum(hit), big.mark = ',')} kicks resolve at a fought contest ({format(n_new, big.mark = ',')} not scorable as plain disposals); defence won {round(100 * mean(out$def_win[hit]), 1)}%; mean |contest surprise| {round(mean(abs(out$cont_surprise[hit])), 3)}, mean |ground surprise| {round(mean(abs(out$ground_surprise[hit])), 3)}")
  }
  out[is.na(contested), contested := FALSE]
  out
}

#' Decide, per ledger row, who is paid what
#'
#' Every row's `hm` is split into four non-overlapping parts that sum back to
#' it exactly:
#'
#' \describe{
#'   \item{`own_hm`}{what the actor keeps}
#'   \item{`recv_hm`}{paid to the receiver of a retained disposal}
#'   \item{`team_hm`}{paid to the actor's team pool (offence, D9)}
#'   \item{`cede_hm`}{paid to the OPPOSITION for a turnover, routed by
#'     `.np_defensive_pool()` (ball-winner share, then the spread)}
#' }
#'
#' Under `credit = "flat"` the split is the pre-v4 rule: a retained disposal
#' gives `alpha` to the receiver, a turnover cedes `phi`, nothing goes to an
#' offence pool. Under `credit = "difficulty"` a scored disposal follows the
#' identity in `.np_difficulty_terms()`:
#'
#' \itemize{
#'   \item retained: actor `(1 - omega) * (decision + p * surprise)`, receiver
#'     `(1 - omega) * (1 - p) * surprise`, team pool `omega * hm` (D6, D9);
#'   \item turnover: actor `decision + beta * surprise`, opposition
#'     `(1 - beta) * surprise` (D7);
#'   \item terminal (a score, a restart): actor `(1 - omega) * hm`, team pool
#'     `omega * hm`.
#' }
#'
#' A disposal the difficulty model could not score (no resolvable outcome row,
#' 4.8% in 2026) falls back to the flat rule; the count is logged, because a
#' silent fallback here would be indistinguishable from the rule working.
#'
#' @param led Ledger from `.np_build_ledger()`.
#' @param credit `"flat"` or `"difficulty"`.
#' @param alpha Flat receiver share.
#' @param phi Flat defensive share.
#' @param beta Disposer's share of a lost surprise (difficulty).
#' @param omega Offence team-pool share (difficulty).
#' @param terms Output of `.np_difficulty_terms()`, or `NULL` under flat.
#' @return `led` with `kind`, `scored`, `contested` and the six parts `own_hm`,
#'   `recv_hm`, `win_hm` (a same-team contest winner), `team_hm`, `cede_hm`
#'   (ground-ball value ceded to the opposition) and `cede_c_hm` (contest
#'   value ceded to the opposition, routed to the contest winner by
#'   `NP_CONTEST_WINNER_SHARE`).
#' @keywords internal
.np_credit_terms <- function(led, credit, alpha, phi, beta = 0, omega = 0,
                             terms = NULL) {
  for (nm in c("alpha", "phi", "beta", "omega")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 0 || v > 1) {
      cli::cli_abort("{.arg {nm}} must be one number between 0 and 1 inclusive, not {.val {v}}.")
    }
  }
  l <- data.table::copy(led)
  if (!"is_stoppage" %in% names(l)) l[, is_stoppage := FALSE]
  l[, is_disp := description %in% NP_DISPOSAL_DESCS & !is_stoppage]
  # A row where the NEXT act belongs to the opposition is a turnover, and until
  # 2026-09-07 that was only true of kicks and handballs: every other act fell
  # to "act", which pays 100% to the actor and credits nobody. So a player who
  # was tackled after a loose ball get, or who lost a handball receive, was
  # debited the whole swing and the opponent who took the ball was paid nothing.
  # Measured on 2026: 15.9 such rows a match carrying 27.1 points of swing, none
  # of it credited, against 156 disposal turnovers a match where the opponent is
  # paid 88% of the time. The uncredited events are worth 1.72 each against 0.54,
  # because they are the tackled-in-possession ones. Every one of the 20
  # descriptions involved is an act by the side in possession -- spoils and
  # tackles are not in the play-by-play, so no defensive act is blamed here.
  # `NP_TURNOVER_ON_ALL_ACTS` gates it because it moves published ratings.
  l[, kind := data.table::fcase(
    is_stoppage,                                       "stoppage",
    !is_disp & isTRUE(NP_TURNOVER_ON_ALL_ACTS) &
      !is.na(next_team) & next_team != team,           "turnover",
    !is_disp,                                          "act",
    is.na(next_team),                                  "terminal",
    next_team == team & !is.na(next_player),           "retained",
    next_team == team,                                 "terminal",
    default =                                          "turnover")]

  # flat rule everywhere first; difficulty overwrites the rows it can score
  l[, `:=`(scored = FALSE, contested = FALSE,
           own_hm  = data.table::fcase(kind == "retained", hm * (1 - alpha),
                                       kind == "turnover", hm * (1 - phi),
                                       default = hm),
           recv_hm = data.table::fifelse(kind == "retained", hm * alpha, 0),
           win_hm = 0,
           team_hm = 0,
           cede_hm = data.table::fifelse(kind == "turnover", hm * phi, 0),
           cede_c_hm = 0, stop_hm = 0,
           winner_pid = NA_character_, cont_desc = NA_character_)]
  # a stoppage row is paid by .np_stoppage_credit(), not by any rule below
  l[kind == "stoppage", `:=`(own_hm = 0, recv_hm = 0, cede_hm = 0, stop_hm = hm)]

  if (identical(credit, "difficulty")) {
    if (is.null(terms) || nrow(terms) == 0) {
      cli::cli_abort("{.arg credit = \"difficulty\"} needs difficulty terms and none were supplied.")
    }
    t <- data.table::as.data.table(terms)
    for (v in c("contested", "cont_desc", "cont_surprise", "ground_surprise",
                "def_win", "winner_pid")) {
      if (!v %in% names(t)) {
        data.table::set(t, j = v, value = switch(v, contested = FALSE,
                                                 def_win = NA,
                                                 cont_desc = NA_character_,
                                                 winner_pid = NA_character_,
                                                 NA_real_))
      }
    }
    t <- t[, .(match_id = as.character(match_id), display_order, p_hat, decision,
               surprise, contested, cont_desc, cont_surprise, ground_surprise,
               def_win, winner_pid)]
    l[t, on = .(match_id, display_order),
      `:=`(p_hat = i.p_hat, dec = i.decision, sur = i.surprise,
           contested = i.contested, cont_desc = i.cont_desc, csur = i.cont_surprise,
           gsur = i.ground_surprise, def_win = i.def_win, winner_pid = i.winner_pid)]
    l[is.na(contested), contested := FALSE]
    l[, sgn := data.table::fifelse(home_away == "Home", 1, -1)]
    l[, `:=`(dec_hm = dec * sgn, sur_hm = sur * sgn,
             c_hm = csur * sgn, g_hm = gsur * sgn)]
    # a row repriced to a stoppage baseline: the surprise (and, on a contested
    # kick, its ground-ball part) moves with it, the decision does not
    if (!"reprice_hm" %in% names(l)) l[, reprice_hm := 0]
    l[reprice_hm != 0, `:=`(sur_hm = sur_hm + reprice_hm, g_hm = g_hm + reprice_hm)]
    l[, scored := is_disp & !is.na(p_hat) & is.finite(dec_hm) & is.finite(sur_hm)]

    # The row identity, asserted rather than assumed: decision + surprise must
    # rebuild the row's value. If it does not, the terms came from a different
    # PBP vintage than the ledger and every split below would be fiction.
    gap <- if (any(l$scored)) l[scored == TRUE, max(abs(dec_hm + sur_hm - hm))] else 0
    if (!is.finite(gap) || gap > 1e-9) {
      cli::cli_abort(c(
        "Difficulty terms do not rebuild the ledger row: max |decision + surprise - delta| = {signif(gap, 3)}.",
        "x" = "The terms and the play-by-play are not the same data."
      ))
    }
    n_disp <- sum(l$is_disp)
    n_unscored <- sum(l$is_disp & !l$scored)
    cli::cli_alert_info(
      "Difficulty credit: {format(n_disp - n_unscored, big.mark = ',')} of {format(n_disp, big.mark = ',')} disposals scored ({round(100 * (n_disp - n_unscored) / n_disp, 1)}%); {format(n_unscored, big.mark = ',')} fall back to the flat rule. Row identity max gap {signif(gap, 2)}.")

    l[scored == TRUE & kind == "retained", `:=`(
      own_hm  = (1 - omega) * (dec_hm + p_hat * sur_hm),
      recv_hm = (1 - omega) * (1 - p_hat) * sur_hm,
      team_hm = omega * hm,
      cede_hm = 0)]
    l[scored == TRUE & kind == "turnover", `:=`(
      own_hm  = dec_hm + beta * sur_hm,
      recv_hm = 0,
      team_hm = 0,
      cede_hm = (1 - beta) * sur_hm)]
    l[scored == TRUE & kind == "terminal", `:=`(
      own_hm  = (1 - omega) * hm,
      recv_hm = 0,
      team_hm = omega * hm,
      cede_hm = 0)]

    # --- contested kicks (D8): three terms, three recipients -----------------
    # The contest surprise goes to whoever won the contest: a same-team winner
    # is paid directly (win_hm); a defensive winner is paid through cede_c_hm,
    # which .np_defensive_pool() routes by NP_CONTEST_WINNER_SHARE. The
    # ground-ball surprise follows the next possession exactly as an ordinary
    # disposal does. The disposer keeps the decision and, on anything lost,
    # `beta` of the loss.
    cs <- l$scored & l$contested & is.finite(l$c_hm) & is.finite(l$g_hm) &
      !is.na(l$def_win)
    if (any(cs)) {
      bad_win <- l[cs & !is.na(resolve_player) & !is.na(winner_pid) &
                     resolve_player != winner_pid, .N]
      if (bad_win > 0) {
        cli::cli_warn(
          "{bad_win} contested kick{?s} name a different winner in the terms than in the ledger's resolution; the terms' winner is used.")
      }
      aw <- cs & !l$def_win
      dw <- cs & l$def_win
      l[aw & kind == "retained", `:=`(
        own_hm = (1 - omega) * dec_hm, win_hm = (1 - omega) * c_hm,
        recv_hm = (1 - omega) * g_hm, team_hm = omega * hm, cede_hm = 0, cede_c_hm = 0)]
      l[aw & kind == "terminal", `:=`(
        own_hm = (1 - omega) * (dec_hm + g_hm), win_hm = (1 - omega) * c_hm,
        recv_hm = 0, team_hm = omega * hm, cede_hm = 0, cede_c_hm = 0)]
      l[aw & kind == "turnover", `:=`(
        own_hm = dec_hm + beta * g_hm, win_hm = c_hm, recv_hm = 0, team_hm = 0,
        cede_hm = (1 - beta) * g_hm, cede_c_hm = 0)]
      l[dw & kind == "retained", `:=`(
        own_hm = dec_hm + beta * c_hm, win_hm = 0, recv_hm = (1 - omega) * g_hm,
        team_hm = omega * g_hm, cede_hm = 0, cede_c_hm = (1 - beta) * c_hm)]
      l[dw & kind == "turnover", `:=`(
        own_hm = dec_hm + beta * c_hm + beta * g_hm, win_hm = 0, recv_hm = 0,
        team_hm = 0, cede_hm = (1 - beta) * g_hm, cede_c_hm = (1 - beta) * c_hm)]
      l[dw & kind == "terminal", `:=`(
        own_hm = dec_hm + beta * c_hm + (1 - omega) * g_hm, win_hm = 0, recv_hm = 0,
        team_hm = omega * g_hm, cede_hm = 0, cede_c_hm = (1 - beta) * c_hm)]
      cli::cli_alert_info(
        "Contest credit: {format(sum(cs), big.mark = ',')} kicks split at the contest; {format(sum(dw), big.mark = ',')} won by the defence ({round(sum(abs(l$cede_c_hm)), 1)} points ceded at the contest), {format(sum(aw), big.mark = ',')} by the attack ({round(sum(abs(l$win_hm)), 1)} points to same-team winners)")
    }
    l[, c("dec", "sur", "sgn", "csur", "gsur") := NULL]
  }

  # the four parts must rebuild every row, whichever rule produced them
  gap4 <- l[, max(abs(own_hm + recv_hm + win_hm + team_hm + cede_hm + cede_c_hm + stop_hm - hm))]
  if (!is.finite(gap4) || gap4 > 1e-9) {
    cli::cli_abort("Credit terms do not sum to the row value (max gap {signif(gap4, 3)}).")
  }
  l
}

#' Pay the named recipients: the actor and the receiver
#'
#' Reads the per-row split from `.np_credit_terms()`. `np_direct` is the actor's
#' own rows at FACE value (own + everything ceded, i.e. before anything is
#' transferred to the opposition) plus what he received as a receiver or as a
#' same-team contest winner, so that `np_ceded` can report the transfer
#' separately. Conservation within a team-match is
#' untouched: the receiver share and the offence pool move value between
#' teammates only.
#'
#' @param l Output of `.np_credit_terms()`.
#' @return `match_id`, `team`, `player_id`, `np_direct`.
#' @keywords internal
.np_direct_credit <- function(l) {
  actor <- l[kind != "stoppage", .(match_id, team, player_id, v = own_hm + cede_hm + cede_c_hm)]
  recv <- l[kind == "retained", .(match_id, team, player_id = next_player, v = recv_hm)]
  win <- l[win_hm != 0 & !is.na(winner_pid),
           .(match_id, team, player_id = winner_pid, v = win_hm)]
  out <- data.table::rbindlist(list(actor, recv, win))[
    , .(np_direct = sum(v)), by = .(match_id, team, player_id)]
  moved <- l[kind == "retained", sum(abs(recv_hm))]
  cli::cli_alert_info(
    "Receiver split: {format(l[kind == 'retained', .N], big.mark = ',')} retained disposals, {round(moved, 1)} points of |value| to receivers")
  out
}

#' The offence pool: each team's own slice of its retained disposals (D9)
#'
#' @param l Output of `.np_credit_terms()`.
#' @return Pool rows shaped for `.np_spread_pool()` (`winner_slot` is `NA`, so
#'   the spread is by the rule's non-mirror weighting), or `NULL`.
#' @keywords internal
.np_offence_pool <- function(l) {
  p <- l[team_hm != 0, .(pool_hm = sum(team_hm)), by = .(match_id, def_team = team)]
  if (nrow(p) == 0) return(NULL)
  p[, `:=`(winner_slot = NA_character_, loser_pid = NA_character_)]
  cli::cli_alert_info(
    "Offence pool: {round(sum(abs(l$team_hm)), 1)} points gross across {nrow(p)} team-matches")
  p
}

#' Route what a turnover cedes to the opposition
#'
#' The pool is keyed on the DISPOSER's position, because that is what the mirror
#' map is indexed by: the player who lost the ball tells us which opposing slot
#' was most likely responsible for winning it.
#'
#' @param l Output of `.np_credit_terms()`; `cede_hm` is what each turnover
#'   hands to the opposition (flat: `phi * hm`; difficulty: the defence's share
#'   of the surprise).
#' @param lineup Per-match roster: `match_id`, `team`, `player_id`, `position`,
#'   `tog`, `def_acts`.
#' @param psi Share of the ceded value paid straight to the OBSERVED ball-winner
#'   (the actor on the next row). The remainder is spread by `.np_spread_pool()`.
#' @param by_act Route the ball-winner's share by HOW he won it
#'   (`NP_BALL_WINNER_SHARE_BY_ACT`, keyed on the next row's description)
#'   instead of the flat `psi`. On under difficulty credit (D11).
#' @return A list of `debits` (per disposer, negative of what they ceded),
#'   `won` (paid directly to identified ball-winners), `contest_won` (paid to
#'   the named winner of a contest the defence won) and `pool` (the remainder,
#'   per `match_id`, `def_team`, `winner_slot`).
#' @keywords internal
.np_defensive_pool <- function(l, lineup, psi = 0, by_act = FALSE) {
  if (!is.numeric(psi) || length(psi) != 1 || is.na(psi) || psi < 0 || psi > 1) {
    cli::cli_abort("{.arg ball_winner_share} must be one number in [0, 1], not {.val {psi}}.")
  }
  to <- l[kind == "turnover" | cede_c_hm != 0]
  if (nrow(to) == 0) {
    cli::cli_warn("No turnovers found -- the defensive pool is empty.")
    return(list(debits = NULL, won = NULL, contest_won = NULL, pool = NULL))
  }

  pos <- lineup[, .(match_id, team, player_id, position)]
  to <- merge(to, pos, by = c("match_id", "team", "player_id"), all.x = TRUE)
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
  debits <- to[, .(np_ceded = -sum(cede_hm + cede_c_hm)), by = .(match_id, team, player_id)]

  # This split exists because routing the whole pool by positional mirror was
  # measurably wrong: the mirror of a midfielder who coughs it up is another
  # midfielder, so the credit defenders earned was paid to midfielders. The
  # ball-winner needs no inference -- he is the actor on the very next row.
  to[, has_winner := kind == "turnover" & !is.na(next_player)]
  # The ball-winner's share: flat `psi`, or under difficulty credit a share
  # for how he won it -- an intercept mark or a free is nearly all his, a loose
  # ball 30m from the fall of a smothered kick is mostly other people's pressure
  # (D11). Keyed on what the winner DID (the next row), not on what the kick
  # resolved into, because the contest itself is paid separately.
  to[, psi_row := psi]
  if (isTRUE(by_act)) to[has_winner == TRUE, psi_row := .np_ball_winner_share(next_desc)]
  won <- to[has_winner == TRUE,
            .(np_defensive_won = sum(cede_hm * psi_row)),
            by = .(match_id, team = next_team, player_id = next_player)]
  # Any pool with no identifiable winner keeps its full value for the spread,
  # rather than quietly losing `psi` of it.
  to[, spread_hm := cede_hm * data.table::fifelse(has_winner, 1 - psi_row, 1)]

  # Contest cessions: the named contest winner takes NP_CONTEST_WINNER_SHARE
  # for how the contest was won (a mark is nearly all his, a spoil half), the
  # rest joins the pool for the pressure around the contest. The winner's
  # team is the opposition of the kicker by construction (def_win).
  to[, rho := 0]
  to[cede_c_hm != 0, rho := .np_contest_winner_share(cont_desc)]
  to[cede_c_hm != 0 & is.na(winner_pid), rho := 0]
  # The opposition is the OTHER team in the match, taken from the roster --
  # never from the resolution row, whose team belongs to whoever that lookahead
  # landed on and can disagree with the winner the contest models named
  # (review finding, 2026-09-06). A match has exactly two teams; a turnover's
  # next_team must be that other team, and both are asserted.
  tt <- lineup[, .(teams = list(sort(unique(team)))), by = match_id]
  n_teams <- lengths(tt$teams)
  if (any(n_teams != 2)) {
    cli::cli_abort("{sum(n_teams != 2)} match{?es} do not have exactly two teams on the roster.")
  }
  tt[, `:=`(t1 = vapply(teams, `[`, "", 1L), t2 = vapply(teams, `[`, "", 2L))]
  to[tt, on = "match_id", `:=`(t1 = i.t1, t2 = i.t2)]
  to[, opp_team := data.table::fifelse(team == t1, t2, t1)]
  n_wrong <- to[kind == "turnover" & !is.na(next_team) & next_team != opp_team, .N]
  if (n_wrong > 0) {
    cli::cli_abort("{n_wrong} turnover{?s} have a next_team that is neither team in the match.")
  }
  to[, on_roster := FALSE]
  to[lineup[, .(match_id, team, player_id)], on = .(match_id, opp_team = team, winner_pid = player_id),
     on_roster := TRUE]
  off <- to[cede_c_hm != 0 & !is.na(winner_pid) & !on_roster]
  if (nrow(off) > 0) {
    cli::cli_warn(c(
      "{nrow(off)} contest winner{?s} {?is/are} not on the opposition's roster; {?its/their} share ({round(sum(abs(off$cede_c_hm)), 2)} points) goes to the pool instead.",
      "i" = "The contest models and the roster disagree on who this player is."
    ))
    to[cede_c_hm != 0 & !is.na(winner_pid) & !on_roster, `:=`(winner_pid = NA_character_, rho = 0)]
  }
  cwon <- to[cede_c_hm != 0 & !is.na(winner_pid),
             .(np_contest_won = sum(cede_c_hm * rho)),
             by = .(match_id, team = opp_team, player_id = winner_pid)]
  if (nrow(cwon) == 0) cwon <- NULL
  to[, spread_hm := spread_hm + cede_c_hm * (1 - rho)]
  # keyed by the disposer too, so a context spread can use who was observed
  # contesting HIM
  pool <- to[, .(pool_hm = sum(spread_hm)),
             by = .(match_id, def_team = opp_team, winner_slot, loser_pid = player_id)]
  if (anyNA(pool$def_team)) {
    lost <- pool[is.na(def_team), sum(abs(pool_hm))]
    cli::cli_abort("{round(lost, 2)} points of ceded value have no opposing team to receive them.")
  }

  # Report the GROSS moved, not the net. The net is near zero by construction --
  # home and away turnover debits carry opposite signs in the home-margin frame
  # and cancel across the season.
  cli::cli_alert_info(
    "Defensive pool: {format(sum(to$kind == 'turnover'), big.mark = ',')} turnovers cede {round(sum(abs(to$cede_hm)), 1)} points gross ({if (isTRUE(by_act)) 'ball-winner share by act' else paste0(round(100 * psi), '% to the observed ball-winner')}) plus {round(sum(abs(to$cede_c_hm)), 1)} at contests; rest spread")
  list(debits = debits, won = won, contest_won = cwon, pool = pool,
       rows = to[, .(match_id, display_order, opp_team, psi_row, rho, winner_pid,
                     next_player, has_winner, cede_hm, cede_c_hm)])
}

#' Ball-winner's share of a ceded ground ball, by what he did to win it (D11)
#' @param desc PBP description of the winning row.
#' @return Numeric vector of shares.
#' @keywords internal
.np_ball_winner_share <- function(desc) {
  out <- unname(NP_BALL_WINNER_SHARE_BY_ACT[desc])
  out[is.na(out)] <- NP_BALL_WINNER_SHARE_BY_ACT_DEFAULT
  out
}

#' Named attacker-versus-defender pairings observed in chains (D12)
#'
#' A `Contest Target` row names the player a kick was aimed at; when the very
#' next row is an opponent's act (a spoil, a contested mark, a crumb) it names
#' who beat him. 2026: 12.7 such pairs a match, 13.6% of distinct pairs meeting
#' twice or more in a match. Thin, but it is the only direct evidence of who
#' was matched on whom, and a repeated pairing is a tag.
#'
#' @param chains Raw chains.
#' @return `match_id`, `att`, `def`, `n`.
#' @keywords internal
.np_contest_pairs <- function(chains) {
  ch <- data.table::as.data.table(chains)
  detect_chains_columns(ch)
  c2 <- ch[, .(match_id = as.character(match_id), display_order, description,
               player_id, team_id)]
  data.table::setorder(c2, match_id, display_order)
  c2[, `:=`(n_pid = data.table::shift(player_id, -1L),
            n_tid = data.table::shift(team_id, -1L)), by = match_id]
  pr <- c2[description == "Contest Target" & !is.na(player_id) & !is.na(n_pid) &
             !is.na(team_id) & !is.na(n_tid) & n_tid != team_id,
           .(n = .N), by = .(match_id, att = player_id, def = n_pid)]
  cli::cli_alert_info(
    "Contest pairs: {format(sum(pr$n), big.mark = ',')} attacker-vs-defender pairings over {format(data.table::uniqueN(pr$match_id), big.mark = ',')} matches")
  pr
}

#' Individual share of a contest cession, by how the contest was won (D11)
#' @param desc Chains description of the resolving row.
#' @return Numeric vector of shares.
#' @keywords internal
.np_contest_winner_share <- function(desc) {
  out <- unname(NP_CONTEST_WINNER_SHARE[desc])
  out[is.na(out)] <- NP_CONTEST_WINNER_SHARE_DEFAULT
  out
}

#' Pay a stoppage swing: rucks, the first-possession player, both pools (D15)
#'
#' The swing on a stoppage row is baseline -> first possession, in the
#' home-margin frame. `NP_STOPPAGE_LOSER_SHARE` of it is worn by the side that
#' lost the ball; the rest is credited to the side that won it. Each side's
#' half splits by how the ball came out (`NP_STOPPAGE_SPLIT`): the ruck share
#' goes to that side's rucks in proportion to `hitouts_to_advantage` (winner)
#' or `ruck_contests - hitouts` (loser) that match, the player share to the
#' first-possession player, the rest to the side's pool. A side with no ruck
#' credited passes the ruck share to its pool. A stoppage with no first
#' possession (a stoppage straight into another stoppage) splits its swing
#' between the two pools.
#'
#' @param l Output of `.np_credit_terms()`.
#' @param lineup Roster with `hitouts_to_advantage`, `ruck_contests`, `hitouts`.
#' @param loser_share `NP_STOPPAGE_LOSER_SHARE`.
#' @param split `NP_STOPPAGE_SPLIT`.
#' @return `individual` (`match_id`, `team`, `player_id`, `np_stoppage`),
#'   `pool` rows shaped for `.np_spread_pool()`, and `rows` for the payment
#'   table.
#' @keywords internal
.np_stoppage_credit <- function(l, lineup, loser_share = NP_STOPPAGE_LOSER_SHARE,
                                split = NP_STOPPAGE_SPLIT) {
  st <- l[kind == "stoppage" & hm != 0]
  if (nrow(st) == 0) return(list(individual = NULL, pool = NULL, rows = NULL))
  if (!is.numeric(loser_share) || length(loser_share) != 1 || is.na(loser_share) ||
      loser_share < 0 || loser_share > 1) {
    cli::cli_abort("{.arg stoppage_loser_share} must be one number between 0 and 1 inclusive.")
  }
  for (nm in names(split)) {
    if (abs(sum(split[[nm]]) - 1) > 1e-9) {
      cli::cli_abort("NP_STOPPAGE_SPLIT${nm} must sum to 1, not {sum(split[[nm]])}.")
    }
  }
  tt <- lineup[, .(teams = list(sort(unique(team)))), by = match_id]
  if (any(lengths(tt$teams) != 2)) {
    cli::cli_abort("{sum(lengths(tt$teams) != 2)} match{?es} do not have exactly two teams on the roster.")
  }
  tt[, `:=`(t1 = vapply(teams, `[`, "", 1L), t2 = vapply(teams, `[`, "", 2L))]
  st[tt, on = "match_id", `:=`(t1 = i.t1, t2 = i.t2)]
  st[, has_winner := !is.na(next_team) & !is.na(next_player)]
  st[, `:=`(win_team = next_team,
            lose_team = data.table::fifelse(next_team == t1, t2, t1))]
  st[, how := data.table::fcase(
    next_desc %chin% NP_STOPPAGE_HITOUT_DESCS,   "hitout",
    next_desc %chin% NP_STOPPAGE_RUCK_OWN_DESCS, "ruck_own",
    default = "ground")]
  sp <- data.table::rbindlist(lapply(names(split), function(k)
    data.table::data.table(how = k, s_ruck = split[[k]][["ruck"]],
                           s_player = split[[k]][["player"]], s_pool = split[[k]][["pool"]])))
  st[sp, on = "how", `:=`(s_ruck = i.s_ruck, s_player = i.s_player, s_pool = i.s_pool)]
  st[, `:=`(W = (1 - loser_share) * hm, L = loser_share * hm)]

  # ruck weights per team-match
  rk <- lineup[, .(match_id, team, player_id,
                   w_win = pmax(hitouts_to_advantage, 0),
                   w_lose = pmax(ruck_contests - hitouts, 0))]
  rk[, `:=`(sw_win = sum(w_win), sw_lose = sum(w_lose)), by = .(match_id, team)]
  st[unique(rk[, .(match_id, team, sw_win)]), on = .(match_id, win_team = team), sw_win := i.sw_win]
  st[unique(rk[, .(match_id, team, sw_lose)]), on = .(match_id, lose_team = team), sw_lose := i.sw_lose]
  st[is.na(sw_win), sw_win := 0]; st[is.na(sw_lose), sw_lose := 0]

  # winner side: player, rucks (or pool), pool; loser side: rucks (or pool), pool
  w_player <- st[has_winner == TRUE, .(match_id, display_order, team = win_team,
                                       player_id = next_player, v = W * s_player,
                                       role = "stoppage_player")]
  w_ruck_pool <- st[has_winner == TRUE, .(match_id, display_order, team = win_team,
                                          v_ruck = W * s_ruck * (sw_win > 0),
                                          v_pool = W * s_pool + W * s_ruck * (sw_win == 0))]
  l_ruck_pool <- st[has_winner == TRUE, .(match_id, display_order, team = lose_team,
                                          v_ruck = L * s_ruck * (sw_lose > 0),
                                          v_pool = L * (1 - s_ruck) + L * s_ruck * (sw_lose == 0))]
  # ruck payments: spread each row's v_ruck across that team's rucks by weight
  ruck_pay <- function(rp, wcol, swcol) {
    x <- merge(rp[v_ruck != 0], rk[get(wcol) > 0, .(match_id, team, player_id, w = get(wcol), sw = get(swcol))],
               by = c("match_id", "team"), allow.cartesian = TRUE)
    x[, .(match_id, display_order, team, player_id, v = v_ruck * w / sw, role = "stoppage_ruck")]
  }
  w_ruck <- ruck_pay(w_ruck_pool, "w_win", "sw_win")
  l_ruck <- ruck_pay(l_ruck_pool, "w_lose", "sw_lose")
  # no winner: both pools, half each
  nw <- st[has_winner == FALSE]
  nw_pool <- data.table::rbindlist(list(
    nw[, .(match_id, display_order, team = t1, v_pool = hm / 2)],
    nw[, .(match_id, display_order, team = t2, v_pool = hm / 2)]))
  pool_rows <- data.table::rbindlist(list(
    w_ruck_pool[, .(match_id, display_order, team, v_pool)],
    l_ruck_pool[, .(match_id, display_order, team, v_pool)],
    nw_pool), use.names = TRUE)[v_pool != 0]

  ind <- data.table::rbindlist(list(w_player, w_ruck, l_ruck), use.names = TRUE)
  individual <- ind[, .(np_stoppage = sum(v)), by = .(match_id, team, player_id)]
  pool <- pool_rows[, .(pool_hm = sum(v_pool)), by = .(match_id, def_team = team)]
  pool[, `:=`(winner_slot = NA_character_, loser_pid = NA_character_)]

  paid <- sum(ind$v) + sum(pool_rows$v_pool); owed <- sum(st$hm)
  if (abs(paid - owed) > 1e-6) {
    cli::cli_abort("Stoppage credit does not conserve: owed {round(owed, 4)}, paid {round(paid, 4)}.")
  }
  cli::cli_alert_info(
    "Stoppage credit: {format(nrow(st), big.mark = ',')} stoppages ({format(sum(st$has_winner), big.mark = ',')} with a first possession; {format(st[how == 'hitout', .N], big.mark = ',')} from a hitout); {round(sum(abs(ind[role == 'stoppage_ruck']$v)), 1)} points to rucks, {round(sum(abs(w_player$v)), 1)} to first-possession players, {round(sum(abs(pool_rows$v_pool)), 1)} to pools")
  rows <- data.table::rbindlist(list(
    ind[, .(match_id, display_order, role, team, player_id, hm = v)],
    pool_rows[, .(match_id, display_order, role = "stoppage_pool", team, player_id = NA_character_, hm = v_pool)]),
    use.names = TRUE)
  list(individual = individual, pool = pool, rows = rows)
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
#' @param spread How a team pool is shared across the players on the ground:
#'   `"matchup"` (the disposer's positional mirror takes `mirror_share`, rest by
#'   TOG), `"defensive_acts"` (by box-score defensive acts), `"tog"` (flat by
#'   time on ground) or `"context"` (Pete's D12 mix: observed pairings from
#'   chains contest targets, defensive acts, mirror and TOG, weighted by
#'   `NP_CONTEXT_WEIGHTS`; needs `chains` or `contest_pairs` for the pairings).
#' @param mirror_share Share the mirror slot takes under "matchup".
#' @return A data.table of `match_id`, `team`, `player_id`, `np_defensive`.
#' @keywords internal
.np_spread_pool <- function(pool, lineup, spread, mirror_share, pairs = NULL) {
  if (is.null(pool) || nrow(pool) == 0) return(NULL)
  if (!"loser_pid" %in% names(pool)) pool[, loser_pid := NA_character_]
  grp <- c("match_id", "def_team", "winner_slot", "loser_pid")

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
    context = 0,  # filled below
    cli::cli_abort("Unknown {.arg spread}: {.val {spread}}")
  )]

  if (identical(spread, "matchup")) {
    a[, is_mirror := !is.na(winner_slot) & position == winner_slot]
    a[, n_mirror := sum(is_mirror), by = grp]
    a[, tog_mirror := sum(tog * is_mirror), by = grp]
    a[, tog_other := sum(tog * !is_mirror), by = grp]
    a[, w := data.table::fcase(
      n_mirror == 0,  tog,                                    # no mirror on park
      is_mirror,      mirror_share * tog / pmax(tog_mirror, 1e-9),
      default =       (1 - mirror_share) * tog / pmax(tog_other, 1e-9)
    )]
  }

  if (identical(spread, "context")) {
    # Pete's rule (D12): use as much context as the data gives. Four
    # components, each normalised to sum to 1 across the team, weighted by
    # NP_CONTEXT_WEIGHTS; a component with no support in a pool (no pairing
    # observed, no mirror on the ground) contributes nothing and the rest
    # share the pool pro rata.
    # Pairing evidence outranks the mirror prior, because mirror alone
    # widened the forward/defender gap (EPV-NET-POINTS.md s5).
    cw <- NP_CONTEXT_WEIGHTS
    if (!is.null(pairs) && nrow(pairs) > 0) {
      # Aggregate first: an update-join keeps only the LAST matching row, so a
      # caller-supplied table with a repeated (match, att, def) key would
      # silently lose evidence (review finding, 2026-09-06).
      pr <- data.table::as.data.table(pairs)[, .(n_pair = sum(n)),
                                             by = .(match_id = as.character(match_id),
                                                    loser_pid = att, player_id = def)]
      a[pr, on = .(match_id, loser_pid, player_id), n_pair := i.n_pair]
    }
    if (!"n_pair" %in% names(a)) a[, n_pair := NA_real_]
    a[is.na(n_pair), n_pair := 0]
    a[, is_mirror := !is.na(winner_slot) & !is.na(position) & position == winner_slot]
    a[, `:=`(s_pair = sum(n_pair), s_acts = sum(def_acts), s_mirror = sum(is_mirror),
             s_tog = sum(tog)), by = grp]
    a[, `:=`(
      c_pair   = data.table::fifelse(s_pair > 0, n_pair / s_pair, 0),
      c_acts   = data.table::fifelse(s_acts > 0, def_acts / s_acts, 0),
      c_mirror = data.table::fifelse(s_mirror > 0, as.numeric(is_mirror) / s_mirror, 0),
      c_tog    = data.table::fifelse(s_tog > 0, tog / s_tog, 0))]
    # No explicit renormalisation for components without support: `wsum`
    # below divides by the group's total weight, so a dropped component's
    # share is redistributed pro rata by construction (a per-group constant
    # divisor here would cancel out -- review, 2026-09-06).
    a[, w := cw[["pair"]] * c_pair + cw[["acts"]] * c_acts +
             cw[["mirror"]] * c_mirror + cw[["tog"]] * c_tog]
    n_paired <- data.table::uniqueN(a[s_pair > 0, .SD, .SDcols = grp])
    cli::cli_alert_info(
      "Context spread: {format(n_paired, big.mark = ',')} of {format(data.table::uniqueN(a[, .SD, .SDcols = grp]), big.mark = ',')} pools carry pairing evidence")
  }

  a[!is.finite(w) | w < 0, w := 0]
  a[, wsum := sum(w), by = grp]
  # A group with no weight anywhere still has to be paid: fall back to flat --
  # and SAY SO. Silently degrading a targeted spread rule to flat is exactly the
  # failure this module logs everywhere else. Under `defensive_acts` this fires
  # when a whole team-match recorded no tackles/pressure/spoils/intercepts, which
  # in practice means their player_stats rows failed to join.
  flat_groups <- unique(a[wsum <= 0, .SD, .SDcols = grp])
  if (nrow(flat_groups)) {
    flat_pts <- sum(abs(unique(a[wsum <= 0, .SD, .SDcols = c(grp, "pool_hm")])$pool_hm))
    cli::cli_warn(c(
      "{nrow(flat_groups)} pool group{?s} had zero weight under {.val {spread}} and fell back to a FLAT spread ({round(flat_pts, 1)} points).",
      "i" = "Check that {.arg player_stats} joined for those teams."
    ))
  }
  a[wsum <= 0, w := 1]
  a[, wsum := sum(w), by = grp]
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
#' @section Why "sum" is the default, measured rather than assumed:
#' The identity that matters -- Oliver's, and the one actually asked for -- is
#' that the whole match sums to the margin, which in own-team frames reads as
#' `sum(home) - sum(away) == margin`. **Pinning the SUM alone achieves that
#' exactly.** `level = "half_margin"` adds a second, cosmetic constraint (each
#' team's total lands on `margin/2`) and it is very expensive. Measured over 211
#' matches of 2026:
#'
#' \tabular{lrr}{
#'   \tab `sum` \tab `half_margin` \cr
#'   team difference == margin \tab 4.3e-14 \tab 5.7e-14 \cr
#'   median |np_residual| \tab 0.10 \tab 2.64 \cr
#'   residual as \% of |net_points| \tab 3\% \tab 102\% \cr
#'   Spearman(raw, final) \tab 0.9993 \tab 0.7425
#' }
#'
#' Under `half_margin` the correction is LARGER than the thing it corrects and
#' it reorders players, because it is spread by time on ground and TOG varies
#' (`cor(np_residual, tog) = -0.481`, median 1.50 points of spread between the
#' longest and shortest stint in a team-match). An earlier version of this note
#' claimed the residual "shifts the level without reordering anyone" -- that was
#' asserted, never measured, and it is false.
#'
#' The underlying reason the level needs such a big push is worth knowing: the
#' raw ledger tracks a team's OWN SCORE (cor 0.906) better than the margin
#' (0.786), because `delta_epv` measures scoring production against expectation.
#' In a high-scoring game both teams read strongly positive at once, and the
#' median distance from `margin/2` is 61.4 points. `half_margin` does not fix
#' that; it flattens it with a term big enough to distort the ranking. Keep the
#' level free and report the difference, which is the quantity that is actually
#' anchored.
#'
#' @param np Per-player frame already carrying `np_raw` (home-margin frame),
#'   `margin`, `team`, `home_away` and `tog`.
#' @param level `"sum"` (default) pins only the match total, which is the whole
#'   identity. `"half_margin"` additionally pins each team total to `margin/2`.
#' @return `np` with an `np_residual` column added, by reference.
#' @keywords internal
.np_reconcile <- function(np, level = c("sum", "half_margin")) {
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
#' @param credit `"flat"` (the default) pays fixed shares: `receiver_share` of a
#'   retained disposal to the receiver, `defensive_share` of a turnover to the
#'   opposition. `"difficulty"` follows the v4 identity per disposal (see
#'   `.np_credit_terms()`): the decision term to the disposer, the surprise split
#'   by the modelled chance of losing the ball, `blame_share` of a lost surprise
#'   kept by the disposer, `offence_pool_share` of every non-turnover disposal to
#'   the attacking team's pool. Needs `chains` or `difficulty_terms`.
#' @param blame_share Difficulty only: the disposer's share of a turnover's
#'   surprise. See `NP_BLAME_SHARE`.
#' @param offence_pool_share Difficulty only: the slice of each non-turnover
#'   disposal paid to the attacking team's pool. See `NP_OFFENCE_POOL_SHARE`.
#' @param difficulty_terms Precomputed output of `.np_difficulty_terms()`, so a
#'   share sweep fits the models once. Ignored under `credit = "flat"`.
#' @param leak_safe Difficulty only: fit each season's models on earlier
#'   seasons. With a single season the fit is in-sample and says so.
#' @param contest_pairs Precomputed output of `.np_contest_pairs()` for
#'   `spread = "context"`; computed from `chains` when absent.
#' @param stoppages `"exclude"` (the default) drops centre bounces, ball-ups and
#'   throw-ins as before, so their swing falls into the residual; `"allocate"`
#'   (difficulty credit only) values each at a neutral baseline for its type and
#'   location and pays the swing to the side that won the first possession
#'   (rucks by hitouts to advantage, the first-possession player, the pool) and
#'   against the side that lost it. See `NP_STOPPAGE_SPLIT`.
#' @param stoppage_baseline Precomputed `.np_stoppage_baseline()` table, or
#'   `NULL` to estimate it from the data.
#' @param stoppage_loser_share Share of each stoppage swing worn by the losing
#'   side. See `NP_STOPPAGE_LOSER_SHARE`.
#' @param return_payments Attach the per-act payment table as attribute
#'   `np_payments`: one row per (act, recipient) with `role` (actor, receiver,
#'   contest_winner, ball_winner, attack_pool, defence_pool), the recipient's
#'   `team` and `player_id` (`NA` for a pool, which is spread later) and the
#'   home-margin amount `hm`. This is the role tagging of the design (D3); the
#'   explainer reads it. Pool rows carry what was pooled, not who got it.
#' @param chains Raw chains for the same matches, or `NULL` (the default). With
#'   chains the allocation is **identical** -- every point still comes from a
#'   PBP row, and this is asserted by `data-raw/04-analysis/np_chains_ledger_equivalence.R`
#'   -- but each disposal in the ledger also carries what it resolved into
#'   (a spoil, a contested mark, a goal), which PBP drops. Later credit rules
#'   need that; nothing in this function uses it yet.
#' @param defensive_share Fraction of each turnover paid to the team that won
#'   the ball. Not identifiable from conservation -- see `NP_DEFENSIVE_SHARE`.
#' @param receiver_share Fraction of a retained disposal paid to the receiver.
#' @param ball_winner_share Fraction of each defensive pool paid straight to the
#'   player observed to win the ball (the actor on the next row). The remainder
#'   goes through `spread`, for the pressure that forced the turnover. Routing
#'   the whole pool through `spread` was measurably wrong -- see
#'   `NP_BALL_WINNER_SHARE`.
#'
#'   **Only reaches `credit = "flat"`.** Under `credit = "difficulty"` the share
#'   is keyed on HOW the winner won it (`NP_BALL_WINNER_SHARE_BY_ACT`: an
#'   intercept mark or free is nearly all his, a loose ball mostly other
#'   people's pressure), and that table overwrites this argument on every row
#'   with a named winner. Passing a non-default value there therefore did
#'   nothing at all -- silently, until 2026-09-07, when a gate arm raising it to
#'   0.85 returned numbers identical to the default. That is deliberate, and
#'   `test-epv-net-points.R` asserts it by passing 1 and expecting the by-act
#'   0.8, so it is documented rather than policed. To move the winner's cut
#'   under difficulty credit, edit `NP_BALL_WINNER_SHARE_BY_ACT`.
#' @param spread How the defensive pool is divided: `"matchup"` (the positional
#'   mirror takes `mirror_share`, rest by TOG), `"defensive_acts"` (by box-score
#'   defensive work) or `"tog"` (flat by time on ground).
#' @param mirror_share Share the mirror slot takes when `spread = "matchup"`.
#' @param level `"sum"` (default) pins the match total to the margin, which is
#'   the Oliver identity and all that is required. `"half_margin"` additionally
#'   forces each team's total to `margin/2` -- cosmetic, and it costs a residual
#'   larger than the signal that reorders players. See `.np_reconcile()`.
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
#'     \item{`np_defensive`}{his share of the pressure pools his team won and,
#'       under `stoppages = "allocate"`, of the stoppage pools (each side's
#'       pool slice of every stoppage swing)}
#'     \item{`np_ceded`}{the part of his own turnover debit that was paid to the
#'       opposition instead of to him. Read the SIGN carefully: this is a
#'       positive number that REDUCES his debit, because the debit itself is
#'       already in `np_direct` at full size and this cancels `defensive_share`
#'       of it. It is not a penalty column.}
#'     \item{`np_contest_won`}{paid for contests he won against a kick (a
#'       spoil, an intercept mark), difficulty credit only}
#'     \item{`np_stoppage`}{stoppage swings paid to him as a ruck or as the
#'       first-possession player, both signs (zero unless
#'       `stoppages = "allocate"`)}
#'     \item{`np_team`}{his share of his own team's offence pools (zero under
#'       `credit = "flat"`)}
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
                             chains = NULL,
                             credit = c("flat", "difficulty"),
                             defensive_share = NP_DEFENSIVE_SHARE,
                             receiver_share = NP_RECEIVER_SHARE,
                             ball_winner_share = NP_BALL_WINNER_SHARE,
                             spread = c("matchup", "defensive_acts", "tog", "context"),
                             mirror_share = NP_MIRROR_SHARE,
                             level = c("sum", "half_margin"),
                             reconcile = TRUE,
                             blame_share = NP_BLAME_SHARE,
                             offence_pool_share = NP_OFFENCE_POOL_SHARE,
                             difficulty_terms = NULL,
                             leak_safe = TRUE,
                             contest_pairs = NULL,
                             return_payments = FALSE,
                             stoppages = c("exclude", "allocate"),
                             stoppage_baseline = NULL,
                             stoppage_loser_share = NP_STOPPAGE_LOSER_SHARE) {
  stoppages <- match.arg(stoppages)
  spread <- match.arg(spread)
  level <- match.arg(level)
  credit <- match.arg(credit)
  if (is.null(pbp_data)) pbp_data <- load_pbp(TRUE)
  if (is.null(player_stats)) player_stats <- load_player_stats(TRUE)
  if (is.null(results)) results <- load_results(TRUE)

  if (identical(stoppages, "allocate") && !identical(credit, "difficulty")) {
    cli::cli_abort("{.arg stoppages = \"allocate\"} is part of the difficulty rule set; use {.arg credit = \"difficulty\"}.")
  }
  led <- .np_build_ledger(pbp_data, chains, stoppages, stoppage_baseline)

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
  roster <- attr(led, "np_roster")
  if (is.null(roster)) roster <- unique(led[, .(match_id, team, player_id)])
  for (v in c("hitouts_to_advantage", "ruck_contests", "hitouts")) {
    if (!v %in% names(ps)) data.table::set(ps, j = v, value = NA_real_)
  }
  lineup <- merge(
    roster,
    ps[, .(match_id, player_id, position,
           tog = pmax(time_on_ground_percentage / 100, 0.01),
           def_acts = .def_acts,
           hitouts_to_advantage, ruck_contests, hitouts)],
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
  for (v in c("hitouts_to_advantage", "ruck_contests", "hitouts")) {
    data.table::set(lineup, i = which(is.na(lineup[[v]])), j = v, value = 0)
  }
  if (identical(stoppages, "allocate") && sum(lineup$hitouts_to_advantage) == 0) {
    cli::cli_abort(c(
      "No player has any hitouts to advantage in {.arg player_stats}.",
      "x" = "Every stoppage's ruck share would fall through to the pool without saying so."
    ))
  }

  # --- allocate -------------------------------------------------------------
  terms <- NULL
  if (identical(credit, "difficulty")) {
    if (!is.null(difficulty_terms)) {
      terms <- difficulty_terms
    } else if (is.null(chains)) {
      cli::cli_abort(c(
        "{.arg credit = \"difficulty\"} needs {.arg chains} (kick length and landing come from the chains rows) or precomputed {.arg difficulty_terms}.",
        "i" = "Pass {.code chains = load_chains(...)} for the same matches."
      ))
    } else {
      terms <- .np_difficulty_terms(pbp_data, chains, leak_safe)
    }
  }
  l <- .np_credit_terms(led, credit, receiver_share, defensive_share,
                        blame_share, offence_pool_share, terms)
  pairs <- NULL
  if (identical(spread, "context")) {
    if (!is.null(contest_pairs)) {
      pairs <- contest_pairs
    } else if (!is.null(chains)) {
      pairs <- .np_contest_pairs(chains)
    } else {
      cli::cli_warn("{.arg spread = \"context\"} without {.arg chains}: no pairing evidence, spreading by defensive acts, mirror and time on ground only.")
    }
  }
  direct <- .np_direct_credit(l)
  # `ball_winner_share` is deliberately overridden under difficulty credit: the
  # by-act table decides how much of the pool the winner keeps, and three tests
  # in test-epv-net-points.R assert exactly that by passing 1 and expecting the
  # by-act 0.8. An abort added here on 2026-09-07 broke that contract and took
  # 21 assertions out of the suite; the argument is documented rather than
  # policed. What the episode was really about is that a GATE ARM varying it
  # under difficulty credit tests nothing -- vary NP_BALL_WINNER_SHARE_BY_ACT.
  .by_act <- identical(credit, "difficulty")
  dp <- .np_defensive_pool(l, lineup, ball_winner_share, by_act = .by_act)
  sc <- .np_stoppage_credit(l, lineup, stoppage_loser_share)
  pool_all <- data.table::rbindlist(list(dp$pool, sc$pool), use.names = TRUE, fill = TRUE)
  if (!is.null(pool_all) && nrow(pool_all) == 0) pool_all <- NULL
  alloc <- .np_spread_pool(pool_all, lineup, spread, mirror_share, pairs)
  team_alloc <- .np_spread_pool(.np_offence_pool(l), lineup, spread, mirror_share, pairs)
  if (!is.null(team_alloc)) data.table::setnames(team_alloc, "np_defensive", "np_team")

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
  if (!is.null(team_alloc)) {
    np <- merge(np, team_alloc, by = c("match_id", "team", "player_id"), all = TRUE)
  } else {
    np[, np_team := 0]
  }
  if (!is.null(dp$contest_won)) {
    np <- merge(np, dp$contest_won, by = c("match_id", "team", "player_id"), all = TRUE)
  } else {
    np[, np_contest_won := 0]
  }
  if (!is.null(sc$individual)) {
    np <- merge(np, sc$individual, by = c("match_id", "team", "player_id"), all = TRUE)
  } else {
    np[, np_stoppage := 0]
  }
  for (v in c("np_direct", "np_ceded", "np_defensive", "np_defensive_won", "np_team",
              "np_contest_won", "np_stoppage")) {
    data.table::set(np, i = which(is.na(np[[v]])), j = v, value = 0)
  }
  np[, np_raw := np_direct + np_ceded + np_defensive + np_defensive_won + np_team +
        np_contest_won + np_stoppage]

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
             "np_team", "np_contest_won", "np_stoppage", "np_residual")
  for (v in parts) {
    data.table::set(np, j = v, value = np[[v]] * np$.sgn)
  }
  np[, net_points := net_points_hm * .sgn]

  out <- np[, .(match_id, team, player_id, home_away,
                np_direct, np_defensive_won, np_contest_won, np_defensive,
                np_ceded, np_team, np_stoppage, np_residual, net_points_hm,
                net_points, margin)]
  # The parts must sum to the whole, in whichever frame they are read.
  gap <- max(abs(rowSums(as.matrix(out[, ..parts])) - out$net_points))
  if (gap > 1e-8) {
    cli::cli_abort("Net points components do not sum to the total (max gap {signif(gap, 3)}).")
  }
  if (isTRUE(return_payments)) {
    pay <- list(
      l[own_hm != 0, .(match_id, display_order, role = "actor", team, player_id, hm = own_hm)],
      l[recv_hm != 0, .(match_id, display_order, role = "receiver", team, player_id = next_player, hm = recv_hm)],
      l[win_hm != 0 & !is.na(winner_pid), .(match_id, display_order, role = "contest_winner", team, player_id = winner_pid, hm = win_hm)],
      l[team_hm != 0, .(match_id, display_order, role = "attack_pool", team, player_id = NA_character_, hm = team_hm)]
    )
    if (!is.null(dp$rows)) {
      r <- dp$rows
      pay <- c(pay, list(
        r[has_winner == TRUE & cede_hm != 0, .(match_id, display_order, role = "ball_winner", team = opp_team, player_id = next_player, hm = cede_hm * psi_row)],
        r[cede_c_hm != 0 & !is.na(winner_pid) & rho > 0, .(match_id, display_order, role = "contest_winner", team = opp_team, player_id = winner_pid, hm = cede_c_hm * rho)],
        r[, .(match_id, display_order, role = "defence_pool", team = opp_team, player_id = NA_character_,
              hm = cede_hm * data.table::fifelse(has_winner, 1 - psi_row, 1) + cede_c_hm * (1 - rho))][hm != 0]
      ))
    }
    if (!is.null(sc$rows)) pay <- c(pay, list(sc$rows))
    pay <- data.table::rbindlist(pay, use.names = TRUE)
    paid <- sum(pay$hm); owed <- sum(l$hm)
    if (abs(paid - owed) > 1e-6) {
      cli::cli_abort("Payment table does not rebuild the ledger: paid {round(paid, 4)}, ledger {round(owed, 4)}.")
    }
    data.table::setorder(pay, match_id, display_order, role)
    data.table::setattr(out, "np_payments", pay)
  }
  data.table::setattr(out, "np_params",
                      list(defensive_share = defensive_share,
                           receiver_share = receiver_share,
                           ball_winner_share = ball_winner_share,
                           spread = spread, mirror_share = mirror_share,
                           level = level, reconciled = isTRUE(reconcile),
                           chains = !is.null(chains), credit = credit,
                           blame_share = blame_share,
                           offence_pool_share = offence_pool_share,
                           leak_safe = leak_safe, stoppages = stoppages,
                           stoppage_loser_share = stoppage_loser_share))
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


#' Difficulty terms for one season, fitted on another (the pipeline's entry point)
#'
#' Loads only the columns the models read for the training season, fits, scores
#' the target season and frees the training data. The default training season
#' is the one before; the first season we hold chains for (2021) is fitted on
#' the one after, a reverse fit that touches only that season (D14).
#'
#' @param season Season to score.
#' @param pbp_data,chains The target season's data (loaded when `NULL`).
#' @param train_season Season to fit on. Defaults to `season - 1`, or
#'   `season + 1` when `season` is the earliest in `available`.
#' @param available Seasons chains exist for.
#' @return `.np_difficulty_terms()` output for `season`.
#' @export
np_difficulty_terms_for_season <- function(season, pbp_data = NULL, chains = NULL,
                                           train_season = NULL,
                                           available = 2021:get_afl_season()) {
  season <- as.integer(season)
  if (is.null(train_season)) {
    train_season <- if (season <= min(available)) season + 1L else season - 1L
  }
  if (is.null(pbp_data)) pbp_data <- load_pbp(season, rounds = TRUE)
  if (is.null(chains)) chains <- load_chains(season, rounds = TRUE)
  cli::cli_alert_info("Difficulty terms for {season}: fitting on {train_season}")
  tr_pbp <- load_pbp(train_season, rounds = TRUE,
                     columns = c("match_id", "display_order", "exp_pts", "delta_epv"))
  tr_ch <- load_chains(train_season, rounds = TRUE)
  on.exit(rm(tr_pbp, tr_ch), add = TRUE)
  .np_difficulty_terms(pbp_data, chains, leak_safe = FALSE,
                       train_pbp = tr_pbp, train_chains = tr_ch)
}

#' Net points as the v4 EPV engine's player-game frame
#'
#' The margin each match comes from the official results where they exist and
#' from the play-by-play's own running score otherwise, so a match in progress
#' balances to the score so far. The two disagree more than you would think:
#' on 2026 the booked running score differed from the official result in 24%
#' of matches, by up to 6 points, so the official margin is the one the ledger
#' is pinned to and the difference sits in the reconciliation residual.
#'
#' @param pbp_data Play-by-play for the matches to rate.
#' @param player_stats Box-score stats for the same matches.
#' @param chains Raw chains for the same matches.
#' @param difficulty_terms Precomputed terms (leak-safe from
#'   `np_difficulty_terms_for_season()`); fitted in-sample, with a warning,
#'   when `NULL`.
#' @param results Official results, or `NULL` to load them for the seasons in
#'   `pbp_data` (falling back to the running score when that fails).
#' @return `build_net_points()` output under the v4 rule set.
#' @keywords internal
.np_engine_frame <- function(pbp_data, player_stats, chains, difficulty_terms = NULL,
                             results = NULL) {
  p <- data.table::as.data.table(pbp_data)
  need <- c("match_id", "home_team_name", "away_team_name", "home_points", "away_points")
  miss <- setdiff(need, names(p))
  if (length(miss)) cli::cli_abort("Play-by-play is missing {.val {miss}} for the v4 engine.")
  data.table::setorder(p, match_id, display_order)
  res <- p[, .(home_team_name = torp_replace_teams(home_team_name[1]),
               away_team_name = torp_replace_teams(away_team_name[1]),
               home_score = home_points[.N], away_score = away_points[.N]),
           by = match_id]
  bad <- res[is.na(home_score) | is.na(away_score)]
  if (nrow(bad)) cli::cli_abort("{nrow(bad)} match{?es} have no running score on the last play-by-play row.")

  if (is.null(results)) {
    seasons <- sort(unique(as.integer(substr(res$match_id, 5, 8))))
    results <- tryCatch(load_results(seasons), error = function(e) {
      cli::cli_alert_warning("Official results unavailable ({conditionMessage(e)}); using the running score.")
      NULL
    })
  }
  if (!is.null(results)) {
    off <- data.table::as.data.table(results)[
      !is.na(home_score) & !is.na(away_score),
      .(match_id = as.character(match_id), off_home = home_score, off_away = away_score)]
    res[off, on = "match_id", `:=`(off_home = i.off_home, off_away = i.off_away)]
    n_off <- sum(!is.na(res$off_home))
    n_diff <- res[!is.na(off_home) & (off_home - off_away) != (home_score - away_score), .N]
    cli::cli_alert_info(
      "v4 margins: {n_off} of {nrow(res)} matches from official results ({n_diff} differ from the running score), {nrow(res) - n_off} from the running score")
    if (n_off == 0 && nrow(off) > 0 && nrow(res) >= 5) {
      cli::cli_abort(c(
        "Official results were supplied for {nrow(off)} match{?es} but none of the {nrow(res)} play-by-play matches matched.",
        "x" = "That is a match_id format or competition mismatch, not a live match; refusing to fall back to the running score for a whole season."
      ))
    }
    res[!is.na(off_home), `:=`(home_score = off_home, away_score = off_away)]
    res[, c("off_home", "off_away") := NULL]
  }
  np <- build_net_points(pbp_data, player_stats, res, chains = chains,
                         credit = "difficulty", stoppages = "allocate",
                         difficulty_terms = difficulty_terms,
                         leak_safe = is.null(difficulty_terms),
                         return_payments = isTRUE(NP_TEAM_MARGIN_CONVENTION))
  if (isTRUE(NP_TEAM_MARGIN_CONVENTION)) {
    np <- .np_team_margin(np, pbp_data, player_stats, res)
  }
  np
}


#' Re-allocate so each team's players sum to that team's OWN margin
#'
#' @description The convention ESPN's Net Points uses: a team's players total
#'   that team's margin, +63 and -63, rather than only the difference between
#'   the two sides being pinned. Every row's value is allocated TWICE -- as
#'   credit to the side that gained it and as blame to the side that conceded
#'   it -- so each point of margin appears once on each team's books.
#'
#'   Within a side, each row splits `NP_TEAM_MARGIN_NAMED_SHARE` to the named
#'   player and the rest to a team pool, spread by `NP_TEAM_MARGIN_POOL_BY`.
#'   Where a side has no named recipient on a row -- most rows, because a
#'   retained disposal names nobody on defence -- the whole amount pools.
#'
#' @section Why this runs inside the engine:
#' An earlier attempt applied the same transform to `epv` on a FINISHED
#' player-game frame. The rating layer reads position- and opponent-adjusted
#' columns derived upstream, so that edit was inert: both arms of a gate
#' produced byte-identical ratings while a guard comparing the raw column
#' reported them as 5.5 points apart (2026-09-07). Anything that changes value
#' has to change it here, before the channels are mapped and the adjusted
#' columns are built.
#'
#' @param np `build_net_points()` output carrying the `np_payments` attribute.
#' @param pbp_data,player_stats,res As given to the engine.
#' @return `np` with each team's players summing to that team's own margin. The
#'   difference is booked into `np_team` rather than rescaling every component,
#'   because rescaling explodes for players whose parts nearly cancel. The
#'   components still sum to `net_points` for each player.
#' @keywords internal
.np_team_margin <- function(np, pbp_data, player_stats, res) {
  pay <- attr(np, "np_payments")
  if (is.null(pay) || nrow(pay) == 0) {
    cli::cli_abort(c(
      "{.fn .np_team_margin} needs the payment table.",
      "i" = "Build with {.code return_payments = TRUE}."
    ))
  }
  pay <- data.table::as.data.table(pay)
  np <- data.table::as.data.table(np)
  p <- data.table::as.data.table(pbp_data)
  pay[, match_id := as.character(match_id)]
  ha <- unique(p[, .(match_id = as.character(match_id), team, home_away)])
  pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)
  pay[, own := hm * data.table::fifelse(home_away == "Home", 1, -1)]
  pay[, v := sum(hm), by = .(match_id, display_order)]
  pay <- pay[abs(v) > 1e-12]
  pay[, gain_home := v > 0]
  pay[, side := data.table::fifelse((home_away == "Home") == gain_home, "gain", "concede")]
  pay[, side_sum := sum(own), by = .(match_id, display_order, side)]
  pay[, target := data.table::fifelse(side == "gain", abs(v), -abs(v))]
  # A side whose payments nearly cancel would need a huge multiplier, which
  # would invent value out of rounding. Those pool instead.
  pay[, scaled := data.table::fifelse(abs(side_sum) > 0.05 * abs(v),
                                      own * target / side_sum, NA_real_)]

  # The named/pool split is computed on `pay` directly rather than through a
  # rebuilt allocation table. An earlier version disagreed with an independent
  # row-by-row audit by 5.3 points on one player (Grundy, 2026 R14: audit +2.99,
  # engine -2.36), and three attempts to explain the difference by reading the
  # code all failed. This is the form data-raw/04-analysis/np_row_audit.R
  # verifies, so the two agree by construction. If they diverge again, run it.
  ns <- NP_TEAM_MARGIN_NAMED_SHARE
  # NAMED payments only. Summing every payment on the row, pool included, makes
  # each named player's share smaller than the half he is owed: on 2026 R14 that
  # leaked 34 points of Sydney's 150 and 28 of Carlton's 78 into the
  # reconciliation, where it was spread by minutes instead of going to the
  # players who earned it.
  pay[, named_tot := sum(scaled[!is.na(player_id)], na.rm = TRUE),
      by = .(match_id, display_order, team)]
  if (is.finite(ns)) {
    # Each side keeps `ns` of its own charge on a row, divided by the shares the
    # rescale already gave it. A side whose named payments nearly cancel would
    # need a huge multiplier, so it pools instead. The guard is relative to the
    # row: an absolute epsilon lets a multiplier of a thousand through on a
    # small row, which is how an earlier version put channel values in the
    # hundreds.
    pay[, keep := !is.na(scaled) & !is.na(player_id) &
          abs(named_tot) > 0.05 * abs(target)]
    pay[keep == TRUE, conv := scaled * ns * target / named_tot]
    namd <- pay[keep == TRUE, .(named = sum(conv)),
                by = .(match_id, team, player_id = as.character(player_id))]
    prow <- pay[, .(target = target[1], any_named = any(keep)),
                by = .(match_id, display_order, team)]
    prow[, pool_row := data.table::fifelse(any_named, (1 - ns) * target, target)]
  } else {
    namd <- pay[!is.na(scaled) & !is.na(player_id), .(named = sum(scaled)),
                by = .(match_id, team, player_id = as.character(player_id))]
    # The pool is the charge MINUS THE NAMED PART, not minus every rescaled
    # payment. Subtracting all of them double-counts the pool's own payments
    # and leaves named + pool short of the charge. The invariant below caught
    # it the first time this branch ran, which is what the invariant is for.
    prow <- pay[, .(target = target[1],
                    got_named = sum(scaled[!is.na(player_id)], na.rm = TRUE)),
                by = .(match_id, display_order, team)]
    prow[, pool_row := target - got_named]
  }
  pool <- prow[, .(pool = sum(pool_row)), by = .(match_id, team)]

  # THE INVARIANT, with an honest note on its reach. Named + pool == the
  # side's charge has real teeth only when `ns` is finite. In the shipped NA
  # branch the pool is DEFINED as charge minus named, so the identity holds by
  # algebra whatever the named part is; a review caught me claiming otherwise.
  # The NA branch is policed by the rescale check below instead, which is a
  # real statement about the doubling step and can fail.
  .nm <- namd[, .(named = sum(named)), by = .(match_id, team)]
  .tg <- prow[, .(charged = sum(target)), by = .(match_id, team)]
  .id <- merge(merge(.nm, pool, by = c("match_id", "team"), all = TRUE),
               .tg, by = c("match_id", "team"), all = TRUE)
  for (.c in c("named", "pool", "charged")) {
    data.table::set(.id, i = which(is.na(.id[[.c]])), j = .c, value = 0)
  }
  .gap <- max(abs(.id$named + .id$pool - .id$charged))
  if (!is.finite(.gap) || .gap > 1e-6) {
    cli::cli_abort(c(
      "Team-margin convention: named + pool does not equal the side's charge (gap {signif(.gap, 3)}).",
      "i" = "Audit it with {.file data-raw/04-analysis/np_row_audit.R}."))
  }
  # The rescale check: on every row-side the convention touched, the payments
  # must add up to that side's full charge. This one can fail in either branch.
  .rs <- pay[!is.na(scaled), .(got = sum(scaled), want = target[1]),
             by = .(match_id, display_order, team)]
  .rgap <- max(abs(.rs$got - .rs$want))
  if (!is.finite(.rgap) || .rgap > 1e-6) {
    cli::cli_abort(c(
      "Team-margin convention: a rescaled row-side does not sum to its charge (gap {signif(.rgap, 3)}).",
      "x" = "The doubling step is not conserving value."))
  }

  ps <- data.table::as.data.table(player_stats)
  dz <- function(x) pmax(dplyr::coalesce(as.numeric(x), 0), 0)
  lu <- ps[, .(match_id = as.character(match_id), player_id = as.character(player_id),
               tog = pmax(time_on_ground_percentage, 1) / 100,
               dacts = dz(tackles) + dz(intercepts) + dz(one_percenters))]
  lu <- merge(lu, unique(np[, .(match_id = as.character(match_id),
                                player_id = as.character(player_id), team)]),
              by = c("match_id", "player_id"))
  # `time_on_ground_percentage` is NA for some rows in player_stats, and both
  # weightings below divide by a GROUP SUM of it. A group sum is a scalar, so a
  # single NA does not spoil one player, it makes every value on his team NA and
  # the abort downstream then blames the components rather than the lineup.
  # (`dacts` cannot be NA: dz() coalesces it.)
  .miss <- lu[is.na(tog)]
  if (nrow(.miss) > 0) {
    cli::cli_warn(c(
      "{nrow(.miss)} player{?s} have no time on ground; defaulting to 0.75 so the pool split stays finite.",
      "i" = "First few: {.val {utils::head(unique(.miss$player_id), 5)}}."))
    lu[is.na(tog), tog := 0.75]
  }
  lu[, w := if (identical(NP_TEAM_MARGIN_POOL_BY, "tog")) tog else pmax(dacts, 0.5)]

  ros <- merge(lu, pool, by = c("match_id", "team"), all.x = TRUE)[is.na(pool), pool := 0]
  ros[, share := pool * w / sum(w), by = .(match_id, team)]
  out <- merge(ros[, .(match_id, team, player_id, tog, share)], namd,
               by = c("match_id", "team", "player_id"), all = TRUE)
  out[is.na(named), named := 0][is.na(share), share := 0][, val := named + share]
  # This merge is all = TRUE, so a player who took a named payment but has no
  # row in player_stats lands here with tog NA -- which the guard above cannot
  # see, because `lu` is built with an inner merge and he is simply absent from
  # it. The reconciliation below divides by sum(tog), so leaving it would take
  # his whole team with him.
  if (anyNA(out$tog)) {
    .nolu <- unique(out[is.na(tog)]$player_id)
    cli::cli_warn(c(
      "{length(.nolu)} paid player{?s} have no lineup row; defaulting time on ground to 0.75.",
      "i" = "First few: {.val {utils::head(.nolu, 5)}}."))
    out[is.na(tog), tog := 0.75]
  }

  mg <- data.table::as.data.table(res)[, .(match_id = as.character(match_id),
                                           margin = home_score - away_score)]
  chk <- merge(merge(out[, .(tot = sum(val)), by = .(match_id, team)], ha,
                     by = c("match_id", "team")), mg, by = "match_id")
  chk[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
  out <- merge(out, chk[, .(match_id, team, short = want - tot)],
               by = c("match_id", "team"))
  # Keep the reconciliation as its own column rather than folding it into
  # `val`. The attribute below advertises three parts, and a reader tracing one
  # player's number by hand needs to see all three separately.
  out[, recon := short * tog / sum(tog), by = .(match_id, team)]
  out[, val := val + recon]

  # Book the CHANGE against the pool channel rather than rescaling every
  # component by new/old. Rescaling explodes when a player has large
  # offsetting components summing to almost nothing: 1.8% of player-games
  # total under 0.5 points, and multiplying their parts by 20 put channel
  # values in the hundreds, which the per-channel standardisation downstream
  # would have turned into nonsense. The convention re-allocates value at the
  # team level, so the team pool is the honest home for the difference.
  np[, match_id := as.character(match_id)]
  np[, player_id := as.character(player_id)]
  np <- merge(np, out[, .(match_id, player_id, .new = val)],
              by = c("match_id", "player_id"), all.x = TRUE)
  np[is.na(.new), .new := 0]
  np[, np_team := np_team + (.new - net_points)]
  np[, net_points := .new]
  np[, .new := NULL]
  parts <- intersect(c("np_direct", "np_defensive_won", "np_contest_won",
                       "np_defensive", "np_ceded", "np_team", "np_stoppage",
                       "np_residual"), names(np))
  psum <- rowSums(as.matrix(np[, ..parts]))
  pgap <- max(abs(psum - np$net_points))
  if (!is.finite(pgap) || pgap > 1e-8) {
    cli::cli_abort("Team-margin convention: components stopped summing to the total (gap {signif(pgap, 3)}).")
  }

  fin <- merge(np[, .(tot = sum(net_points)), by = .(match_id, team)], ha,
               by = c("match_id", "team"))
  fin <- merge(fin, mg, by = "match_id")
  fin[, want := margin * data.table::fifelse(home_away == "Home", 1, -1)]
  gap <- max(abs(fin$tot - fin$want))
  if (!is.finite(gap) || gap > 1e-6) {
    cli::cli_abort(c(
      "Team-margin convention: a team total is {signif(gap, 3)} off its own margin.",
      "x" = "The identity this convention exists for does not hold."
    ))
  }
  # expose the per-player split so the arithmetic can be audited: named part,
  # pool slice and reconciliation, which is what a reader needs to see when a
  # number looks wrong
  data.table::setattr(np, "np_team_margin_parts",
                      out[, .(match_id, player_id, team, named, share, recon, val)])
  cli::cli_alert_info(
    "Team-margin convention: every team sums to its own margin (max gap {signif(gap, 2)}); named share {ns}, pool by {NP_TEAM_MARGIN_POOL_BY}.")
  np[]
}


#' Map a Net Points frame onto the v4 engine's three channels
#'
#' Own acts (decisions, surprises, what he received, net of what his losses
#' ceded), what he won back (turnovers, contests, stoppages), and his share of
#' the pools plus the reconciliation residual. The three sum to `net_points`.
#'
#' @param np Output of `build_net_points()`.
#' @return `player_id`, `match_id`, `np_own`, `np_won`, `np_pool`, `net_points`.
#' @keywords internal
.np_v4_channels <- function(np) {
  out <- np[, .(player_id, match_id,
                np_own = np_direct + np_ceded,
                np_won = np_defensive_won + np_contest_won + np_stoppage,
                np_pool = np_defensive + np_team + np_residual,
                net_points)]
  gap <- max(abs(out$np_own + out$np_won + out$np_pool - out$net_points))
  if (!is.finite(gap) || gap > 1e-8) {
    cli::cli_abort("v4 channels do not sum to net points (max gap {signif(gap, 3)}).")
  }
  out
}
