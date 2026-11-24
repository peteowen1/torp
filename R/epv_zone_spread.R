# Positional mirror arm: spread the turnover pool by zone (NP_POOL_SPREAD = "zone")
# Plan: docs/plans/POSITIONAL-MIRROR-ARM.md. Nothing here runs unless the pool
# spread is "zone"; under "matchup" (production) these functions are never called.

#' Defending zone of a lineup slot (NA for an unknown slot)
#' @param slot Character vector of `lineup_position` slots.
#' @return Character vector of zones, same length.
#' @keywords internal
.np_zone_of <- function(slot) {
  unname(NP_ZONE_OF_SLOT[slot])
}

#' Per kick: the named contest defender and the contest target
#'
#' The defender comes from the play-by-play kick row (`contest_defender_id`,
#' set by `add_contest_vars_dt()` when the row after a contest target is an
#' opponent at the same spot). The target comes from the RAW chains, where a
#' `Contest Target` row exists whether or not a defender is named; it is mapped
#' to the kick up to five rows before it, the same lookback
#' `add_contest_vars_dt()` uses.
#'
#' @param pbp_data Play-by-play.
#' @param chains Raw chains for the same matches.
#' @return `match_id`, `display_order` (the kick), `dfd`, `tgt`.
#' @keywords internal
.np_zone_keys <- function(pbp_data, chains) {
  p <- data.table::as.data.table(pbp_data)
  need <- c("match_id", "display_order", "contest_defender_id")
  if (!all(need %in% names(p))) {
    cli::cli_abort("{.arg pbp_data} is missing {.val {setdiff(need, names(p))}}; the zone spread needs the contest columns.")
  }
  pk <- p[!is.na(contest_defender_id),
          .(match_id = as.character(match_id), display_order = as.integer(display_order),
            dfd = as.character(contest_defender_id))]
  pk <- unique(pk, by = c("match_id", "display_order"))

  tk <- NULL
  if (!is.null(chains)) {
    ch <- data.table::copy(
      if (data.table::is.data.table(chains)) chains else data.table::as.data.table(chains))
    detect_chains_columns(ch)
    c2 <- ch[, .(match_id = as.character(match_id), display_order = as.integer(display_order),
                 description, player_id = as.character(player_id))]
    data.table::setorder(c2, match_id, display_order)
    kd <- c("Kick", "Ground Kick")
    for (k in 1:5) {
      c2[, c(paste0(".d", k), paste0(".o", k)) :=
           list(data.table::shift(description, k), data.table::shift(display_order, k)),
         by = match_id]
    }
    ct <- c2[description %chin% CHAINS_CONTEST_TARGET_DESCS & !is.na(player_id)]
    ct[, kick_do := data.table::fcase(.d1 %chin% kd, .o1, .d2 %chin% kd, .o2,
                                      .d3 %chin% kd, .o3, .d4 %chin% kd, .o4,
                                      .d5 %chin% kd, .o5, default = NA_integer_)]
    tk <- unique(ct[!is.na(kick_do), .(match_id, display_order = kick_do, tgt = player_id)],
                 by = c("match_id", "display_order"))
  } else {
    cli::cli_warn("Zone spread without {.arg chains}: no contest targets, so unnamed contests are keyed on the disposer's zone.")
  }
  out <- if (is.null(tk)) pk[, tgt := NA_character_][] else
    merge(pk, tk, by = c("match_id", "display_order"), all = TRUE)
  out
}

#' Fit the zone mixes: which defending zone takes the pool, by source zone
#'
#' Two key types. `T:<zone>`: named contests, the target's zone against the
#' named defender's zone. `D:<zone>`: plain turnovers (a disposal whose next
#' play-by-play row is the other team, with no contest target), the disposer's
#' zone against the zone of the player who won the ball. Each source zone's mix
#' is shrunk toward that key type's overall mix by `shrink_n` pseudo-events.
#'
#' @param pbp_data,chains,player_stats One season's inputs (the previous season
#'   for a leak-safe fit). `player_stats` supplies `position` (the slot).
#' @param shrink_n See `NP_ZONE_SHRINK_N`.
#' @return `zone_key`, `def_zone`, `share`, `n_src` (events behind the source zone).
#' @keywords internal
.np_fit_zone_mix <- function(pbp_data, chains, player_stats, shrink_n = NP_ZONE_SHRINK_N) {
  ps <- data.table::as.data.table(player_stats)
  slots <- unique(ps[, .(match_id = as.character(match_id), player_id = as.character(player_id),
                         zone = .np_zone_of(position))], by = c("match_id", "player_id"))
  p <- data.table::as.data.table(pbp_data)[
    , .(match_id = as.character(match_id), display_order = as.integer(display_order),
        description, team_id, player_id = as.character(player_id),
        contest_target_id = as.character(contest_target_id),
        contest_defender_id = as.character(contest_defender_id))]
  data.table::setorder(p, match_id, display_order)
  p[, `:=`(n_tid = data.table::shift(team_id, -1L), n_pid = data.table::shift(player_id, -1L)),
    by = match_id]

  # T: named contests
  tt <- p[!is.na(contest_target_id) & !is.na(contest_defender_id),
          .(match_id, src = contest_target_id, dst = contest_defender_id)]
  # D: plain turnovers with no contest target (named or chains-only)
  keys <- .np_zone_keys(pbp_data, chains)
  dd <- p[description %chin% c("Kick", "Ground Kick", "Handball") & !is.na(n_tid) &
            team_id != n_tid & !is.na(n_pid)]
  dd <- dd[!keys[!is.na(tgt) | !is.na(dfd)], on = .(match_id, display_order)]
  dd <- dd[, .(match_id, src = player_id, dst = n_pid)]

  one <- function(ev, type) {
    ev[slots, on = .(match_id, src = player_id), sz := i.zone]
    ev[slots, on = .(match_id, dst = player_id), dz := i.zone]
    used <- ev[!is.na(sz) & !is.na(dz)]
    cli::cli_alert_info(
      "Zone mix {type}: {format(nrow(used), big.mark = ',')} of {format(nrow(ev), big.mark = ',')} events have both slots")
    if (nrow(used) == 0) {
      cli::cli_abort("Zone mix {type}: no events with both slots; cannot fit.")
    }
    cnt <- used[, .N, by = .(src_zone = sz, def_zone = dz)]
    overall <- cnt[, .(n = sum(N)), by = def_zone][, p0 := n / sum(n)]
    grid <- data.table::CJ(src_zone = unique(cnt$src_zone), def_zone = overall$def_zone)
    grid[cnt, on = .(src_zone, def_zone), N := i.N]
    grid[is.na(N), N := 0L]
    grid[overall, on = "def_zone", p0 := i.p0]
    grid[, n_src := sum(N), by = src_zone]
    grid[, share := (N + shrink_n * p0) / (n_src + shrink_n)]
    grid[, .(zone_key = paste0(type, ":", src_zone), def_zone, share, n_src)]
  }
  out <- data.table::rbindlist(list(one(tt, "T"), one(dd, "D")))
  bad <- out[, abs(sum(share) - 1) > 1e-9, by = zone_key][V1 == TRUE]
  if (nrow(bad)) cli::cli_abort("Zone mix shares do not sum to 1 for {.val {bad$zone_key}}.")
  out
}

#' Key each turnover-pool row for the zone spread
#'
#' `P:<pid>` when the kick names a contest defender who is on the defending
#' side's roster; `T:<zone>` when it had a contest target (the target's zone);
#' `D:<zone>` otherwise (the disposer's zone); `NA` when no slot is known, which
#' spreads by time on ground.
#'
#' @param to The turnover rows inside `.np_defensive_pool()` (carrying
#'   `position` for the disposer and `opp_team`).
#' @param lineup Per-match roster with `position`.
#' @param zone_keys Output of `.np_zone_keys()`.
#' @return `to`, with `zone_key` added by reference.
#' @keywords internal
.np_assign_zone_key <- function(to, lineup, zone_keys) {
  zk <- data.table::copy(zone_keys)
  to[, `:=`(.mid = as.character(match_id), .do = as.integer(display_order))]
  to[zk, on = .(.mid = match_id, .do = display_order), `:=`(.dfd = i.dfd, .tgt = i.tgt)]
  if (!".dfd" %in% names(to)) to[, .dfd := NA_character_]
  if (!".tgt" %in% names(to)) to[, .tgt := NA_character_]
  ros <- lineup[, .(match_id = as.character(match_id), team, player_id = as.character(player_id), position)]
  to[, .dfd_ok := FALSE]
  to[ros, on = .(.mid = match_id, opp_team = team, .dfd = player_id), .dfd_ok := TRUE]
  n_off <- to[!is.na(.dfd) & !.dfd_ok, .N]
  if (n_off > 0) {
    cli::cli_alert_warning(
      "Zone spread: {n_off} named contest defender{?s} not on the defending roster; keyed on the target or disposer instead.")
  }
  to[ros, on = .(.mid = match_id, team = team, .tgt = player_id), .tgt_zone := .np_zone_of(i.position)]
  if (!".tgt_zone" %in% names(to)) to[, .tgt_zone := NA_character_]
  to[, .disp_zone := .np_zone_of(position)]
  to[, zone_key := data.table::fcase(
    .dfd_ok, paste0("P:", .dfd),
    !is.na(.tgt_zone), paste0("T:", .tgt_zone),
    !is.na(.disp_zone), paste0("D:", .disp_zone),
    default = NA_character_)]
  to[, c(".mid", ".do", ".dfd", ".tgt", ".dfd_ok", ".tgt_zone", ".disp_zone") := NULL]
  invisible(to)
}

#' Zone-spread weights inside `.np_spread_pool()`
#'
#' @param a The pool x lineup frame (one row per pool group x player).
#' @param grp Pool group columns.
#' @param zone_mix Output of `.np_fit_zone_mix()`.
#' @return `a`, with `w` set by reference.
#' @keywords internal
.np_zone_weights <- function(a, grp, zone_mix) {
  if (is.null(zone_mix) || nrow(zone_mix) == 0) {
    cli::cli_abort("{.arg spread = \"zone\"} needs a fitted zone mix.")
  }
  zm <- data.table::as.data.table(zone_mix)[, .(zone_key, def_zone, share)]
  a[, `:=`(.pz = .np_zone_of(position), .kt = substr(zone_key, 1L, 1L))]
  a[, w := tog]                                    # NA key (stoppage, offence pools): by TOG
  a[.kt == "P", w := as.numeric(player_id == substring(zone_key, 3L))]
  a[zm, on = .(zone_key, .pz = def_zone), .zs := i.share]
  if (!".zs" %in% names(a)) a[, .zs := NA_real_]
  a[.kt %chin% c("T", "D"), .tz := sum(tog), by = c(grp, ".pz")]
  a[.kt %chin% c("T", "D"),
    w := data.table::fifelse(is.na(.zs) | is.na(.pz), 0, .zs * tog / pmax(.tz, 1e-9))]
  # A keyed group with no weight (an unseen source zone, a named defender absent
  # from the group) is spread by TOG -- and said so, never silently flat.
  a[, .ws := sum(w), by = grp]
  n_fb <- data.table::uniqueN(a[!is.na(zone_key) & .ws <= 0, .SD, .SDcols = grp])
  if (n_fb > 0) {
    cli::cli_alert_warning("Zone spread: {n_fb} keyed pool group{?s} had no zone weight and spread by time on ground.")
    a[!is.na(zone_key) & .ws <= 0, w := tog]
  }
  # How much of the T/D mix fell on zones nobody occupied (renormalised away)
  td <- unique(a[.kt %chin% c("T", "D") & !is.na(.zs), .SD, .SDcols = c(grp, ".pz", ".zs")])
  miss <- td[, .(present = sum(.zs)), by = grp]
  if (nrow(miss)) {
    cli::cli_alert_info(
      "Zone spread: {format(nrow(miss), big.mark = ',')} zone-keyed pools; mean {round(100 * (1 - mean(miss$present)), 1)}% of the mix fell on empty zones and was renormalised")
  }
  a[, c(".pz", ".kt", ".zs", ".tz", ".ws") := NULL]
  invisible(a)
}

#' Fit the zone mix on the season before the one being scored
#'
#' Leak-safe by construction: the mix that spreads a season's pools never sees
#' that season. The first season (`AFL_MIN_SEASON`, no earlier data) fits on
#' itself and says so.
#' @param pbp_data,chains,player_stats The season being scored (one season).
#' @return Output of `.np_fit_zone_mix()`.
#' @keywords internal
.np_zone_mix_prior_season <- function(pbp_data, chains, player_stats) {
  s <- unique(stats::na.omit(data.table::as.data.table(pbp_data)$season))
  if (length(s) != 1) {
    cli::cli_abort(c(
      "The zone spread fits its mix on the previous season, so it scores one season at a time.",
      "x" = "{.arg pbp_data} holds {length(s)} season{?s}: {.val {s}}."))
  }
  prev <- as.integer(s) - 1L
  if (prev < AFL_MIN_SEASON) {
    cli::cli_warn("Zone mix for {s}: no {prev} data, so it is fitted IN-SAMPLE on {s}.")
    return(.np_fit_zone_mix(pbp_data, chains, player_stats))
  }
  cli::cli_alert_info("Zone mix for {s}: fitted on {prev}")
  .np_fit_zone_mix(load_pbp(prev, rounds = TRUE), load_chains(prev, rounds = TRUE),
                   load_player_stats(prev))
}
