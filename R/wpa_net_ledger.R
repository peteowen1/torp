# WPA as a Net Points ledger.
# ===========================================================================
# Design, worked examples and Pete's sign-off: ../docs/plans/WPA-NET-LEDGER.md
#
# create_wp_credit() splits each play's win probability change between the
# disposer and the receiver and never debits the side that conceded, so a
# team's players can total several wins and both teams in a match can come out
# positive. This file runs the SAME change through the net points engine
# instead, with one different target: each team's players sum to that team's
# result (1 win, 0.5 draw, 0 loss) minus its pre-match win chance, and the two
# teams cancel. The routing (receiver split, defensive pool, blame side, siren,
# centre-bounce exclusion, the own-team re-allocation) is net points' own, so
# the two ledgers can only differ in what they are measuring.
#
# Two versions, same per-play credit, different starting point:
#   wpa_net      -- every team starts at its pre-match forecast, so beating a
#                   side you were expected to beat is worth little
#   wpa_neutral  -- every team starts at WPA_NEUTRAL_HOME_PROB (home) or its
#                   complement, so a win is worth about the same to anyone
# The per-play values come from a win probability model with no team-strength
# input, so only the target differs between the two.

#' Build the WPA Net Points ledger
#'
#' Runs per-play win probability change through `build_net_points()` so each
#' team's players sum to that team's result minus its pre-match win chance.
#' Run it with `pre_match = .wpa_neutral_pre_match(...)` for the team-neutral
#' version, where every match starts at the same home win chance.
#'
#' The engine runs in its plain configuration (`credit = "flat"`,
#' `stoppages = "exclude"`): the difficulty terms and stoppage baselines the
#' EPV engine uses are fitted in points and do not transfer to win
#' probability. Its log messages say "points"; here they mean percentage
#' points of win probability (see `WPA_LEDGER_SCALE`).
#'
#' @param pbp_data Play-by-play carrying `wpa` plus everything
#'   `build_net_points()` needs.
#' @param player_stats Box-score stats for the same matches (time on ground
#'   spreads the team pools).
#' @param pre_match A data frame of `match_id` and `home_win_prob`, and
#'   optionally `source` (recorded as `p0_source`, e.g. "locked" or
#'   "retrodiction"). Matches with no row are left out of the ledger, with a
#'   warning, and listed in the `skipped` attribute.
#' @param results Official results, as for `.np_match_results()`; `NULL`
#'   loads them.
#' @param scale Units the engine runs in; see `WPA_LEDGER_SCALE`.
#' @return A data.table, one row per player-match, of `player_id`,
#'   `match_id`, `wpa_net`, and the three parts
#'   `wpa_own` + `wpa_won` + `wpa_team` = `wpa_net`. All in win probability
#'   (fractions), each in the player's own team frame. Attribute `targets`
#'   holds each match's `p0`, its source, the result and the home target;
#'   attribute `skipped` the match_ids left out for want of a forecast.
#' @export
build_wpa_ledger <- function(pbp_data, player_stats, pre_match,
                             results = NULL, scale = WPA_LEDGER_SCALE) {
  p <- data.table::as.data.table(data.table::copy(pbp_data))
  need <- c("match_id", "wpa", "home_away")
  miss <- setdiff(need, names(p))
  if (length(miss)) cli::cli_abort("Play-by-play is missing {.val {miss}} for the WPA ledger.")
  if (!is.numeric(scale) || length(scale) != 1 || !is.finite(scale) || scale <= 0) {
    cli::cli_abort("{.arg scale} must be one positive number, not {.val {scale}}.")
  }
  p[, match_id := as.character(match_id)]

  res <- .np_match_results(p, results)
  res[, match_id := as.character(match_id)]
  res[, result := data.table::fifelse(home_score > away_score, 1,
                    data.table::fifelse(home_score < away_score, 0, 0.5))]

  # Pre-match chance comes only from a forecast. There is deliberately NO
  # fallback to the win probability model's first play: that model has no
  # team-strength input, and on the 112 matches of 2026 with a locked forecast
  # its opening value correlated -0.03 with it (mean gap 0.23). A match with no
  # forecast is left out and said so, rather than pinned to noise.
  pm <- data.table::as.data.table(pre_match)
  pm_miss <- setdiff(c("match_id", "home_win_prob"), names(pm))
  if (length(pm_miss)) cli::cli_abort("{.arg pre_match} is missing {.val {pm_miss}}.")
  if (!"source" %in% names(pm)) pm[, source := "forecast"]
  dup <- pm[!is.na(home_win_prob), .(n = data.table::uniqueN(home_win_prob)), by = match_id][n > 1]
  if (nrow(dup)) {
    cli::cli_alert_warning(
      "WPA ledger: {nrow(dup)} match{?es} have more than one pre-match chance; keeping the first row for each: {.val {head(as.character(dup$match_id), 5)}}.")
  }
  pm <- unique(pm[!is.na(home_win_prob), .(match_id = as.character(match_id),
                                           p0 = as.numeric(home_win_prob), p0_source = as.character(source))],
               by = "match_id")
  bad_p <- pm[p0 < 0 | p0 > 1]
  if (nrow(bad_p)) cli::cli_abort("{nrow(bad_p)} pre-match win chance{?s} outside 0 to 1, e.g. {.val {bad_p$match_id[1]}}.")
  res[pm, on = "match_id", `:=`(p0 = i.p0, p0_source = i.p0_source)]
  skipped <- res[is.na(p0), match_id]
  if (length(skipped)) {
    cli::cli_alert_warning(
      "WPA ledger: {length(skipped)} of {nrow(res)} match{?es} have no pre-match forecast and are left out (their players get no WPA ledger row). First: {.val {skipped[1]}}.")
    res <- res[!is.na(p0)]
    p <- p[match_id %in% res$match_id]
    player_stats <- data.table::as.data.table(player_stats)[as.character(match_id) %in% res$match_id]
  }
  if (nrow(res) == 0) cli::cli_abort("WPA ledger: no match has a pre-match forecast.")
  res[, home_target := scale * (result - p0)]

  cli::cli_alert_info(
    "WPA ledger: running the net points engine on win probability change (its messages say 'points'; read percentage points of win probability).")
  p[, delta_epv := scale * wpa]
  eng_res <- res[, .(match_id, home_team_name, away_team_name,
                     home_score = home_target, away_score = 0)]
  np <- build_net_points(p, player_stats, eng_res, credit = "flat",
                         stoppages = "exclude", return_payments = TRUE)
  np <- .np_team_margin(np, p, player_stats, eng_res)
  np <- data.table::as.data.table(np)

  # A match can have a forecast and still come back from the engine with no
  # rows, or with only one side (build_net_points() drops rows missing a team,
  # player or orientation before its own dropped-match count is taken). Such a
  # match is not rated: it joins `skipped`, so callers leave its players NA. If
  # it were missing from `skipped` they would read "no act" and write 0, which
  # looks like a real, average game.
  sides <- np[, .(n_sides = data.table::uniqueN(home_away)), by = .(match_id = as.character(match_id))]
  lost_m <- setdiff(res$match_id, sides[n_sides == 2, match_id])
  if (length(lost_m)) {
    cli::cli_alert_warning(
      "WPA ledger: {length(lost_m)} match{?es} with a forecast came back from the engine with no rows or only one side and {?is/are} left out: {.val {head(lost_m, 5)}}.")
    np <- np[!as.character(match_id) %in% lost_m]
    res <- res[!match_id %in% lost_m]
    skipped <- c(skipped, lost_m)
  }
  if (nrow(res) == 0) cli::cli_abort("WPA ledger: no match survived the engine.")

  # The identity this whole file exists for: every team sums to its own
  # result minus its pre-match chance. Every remaining match has both sides
  # (above), so this inner join covers all of them.
  chk <- np[, .(got = sum(net_points)), by = .(match_id = as.character(match_id), home_away)]
  chk <- merge(chk, res[, .(match_id, home_target)], by = "match_id")
  chk[, want := data.table::fifelse(home_away == "Home", home_target, -home_target)]
  gap <- max(abs(chk$got - chk$want))
  if (!is.finite(gap) || gap > 1e-8 * scale) {
    cli::cli_abort("WPA ledger: a team total is {signif(gap / scale, 3)} win probability off its result minus pre-match chance.")
  }

  ch <- .np_v4_channels(np)
  out <- ch[, .(player_id = as.character(player_id), match_id = as.character(match_id),
                wpa_net = net_points / scale,
                wpa_own = np_own / scale,
                wpa_won = np_won / scale,
                wpa_team = np_pool / scale)]
  data.table::setattr(out, "targets",
                      res[, .(match_id, p0, p0_source, result, home_target = home_target / scale)])
  data.table::setattr(out, "skipped", skipped)
  out
}

#' Pre-match forecasts for the WPA ledger
#'
#' The locked forecast (made before the match) where one exists, the
#' retrodiction otherwise. Locked forecasts are missing for 2021 rounds 1-13
#' and 2026 rounds 0-12. Retrodictions apply end-of-season ratings to every
#' match, so they know how the season went; on the 967 matches of 2022-2026
#' with both, they sit a mean 0.07 from the locked forecast (correlation
#' 0.94). They fill 2026's early rounds; 2021 rounds 1-13 have neither, so
#' those matches are left out of the ledger.
#'
#' @param seasons Seasons to load.
#' @return A data.table of `match_id`, `home_win_prob`, `source`
#'   ("locked" or "retrodiction").
#' @keywords internal
.wpa_pre_match <- function(seasons) {
  grab <- function(loader, src) {
    data.table::rbindlist(lapply(seasons, function(yr) {
      d <- tryCatch(data.table::as.data.table(loader(yr, rounds = TRUE)), error = function(e) {
        cli::cli_alert_warning("WPA ledger: {src} forecasts for {yr} unavailable ({conditionMessage(e)}).")
        NULL
      })
      if (is.null(d) || !nrow(d)) return(NULL)
      miss <- setdiff(c("match_id", "pred_win"), names(d))
      if (length(miss)) {
        cli::cli_alert_warning("WPA ledger: {src} forecasts for {yr} loaded but have no {.field {miss}}; not used.")
        return(NULL)
      }
      d[, .(match_id = as.character(match_id), home_win_prob = as.numeric(pred_win), source = src)]
    }))
  }
  locked <- grab(load_predictions, "locked")
  retro <- grab(load_retrodictions, "retrodiction")
  out <- rbind(locked, retro[!match_id %in% locked$match_id])
  unique(out[!is.na(home_win_prob)], by = "match_id")
}

#' Even pre-match chances for the team-neutral WPA ledger
#'
#' @param match_ids Matches to cover.
#' @param home_prob Home win chance every match starts at.
#' @return A data.table of `match_id`, `home_win_prob`, `source` ("neutral").
#' @keywords internal
.wpa_neutral_pre_match <- function(match_ids, home_prob = WPA_NEUTRAL_HOME_PROB) {
  data.table::data.table(match_id = unique(as.character(match_ids)),
                         home_win_prob = home_prob, source = "neutral")
}

#' Keep the WPA ledger's team totals when a frame drops some players
#'
#' `create_player_game_data()` keeps only players with a disposal, so a player
#' whose ledger value came entirely from receptions or a pool share has no row
#' to land on. Dropping him would move his team's total off its target. His
#' value goes to his team-mates in that match by time on ground instead, the
#' same rule the v4 EPV branch applies to net points, and the amount is logged.
#'
#' @param wpn `build_wpa_ledger()` output.
#' @param keep A data frame of the `player_id`, `match_id` pairs the frame has.
#' @param pbp_data Play-by-play, for each player's team.
#' @param player_stats Box-score stats, for time on ground.
#' @return `wpn` restricted to `keep`, with the dropped value re-spread into
#'   `wpa_net` and `wpa_team` (a team share, not his team-mates' own play).
#' @keywords internal
.wpa_respread_lost <- function(wpn, keep, pbp_data, player_stats) {
  wpn <- data.table::copy(data.table::as.data.table(wpn))
  keep <- unique(data.table::as.data.table(keep)[, .(player_id = as.character(player_id),
                                                     match_id = as.character(match_id))])
  lost <- wpn[!keep, on = .(player_id, match_id)]
  if (nrow(lost) == 0) return(wpn)
  tm <- unique(data.table::as.data.table(pbp_data)[!is.na(player_id) & !is.na(team),
          .(player_id = as.character(player_id), match_id = as.character(match_id), team)])
  # One team per player-match, or his value could land on the wrong side and
  # the conservation check below (built from the same lookup) could not see it.
  two <- tm[, .N, by = .(player_id, match_id)][N > 1]
  if (nrow(two)) {
    cli::cli_abort("WPA ledger: {nrow(two)} player-match{?es} carry more than one team in the play-by-play, e.g. {.val {two$player_id[1]}} in {.val {two$match_id[1]}}.")
  }
  lost <- merge(lost, tm, by = c("player_id", "match_id"), all.x = TRUE)
  if (anyNA(lost$team)) cli::cli_abort("WPA ledger: {sum(is.na(lost$team))} dropped player-match{?es} have no team in the play-by-play.")
  owed <- lost[, .(v = sum(wpa_net)), by = .(match_id, team)]
  wpn <- merge(wpn[keep, on = .(player_id, match_id), nomatch = NULL], tm,
               by = c("player_id", "match_id"), all.x = TRUE)
  tg <- data.table::as.data.table(player_stats)[, .(player_id = as.character(player_id),
          match_id = as.character(match_id), tog = pmax(time_on_ground_percentage, 1))]
  wpn[tg, on = .(player_id, match_id), tog := i.tog]
  wpn[is.na(tog), tog := 75]
  wpn[owed, on = .(match_id, team), v_lost := i.v]
  wpn[is.na(v_lost), v_lost := 0]
  wpn[, share := v_lost * tog / sum(tog), by = .(match_id, team)]
  got <- wpn[, .(got = sum(share)), by = .(match_id, team)]
  chk <- merge(owed, got, by = c("match_id", "team"), all.x = TRUE)
  chk[is.na(got), got := 0]
  if (max(abs(chk$v - chk$got)) > 1e-10) {
    cli::cli_abort("WPA ledger: {round(sum(abs(chk$v - chk$got)), 4)} win probability belongs to a team with no player row in the frame.")
  }
  cli::cli_alert_info(
    "WPA ledger: {nrow(lost)} player-match{?es} with no disposal row; their {round(sum(abs(lost$wpa_net)), 3)} win probability re-spread within their team by time on ground.")
  wpn[, `:=`(wpa_net = wpa_net + share, wpa_team = wpa_team + share)]
  wpn[, c("team", "tog", "v_lost", "share") := NULL]
  wpn[]
}
