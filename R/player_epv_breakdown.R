# Per-player EPV breakdown by event category
# ==========================================
# Answers "where does this player's value actually come from?" at the resolution
# the model itself uses, rather than the four-channel summary.
#
# Plan and measurements: ../../docs/plans/PLAYER-EPV-BREAKDOWN-PLAN.md

#' Box-score terms that make up each EPV channel
#'
#' The exact composition of `create_player_game_data()`. Each entry is `stat
#' column -> weight name in [default_epv_params()]`, and every term enters its
#' channel linearly as `count * weight` — which is what makes this decomposition
#' arithmetic rather than an approximation.
#'
#' @section Two source locations, not one:
#' The four channels are **not** all built in the same place, and assuming they
#' are is how this list drifts:
#'
#' * `disp` and `recv` — the final mutate, `player_credit.R:1001-1012`.
#' * `spoil` and `hitout` — **Step 4**, `player_credit.R:857-859`, inside
#'   `spoil_hitout_df`. The final mutate only zero-fills these two
#'   (`replace_na(epv_spoil, 0)`), so a maintainer who checks only the mutate
#'   will not see their formulas at all.
#'
#' Under v2 `epv_spoil` and `epv_hitout` are **pure box score with no chain
#' component**, so a near-zero `chain` residual on those channels is correct
#' behaviour rather than a missing term.
#'
#' **Keep this in lockstep with both locations.** If a term is added to either
#' and not here, [player_epv_breakdown()]'s residual silently absorbs it and the
#' category is simply missing from the profile. The `verify` gate catches a
#' *total* mismatch, not a term quietly landing in the residual — and a term
#' placed in the **wrong channel** reconciles perfectly while showing a player's
#' value under the wrong heading. None of these three checks substitutes for
#' another.
#'
#' @keywords internal
EPV_BOX_TERMS <- list(
  disp = c(
    inside50s          = "inside50s_wt",
    clangers           = "clangers_wt",
    score_involvements = "score_involvements_wt",
    kicks              = "kicks_wt",
    handballs          = "handballs_wt",
    metres_gained      = "metres_gained_wt",
    turnovers          = "turnovers_wt",
    goal_assists       = "goal_assists_wt",
    goals              = "goals_wt",
    behinds            = "behinds_wt",
    shots_at_goal      = "shots_at_goal_wt"
  ),
  recv = c(
    contested_possessions   = "contested_poss_wt",
    contested_marks         = "contested_marks_wt",
    ground_ball_gets        = "ground_ball_gets_wt",
    marks_inside50          = "marks_inside50_wt",
    marks                   = "marks_wt",
    uncontested_possessions = "uncontested_poss_wt",
    frees_for               = "frees_for_wt"
  ),
  spoil = c(
    spoils                 = "spoil_wt",
    tackles                = "tackle_wt",
    pressure_acts          = "pressure_wt",
    def_half_pressure_acts = "def_pressure_wt",
    intercepts             = "intercepts_wt",
    one_percenters         = "one_percenters_wt",
    rebound50s             = "rebound50s_wt",
    frees_against          = "frees_against_wt"
  ),
  hitout = c(
    hitouts              = "hitout_wt",
    hitouts_to_advantage = "hitout_adv_wt",
    ruck_contests        = "ruck_contest_wt"
  )
)

#' Break a player's EPV down by event category
#'
#' Decomposes each player-game's EPV into the 29 box-score categories the credit
#' model is actually built from, plus a per-channel `chain` residual holding the
#' play-by-play-derived value that has no counting stat behind it.
#'
#' @section Why there is a residual:
#' Each EPV channel is *chain EPV plus a linear box-score sum*. The box-score
#' half decomposes exactly, because every term is `count * weight`. The chain
#' half — the disposer/receiver split, the difficulty split and contest credit —
#' does not have a per-category form without instrumenting the credit path, so it
#' is reported whole, per channel, as `chain`.
#'
#' **The residual is a finding, not a leftover.** Its size relative to a player's
#' total says how much of his rating comes from context rather than counting
#' stats, which is exactly the thing a counting-stat profile page cannot show.
#'
#' @section What this is not:
#' It is **not** a sum of `delta_epv` by description. That was the obvious
#' approach and it is wrong: summing pbp's per-row `delta_epv` by player
#' correlates only **0.626** with published EPV and is out by a mean of **4.82
#' points per player-game**, because `delta_epv` is the swing *caused by* an
#' event while the credit model splits it between disposer and receiver. Anything
#' built that way is a different statistic wearing the same name.
#'
#' @param player_game Player-game ratings frame, as from
#'   [load_player_game_ratings()]. Supplies the published `epv` the breakdown is
#'   verified against.
#' @param player_stats Box-score frame, as from [load_player_stats()]. Supplies
#'   the counts. When `NULL`, loaded for the seasons present in `player_game`
#'   if it was supplied, otherwise for `seasons`.
#' @param seasons Seasons to load when frames are not supplied.
#' @param epv_params Weight list; defaults to [default_epv_params()].
#' @param verify When `TRUE` (default), aborts unless the categories plus
#'   residual reproduce the published `epv` for every player-game. This check
#'   closes by construction (the residual absorbs whatever the box terms miss),
#'   so it catches fan-out and a stale `player_game`, not a wrong or missing
#'   term in `EPV_BOX_TERMS`.
#' @param tolerance Absolute tolerance for that check.
#'
#' @return A long `data.table`: `player_id`, `match_id`, `season`, `round`,
#'   `channel`, `category`, `stat` (the count, `NA` for the chain residual),
#'   `epv`, and `share` (the category's signed share of the player-game's total
#'   absolute EPV).
#'
#' @seealso [create_player_game_data()] for the credit model this decomposes.
#' @export
player_epv_breakdown <- function(player_game = NULL,
                                 player_stats = NULL,
                                 seasons = get_afl_season(),
                                 epv_params = NULL,
                                 verify = TRUE,
                                 tolerance = 1e-6) {
  p <- if (is.null(epv_params)) default_epv_params() else epv_params
  stats_seasons <- seasons
  if (is.null(player_game)) {
    player_game <- load_player_game_ratings(seasons = seasons)
  } else if ("season" %in% names(player_game)) {
    stats_seasons <- sort(unique(player_game$season[!is.na(player_game$season)]))
  }
  if (is.null(player_stats)) player_stats <- load_player_stats(seasons = stats_seasons)

  pg <- if (data.table::is.data.table(player_game)) player_game else data.table::as.data.table(player_game)
  ps <- if (data.table::is.data.table(player_stats)) player_stats else data.table::as.data.table(player_stats)

  meta_cols <- c("player_id", "match_id", "season", "round", "player_name",
                 "position_group", "team", "tog")
  channel_cols <- c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout")
  box_stats <- unlist(lapply(EPV_BOX_TERMS, names))

  need <- c("player_id", "match_id", "epv", "epv_recv", "epv_disp",
            "epv_spoil", "epv_hitout")
  miss <- setdiff(need, names(pg))
  if (length(miss) > 0) {
    cli::cli_abort(c(
      "{.arg player_game} is missing {.field {miss}}.",
      "i" = "Expected the shape {.fn load_player_game_ratings} returns."
    ))
  }

  keep <- intersect(c("player_id", "match_id", box_stats), names(ps))
  absent <- setdiff(box_stats, names(ps))
  if (length(absent) > 0) {
    # Name them rather than letting the residual quietly swallow the value: a
    # missing stat column looks exactly like "this player did none of it".
    cli::cli_warn(c(
      "{length(absent)} box-score column{?s} absent from {.arg player_stats}: {.field {absent}}.",
      "!" = "Their value falls into the {.val chain} residual rather than appearing as its own category."
    ))
  }
  ps <- ps[, ..keep]

  # A duplicated (player_id, match_id) in either frame fans the merge out and
  # double-counts, which verification would only report as a wrong box term.
  if (anyDuplicated(ps, by = c("player_id", "match_id")) > 0) {
    cli::cli_abort("{.arg player_stats} has duplicate {.field player_id}/{.field match_id} rows.")
  }
  if (anyDuplicated(pg, by = c("player_id", "match_id")) > 0) {
    cli::cli_abort("{.arg player_game} has duplicate {.field player_id}/{.field match_id} rows.")
  }

  d <- merge(pg[, .SD, .SDcols = intersect(c(meta_cols, "epv", channel_cols), names(pg))],
             ps, by = c("player_id", "match_id"), all.x = TRUE)

  # Player-games with no box-score row get every count set to 0 below, which
  # pushes their whole channel value into the chain residual.
  if (length(keep) > 2) {
    n_unmatched <- nrow(d) - nrow(merge(d[, .(player_id, match_id)],
                                        ps[, .(player_id, match_id)],
                                        by = c("player_id", "match_id")))
    if (n_unmatched > 0) {
      cli::cli_warn(c(
        "{n_unmatched} of {nrow(d)} player-game{?s} ha{?s/ve} no matching {.arg player_stats} row.",
        "!" = "Their box-score counts are treated as 0 and their value falls into the {.val chain} residual."
      ))
    }
  }

  long <- data.table::rbindlist(lapply(names(EPV_BOX_TERMS), function(ch) {
    terms <- EPV_BOX_TERMS[[ch]]
    terms <- terms[names(terms) %in% names(d)]
    if (length(terms) == 0) return(NULL)
    data.table::rbindlist(lapply(names(terms), function(stat) {
      w <- p[[terms[[stat]]]]
      if (is.null(w)) {
        cli::cli_abort("Weight {.field {terms[[stat]]}} not found in {.arg epv_params}.")
      }
      cnt <- as.numeric(d[[stat]])
      cnt[is.na(cnt)] <- 0
      data.table::data.table(
        player_id = d$player_id, match_id = d$match_id,
        channel = ch, category = stat, stat = cnt, epv = cnt * w
      )
    }))
  }))

  # The chain residual, per channel: published channel minus its box-score terms.
  box_by_ch <- long[, .(box = sum(epv)), by = .(player_id, match_id, channel)]
  chan_long <- data.table::melt(
    d[, .(player_id, match_id, disp = epv_disp, recv = epv_recv,
          spoil = epv_spoil, hitout = epv_hitout)],
    id.vars = c("player_id", "match_id"),
    variable.name = "channel", value.name = "channel_epv")
  chan_long[, channel := as.character(channel)]
  resid <- merge(chan_long, box_by_ch, by = c("player_id", "match_id", "channel"),
                 all.x = TRUE)
  resid[is.na(box), box := 0]
  resid[, `:=`(category = "chain", stat = NA_real_, epv = channel_epv - box)]

  out <- data.table::rbindlist(
    list(long, resid[, .(player_id, match_id, channel, category, stat, epv)]),
    use.names = TRUE)

  meta <- d[, .SD, .SDcols = intersect(meta_cols, names(d))]
  out <- merge(out, meta, by = c("player_id", "match_id"), all.x = TRUE)

  # Share of the player-game's total ABSOLUTE epv. Signed shares over a signed
  # total are meaningless when the total is near zero -- a player whose value
  # nets to 0.01 would show categories in the thousands of percent.
  out[, share := epv / sum(abs(epv)), by = .(player_id, match_id)]
  out[!is.finite(share), share := NA_real_]

  if (isTRUE(verify)) .verify_breakdown(out, pg, tolerance)

  data.table::setcolorder(out, intersect(
    c("player_id", "player_name", "match_id", "season", "round", "team",
      "position_group", "tog", "channel", "category", "stat", "epv", "share"),
    names(out)))
  out[]
}

#' Assert the breakdown reproduces published EPV
#'
#' The whole value of the breakdown is that its parts add to the whole. A
#' decomposition that is merely close would put numbers on a profile page that
#' visibly disagree with the rating printed beside them.
#'
#' @keywords internal
.verify_breakdown <- function(brk, pg, tolerance = 1e-6) {
  agg <- brk[, .(rebuilt = sum(epv)), by = .(player_id, match_id)]
  cmp <- merge(pg[, .(player_id, match_id, epv)], agg,
               by = c("player_id", "match_id"))
  if (nrow(cmp) == 0) {
    cli::cli_abort("Verification found no overlapping player-games to compare.")
  }
  gap <- abs(cmp$epv - cmp$rebuilt)
  n_na <- sum(is.na(gap))
  if (n_na > 0) {
    cli::cli_warn("{n_na} of {nrow(cmp)} player-game{?s} ha{?s/ve} an NA published or rebuilt {.field epv} and {?was/were} not verified.")
  }
  if (all(is.na(gap))) {
    cli::cli_abort("Verification has no non-NA player-games to compare.")
  }
  worst <- max(gap, na.rm = TRUE)
  if (!is.finite(worst) || worst > tolerance) {
    bad <- cmp[which.max(gap)]
    cli::cli_abort(c(
      "EPV breakdown does not reproduce published {.field epv}.",
      "x" = "Worst gap {round(worst, 8)} over {sum(!is.na(gap))} checked player-game{?s} (tolerance {tolerance}).",
      "i" = "First offender: player {bad$player_id} in {bad$match_id} -- published {round(bad$epv, 6)}, rebuilt {round(bad$rebuilt, 6)}.",
      "i" = "Duplicate player-game rows fan the merge out; failing that, a stale {.arg player_game}."
    ))
  }
  invisible(TRUE)
}
