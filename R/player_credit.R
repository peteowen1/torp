#' Default EPV assignment parameters
#'
#' Returns a named list of all EPV assignment parameters with their default
#' values. Used by \code{create_player_game_data()} when no custom params are provided.
#'
#' @return A named list of EPV assignment parameters.
#' @keywords internal
default_epv_params <- function() {
  list(
    bounce_wt         = EPV_BOUNCE_WT,
    disp_neg_offset   = EPV_DISP_NEG_OFFSET,
    disp_pos_offset   = EPV_DISP_POS_OFFSET,
    disp_scale        = EPV_DISP_SCALE,
    recv_neg_mult     = EPV_RECV_NEG_MULT,
    recv_neg_offset   = EPV_RECV_NEG_OFFSET,
    recv_pos_mult     = EPV_RECV_POS_MULT,
    recv_pos_offset   = EPV_RECV_POS_OFFSET,
    recv_scale        = EPV_RECV_SCALE,
    recv_intercept_mark_scale = EPV_RECV_INTERCEPT_MARK_SCALE,
    recv_failed_contest_wt = EPV_RECV_FAILED_CONTEST_WT,
    spoil_wt          = EPV_SPOIL_WT,
    tackle_wt         = EPV_TACKLE_WT,
    pressure_wt       = EPV_PRESSURE_WT,
    def_pressure_wt   = EPV_DEF_PRESSURE_WT,
    hitout_wt         = EPV_HITOUT_WT,
    hitout_adv_wt     = EPV_HITOUT_ADV_WT,
    ruck_contest_wt        = EPV_RUCK_CONTEST_WT,
    contested_poss_wt      = EPV_CONTESTED_POSS_WT,
    contested_marks_wt     = EPV_CONTESTED_MARKS_WT,
    ground_ball_gets_wt    = EPV_GROUND_BALL_GETS_WT,
    marks_inside50_wt      = EPV_MARKS_INSIDE50_WT,
    inside50s_wt           = EPV_INSIDE50S_WT,
    clangers_wt            = EPV_CLANGERS_WT,
    score_involvements_wt  = EPV_SCORE_INVOLVEMENTS_WT,
    intercepts_wt          = EPV_INTERCEPTS_WT,
    one_percenters_wt      = EPV_ONE_PERCENTERS_WT,
    rebound50s_wt          = EPV_REBOUND50S_WT,
    frees_against_wt       = EPV_FREES_AGAINST_WT,
    frees_for_wt           = EPV_FREES_FOR_WT,
    goals_wt               = EPV_GOALS_WT,
    behinds_wt             = EPV_BEHINDS_WT,
    marks_wt               = EPV_MARKS_WT,
    uncontested_poss_wt    = EPV_UNCONTESTED_POSS_WT,
    shots_at_goal_wt       = EPV_SHOTS_AT_GOAL_WT,
    kicks_wt               = EPV_KICKS_WT,
    handballs_wt           = EPV_HANDBALLS_WT,
    metres_gained_wt       = EPV_METRES_GAINED_WT,
    turnovers_wt           = EPV_TURNOVERS_WT,
    goal_assists_wt        = EPV_GOAL_ASSISTS_WT
  )
}

#' Should the per-channel points scale apply?
#'
#' The single decision point for "per-channel or one global factor", so the two
#' places that scale EPV into points cannot drift apart. They already had:
#' \code{get_player_game_ratings()} carried its own copy that keyed on the
#' \code{EPV_ENGINE} global, ignored \code{EPV_PER_CHANNEL_POINTS_SCALE}, and
#' kept an \code{!all(scale == 1)} fall-through that had been removed here as a
#' bug. All three made it disagree with the published pipeline silently.
#'
#' Until 2026-11-16 a frame stamped \code{"v3"} also switched this on; that
#' engine has been removed, so only \code{EPV_PER_CHANNEL_POINTS_SCALE} decides
#' now. The argument is kept so the call sites still say which frame they are
#' asking about.
#'
#' @param engine_attr The frame's \code{epv_engine} attribute, or NULL. No
#'   longer changes the answer.
#' @return TRUE if \code{EPV3_POINTS_SCALE} should be applied per channel.
#' @keywords internal
.use_per_channel_scale <- function(engine_attr) {
  isTRUE(EPV_PER_CHANNEL_POINTS_SCALE)
}

#' Read the engine a frame was priced with
#'
#' The single place the `epv_engine` attribute is read. There were three
#' (`analyze_match.R`, `centre_epv_by_position()`, `adjust_epv_for_opponents()`),
#' each spelling out `attr(x, "epv_engine")` and each independently responsible
#' for noticing when it came back NULL. None did.
#'
#' **Why an attribute at all, and why it is a trap.** `create_player_game_data()`
#' stamps the engine onto the frame it returns, and R drops attributes on
#' `merge()`, `rbind()` and most dplyr verbs -- which is why
#' `adjust_epv_for_opponents()` has to re-attach it by hand after its own merge,
#' and why the v3 experiment scripts in `data-raw/04-analysis/` call
#' `setattr(d, "epv_engine", "v3")` six-plus times. A frame that loses the stamp
#' is priced as v2 with nothing said.
#'
#' **This does not change what is returned** -- NULL still means v2, exactly as
#' before. It makes the fallback AUDIBLE, and only in the case where it is
#' wrong: when `EPV_ENGINE` is `"v3"` a missing stamp is almost certainly a lost
#' attribute rather than a genuine v2 frame. While `EPV_ENGINE` is `"v2"` a
#' missing stamp and a v2 stamp mean the same thing, so warning would be noise.
#'
#' The fuller fix -- carrying the engine as a column, which survives every
#' transform -- WAS done on 2026-08-18, reversing the earlier decision recorded
#' here. The reason it was deferred (adding a column changes `player_game_data`'s
#' released schema) was real but was outweighed: the attribute does not survive
#' the parquet round-trip the frame makes through the release, so Stage 3 reloaded
#' it unstamped and priced v3 data as v2 while this function only warned. See
#' `create_player_game_data()` for the column write and
#' `.restore_epv_engine_attr()` for the read-back.
#'
#' @param x A frame produced by `create_player_game_data()`, possibly after
#'   transforms that dropped attributes.
#' @param what Label used in the warning to say which frame lost its stamp.
#' @param configured The engine the package is configured for. An argument
#'   rather than a direct read of the constant so the warning path is testable
#'   without rewriting a locked namespace binding; production never passes it.
#' @return The engine string, or NULL. NULL is the v2 answer.
#' @keywords internal
.frame_epv_engine <- function(x, what = "frame", configured = EPV_ENGINE) {
  eng <- attr(x, "epv_engine")
  if (is.null(eng) && configured %in% c("v3", "v4")) {
    cli::cli_warn(c(
      "The {what} carries no {.field epv_engine} attribute while {.code EPV_ENGINE} is {.val {configured}}.",
      "x" = "It will be priced as v2. R drops attributes on {.code merge()}/{.code rbind()}/most dplyr verbs, so this is far more likely a lost stamp than a genuine v2 frame.",
      "i" = "Re-attach after the transform that dropped it: {.code data.table::setattr(x, \"epv_engine\", \"v3\")}."
    ))
  }
  eng
}

#' Centre EPV channels on their listed position's level, per round
#'
#' The positional level correction, applied at the layer that creates it.
#' \code{.position_adjust()} already centres every channel to machine-precision
#' zero -- but by \code{lineup_position}, the weekly on-field role. That removes
#' the ROLE effect and leaves the PLAYER-TYPE one: key defenders are a subset of
#' the players filling full-back and centre-half-back, and they sit below those
#' roles' own means. Measured on 2026 per-game data, \code{epv_adj} spans 2.94
#' points across listed buckets (key_def -2.17, key_fwd +0.77) even though every
#' one of the 20 lineup positions reads exactly 0.
#'
#' That is the whole gap \code{centre_epr_by_position()} was subtracting one
#' layer downstream. Correcting it here flows to EPR, to PSV blending and to the
#' per-game displays in \code{get_player_game_ratings()} at once, instead of
#' each needing its own correction.
#'
#' \strong{Weighted by TOG, and grouped per \code{(season, round)}, because that
#' is what makes EPR's numerator vanish.} EPR forms
#' \code{sum(x * tog_safe * decay)} over past games; the decay factor is
#' effectively constant within a round, so zeroing the TOG-weighted mean of
#' \code{x} in each round drives that sum to zero for the whole bucket. An
#' unweighted mean would not. Per-round grouping is also the leak-safe choice:
#' a full-history mean would centre early rounds using games that had not
#' happened yet.
#'
#' \strong{This does not make EPR exactly centred, and is not meant to.}
#' \code{.bayesian_shrink()} pulls each player toward \code{prior_rate}
#' (-0.7 / -0.3, not zero) by an amount set by their \code{wt_gms}, so positions
#' that differ in games played and time on ground keep a small residual level.
#' \code{EPR_POSITION_CENTRE} remains as the backstop that removes it.
#'
#' Operates on the \code{_oadj} (opponent-adjusted) channels when they exist and
#' \code{_adj} otherwise -- matching whichever set EPR actually consumes. Order
#' matters: this must run AFTER \code{adjust_epv_for_opponents()}, or the
#' opponent adjustment reintroduces a level on top of the correction.
#'
#' \strong{Thin cells.} A (season, round, bucket) cell can be very small -- the
#' Grand Final has two rucks and four key forwards -- and a full correction then
#' subtracts those few players' own noise from each of them (2025 round 28
#' key_fwd: -7.05 points off four players). \code{EPV_POSITION_SHRINK} blends the
#' round's mean with the bucket's mean over strictly EARLIER rounds in
#' proportion to cell weight, so a thin cell is judged mostly on the position's
#' history and a normal cell is essentially unchanged. It shrinks toward that
#' history rather than toward zero precisely so that no positional level is
#' handed back; see the constant's own documentation for the measurement.
#'
#' @param pgd Player game data, after opponent adjustment.
#' @param channels Channel stems to centre. Defaults to
#'   \code{EPV_LEVEL_CENTRE_CHANNELS}.
#' @return \code{pgd} with the live channel set centred and its total rebuilt,
#'   carrying an \code{"epv_level_centring"} attribute: one row per
#'   (cell, channel) with the round mean, the prior mean, the shrinkage weight,
#'   the correction applied and \code{resid_expected}, the residual the cell
#'   should be left with. The pipeline guard checks observed residuals against
#'   that column, so shrinkage never requires loosening its tolerance.
#' @keywords internal
centre_epv_by_position <- function(pgd, channels = EPV_LEVEL_CENTRE_CHANNELS) {
  dt <- data.table::as.data.table(pgd)

  if (!"position_group" %in% names(dt)) {
    cli::cli_abort(c(
      "Cannot centre EPV by position: no {.field position_group} column.",
      "x" = "Refusing to return uncentred values that callers will treat as centred."
    ))
  }

  # Use the same channel set EPR does. Centring _adj while EPR reads _oadj would
  # be a silent no-op: every check here would pass and nothing downstream would
  # change.
  suffix <- if (all(paste0(channels, "_oadj") %in% names(dt))) "_oadj" else "_adj"
  cols <- paste0(channels, suffix)
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Cannot centre EPV by position: {length(missing_cols)} channel{?s} absent: {.val {missing_cols}}",
      "x" = "A partial channel set would rebuild the total from part of its parts."
    ))
  }

  dt[, .cpg := .collapse_listed_position(position_group)]
  n_missing  <- sum(is.na(dt$position_group))
  n_unmapped <- sum(!is.na(dt$position_group) & is.na(dt$.cpg))
  if (n_unmapped > 0) {
    cli::cli_alert_danger(
      "{n_unmapped} player-game{?s} carr{?ies/y} an UNMAPPED {.field position_group} and {?was/were} left UNCENTRED at the EPV layer.")
  }

  dt[, .ctog := pmax(dplyr::coalesce(time_on_ground_percentage / 100, 0.1), 0.1)]

  # Per-cell sufficient statistics, one channel at a time. Built as a table
  # rather than inside a by-group expression because the correction needs a
  # SECOND cell's mean (the shrinkage prior) that a by-group cannot see.
  shrink_on <- isTRUE(EPV_POSITION_SHRINK)
  .cell_stats <- function(cc) {
    # Bind the channel to a plain column once: get() inside the i-clause blocks
    # data.table's optimised subsetting.
    data.table::set(dt, j = ".cx", value = dt[[cc]])
    s <- dt[!is.na(.cpg) & is.finite(.cx) & is.finite(.ctog) & .ctog > 0,
            .(sx = sum(.cx * .ctog), sw = sum(.ctog)),
            by = .(season, round, .cpg)]
    dt[, .cx := NULL]
    if (nrow(s) == 0) return(s)
    s[, m_round := sx / sw]
    data.table::setorder(s, .cpg, season, round)

    # The shrinkage prior is the bucket's own mean over every STRICTLY EARLIER
    # (season, round) -- never the same round, never a later one. A whole-season
    # mean would be the obvious choice and is wrong for the same reason the
    # grouping above is per-round: it would centre a round-1 value using games
    # that had not been played yet.
    s[, `:=`(px = cumsum(sx) - sx, pw = cumsum(sw) - sw), by = .cpg]
    s[, m_prior := data.table::fifelse(pw > 0, px / pw, NA_real_)]

    # Shrink toward that prior, not toward zero. Weight, not row count, is the
    # evidence: a cell of 20 fringe players carrying almost no TOG deserves the
    # same scepticism as a cell of two regulars.
    #
    # Toward the PRIOR rather than toward zero is the whole point (measured
    # 2026-07-30, measure_epv_shrink_priors.R). Shrinking toward zero withholds
    # correction, and every point withheld is a point of positional level handed
    # back -- at prior 5 that restored a spread of 0.477 against the 2.94 the v2
    # fix removed, and most of it came from NORMAL cells, not the thin ones.
    # Shrinking toward the bucket's own history still subtracts a full position
    # level; it only stops a Grand Final key forward being judged against the
    # three other key forwards who happened to play that day.
    s[, lam := if (!shrink_on) {
      1
    } else if (identical(EPV_POSITION_SHRINK_RULE, "floor")) {
      # Cells at or above the floor keep the FULL correction, bit-identical to
      # production. Only thinner cells ramp. The smooth "prior" rule below
      # touches every cell instead, which is why it failed its gate -- it
      # diluted 55,758 of 56,162 rows to reach the 22 that needed it.
      pmin(1, sw / EPV_POSITION_SHRINK_FLOOR)
    } else if (identical(EPV_POSITION_SHRINK_RULE, "prior")) {
      sw / (sw + EPV_POSITION_SHRINK_PRIOR)
    } else {
      cli::cli_abort(c(
        "Unknown {.code EPV_POSITION_SHRINK_RULE}: {.val {EPV_POSITION_SHRINK_RULE}}",
        "x" = "Refusing to guess a shrinkage rule -- expected {.val floor} or {.val prior}."
      ))
    }]
    # No earlier history at all (the first round in the frame) means there is
    # nothing to shrink toward. Apply the round mean in full rather than
    # silently leaving the cell uncentred -- an uncentred cell is the failure
    # this function exists to prevent.
    s[, corr := data.table::fifelse(is.na(m_prior), m_round, lam * m_round + (1 - lam) * m_prior)]
    s
  }

  # Kept so the pipeline guard can check the EXACT residual each cell should
  # have been left with, instead of loosening its tolerance to accommodate
  # shrinkage. Widening a production guard to fit a change is how the EPR
  # version of this got shipped and reverted.
  centring_cells <- vector("list", length(cols))
  names(centring_cells) <- cols

  for (cc in cols) {
    s <- .cell_stats(cc)
    if (nrow(s) == 0) {
      cli::cli_alert_danger(
        "No cell of {.field {cc}} had a finite value and a mapped position -- channel left UNCENTRED.")
      next
    }
    dt[s, .corr := i.corr, on = .(season, round, .cpg)]
    dt[!is.na(.cpg) & !is.na(.corr), (cc) := get(cc) - .corr]
    dt[, .corr := NULL]
    centring_cells[[cc]] <- s[, .(season, round, pos_bucket = .cpg, channel = cc,
                                  wt = sw, m_round, m_prior, lam, corr)]
  }

  # Points-scale calibration, applied at the VALUE layer so it flows into the
  # rating (Pete's call 2026-07-29 -- the same principle as the level fix).
  # EPR converted at 0.919 points per rating point, so new = 0.919 * old.
  #
  # Scaling here is NOT sufficient on its own: .bayesian_shrink() adds
  # prior_games * prior_rate AFTER the value, so an unscaled prior would leave
  # EPR a blend of scaled and unscaled parts rather than a clean rescale. The
  # EPR_PRIOR_RATE_* constants carry the same factor for exactly that reason --
  # if you change one, change both.
  #
  # Under v3 the scale is PER CHANNEL (`EPV3_POINTS_SCALE`), because the whole
  # point of v3's channels is that one unit of each is one point of margin --
  # and they convert at very different rates, so a single global factor cannot
  # deliver that. v2 keeps the global `EPV_POINTS_SCALE` unchanged.
  # Keyed on the ENGINE ACTUALLY IN USE, not the global default. Keying it on
  # EPV_ENGINE meant a caller passing epv_engine = "v3" while the constant still
  # read "v2" would silently get no per-channel scaling -- which is exactly how
  # every arm in this session was run.
  #
  # NOT also conditioned on the vector being non-trivial, and that matters. An
  # earlier version fell through to the global EPV_POINTS_SCALE whenever
  # EPV3_POINTS_SCALE was all 1s, so a v3 build with the scale not yet fitted
  # silently got v2's 0.919 -- and a constant fitted against THAT baseline then
  # verified at 0.919 instead of 1.000 when applied, because the 0.919 had
  # disappeared underneath it. Under v3 the per-channel vector is authoritative
  # even when it is all 1s; all 1s means no scaling, not "fall back to v2's".
  #
  # EPV_PER_CHANNEL_POINTS_SCALE opens the same path to v2: one global 0.919
  # cannot make four channels that each convert at a different rate read one
  # point per unit, and raw v2 `epv` conserves at only 0.4778 because of it.
  # The vector is engine-agnostic despite its name, and `lbl` below already
  # maps all four v2 channels onto it.
  #
  # Deliberately no per-channel figures here. This comment previously quoted
  # 0.893 / 1.556 / 0.344 as v2's, which the `EPV_PER_CHANNEL_POINTS_SCALE`
  # docstring in constants_ratings.R retracted on 2026-08-06 -- those are the v3
  # ship frame's RAW-layer scales, and v2's own raw-layer fit is recv 0.611,
  # disp 0.552, spoil -0.481. Numbers live in one place; read them there.
  frame_engine <- .frame_epv_engine(pgd, "player-game frame")
  use_v3_scale <- .use_per_channel_scale(frame_engine)
  # Defined unconditionally so the residual-expectation block below can use it
  # without depending on which branch ran.
  lbl <- EPV_CHANNEL_SCALE_KEYS
  if (use_v3_scale) {
    scaled <- character()
    unscaled <- character()
    for (cc in cols) {
      stem <- sub("_(oadj|adj)$", "", cc)
      slot <- lbl[[stem]]
      k <- EPV3_POINTS_SCALE[[slot]]
      if (is.finite(k)) {
        dt[, (cc) := get(cc) * k]
        scaled <- c(scaled, paste0(slot, "=", round(k, 4)))
      } else {
        unscaled <- c(unscaled, cc)
      }
    }
    if (length(unscaled) > 0) {
      cli::cli_warn(
        "Per-channel points scale is not finite for {.field {unscaled}}; {?that channel is/those channels are} left UNSCALED.")
    }
    scaled_msg <- paste(scaled, collapse = ", ")
    cli::cli_alert_info("Applied per-channel points scale: {scaled_msg}")
  } else if (is.finite(EPV_POINTS_SCALE) && !isTRUE(all.equal(EPV_POINTS_SCALE, 1))) {
    if (identical(frame_engine, "v4")) {
      # v4 is already in margin points (the ledger sums to the margin, and
      # .epr_prior_points_scale() returns 1 for v4), yet this branch applies
      # v2's global 0.919 to the data term while the shrinkage prior stays
      # unscaled -- the mismatch the comment above warns about. NOT changed
      # here: doing so moves every published v4 rating, and EPR_UNITS_SCALE_V4
      # (2.40) was fitted with the 0.919 in. Flagged for an owner decision
      # (review B56; same fall-through in analyze_match.R).
      cli::cli_alert_warning(
        "v4 frame is being scaled by v2's global EPV_POINTS_SCALE ({EPV_POINTS_SCALE}); v4 channels are already in margin points (review B56).")
    }
    for (cc in cols) dt[, (cc) := get(cc) * EPV_POINTS_SCALE]
  }

  # Rebuild the total from its parts, exactly as the EPR centring does, so the
  # total and its channels cannot disagree.
  total_col <- paste0("epv", suffix)
  if (total_col %in% names(dt)) {
    finite_all <- Reduce(`&`, lapply(cols, function(cc) is.finite(dt[[cc]])))
    dt[, (total_col) := rowSums(as.matrix(.SD), na.rm = TRUE), .SDcols = cols]
    if (sum(!finite_all) > 0) dt[!finite_all, (total_col) := NA_real_]
  }

  dt[, c(".cpg", ".ctog") := NULL]

  # The residual each cell SHOULD be left with, in the units the caller will
  # measure it in (i.e. after the points scale above). Full correction leaves
  # exactly zero; a shrunk correction leaves (1 - lam) * (m_round - m_prior),
  # which is just as exactly checkable -- so the pipeline guard never has to
  # trade its tolerance for this feature.
  cells <- data.table::rbindlist(centring_cells)
  if (nrow(cells) > 0) {
    # The residual has to be quoted in the units the caller measures it in,
    # which under v3 means the channel's OWN factor -- a single global scale
    # here would put the guard's expectation on a different scale from the data
    # for three of the four channels. Harmless today only because full centring
    # leaves exactly zero either way; it stops being harmless the moment
    # EPV_POSITION_SHRINK is turned on.
    scale_of <- function(cc) {
      if (use_v3_scale) {
        k <- EPV3_POINTS_SCALE[[lbl[[sub("_(oadj|adj)$", "", cc)]]]]
        if (is.finite(k)) k else 1
      } else if (is.finite(EPV_POINTS_SCALE)) EPV_POINTS_SCALE else 1
    }
    cells[, resid_expected := (m_round - corr) * vapply(channel, scale_of, numeric(1))]
  }
  data.table::setattr(dt, "epv_level_centring", cells)

  cli::cli_alert_success(
    "Centred {length(cols)} EPV channel{?s} ({suffix}) on listed-position levels per round ({nrow(dt)} player-games; {n_missing} with no position group, {n_unmapped} unmapped)")
  if (shrink_on && nrow(cells) > 0) {
    n_bit <- cells[lam < 1, .N]
    knob <- if (identical(EPV_POSITION_SHRINK_RULE, "floor"))
      paste("floor", EPV_POSITION_SHRINK_FLOOR) else
      paste("prior", EPV_POSITION_SHRINK_PRIOR)
    cli::cli_alert_info(
      "EPV position shrinkage ON ({knob}): {n_bit} of {nrow(cells)} cell-channels shrunk at all toward their bucket's earlier mean; thinnest lambda {signif(min(cells$lam), 3)}")
  }
  dt[]
}

#' TOG-weighted standard deviation
#'
#' @param x Numeric vector.
#' @param w Numeric weights (time on ground).
#' @return Weighted SD, or \code{NA_real_} when nothing is observed.
#' @keywords internal
.wtd_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  m <- stats::weighted.mean(x[ok], w[ok])
  sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok]))
}

#' Positional adjustment with a BLENDED reference between two cells
#'
#' The hard-threshold version asks "are you a ruck, yes or no" and compares you
#' with one cell mean or the other. This asks "how much of a ruck are you" and
#' compares you with a weighted mix, the weight ramping linearly across
#' \code{lo} to \code{hi} contests.
#'
#' It removes the threshold cliff WITHOUT conditioning on anything correlated
#' with output. Both continuous alternatives tried on 2026-08-06 -- smoothing on
#' contest volume, and on share of contests -- killed the cliff and also killed
#' the channel's link to production, because the conditioning variable carried
#' the output. Here the references are two fixed cell means; only the weight
#' between them moves.
#'
#' @param p80 Per-80 channel values.
#' @param tog Time-on-ground fraction, same length.
#' @param involvement The role measure the blend ramps on (ruck contests).
#' @param lo,hi Involvement at which the weight is 0 and 1.
#' @param pooled_sd,standardise As \code{.position_adjust()}.
#' @return Adjusted values, same length as \code{p80}.
#' @keywords internal
.blend_adjust <- function(p80, tog, involvement, lo, hi, pooled_sd, standardise) {
  w <- pmin(pmax((involvement - lo) / max(hi - lo, 1e-9), 0), 1)
  hi_grp <- w > 0.5
  # The two reference cells are the CLEAR cases at each end, not the blended
  # middle -- estimating a cell mean from rows that are themselves half-weighted
  # would fold the blend back into its own reference.
  m_hi <- stats::weighted.mean(p80[hi_grp], tog[hi_grp], na.rm = TRUE)
  m_lo <- stats::weighted.mean(p80[!hi_grp], tog[!hi_grp], na.rm = TRUE)
  # An empty reference cell yields NaN, and substituting 0 silently centres
  # against an ASSUMED zero that is indistinguishable downstream from a measured
  # one. `centre_epv_by_position()` shouts in the same situation; match it, so a
  # missing side shows up in the log instead of as a plausible number.
  if (!is.finite(m_hi) || !is.finite(m_lo)) {
    cli::cli_alert_danger(
      "Blend reference cell empty ({sum(hi_grp)} above / {sum(!hi_grp)} below the ramp) -- that side is centred against an ASSUMED zero, not a measurement.")
  }
  if (!is.finite(m_hi)) m_hi <- 0
  if (!is.finite(m_lo)) m_lo <- 0
  centred <- p80 - (w * m_hi + (1 - w) * m_lo)
  if (!isTRUE(standardise)) return(centred * tog)
  s_hi <- .wtd_sd(p80[hi_grp], tog[hi_grp])
  s_lo <- .wtd_sd(p80[!hi_grp], tog[!hi_grp])
  # Same NA guard the two means get above, and for a sharper reason: `0 * NA` is
  # NA in R, not 0, so an empty group on ONE side of the ramp would otherwise
  # make `s` NA for every row -- including rows whose weight gives that side no
  # say at all -- and silently drop standardisation for the whole vector.
  if (!is.finite(s_hi)) s_hi <- 0
  if (!is.finite(s_lo)) s_lo <- 0
  s <- w * s_hi + (1 - w) * s_lo
  bad <- !is.finite(s) | s < 1e-6 | !is.finite(pooled_sd)
  out <- centred * tog
  out[!bad] <- (centred[!bad] / s[!bad]) * pooled_sd * tog[!bad]
  out
}

#' Replace a bench starting slot with the role the player actually filled
#'
#' \code{lineup_position} records where a player STARTED. \code{INT} is not a
#' role, and using it as a centring cell measures a bench-starting specialist
#' against the bench -- see
#' \code{docs/reviews/INT-CENTRING-BUG-2026-08-06.md}.
#'
#' Resolution order, most specific first:
#' \enumerate{
#'   \item his modal non-bench slot \strong{this season}
#'   \item his modal non-bench slot in \strong{any} season
#'   \item a representative slot for his listed position (\code{ROLE_FALLBACK_SLOT})
#'   \item unchanged, if even that is unavailable
#' }
#'
#' \strong{Tier 1 uses the whole season, including later rounds.} That is a mild
#' look-ahead and it is deliberate: the output is a role LABEL, not a
#' performance measure, and a player's role is close to fixed within a season.
#' The alternative -- prior rounds only -- leaves every round-1 bench start
#' unresolvable, which is the case the fix exists for. Stated rather than hidden;
#' if it ever needs to be leak-free, tier 1 becomes prior-rounds-only and tier 2
#' absorbs the rest.
#'
#' @param slot Character vector of `lineup_position`.
#' @param player_id,season Same length as `slot`.
#' @param listed Listed `position_group`, for the tier-3 fallback.
#' @return `slot` with bench entries replaced where a role could be resolved.
#' @keywords internal
.remap_bench_role <- function(slot, player_id, season, listed) {
  bench <- slot %in% ROLE_BENCH_SLOTS
  if (!any(bench)) return(slot)
  # Fail on the actual problem rather than letting a NULL reach data.table's
  # `by`, which reports "column of 'by' is type NULL" and names neither the
  # argument nor the caller.
  for (nm in c("player_id", "season", "listed")) {
    v <- switch(nm, player_id = player_id, season = season, listed = listed)
    if (is.null(v) || length(v) != length(slot)) {
      cli::cli_abort(c(
        "{.arg {nm}} must be non-NULL and the same length as {.arg slot}.",
        "x" = "got {if (is.null(v)) 'NULL' else paste0('length ', length(v))} against {length(slot)}."))
    }
  }
  d <- data.table::data.table(i = seq_along(slot), slot = slot,
                              pid = as.character(player_id),
                              season = season, listed = as.character(listed),
                              bench = bench)
  # Mode of the non-bench slots, computed once per grouping.
  .mode <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_character_ else
    names(sort(table(x), decreasing = TRUE))[1] }
  on_ground <- d[bench == FALSE]
  by_ps <- on_ground[, .(m = .mode(slot)), by = .(pid, season)]
  by_p  <- on_ground[, .(m = .mode(slot)), by = .(pid)]

  d <- merge(d, by_ps, by = c("pid", "season"), all.x = TRUE, sort = FALSE)
  data.table::setnames(d, "m", "m_season")
  d <- merge(d, by_p, by = "pid", all.x = TRUE, sort = FALSE)
  data.table::setnames(d, "m", "m_career")
  d[, m_listed := unname(ROLE_FALLBACK_SLOT[listed])]

  d[, out := slot]
  d[bench == TRUE & !is.na(m_season), out := m_season]
  d[bench == TRUE & is.na(m_season) & !is.na(m_career), out := m_career]
  d[bench == TRUE & is.na(m_season) & is.na(m_career) & !is.na(m_listed), out := m_listed]
  data.table::setorder(d, i)

  n <- sum(bench)
  t1 <- d[bench == TRUE & !is.na(m_season), .N]
  t2 <- d[bench == TRUE & is.na(m_season) & !is.na(m_career), .N]
  t3 <- d[bench == TRUE & is.na(m_season) & is.na(m_career) & !is.na(m_listed), .N]
  t4 <- n - t1 - t2 - t3
  cli::cli_alert_info(paste0(
    "Bench-role remap: {n} bench player-game{?s} ",
    "({round(100 * n / length(slot), 1)}% of all) -> ",
    "{t1} by season role, {t2} by career role, {t3} by listed position, ",
    "{t4} left unresolved."))
  if (t4 > 0) cli::cli_alert_warning(
    "{t4} bench player-game{?s} still centred against the bench -- no role on record.")
  d$out
}

#' Position-adjust a per-80 EPV channel
#'
#' Recentres within position, and -- when \code{standardise} is TRUE --
#' rescales to the pooled cross-position spread as well, so the channel's
#' overall units are preserved while between-position spread differences are
#' removed. See \code{EPV_POSITION_STANDARDISE}.
#'
#' Falls back to centre-only when the within-position SD is absent or
#' degenerate; dividing by a near-zero SD amplifies without bound, which is
#' exactly the failure mode that excludes the hitout channel.
#'
#' @param p80 Per-80 channel value.
#' @param tog Time-on-ground weight.
#' @param pooled_sd Pooled (all-position) weighted SD for this channel.
#' @param standardise Logical; rescale as well as recentre.
#' @return Position-adjusted, TOG-scaled channel value.
#' @keywords internal
.position_adjust <- function(p80, tog, pooled_sd, standardise) {
  centred <- p80 - stats::weighted.mean(p80, tog, na.rm = TRUE)
  if (!isTRUE(standardise)) return(centred * tog)
  s <- .wtd_sd(p80, tog)
  if (is.na(s) || s < 1e-6 || is.na(pooled_sd)) return(centred * tog)
  centred / s * pooled_sd * tog
}

#' Zero-fill the contest / contextual-spoil columns after their left joins
#'
#' A player-match that took part in no priced contest or spoil has no row in
#' those tables, so the join leaves NA; the honest value is zero. Under v4
#' those tables are always empty, so every one of these columns is zero.
#'
#' @param df Player-match frame carrying the joined contest columns.
#' @return \code{df} with those columns zero-filled (counts as integer).
#' @noRd
.zero_fill_contest_cols <- function(df) {
  dplyr::mutate(
    df,
    contest_epv = tidyr::replace_na(contest_epv, 0),
    aerial_target_wins = as.integer(tidyr::replace_na(aerial_target_wins, 0)),
    aerial_target_losses = as.integer(tidyr::replace_na(aerial_target_losses, 0)),
    aerial_def_wins = as.integer(tidyr::replace_na(aerial_def_wins, 0)),
    aerial_def_losses = as.integer(tidyr::replace_na(aerial_def_losses, 0)),
    spoil_epv_ctx = tidyr::replace_na(spoil_epv_ctx, 0),
    spoils_priced = as.integer(tidyr::replace_na(spoils_priced, 0))
  )
}

#' Create Player Game Data
#'
#' Transforms raw play-by-play data and player stats into processed per-game
#' player performance data used by the TORP ratings pipeline.
#'
#' Computes disposal points, reception points, spoil/tackle points, and
#' hitout points for each player-game combination.
#'
#' \strong{The position adjustment here is full-history, not as-of.} The
#' \code{epv_*_adj} columns centre each channel against a reference computed
#' over every row in the frame passed in (grouped by role, with no season or
#' round in the key), so the reference for an early round includes games
#' played later. That is fine for the descriptive per-game display they feed,
#' and it means these columns must not be used as an as-of / predictive feature
#' without recomputing the reference from strictly earlier rounds, which is
#' what \code{centre_epv_by_position()} does for the rating layer.
#'
#' @param pbp_data Play-by-play data from \code{load_pbp()}. If NULL, loads all available.
#' @param player_stats Raw player stats from \code{load_player_stats()}. If NULL, loads all available.
#' @param teams Team lineup data from \code{load_teams()}. If NULL, loads all available.
#' @param chains Raw chains data from \code{load_chains()}. If NULL, loads all
#'   available. Used to compute failed reception credit from aerial contests.
#' @param decay Decay factor for time-weighting games. Default is \code{EPR_DECAY_DEFAULT_DAYS} (486).
#' @param epv_params Named list of EPV assignment parameters. If NULL,
#'   uses \code{default_epv_params()}.
#' @param epv_engine Which EPV engine to run. Only \code{"v4"} (Net Points)
#'   exists; anything else is an error (the v2 and v3 engines were removed on
#'   2026-11-16). Defaults to the \code{EPV_ENGINE} constant. Recorded on the
#'   returned frame as its \code{epv_engine} attribute and column.
#'
#' @param difficulty_terms v4 only: precomputed difficulty terms from
#'   `np_difficulty_terms_for_season()` (leak-safe). Fitted in-sample, with a
#'   warning, when `NULL`.
#' @param pre_match Pre-match forecasts (`match_id`, `home_win_prob`,
#'   optional `source`) for the WPA ledger (`build_wpa_ledger()`); when
#'   `NULL`, locked forecasts filled by retrodictions for the seasons in
#'   `pbp_data`.
#' @param np_fit v4 only: a season fit bundle (the nightly's stoppage baseline
#'   and difficulty and contest models, see `get_player_game_ratings()`). When
#'   given, these matches are scored on it and nothing is fitted, so a single
#'   live match gets the same pricing as the season build. Needs `chains` for
#'   the same matches.
#' @param keep_np_fit v4 only: attach what this call priced on as attribute
#'   `np_fit`, for the nightly to save as a release asset.
#' @return A data.table with one row per player per match, containing:
#'   identifiers (\code{player_id}, \code{match_id}, \code{season}, \code{round},
#'   \code{player_name}, \code{team}, \code{opponent}, \code{position_group}, \code{lineup_position}, \code{team_id},
#'   \code{utc_start_time}), position-adjusted EPV (\code{epv_adj},
#'   \code{epv_recv_adj}, \code{epv_disp_adj}, \code{epv_spoil_adj}, \code{epv_hitout_adj}),
#'   raw EPV (\code{epv}, \code{epv_recv}, \code{epv_disp}, \code{epv_spoil},
#'   \code{epv_hitout}), the retired v2 contest and contextual spoil columns
#'   (\code{contest_epv}, \code{aerial_*}, \code{spoil_epv_ctx},
#'   \code{spoils_priced}; always zero under v4, kept for the released schema),
#'   and key box-score stats.
#'
#' @export
#'
#' @importFrom dplyr arrange select mutate group_by summarise left_join filter ungroup if_else last n_distinct
#' @importFrom tidyr replace_na
#' @importFrom lubridate year
#' @importFrom stats quantile
#' @importFrom cli cli_warn
create_player_game_data <- function(pbp_data = NULL,
                                    player_stats = NULL,
                                    teams = NULL,
                                    chains = NULL,
                                    decay = EPR_DECAY_DEFAULT_DAYS,
                                    epv_params = NULL,
                                    epv_engine = EPV_ENGINE,
                                    difficulty_terms = NULL,
                                    pre_match = NULL,
                                    np_fit = NULL,
                                    keep_np_fit = FALSE) {

  # v4 is the only engine. The v2 (box-score) and v3 (chain-native contest)
  # engines were removed on 2026-11-16 after ten weeks unpublished; git history
  # keeps them. The argument stays so the engine a frame was built with is still
  # stamped on it, and so an old call asking for v2/v3 fails loudly instead of
  # silently getting v4.
  if (!identical(epv_engine, "v4")) {
    cli::cli_abort(c(
      "Unknown {.arg epv_engine}: {.val {epv_engine}}",
      "x" = "Only {.val v4} exists. The v2 and v3 engines were removed on 2026-11-16; git history keeps them."
    ))
  }
  if (!is.null(np_fit)) {
    if (!is.null(difficulty_terms)) {
      cli::cli_abort("Supply {.arg np_fit} or {.arg difficulty_terms}, not both.")
    }
    # Without chains the v4 arm would load the full history and score one
    # match against it -- the defect torp#250 is about.
    if (is.null(chains)) {
      cli::cli_abort("{.arg np_fit} needs {.arg chains} for the same matches as {.arg pbp_data}.")
    }
  }
  p <- if (is.null(epv_params)) default_epv_params() else epv_params

  if (is.null(pbp_data)) pbp_data <- load_pbp(TRUE)
  if (is.null(player_stats)) player_stats <- load_player_stats(TRUE)

  if (is.null(teams)) teams <- load_teams(TRUE)

  # Compute a single reference date for consistent decay weights across all data sources
  ref_date <- max(as.Date(pbp_data$utc_start_time), na.rm = TRUE)

  # --- Steps 1-3: PBP aggregation using data.table for performance ---
  # Convert once and compute shared columns (avoids two full dplyr scans of PBP)
  dt <- data.table::as.data.table(pbp_data)
  data.table::setorder(dt, match_id, display_order)
  dt[, `:=`(
    weight_gm = exp(as.numeric(-(ref_date - as.Date(utc_start_time))) / decay),
    opp_tm = data.table::fifelse(home_away == "Home", away_team_name, home_team_name)
  )]

  # Steps 1-2 below still build the old flat disposer/receiver split. Under v4
  # its epv_disp / epv_recv are overwritten by the Net Points channels further
  # down; what survives from it is the frame's rows and identifiers and the
  # disposals_pbp / receptions counts. (The v3 aerial-contest exclusion and the
  # EPV_DIFFICULTY_SPLIT re-pricing that used to sit here only ever fed the v2
  # and v3 channels, and were removed with them on 2026-11-16.)

  # Step 1: Disposal points (grouped by player_id + match_id)
  # For contested kicks (contest_target_id is non-NA), reduce disposal scale
  # from 50% to contest_share (1/3) — the remaining credit goes to target/defender.
  # Backward compat: if contest_target_id column doesn't exist in older PBP, use
  # full disp_scale for all rows.
  contest_share <- p$contest_share %||% (1 / 3)
  has_contest_col <- "contest_target_id" %in% names(dt)
  dt[, .disp_scale := if (has_contest_col) {
    data.table::fifelse(!is.na(contest_target_id), contest_share, p$disp_scale)
  } else {
    p$disp_scale
  }]
  disp_dt <- dt[, .(
    player_name = max(player_name, na.rm = TRUE),
    utc_start_time = max(utc_start_time),
    weight_gm = max(weight_gm),
    epv_disp = sum(data.table::fifelse(pos_team == -1, delta_epv + p$disp_neg_offset, delta_epv + p$disp_pos_offset) * .disp_scale),
    disposals_pbp = floor(.N / 2L),
    team = team[.N],
    opponent = opp_tm[.N],
    position_group = player_position[.N],
    round = as.numeric(round_week[.N]),
    season = lubridate::year(utc_start_time[.N])
  ), by = .(player_id, match_id)]
  dt[, .disp_scale := NULL]

  # Step 2: Reception points (grouped by lead_player_id + match_id)
  # Exclude rows where lead_desc is a contest target — those plays are credited
  # via contest_epv instead (avoids double-counting the 3-way split)
  # Exclude contested kicks from epv_recv: if contest_target_id is set on a row,

  # it's a 3-player contest handled by contest_epv instead
  dt[, is_contest_target_recv :=has_contest_col & !is.na(contest_target_id)]
  dt[, is_intercept_mark := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]
  recv_dt <- dt[is_contest_target_recv == FALSE, .(
    epv_recv = sum(data.table::fifelse(
      is_intercept_mark,
      ((p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset) * p$recv_intercept_mark_scale,
      data.table::fifelse(
        pos_team == -1L,
        ((p$recv_neg_mult * delta_epv * pos_team) + p$recv_neg_offset) * p$recv_scale,
        ((p$recv_pos_mult * delta_epv * pos_team) + p$recv_pos_offset) * p$recv_scale
      )
    )),
    receptions = .N
  ), by = .(lead_player_id, match_id)]
  dt[, c("is_intercept_mark", "is_contest_target_recv") := NULL]

  # Step 3: Join disposal + reception
  plyr_gm_df <- merge(disp_dt, recv_dt,
    by.x = c("player_id", "match_id"),
    by.y = c("lead_player_id", "match_id"),
    all.x = TRUE, sort = FALSE)

  # --- Step 3b: WPA credit ---
  # wp_failed is read at the zero-fill after the v4 channels: a player-match
  # missing from a run that SUCCEEDED is a real zero, but a failed run must
  # leave the columns NA, never a plausible-looking all-zero channel.
  wp_failed <- FALSE
  wp_dt <- tryCatch({
    wpc <- create_wp_credit(pbp_data)
    wpc[, .(player_id, match_id, wp_credit, wp_disp_credit, wp_recv_credit)]
  }, error = function(e) {
    wp_failed <<- TRUE
    cli::cli_warn("WPA credit skipped, wp_credit columns will be NA: {conditionMessage(e)}")
    data.table::data.table(
      player_id = character(), match_id = character(),
      wp_credit = numeric(), wp_disp_credit = numeric(), wp_recv_credit = numeric()
    )
  })
  plyr_gm_df <- merge(plyr_gm_df, wp_dt,
    by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)

  # --- Step 3c: WPA Net Points ledger (docs/plans/WPA-NET-LEDGER.md) ---
  # Two runs, same per-play credit, different starting point. wpa_net starts
  # each team at its pre-match forecast (locked, else retrodiction; matches
  # with neither are skipped); wpa_neutral starts every home team at
  # WPA_NEUTRAL_HOME_PROB, so it covers every match. wpa_own/won/team are
  # wpa_net's parts and wpa_neutral_own/won/team are wpa_neutral's. The
  # neutral run is the blog's default WPA (Pete, 2026-09-28): like net points,
  # it does not start from team ratings, so it needs the same breakdown. Published beside wp_credit, which stays until the blog
  # has switched. On failure a run's columns are NA, never 0: a zero would read
  # as a real, average game, and torpdata's coverage check fails on NA.
  .wpa_run <- function(label, pm, cols, rename = NULL) {
    tryCatch({
      wpn <- build_wpa_ledger(pbp_data, player_stats, pm)
      skipped <- attr(wpn, "skipped")
      keep <- plyr_gm_df[!as.character(match_id) %in% skipped, .(player_id, match_id)]
      dt <- .wpa_respread_lost(wpn, keep, pbp_data, player_stats)
      if (!is.null(rename)) data.table::setnames(dt, names(rename), unname(rename))
      list(dt = dt[, c("player_id", "match_id", cols), with = FALSE], skipped = skipped)
    }, error = function(e) {
      cli::cli_warn("WPA ledger ({label}) skipped, {.field {cols}} will be NA: {conditionMessage(e)}")
      NULL
    })
  }
  .wpa_attach <- function(frame, run, cols) {
    if (is.null(run)) {
      for (cc in cols) data.table::set(frame, j = cc, value = NA_real_)
      return(frame)
    }
    frame <- merge(frame, run$dt, by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)
    # A player in a rated match with no ledger row made no allocatable act;
    # his share is genuinely zero. A skipped match stays NA: nobody in it has
    # a number, and 0 would say they did.
    rated <- !as.character(frame$match_id) %in% run$skipped
    for (cc in cols) data.table::set(frame, i = which(rated & is.na(frame[[cc]])), j = cc, value = 0)
    frame
  }
  wpn_cols <- c("wpa_net", "wpa_own", "wpa_won", "wpa_team")
  wpn_neutral_cols <- c("wpa_neutral", "wpa_neutral_own", "wpa_neutral_won", "wpa_neutral_team")
  pm <- pre_match
  if (is.null(pm)) {
    pm <- tryCatch(.wpa_pre_match(sort(unique(as.integer(substr(as.character(pbp_data$match_id), 5, 8))))),
                   error = function(e) {
                     cli::cli_warn("WPA ledger: loading pre-match forecasts failed, so wpa_net will be NA: {conditionMessage(e)}")
                     data.table::data.table(match_id = character(), home_win_prob = numeric())
                   })
  }
  plyr_gm_df <- .wpa_attach(plyr_gm_df, .wpa_run("forecast start", pm, wpn_cols), wpn_cols)
  plyr_gm_df <- .wpa_attach(
    plyr_gm_df,
    .wpa_run("neutral start", .wpa_neutral_pre_match(pbp_data$match_id), wpn_neutral_cols,
             rename = c(wpa_net = "wpa_neutral", wpa_own = "wpa_neutral_own",
                        wpa_won = "wpa_neutral_won", wpa_team = "wpa_neutral_team")),
    wpn_neutral_cols)

  # --- Step 3c: the v2 contest and contextual-spoil columns, kept at zero ---
  # contest_epv / aerial_target_* / aerial_def_* (v2's 3-way aerial split) and
  # spoil_epv_ctx / spoils_priced (v2's contextual spoil pricing) are still in
  # player_game_data's released schema, so they stay -- as zeros, which is what
  # v4 has always published in them. The v2 code that filled them
  # (compute_contest_credit(), compute_spoil_credit()) was removed on
  # 2026-11-16. Joined against empty tables so the column types are unchanged.
  empty_contest <- data.table::data.table(
    player_id = character(), match_id = character(),
    contest_epv = numeric(),
    aerial_target_wins = integer(), aerial_target_losses = integer(),
    aerial_def_wins = integer(), aerial_def_losses = integer()
  )
  plyr_gm_df <- merge(plyr_gm_df, empty_contest,
    by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)
  empty_spoil_ctx <- data.table::data.table(
    player_id = character(), match_id = character(),
    spoil_epv_ctx = numeric(), spoils_priced = integer()
  )
  plyr_gm_df <- merge(plyr_gm_df, empty_spoil_ctx,
    by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)

  # --- Step 4: Join spoils/tackles/hitouts from raw player_stats ---
  spoil_hitout_df <- player_stats |>
    dplyr::mutate(
      weight_gm = exp(as.numeric(-(ref_date - as.Date(utc_start_time))) / decay),
      epv_spoil = spoils * p$spoil_wt + tackles * p$tackle_wt + pressure_acts * p$pressure_wt + def_half_pressure_acts * p$def_pressure_wt +
                  intercepts * p$intercepts_wt + one_percenters * p$one_percenters_wt + rebound50s * p$rebound50s_wt + frees_against * p$frees_against_wt,
      epv_hitout = hitouts * p$hitout_wt + hitouts_to_advantage * p$hitout_adv_wt + ruck_contests * p$ruck_contest_wt
    ) |>
    dplyr::select(-dplyr::any_of(c("utc_start_time", "player_name", "given_name", "surname",
                                   "player_captain", "player_jumper_number", "player_photo_url",
                                   "home_team_name", "away_team_name", "last_updated",
                                   "team_status")))

  # A duplicated (player_id, match_id) in player_stats would multiply rows here
  # and double every per-player sum downstream without erroring.
  n_before_join <- nrow(plyr_gm_df)
  plyr_gm_df <- plyr_gm_df |>
    dplyr::left_join(
      spoil_hitout_df,
      by = c("player_id" = "player_id", "match_id" = "match_id")
    )
  if (nrow(plyr_gm_df) != n_before_join) {
    cli::cli_abort(c(
      "Joining player_stats changed the row count ({n_before_join} -> {nrow(plyr_gm_df)}).",
      "x" = "{nrow(plyr_gm_df) - n_before_join} extra row{?s}: {.field player_stats} has duplicated (player_id, match_id) keys, which would double-count those player-matches."
    ))
  }

  # Assert join produced matches (catches upstream schema changes)
  if (all(is.na(plyr_gm_df$epv_spoil))) {
    cli::cli_abort(c(
      "Player stats join produced no matches - all spoil/hitout points are zero.",
      "i" = "Check that {.fn load_player_stats} returns the expected column names."
    ))
  }

  # --- Step 5: Replace NAs and compute totals ---
  # Zero-fill all box-score stats before weighted sums to prevent NA propagation
  box_score_cols <- c(
    "contested_possessions", "contested_marks", "ground_ball_gets",
    "marks_inside50", "marks", "uncontested_possessions", "frees_for",
    "inside50s", "clangers", "score_involvements", "kicks", "handballs",
    "metres_gained", "turnovers", "goal_assists", "goals", "behinds",
    "shots_at_goal"
  )
  for (col in intersect(box_score_cols, names(plyr_gm_df))) {
    plyr_gm_df[[col]] <- tidyr::replace_na(plyr_gm_df[[col]], 0)
  }

  # v4: the Net Points ledger (docs/plans/EPV-V4-CREDIT-RULES.md). One value
  # per player-match that sums, across a match, to the margin. Three
  # channels, Pete's starting point for EPR: own acts (decisions, surprises
  # and what he received, net of what his losses ceded), what he won back
  # (turnovers, contests, stoppages), and his share of the team pools. The
  # reconciliation residual rides with the pools so the channels sum to the
  # total exactly. The four column names are kept so EPR's plumbing reads
  # them unchanged: recv = won, disp = own, spoil = pools, hitout = 0.
  if (is.null(chains) && is.null(difficulty_terms)) chains <- load_chains(TRUE)
  np <- .np_engine_frame(pbp_data, player_stats, chains, difficulty_terms,
                         np_fit = np_fit, return_fit = isTRUE(keep_np_fit))
  np_fit_used <- attr(np, "np_fit")
  np_dt <- .np_v4_channels(np)
  # Where each player-match's net points came from, by play type (published
  # beside the game data; see .np_breakdown()). Only the team-margin
  # convention exposes final-value payments, so it is NULL without it.
  np_bd <- if (isTRUE(NP_TEAM_MARGIN_CONVENTION)) .np_breakdown(np, pbp_data) else NULL
  # A player with net points but no play-by-play act (a pool share for a sub
  # who never touched it) has no row in this frame, in any engine. His value
  # re-spreads across his team-mates in that match by time on ground, so the
  # frame still sums to the margin; the amount is logged (1.56 points across
  # 2026, two player-matches).
  lost <- np_dt[!plyr_gm_df, on = .(player_id, match_id)]
  if (nrow(lost) > 0) {
    cli::cli_alert_warning(
      "v4: {nrow(lost)} player-match{?es} with net points but no play-by-play act ({round(sum(abs(lost$net_points)), 2)} points) re-spread within their team by time on ground.")
    lost_team <- np[lost, on = .(player_id, match_id), .(match_id, team, v = i.net_points)][
      , .(v = sum(v)), by = .(match_id, team)]
    np_dt <- merge(np_dt, np[, .(player_id, match_id, team)], by = c("player_id", "match_id"))
    np_dt <- np_dt[!lost, on = .(player_id, match_id)]
    # Built from the three columns it reads: as.data.table() on the whole
    # player_stats deep-copies every column to keep two.
    tg <- data.table::data.table(
      player_id = player_stats$player_id,
      match_id = player_stats$match_id,
      tog = pmax(player_stats$time_on_ground_percentage, 1))
    np_dt[tg, on = .(player_id, match_id), tog := i.tog]
    np_dt[is.na(tog), tog := 75]
    np_dt[lost_team, on = .(match_id, team), v_lost := i.v]
    np_dt[is.na(v_lost), v_lost := 0]
    np_dt[, share := v_lost * tog / sum(tog), by = .(match_id, team)]
    # every lost point must land on a surviving team-mate; a team with no
    # surviving row at all is a data failure, not a rounding case
    respread <- np_dt[, .(got = sum(share)), by = .(match_id, team)]
    owed <- merge(lost_team, respread, by = c("match_id", "team"), all.x = TRUE)
    owed[is.na(got), got := 0]
    if (nrow(owed) && max(abs(owed$v - owed$got)) > 1e-8) {
      cli::cli_abort(c(
        "v4: {round(sum(abs(owed$v - owed$got)), 2)} points of net points belong to a team with no player row in the frame.",
        "x" = "Nothing to re-spread them across; the frame would no longer sum to the margin."
      ))
    }
    np_dt[, `:=`(np_pool = np_pool + share, net_points = net_points + share)]
    if (!is.null(np_bd)) {
      np_bd <- np_bd[!lost[, .(player_id = as.character(player_id), match_id = as.character(match_id))],
                     on = .(player_id, match_id)]
      np_bd <- rbind(np_bd, np_dt[share != 0, .(match_id = as.character(match_id),
                                                player_id = as.character(player_id),
                                                category = "Re-spread from a team-mate with no act",
                                                value = share)])
    }
    np_dt[, c("team", "tog", "v_lost", "share") := NULL]
  }
  plyr_gm_df <- plyr_gm_df |>
    dplyr::left_join(as.data.frame(np_dt), by = c("player_id", "match_id"))
  # (the WP columns are zero-filled once, below)
  plyr_gm_df <- .zero_fill_contest_cols(plyr_gm_df) |>
    dplyr::mutate(
      epv_recv = tidyr::replace_na(np_won, 0),
      epv_disp = tidyr::replace_na(np_own, 0),
      epv_spoil = tidyr::replace_na(np_pool, 0),
      epv_hitout = 0,
      epv = epv_recv + epv_disp + epv_spoil + epv_hitout,
      net_points = tidyr::replace_na(net_points, 0)
    ) |>
    dplyr::select(-dplyr::any_of(c("np_own", "np_won", "np_pool")))
  gap <- max(abs(plyr_gm_df$epv - plyr_gm_df$net_points))
  if (gap > 1e-8) cli::cli_abort("v4 channels do not sum to net points (max gap {signif(gap, 3)}).")

  # WPA is engine-independent -- it reads the WP model, not the EPV channels.
  # Skipped when create_wp_credit() failed: see wp_failed above.
  if (!wp_failed) {
    plyr_gm_df <- plyr_gm_df |>
      dplyr::mutate(
        wp_credit = tidyr::replace_na(wp_credit, 0),
        wp_disp_credit = tidyr::replace_na(wp_disp_credit, 0),
        wp_recv_credit = tidyr::replace_na(wp_recv_credit, 0)
      )
  }

  # --- Step 6: Join teams data for lineup_position (20-way AFL lineup role) ---
  teams_pos <- teams |>
    dplyr::select(match_id, player_id, lineup_position) |>
    dplyr::distinct()
  plyr_gm_df <- plyr_gm_df |>
    dplyr::select(-dplyr::any_of(c("lineup_position"))) |>
    dplyr::left_join(teams_pos, by = c("match_id", "player_id")) |>
    dplyr::mutate(
      position_group = dplyr::if_else(.data$position_group == "MIDFIELDER_FORWARD",
                                      "MEDIUM_FORWARD", .data$position_group)
    )

  # --- Step 7: Per-80 normalisation then position adjustment ---
  # Normalise to per-full-game rate BEFORE position adjustment so the adjustment
  # compares like-for-like rates, not raw totals that mix ability with TOG.
  # Group on lineup_position (20-way role from teams API) — more reliable than
  # position_group (6-way from PBP) which can be inconsistent with actual role.
  plyr_gm_df <- plyr_gm_df |>
    dplyr::mutate(
      tog_safe = pmax(dplyr::coalesce(.data$time_on_ground_percentage / 100, 0.1), 0.1),
      epv_recv_p80 = .data$epv_recv / .data$tog_safe,
      epv_disp_p80 = .data$epv_disp / .data$tog_safe,
      epv_spoil_p80 = .data$epv_spoil / .data$tog_safe,
      epv_hitout_p80 = .data$epv_hitout / .data$tog_safe,
      wp_credit_p80 = .data$wp_credit / .data$tog_safe,
      wp_disp_credit_p80 = .data$wp_disp_credit / .data$tog_safe,
      wp_recv_credit_p80 = .data$wp_recv_credit / .data$tog_safe
    )

  # Pooled (all-position) spread per channel — the scale the standardised
  # adjustment restores, so the metric keeps its units and only the
  # BETWEEN-position spread differences change. Computed before grouping.
  .epv_ch <- c("recv", "disp", "spoil", "hitout")
  .pooled_sd <- vapply(.epv_ch, function(ch)
    .wtd_sd(plyr_gm_df[[paste0("epv_", ch, "_p80")]], plyr_gm_df$tog_safe),
    numeric(1))
  .std <- stats::setNames(
    isTRUE(EPV_POSITION_STANDARDISE) & .epv_ch %in% EPV_STANDARDISE_CHANNELS,
    .epv_ch
  )

  # The role-adjustment key. Raw `lineup_position` (21 slots) by default; with
  # ROLE_USE_LINEUP_GROUP, the mirror-merged `lineup_group` (16); and with
  # ROLE_REMAP_BENCH, bench starts replaced by the role actually filled.
  #
  # NOTE: it is assigned to a column for the group_by, then DROPPED before the
  # frame is returned -- see the select() below, which explains why. An earlier
  # version of this comment claimed it stayed "inspectable on the returned
  # frame"; it does not, and believing that cost a diagnostic on 2026-08-06.
  # To recover it outside the pipeline, call .remap_bench_role() on the frame's
  # own lineup_position / player_id / season / position_group.
  .slot <- as.character(plyr_gm_df$lineup_position)
  if (isTRUE(ROLE_REMAP_BENCH)) {
    # `season` does not exist yet here -- it is created ~40 lines below from
    # `season.x`, a leftover of an earlier merge. Resolve it rather than reading
    # a NULL, which surfaced as an opaque data.table "by is type NULL" error.
    .season <- plyr_gm_df[["season"]] %||% plyr_gm_df[["season.x"]] %||%
      lubridate::year(plyr_gm_df[["utc_start_time"]])
    .slot <- .remap_bench_role(.slot, plyr_gm_df$player_id, .season,
                               plyr_gm_df$position_group)
  }
  plyr_gm_df$.role_key <- if (isTRUE(ROLE_USE_LINEUP_GROUP)) {
    .collapse_lineup_group(.slot)
  } else {
    .slot
  }
  # A wholly-NA key would silently centre every player against the same global
  # cell -- the "guard degrades to a no-op" failure this repo keeps hitting.
  if (all(is.na(plyr_gm_df$.role_key))) {
    cli::cli_abort(c(
      "EPV role adjustment key is entirely NA.",
      "i" = "ROLE_USE_LINEUP_GROUP = {ROLE_USE_LINEUP_GROUP}",
      "x" = "Refusing to centre every player against one global cell."
    ))
  }

  # The hitout channel gets its OWN cell when EPV_HITOUT_CENTRE_ON_RUCK is on.
  # It is the one channel that only exists for players who ruck, so celling it
  # on a position label compares a part-time ruck with people who never contest
  # a bounce -- see docs/reviews/INT-CENTRING-BUG-2026-08-06.md. The other three
  # channels keep the positional key, where it is doing real work.
  plyr_gm_df$.hitout_key <- if (isTRUE(EPV_HITOUT_CENTRE_ON_RUCK)) {
    # Same failure this file already guards against for `.role_key`, one key
    # along: `coalesce(..., 0)` cannot produce NA, so a missing or all-NA
    # `ruck_contests` does not fail -- it puts EVERY player in "OTHER" and
    # quietly centres the ruck channel against one global cell, which is the
    # exact defect celling on involvement was introduced to fix. The info line
    # below would read "0 of N in the RUCKS cell" and nothing reads it.
    if (!"ruck_contests" %in% names(plyr_gm_df) ||
        !any(is.finite(suppressWarnings(as.numeric(plyr_gm_df$ruck_contests))))) {
      cli::cli_abort(c(
        "EPV_HITOUT_CENTRE_ON_RUCK is on but {.field ruck_contests} is absent or has no finite values.",
        "x" = "Refusing to centre the hitout channel against a single global cell."
      ))
    }
    .rc <- dplyr::coalesce(as.numeric(plyr_gm_df$ruck_contests), 0)
    ifelse(.rc >= EPV_RUCK_INVOLVEMENT_MIN, "RUCKS", "OTHER")
  } else {
    plyr_gm_df$.role_key
  }
  if (isTRUE(EPV_HITOUT_CENTRE_ON_RUCK)) {
    cli::cli_alert_info(
      "Hitout centred on ruck involvement: {sum(plyr_gm_df$.hitout_key == 'RUCKS')} of {nrow(plyr_gm_df)} player-games in the RUCKS cell (>= {EPV_RUCK_INVOLVEMENT_MIN} contests).")
  }

  # Hitout: blended reference if a width is set, otherwise the hard cell.
  .blend_on <- isTRUE(EPV_HITOUT_CENTRE_ON_RUCK) && EPV_RUCK_BLEND_WIDTH > 0
  if (.blend_on) {
    .lo <- EPV_RUCK_INVOLVEMENT_MIN - EPV_RUCK_BLEND_WIDTH / 2
    .hi <- EPV_RUCK_INVOLVEMENT_MIN + EPV_RUCK_BLEND_WIDTH / 2
    plyr_gm_df$epv_hitout_adj <- .blend_adjust(
      plyr_gm_df$epv_hitout_p80, plyr_gm_df$tog_safe,
      dplyr::coalesce(as.numeric(plyr_gm_df$ruck_contests), 0),
      .lo, .hi, .pooled_sd[["hitout"]], .std[["hitout"]])
    # Non-dot names for the message: cli reads `{.lo}` as inline markup, the
    # same class as `{.code}` or `{.val}`, and hard-errors with "Invalid cli
    # literal". Documented in this repo and hit again on 2026-08-06.
    ramp_lo <- .lo; ramp_hi <- .hi
    n_ramp <- sum(dplyr::between(
      dplyr::coalesce(as.numeric(plyr_gm_df$ruck_contests), 0), .lo, .hi))
    cli::cli_alert_info(
      "Hitout reference BLENDED across {ramp_lo}-{ramp_hi} ruck contests ({n_ramp} player-games in the ramp).")
  } else {
    plyr_gm_df <- plyr_gm_df |>
      dplyr::group_by(.data$.hitout_key) |>
      dplyr::mutate(
        epv_hitout_adj = .position_adjust(.data$epv_hitout_p80, .data$tog_safe, .pooled_sd[["hitout"]], .std[["hitout"]])
      ) |>
      dplyr::ungroup()
  }

  plyr_gm_df <- plyr_gm_df |>
    dplyr::group_by(.data$.role_key) |>
    dplyr::mutate(
      epv_recv_adj = .position_adjust(.data$epv_recv_p80, .data$tog_safe, .pooled_sd[["recv"]], .std[["recv"]]),
      epv_disp_adj = .position_adjust(.data$epv_disp_p80, .data$tog_safe, .pooled_sd[["disp"]], .std[["disp"]]),
      epv_spoil_adj = .position_adjust(.data$epv_spoil_p80, .data$tog_safe, .pooled_sd[["spoil"]], .std[["spoil"]]),
      epv_adj = .data$epv_recv_adj + .data$epv_disp_adj + .data$epv_spoil_adj + .data$epv_hitout_adj,
      wp_credit_adj = (.data$wp_credit_p80 - stats::weighted.mean(.data$wp_credit_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe,
      wp_disp_credit_adj = (.data$wp_disp_credit_p80 - stats::weighted.mean(.data$wp_disp_credit_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe,
      wp_recv_credit_adj = (.data$wp_recv_credit_p80 - stats::weighted.mean(.data$wp_recv_credit_p80, .data$tog_safe, na.rm = TRUE)) * .data$tog_safe
    ) |>
    dplyr::ungroup() |>
    # .role_key is dropped here, not kept for inspection: player_game_data is a
    # RELEASED artifact with a declared column schema, and an extra column would
    # fail validation downstream. The arms are distinguished by asserting they
    # differ numerically, which is the stronger check anyway.
    dplyr::select(-"tog_safe", -".role_key", -".hitout_key",
                  -"epv_recv_p80", -"epv_disp_p80", -"epv_spoil_p80", -"epv_hitout_p80",
                  -"wp_credit_p80", -"wp_disp_credit_p80", -"wp_recv_credit_p80")

  # --- Step 8: Handle duplicate season columns and select final columns ---
  if ("season.x" %in% names(plyr_gm_df)) {
    plyr_gm_df <- plyr_gm_df |> dplyr::mutate(season = season.x)
  }

  plyr_gm_df <- plyr_gm_df |>
    dplyr::filter(!is.na(team)) |>
    dplyr::select(
      # Identifiers
      player_id, match_id, season, round,
      player_name, team, opponent, position_group, lineup_position, team_id,
      utc_start_time,
      # EPV (position-adjusted)
      epv_adj, epv_recv_adj, epv_disp_adj, epv_spoil_adj, epv_hitout_adj,
      # EPV (raw)
      epv, epv_recv, epv_disp, epv_spoil, epv_hitout,
      # WPA (position-adjusted)
      wp_credit_adj, wp_disp_credit_adj, wp_recv_credit_adj,
      # WPA (raw)
      wp_credit, wp_disp_credit, wp_recv_credit,
      # Retired v2 contest credit (3-way aerial split): always zero under v4,
      # kept because the released schema declares it
      contest_epv, aerial_target_wins, aerial_target_losses,
      aerial_def_wins, aerial_def_losses,
      # Retired v2 contextual spoil credit (WS2a): always zero under v4
      spoil_epv_ctx, spoils_priced,
      # Under any_of() so the output keeps its declared schema exactly --
      # column_schema.R would reject extras on a released frame. Only
      # net_points is produced now; the other four were v3's.
      dplyr::any_of(c("epv_cont_aerial", "epv_cont_stop",
                      "contests_won", "contests_lost", "net_points")),
      # WPA ledger (Step 3c). any_of() for the same schema reason as above.
      dplyr::any_of(c("wpa_net", "wpa_neutral", "wpa_own", "wpa_won", "wpa_team",
                      "wpa_neutral_own", "wpa_neutral_won", "wpa_neutral_team")),
      # PBP-derived action counts
      disposals_pbp, receptions,
      # EPV model input stats
      spoils, tackles, pressure_acts,
      def_half_pressure_acts,
      hitouts, hitouts_to_advantage, ruck_contests,
      bounces,
      # Core box-score stats
      goals, behinds, kicks, handballs, disposals, marks,
      contested_possessions, uncontested_possessions,
      inside50s, marks_inside50, contested_marks,
      clearances,
      metres_gained, time_on_ground_percentage,
      intercepts, rebound50s, one_percenters,
      frees_for, frees_against, clangers, turnovers,
      score_involvements, shots_at_goal, goal_assists,
      ground_ball_gets,
      # Efficiency stats
      dplyr::any_of(c("effective_disposals", "effective_kicks",
                       "disposal_efficiency", "kick_efficiency"))
    )

  # Tag the frame with the engine that produced it. Downstream steps
  # (centre_epv_by_position) need to know which points-scale convention applies,
  # and reading the global EPV_ENGINE there is wrong: a caller passing
  # epv_engine = "v3" while the constant still reads "v2" would silently get v2
  # scaling, which is exactly how every arm in this session was run.
  attr(plyr_gm_df, "epv_engine") <- epv_engine

  # The play-type breakdown rides as an attribute (the release saves it as its
  # own file). Checked against the FINISHED frame: every player-match's
  # categories must add up to the net_points published beside them.
  if (exists("np_bd", inherits = FALSE) && !is.null(np_bd)) {
    chk <- merge(np_bd[, .(tot = sum(value)), by = .(match_id, player_id)],
                 data.table::data.table(match_id = as.character(plyr_gm_df$match_id),
                                        player_id = as.character(plyr_gm_df$player_id),
                                        net_points = plyr_gm_df$net_points),
                 by = c("match_id", "player_id"), all = TRUE)
    chk[is.na(tot), tot := 0][is.na(net_points), net_points := 0]
    bd_gap <- max(abs(chk$tot - chk$net_points))
    if (!is.finite(bd_gap) || bd_gap > 1e-8) {
      cli::cli_abort("v4: the play-type breakdown misses published net_points by {signif(bd_gap, 3)} points.")
    }
    # The same breakdown, summed into the four columns game-logs publishes
    # (np_disp / np_recv / np_contest / np_team, .NP_PLAY_TYPE_FAMILY). Built
    # here, on the frame the breakdown was checked against, so they travel
    # with player_game_<season> as columns and cannot drift from it (torp#263).
    fam <- .np_family_sums(np_bd)
    key <- data.table::data.table(match_id = as.character(plyr_gm_df$match_id),
                                  player_id = as.character(plyr_gm_df$player_id))
    fam <- fam[key, on = c("match_id", "player_id")]
    for (v in .NP_FAMILY_COLS) {
      x <- fam[[v]]
      x[is.na(x)] <- 0   # a player-match with no category: nothing to split
      plyr_gm_df[[v]] <- x
    }
    .np_family_gate(plyr_gm_df, np_bd)
    attr(plyr_gm_df, "np_breakdown") <- np_bd
  }

  # And again as a COLUMN, because the attribute does not survive the parquet
  # round-trip this frame makes through the release. Without it Stage 3 reloads
  # the frame unstamped and prices v3 data as v2 -- which the pipeline reports
  # in a warning and then carries on regardless. .restore_epv_engine_attr()
  # turns this back into the attribute on load.
  if (!is.null(epv_engine) && nrow(plyr_gm_df) > 0) {
    plyr_gm_df[["epv_engine"]] <- as.character(epv_engine)
  }
  # What these matches were priced on (torp#250). Attached last, and only on
  # request, so the default frame is unchanged; strip it before a parquet write.
  if (isTRUE(keep_np_fit)) attr(plyr_gm_df, "np_fit") <- np_fit_used
  return(plyr_gm_df)
}
