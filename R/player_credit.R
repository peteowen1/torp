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
#' Keyed on the engine ACTUALLY IN USE (the attribute the frame carries), not
#' the global default -- a caller passing \code{epv_engine = "v3"} while the
#' constant still reads \code{"v2"} would otherwise get no per-channel scaling.
#'
#' Deliberately NOT also conditioned on the vector being non-trivial: under v3
#' the per-channel vector is authoritative even when it is all 1s. All 1s means
#' no scaling, not "fall back to v2's 0.919".
#'
#' @param engine_attr The frame's \code{epv_engine} attribute, or NULL.
#' @return TRUE if \code{EPV3_POINTS_SCALE} should be applied per channel.
#' @keywords internal
.use_per_channel_scale <- function(engine_attr) {
  (identical(engine_attr, "v3") || isTRUE(EPV_PER_CHANNEL_POINTS_SCALE)) &&
    exists("EPV3_POINTS_SCALE")
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
  if (is.null(eng) && identical(configured, "v3")) {
    cli::cli_warn(c(
      "The {what} carries no {.field epv_engine} attribute while {.code EPV_ENGINE} is {.val v3}.",
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
    s <- dt[!is.na(.cpg) & is.finite(get(cc)) & is.finite(.ctog) & .ctog > 0,
            .(sx = sum(get(cc) * .ctog), sw = sum(.ctog)),
            by = .(season, round, .cpg)]
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
  use_v3_scale <- .use_per_channel_scale(.frame_epv_engine(pgd, "player-game frame"))
  # Defined unconditionally so the residual-expectation block below can use it
  # without depending on which branch ran.
  lbl <- EPV_CHANNEL_SCALE_KEYS
  if (use_v3_scale) {
    for (cc in cols) {
      stem <- sub("_(oadj|adj)$", "", cc)
      k <- EPV3_POINTS_SCALE[[lbl[[stem]]]]
      if (is.finite(k)) dt[, (cc) := get(cc) * k]
    }
    cli::cli_alert_info(
      "Applied per-channel points scale: {paste(names(EPV3_POINTS_SCALE), round(EPV3_POINTS_SCALE, 4), sep = '=', collapse = ', ')}")
  } else if (is.finite(EPV_POINTS_SCALE) && !isTRUE(all.equal(EPV_POINTS_SCALE, 1))) {
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

#' Create Player Game Data
#'
#' Transforms raw play-by-play data and player stats into processed per-game
#' player performance data used by the TORP ratings pipeline.
#'
#' Computes disposal points, reception points, spoil/tackle points, and
#' hitout points for each player-game combination.
#'
#' @param pbp_data Play-by-play data from \code{load_pbp()}. If NULL, loads all available.
#' @param player_stats Raw player stats from \code{load_player_stats()}. If NULL, loads all available.
#' @param teams Team lineup data from \code{load_teams()}. If NULL, loads all available.
#' @param chains Raw chains data from \code{load_chains()}. If NULL, loads all
#'   available. Used to compute failed reception credit from aerial contests.
#' @param decay Decay factor for time-weighting games. Default is \code{EPR_DECAY_DEFAULT_DAYS} (486).
#' @param epv_params Named list of EPV assignment parameters. If NULL,
#'   uses \code{default_epv_params()}.
#' @param epv_engine Which EPV engine to run: \code{"v2"} (production) or
#'   \code{"v3"} (chain-native, held). Defaults to the \code{EPV_ENGINE}
#'   constant. Passed explicitly rather than read from the constant inside, so
#'   a caller can select an engine the global default does not name; the choice
#'   is recorded on the returned frame as its \code{epv_engine} attribute.
#'
#' @return A data.table with one row per player per match, containing:
#'   identifiers (\code{player_id}, \code{match_id}, \code{season}, \code{round},
#'   \code{player_name}, \code{team}, \code{opponent}, \code{position_group}, \code{lineup_position}, \code{team_id},
#'   \code{utc_start_time}), position-adjusted EPV (\code{epv_adj},
#'   \code{epv_recv_adj}, \code{epv_disp_adj}, \code{epv_spoil_adj}, \code{epv_hitout_adj}),
#'   raw EPV (\code{epv}, \code{epv_recv}, \code{epv_disp}, \code{epv_spoil},
#'   \code{epv_hitout}), contextual spoil credit (\code{spoil_epv_ctx},
#'   \code{spoils_priced} — see \code{compute_spoil_credit()}; not folded into
#'   \code{epv_spoil}), and key box-score stats.
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
                                    epv_engine = EPV_ENGINE) {

  if (!epv_engine %in% c("v2", "v3")) {
    cli::cli_abort(c(
      "Unknown {.arg epv_engine}: {.val {epv_engine}}",
      "x" = "Refusing to guess an engine -- expected {.val v2} or {.val v3}."
    ))
  }
  v3 <- identical(epv_engine, "v3")

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

  # --- v3: aerial contests, computed before the disposer/receiver split ---
  # Every kick resolved in the air is removed from that split and paid instead as
  # disposal (V_pre - exp_pts, to the kicker) plus a zero-sum contest term
  # (winner/loser). Skipping the exclusion would pay the same swing twice.
  aerial <- NULL
  aerial_keys <- NULL
  if (v3) {
    if (is.null(chains)) chains <- load_chains(TRUE)
    aerial <- compute_aerial_credit(chains, pbp_data, player_stats = player_stats)
    aerial_keys <- attr(aerial, "aerial_kick_keys")
    dt[, .is_aerial_kick := FALSE]
    if (!is.null(aerial_keys) && nrow(aerial_keys) > 0) {
      dt[aerial_keys, .is_aerial_kick := TRUE, on = .(match_id, display_order)]
    }
    cli::cli_alert_info(
      "EPV v3: {format(nrow(aerial_keys), big.mark = ',')} aerial contests removed from the disposer/receiver split ({round(100 * mean(dt$.is_aerial_kick), 1)}% of PBP rows)")
  } else {
    dt[, .is_aerial_kick := FALSE]
  }

  # --- Difficulty-weighted disposals, computed before the flat split ---
  # Same shape as the aerial block: the covered disposals are removed from the
  # flat EPV_DISP_SCALE/EPV_RECV_SCALE split and paid instead from `difficulty`,
  # where the share follows P(turnover) rather than being fixed at half. Aerial
  # contests are excluded because they already have their own surprise term.
  difficulty <- NULL
  dt[, .is_diff_disp := FALSE]
  if (isTRUE(EPV_DIFFICULTY_SPLIT)) {
    if (is.null(chains)) chains <- load_chains(TRUE)
    # WHAT IS ALREADY PAID DIFFERS BY ENGINE, and getting this wrong
    # double-counts silently rather than erroring.
    #
    #   v3  aerial contests carry their own surprise term (`aerial`)
    #   v2  contested kicks are paid by `contest_epv`, a 3-way split added into
    #       epv_recv further down -- and v2 ALSO reduces .disp_scale to 1/3 on
    #       those rows for the same reason
    #
    # Either way the covered rows must not also be paid by the difficulty
    # split. Under v2 that exclusion did not exist until 2026-08-05, because
    # the split had only ever been run under v3.
    .excl <- aerial_keys
    if (!v3 && "contest_target_id" %in% names(dt)) {
      .excl <- unique(dt[!is.na(contest_target_id), .(match_id, display_order)])
      cli::cli_alert_info(
        "Difficulty split (v2): {format(nrow(.excl), big.mark = ',')} contested kick{?s} left to `contest_epv`.")
    }
    difficulty <- compute_difficulty_credit(chains, pbp_data,
                                            exclude_keys = .excl)
    diff_keys <- attr(difficulty, "disposal_keys")
    if (!is.null(diff_keys) && nrow(diff_keys) > 0) {
      dt[diff_keys, .is_diff_disp := TRUE, on = .(match_id, display_order)]
    }
    cli::cli_alert_info(
      "Difficulty split: {format(nrow(diff_keys), big.mark = ',')} disposals paid by P(turnover) ({round(100 * mean(dt$.is_diff_disp), 1)}% of PBP rows)")
  }

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
  # Under v3 an aerial kick's disposal credit is V_pre - exp_pts, not a fixed
  # share of the swing, so it is zeroed here and added back from `aerial`.
  if (v3) dt[.is_aerial_kick == TRUE, .disp_scale := 0]
  dt[.is_diff_disp == TRUE, .disp_scale := 0]
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
  has_contest_col <- "contest_target_id" %in% names(dt)
  dt[, is_contest_target_recv := has_contest_col & !is.na(contest_target_id)]
  dt[, is_intercept_mark := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]
  # v3 also drops aerial-kick rows here: the player who marks the ball is paid by
  # the contest channel, and paying him a reception share as well would be the
  # double count kick-anchoring exists to remove.
  recv_dt <- dt[is_contest_target_recv == FALSE & .is_aerial_kick == FALSE &
                  .is_diff_disp == FALSE, .(
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
  wp_dt <- tryCatch({
    wpc <- create_wp_credit(pbp_data)
    wpc[, .(player_id, match_id, wp_credit, wp_disp_credit, wp_recv_credit)]
  }, error = function(e) {
    cli::cli_warn("WPA credit skipped: {conditionMessage(e)}")
    data.table::data.table(
      player_id = character(), match_id = character(),
      wp_credit = numeric(), wp_disp_credit = numeric(), wp_recv_credit = numeric()
    )
  })
  plyr_gm_df <- merge(plyr_gm_df, wp_dt,
    by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)

  # --- Step 3b2: difficulty-weighted disposal credit ---
  # Left-joined like every other channel, so a player who ONLY appears as a
  # receiver is dropped here exactly as he already is by the Step 3 join. The
  # columns are always present so the two mutate branches below can add them
  # unconditionally; they are identically zero when the flag is off.
  if (is.null(difficulty)) {
    data.table::set(plyr_gm_df, j = "epv_disp_diff", value = 0)
    data.table::set(plyr_gm_df, j = "epv_recv_diff", value = 0)
  } else {
    plyr_gm_df <- merge(plyr_gm_df, difficulty,
      by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)
  }

  # --- Step 3c: Contest credit from aerial contests (3-way EPV split) ---
  # v2 only. v3 supersedes this with the surprise-weighted contest in Step 0 and
  # does not read `contest_epv` at all -- so running it under v3 would burn ~2
  # minutes AND emit a column that looks meaningful and is not. An unused column
  # that a reader will reasonably assume is live is worse than a missing one.
  empty_contest <- data.table::data.table(
    player_id = character(), match_id = character(),
    contest_epv = numeric(),
    aerial_target_wins = integer(), aerial_target_losses = integer(),
    aerial_def_wins = integer(), aerial_def_losses = integer()
  )
  contest_dt <- if (v3) empty_contest else tryCatch({
    if (is.null(chains)) chains <- load_chains(TRUE)
    compute_contest_credit(chains, pbp_data,
                           contest_share = p$contest_share %||% (1 / 3))
  }, error = function(e) {
    is_data_unavailable <- grepl(
      "load_chains|download|HTTP|connection|404|timeout",
      conditionMessage(e), ignore.case = TRUE
    )
    if (!is_data_unavailable) {
      cli::cli_abort("Contest credit computation failed: {conditionMessage(e)}")
    }
    cli::cli_warn("Contest credit skipped (data unavailable): {conditionMessage(e)}")
    data.table::data.table(
      player_id = character(), match_id = character(),
      contest_epv = numeric(),
      aerial_target_wins = integer(), aerial_target_losses = integer(),
      aerial_def_wins = integer(), aerial_def_losses = integer()
    )
  })
  plyr_gm_df <- merge(plyr_gm_df, contest_dt,
    by = c("player_id", "match_id"), all.x = TRUE, sort = FALSE)

  # --- Step 3d: Contextual spoil credit (WS2a) ---
  # Prices the ~72% of spoils compute_contest_credit() misses. Emitted as a
  # standalone column only — epv_spoil still uses the flat EPV_SPOIL_WT, so
  # published ratings are unchanged until WS2b decides how this enters.
  #
  # v2 only, for the same reason as Step 3c. v3 prices every spoil inside the
  # aerial contest itself, so this whole workstream is superseded there: the
  # question it was built to answer ("how do we fold contextual spoil value in?")
  # stops existing when the contest channel IS contextual.
  empty_spoil_ctx <- data.table::data.table(
    player_id = character(), match_id = character(),
    spoil_epv_ctx = numeric(), spoils_priced = integer()
  )
  spoil_ctx_dt <- if (v3) empty_spoil_ctx else tryCatch({
    if (is.null(chains)) chains <- load_chains(TRUE)
    compute_spoil_credit(chains, pbp_data,
                         contest_share = p$contest_share %||% (1 / 3))
  }, error = function(e) {
    is_data_unavailable <- grepl(
      "load_chains|download|HTTP|connection|404|timeout",
      conditionMessage(e), ignore.case = TRUE
    )
    if (!is_data_unavailable) {
      cli::cli_abort("Contextual spoil credit computation failed: {conditionMessage(e)}")
    }
    cli::cli_warn("Contextual spoil credit skipped (data unavailable): {conditionMessage(e)}")
    data.table::data.table(
      player_id = character(), match_id = character(),
      spoil_epv_ctx = numeric(), spoils_priced = integer()
    )
  })
  plyr_gm_df <- merge(plyr_gm_df, spoil_ctx_dt,
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

  plyr_gm_df <- plyr_gm_df |>
    dplyr::left_join(
      spoil_hitout_df,
      by = c("player_id" = "player_id", "match_id" = "match_id")
    )

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

  # --- v3 channel assembly ---
  # The thirty box-score weights are gone. Grouped by why each is safe to drop:
  #
  #  (a) already priced by the chain event itself, so v2 paid twice: kicks,
  #      handballs, marks, contested_marks, uncontested/contested_possessions,
  #      ground_ball_gets, inside50s, marks_inside50, metres_gained, rebound50s,
  #      intercepts, goals, behinds, shots_at_goal, goal_assists,
  #      score_involvements, turnovers, clangers, frees_for/against. Every one is
  #      a chain row carrying a delta_epv (Kick 100%, Handball 99.9%,
  #      Loose Ball Get 99.9%, Contested Mark 100% present in PBP).
  #
  #  (b) genuinely absent from chains, so the VALUE survives but the ATTRIBUTION
  #      does not: tackles (chains logs 0.49 Tackle rows per match against ~60
  #      real ones), pressure_acts, def_half_pressure_acts, one_percenters. A
  #      tackle's expected-points effect is in the chain as the turnover the
  #      opponent concedes -- credited to whoever next wins the ball, not to the
  #      tackler. This is v3's single biggest known cost and it is not hidden:
  #      tacklers are under-credited inside EPV, and PSV carries tackling.
  #
  # epv_hitout keeps its box formula. That is the one permitted carve-out and it
  # is not convenience: Centre Bounce and Ball Up Call rows carry a player_id
  # 0.0% of the time, so the ruckmen are simply not in the data.
  if (v3) {
    aerial_dt <- aerial[, .(player_id, match_id, epv_cont_aerial,
                            epv_disp_aerial, contests_won, contests_lost)]
    plyr_gm_df <- plyr_gm_df |>
      dplyr::left_join(as.data.frame(aerial_dt), by = c("player_id", "match_id")) |>
      dplyr::mutate(
        contest_epv = tidyr::replace_na(contest_epv, 0),
        aerial_target_wins = as.integer(tidyr::replace_na(aerial_target_wins, 0)),
        aerial_target_losses = as.integer(tidyr::replace_na(aerial_target_losses, 0)),
        aerial_def_wins = as.integer(tidyr::replace_na(aerial_def_wins, 0)),
        aerial_def_losses = as.integer(tidyr::replace_na(aerial_def_losses, 0)),
        spoil_epv_ctx = tidyr::replace_na(spoil_epv_ctx, 0),
        spoils_priced = as.integer(tidyr::replace_na(spoils_priced, 0)),
        contests_won = as.integer(tidyr::replace_na(contests_won, 0)),
        contests_lost = as.integer(tidyr::replace_na(contests_lost, 0)),
        epv_recv = tidyr::replace_na(epv_recv, 0) +
                   tidyr::replace_na(epv_recv_diff, 0),
        epv_disp = tidyr::replace_na(epv_disp, 0) +
                   tidyr::replace_na(epv_disp_aerial, 0) +
                   tidyr::replace_na(epv_disp_diff, 0),
        epv_cont_aerial = tidyr::replace_na(epv_cont_aerial, 0),
        # cont_stop is the one channel that is not credit/debit: the v2 formula
        # pays EPV_RUCK_CONTEST_WT for every contest ATTENDED, won or lost. With
        # EPV3_STOP_ZERO_SUM the attendance term becomes a win/loss ledger --
        # `ruck_contests - hitouts` is what this ruck lost, since a contest has
        # exactly two rucks and `hitouts` counts the ones he won.
        epv_cont_stop = if (isTRUE(EPV3_STOP_ZERO_SUM)) {
          # These three come straight off the player_stats join and are NOT in
          # the box_score_cols zero-fill above, so a player with no stats row
          # would otherwise produce NA here and silently drop out of the
          # channel. The v2 path never hit this because it zero-fills
          # epv_hitout after summing.
          .ho  <- tidyr::replace_na(hitouts, 0)
          .hta <- tidyr::replace_na(hitouts_to_advantage, 0)
          .rc  <- tidyr::replace_na(ruck_contests, 0)
          # abs() on the contest weight, and it is load-bearing. HERE that term
          # multiplies `hitouts`, so it is the credit for a contest WON, with
          # EPV_RUCK_LOSS_WT carrying the debit on the next line. v2 uses the
          # same constant against `ruck_contests` -- attendance -- where it is a
          # DEBIT and went negative on 2026-08-07. Reading it signed would make
          # v3 charge a ruck for winning. Same idea, opposite sign convention.
          .ho * p$hitout_wt + .hta * p$hitout_adv_wt +
            .ho * abs(p$ruck_contest_wt) -
            pmax(0, .rc - .ho) * EPV_RUCK_LOSS_WT
        } else {
          tidyr::replace_na(epv_hitout, 0)
        },
        # The downstream stack (EPR channels, column schema, blog shapes) is
        # keyed on the v2 names. Aliasing rather than renaming keeps v3 to one
        # file; if it ships, the rename is the follow-up.
        #
        # Under EPV3_CHANNELS = 3 the two contest channels share one slot and the
        # hitout slot is emptied. `epv` is IDENTICAL either way -- the whole
        # 3-vs-4 difference is how EPR aggregates, since each slot carries its own
        # decay and shrinkage prior. EPR_PRIOR_RATE_HITOUT is zeroed to match, so
        # the empty slot contributes exactly zero instead of shrinking toward a
        # prior for a channel that no longer exists.
        #
        # The two contest components are put on a common footing BEFORE they are
        # added (`EPV3_SUB_SCALE`). A raw sum blends them by VARIANCE, and the
        # aerial part has ~3x the spread while carrying no margin signal of its
        # own, so the ruck signal gets swamped rather than carried. Scaling each
        # to one-point-per-unit first makes the merge blend by POINTS. Under 4
        # channels the two never meet, so no sub-scale is needed or applied.
        epv_spoil = if (identical(EPV3_CHANNELS, 3L)) {
          epv_cont_aerial * EPV3_SUB_SCALE[["cont_aerial"]] +
            epv_cont_stop * EPV3_SUB_SCALE[["cont_stop"]]
        } else {
          epv_cont_aerial
        },
        epv_hitout = if (identical(EPV3_CHANNELS, 3L)) 0 else epv_cont_stop,
        # Built from the SLOTS, not from the raw components, so the total and
        # its parts cannot disagree once a sub-scale is live. Identical to the
        # old expression whenever EPV3_SUB_SCALE is all 1s.
        epv = epv_recv + epv_disp + epv_spoil + epv_hitout
      )
  } else {
  plyr_gm_df <- plyr_gm_df |>
    dplyr::mutate(
      contest_epv = tidyr::replace_na(contest_epv, 0),
      aerial_target_wins = as.integer(tidyr::replace_na(aerial_target_wins, 0)),
      aerial_target_losses = as.integer(tidyr::replace_na(aerial_target_losses, 0)),
      aerial_def_wins = as.integer(tidyr::replace_na(aerial_def_wins, 0)),
      aerial_def_losses = as.integer(tidyr::replace_na(aerial_def_losses, 0)),
      spoil_epv_ctx = tidyr::replace_na(spoil_epv_ctx, 0),
      spoils_priced = as.integer(tidyr::replace_na(spoils_priced, 0)),
      epv_recv = tidyr::replace_na(epv_recv, 0) +
                 tidyr::replace_na(epv_recv_diff, 0) + contest_epv +
                 contested_possessions * p$contested_poss_wt + contested_marks * p$contested_marks_wt +
                 ground_ball_gets * p$ground_ball_gets_wt + marks_inside50 * p$marks_inside50_wt +
                 marks * p$marks_wt + uncontested_possessions * p$uncontested_poss_wt +
                 frees_for * p$frees_for_wt,
      epv_disp = tidyr::replace_na(epv_disp, 0) +
                 tidyr::replace_na(epv_disp_diff, 0) +
                 inside50s * p$inside50s_wt + clangers * p$clangers_wt + score_involvements * p$score_involvements_wt +
                 kicks * p$kicks_wt + handballs * p$handballs_wt + metres_gained * p$metres_gained_wt +
                 turnovers * p$turnovers_wt + goal_assists * p$goal_assists_wt +
                 goals * p$goals_wt + behinds * p$behinds_wt + shots_at_goal * p$shots_at_goal_wt,
      epv_spoil = tidyr::replace_na(epv_spoil, 0),
      epv_hitout = tidyr::replace_na(epv_hitout, 0),
      epv = epv_recv + epv_disp + epv_spoil + epv_hitout,
      wp_credit = tidyr::replace_na(wp_credit, 0),
      wp_disp_credit = tidyr::replace_na(wp_disp_credit, 0),
      wp_recv_credit = tidyr::replace_na(wp_recv_credit, 0)
    )
  }

  # WPA is engine-independent -- it reads the WP model, not the EPV channels.
  plyr_gm_df <- plyr_gm_df |>
    dplyr::mutate(
      wp_credit = tidyr::replace_na(wp_credit, 0),
      wp_disp_credit = tidyr::replace_na(wp_disp_credit, 0),
      wp_recv_credit = tidyr::replace_na(wp_recv_credit, 0)
    )

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
      # Contest credit (3-way aerial split)
      contest_epv, aerial_target_wins, aerial_target_losses,
      aerial_def_wins, aerial_def_losses,
      # Contextual spoil credit (WS2a — not yet folded into epv_spoil)
      spoil_epv_ctx, spoils_priced,
      # v3 only. Kept under any_of() so v2 output keeps its declared schema
      # exactly — column_schema.R would reject the extras on a released frame.
      dplyr::any_of(c("epv_cont_aerial", "epv_cont_stop",
                      "contests_won", "contests_lost")),
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

  # And again as a COLUMN, because the attribute does not survive the parquet
  # round-trip this frame makes through the release. Without it Stage 3 reloads
  # the frame unstamped and prices v3 data as v2 -- which the pipeline reports
  # in a warning and then carries on regardless. .restore_epv_engine_attr()
  # turns this back into the attribute on load.
  if (!is.null(epv_engine) && nrow(plyr_gm_df) > 0) {
    plyr_gm_df[["epv_engine"]] <- as.character(epv_engine)
  }
  return(plyr_gm_df)
}

#' Compute Contest Credit from Aerial Contests
#'
#' Joins aerial contest data back to PBP to get the kicker's \code{delta_epv},
#' then splits credit three ways: kicker, target, and defender. When an opponent
#' is involved (spoil, intercept mark), each gets 1/3 of the EPV at stake.
#' The target and defender receive credit from their own team's perspective
#' (positive if they won, negative if they lost).
#'
#' Only applies to contests with a 3rd player from the opposing team. When
#' the target takes the mark themselves (no opponent), the standard 50/50
#' kicker/receiver split is unchanged.
#'
#' @param chains Raw chains data (from \code{load_chains()}).
#' @param pbp_data Clean PBP data (from \code{load_pbp()}) containing
#'   \code{delta_epv} values.
#' @param contest_share Fraction of \code{delta_epv} attributed to each
#'   contest participant. Default \code{1/3}.
#'
#' @return A data.table with columns: \code{player_id}, \code{match_id},
#'   \code{contest_epv} (positive for winners, negative for losers),
#'   \code{aerial_target_wins}, \code{aerial_target_losses},
#'   \code{aerial_def_wins}, \code{aerial_def_losses}.
#'
#' @keywords internal
compute_contest_credit <- function(chains, pbp_data, contest_share = 1 / 3) {
  empty_dt <- data.table::data.table(
    player_id = character(), match_id = character(),
    contest_epv = numeric(),
    aerial_target_wins = integer(), aerial_target_losses = integer(),
    aerial_def_wins = integer(), aerial_def_losses = integer()
  )

  chains_dt <- data.table::as.data.table(chains)
  pbp_dt <- data.table::as.data.table(pbp_data)

  detect_chains_columns(chains_dt)

  target_descs <- CHAINS_CONTEST_TARGET_DESCS
  kick_descs <- c("Kick", "Ground Kick")
  data.table::setorder(chains_dt, match_id, display_order)

  # Build shift columns for forward (outcome) and backward (kicker) lookup
  chains_dt[, `:=`(
    .next_desc = data.table::shift(description, 1L, type = "lead"),
    .next_pid  = data.table::shift(player_id, 1L, type = "lead"),
    .next_tid  = data.table::shift(team_id, 1L, type = "lead"),
    .next_x    = data.table::shift(x, 1L, type = "lead"),
    .next_y    = data.table::shift(y, 1L, type = "lead"),
    .lag1_desc = data.table::shift(description, 1L, type = "lag"),
    .lag2_desc = data.table::shift(description, 2L, type = "lag"),
    .lag3_desc = data.table::shift(description, 3L, type = "lag"),
    .lag4_desc = data.table::shift(description, 4L, type = "lag"),
    .lag5_desc = data.table::shift(description, 5L, type = "lag"),
    .lag1_do   = data.table::shift(display_order, 1L, type = "lag"),
    .lag2_do   = data.table::shift(display_order, 2L, type = "lag"),
    .lag3_do   = data.table::shift(display_order, 3L, type = "lag"),
    .lag4_do   = data.table::shift(display_order, 4L, type = "lag"),
    .lag5_do   = data.table::shift(display_order, 5L, type = "lag")
  ), by = match_id]

  # Filter to contest target rows with opposing-team outcome at same x,y
  contests <- chains_dt[
    description %in% target_descs &
    !is.na(player_id) &
    !is.na(.next_tid) &
    x == .next_x & y == .next_y &
    team_id != .next_tid &
    !is.na(.next_pid)
  ]

  if (nrow(contests) == 0) {
    chains_dt[, grep("^\\.", names(chains_dt), value = TRUE) := NULL]
    return(empty_dt)
  }

  # Find the kicker's display_order (first Kick/Ground Kick within 5 rows back)
  contests[, kick_display_order := data.table::fcase(
    .lag1_desc %chin% kick_descs, .lag1_do,
    .lag2_desc %chin% kick_descs, .lag2_do,
    .lag3_desc %chin% kick_descs, .lag3_do,
    .lag4_desc %chin% kick_descs, .lag4_do,
    .lag5_desc %chin% kick_descs, .lag5_do,
    default = NA_integer_
  )]

  # Build triples table (drop rows without a matched kick)
  triples_dt <- contests[!is.na(kick_display_order), .(
    match_id,
    kick_display_order,
    target_player_id = player_id,
    target_team_id = team_id,
    defender_player_id = .next_pid,
    defender_team_id = .next_tid,
    outcome_desc = .next_desc
  )]

  # Clean up temp columns
  chains_dt[, grep("^\\.", names(chains_dt), value = TRUE) := NULL]

  if (nrow(triples_dt) == 0) return(empty_dt)

  # Join to PBP to get delta_epv from the kicker's row
  triples_dt <- merge(
    triples_dt,
    pbp_dt[, .(match_id, display_order, delta_epv)],
    by.x = c("match_id", "kick_display_order"),
    by.y = c("match_id", "display_order"),
    all.x = TRUE, sort = FALSE
  )
  # Drop rows without delta_epv (shouldn't happen but safety)
  triples_dt <- triples_dt[!is.na(delta_epv)]
  if (nrow(triples_dt) == 0) return(empty_dt)

  # Compute credit: delta_epv is from kicker's team perspective
  # Target (same team as kicker): credit = delta_epv * share
  # Defender (opp team): credit = -delta_epv * share (flip perspective)
  share <- contest_share

  # Build per-player rows: one for target, one for defender
  target_credit <- triples_dt[, .(
    player_id = target_player_id,
    match_id = match_id,
    contest_epv = delta_epv * share,
    is_target = TRUE
  )]
  defender_credit <- triples_dt[, .(
    player_id = defender_player_id,
    match_id = match_id,
    contest_epv = -delta_epv * share,
    is_target = FALSE
  )]
  all_credit <- data.table::rbindlist(list(target_credit, defender_credit))

  # Aggregate per player per match
  all_credit[, .(
    contest_epv = sum(contest_epv),
    aerial_target_wins = sum(is_target & contest_epv > 0),
    aerial_target_losses = sum(is_target & contest_epv <= 0),
    aerial_def_wins = sum(!is_target & contest_epv > 0),
    aerial_def_losses = sum(!is_target & contest_epv <= 0)
  ), by = .(player_id, match_id)]
}


#' Compute Contextual Spoil Credit
#'
#' Prices every spoil by the expected-points swing of the kick it defused,
#' rather than by a flat per-spoil weight.
#'
#' \code{compute_contest_credit()} already prices contests contextually, but its
#' filter keys on \code{CHAINS_CONTEST_TARGET_DESCS}, which matches only ~28\% of
#' spoils — the largest group of spoils simply follows a plain \code{"Kick"} row.
#' This function reaches the rest: it locates every \code{"Spoil"} in chains, scans
#' back up to 5 rows for the kick that produced it (the same scan
#' \code{compute_contest_credit()} uses), and credits the spoiler
#' \code{-delta_epv * contest_share} — the identical sign convention, so the two
#' quantities are on the same scale and can be summed.
#'
#' Spoils already captured as contest triples are excluded, so this never
#' double-counts against \code{contest_epv}.
#'
#' The credit is \strong{signed}: a spoil on a kick that was still good for the
#' attacking team earns negative credit. About 40\% of spoils fall in that group,
#' which is the discrimination a flat weight cannot express.
#'
#' @param chains Raw chains data (from \code{load_chains()}).
#' @param pbp_data Clean PBP data (from \code{load_pbp()}) containing
#'   \code{delta_epv} values.
#' @param contest_share Fraction of \code{delta_epv} attributed to the spoiler.
#'   Default \code{1/3}, matching \code{compute_contest_credit()}.
#'
#' @return A data.table with columns: \code{player_id}, \code{match_id},
#'   \code{spoil_epv_ctx} (signed contextual credit, summed per player-match),
#'   \code{spoils_priced} (spoils this function valued).
#'
#' @keywords internal
compute_spoil_credit <- function(chains, pbp_data, contest_share = 1 / 3) {
  empty_dt <- data.table::data.table(
    player_id = character(), match_id = character(),
    spoil_epv_ctx = numeric(), spoils_priced = integer()
  )

  chains_dt <- data.table::as.data.table(chains)
  pbp_dt <- data.table::as.data.table(pbp_data)

  detect_chains_columns(chains_dt)

  target_descs <- CHAINS_CONTEST_TARGET_DESCS
  kick_descs <- c("Kick", "Ground Kick")
  data.table::setorder(chains_dt, match_id, display_order)

  chains_dt[, `:=`(
    .prev_desc = data.table::shift(description, 1L, type = "lag"),
    .prev_tid  = data.table::shift(team_id, 1L, type = "lag"),
    .prev_x    = data.table::shift(x, 1L, type = "lag"),
    .prev_y    = data.table::shift(y, 1L, type = "lag"),
    .lag1_desc = data.table::shift(description, 1L, type = "lag"),
    .lag2_desc = data.table::shift(description, 2L, type = "lag"),
    .lag3_desc = data.table::shift(description, 3L, type = "lag"),
    .lag4_desc = data.table::shift(description, 4L, type = "lag"),
    .lag5_desc = data.table::shift(description, 5L, type = "lag"),
    .lag1_do   = data.table::shift(display_order, 1L, type = "lag"),
    .lag2_do   = data.table::shift(display_order, 2L, type = "lag"),
    .lag3_do   = data.table::shift(display_order, 3L, type = "lag"),
    .lag4_do   = data.table::shift(display_order, 4L, type = "lag"),
    .lag5_do   = data.table::shift(display_order, 5L, type = "lag"),
    .lag1_tid  = data.table::shift(team_id, 1L, type = "lag"),
    .lag2_tid  = data.table::shift(team_id, 2L, type = "lag"),
    .lag3_tid  = data.table::shift(team_id, 3L, type = "lag"),
    .lag4_tid  = data.table::shift(team_id, 4L, type = "lag"),
    .lag5_tid  = data.table::shift(team_id, 5L, type = "lag")
  ), by = match_id]

  # Exclude spoils compute_contest_credit() already priced: those are the
  # outcome row of a contest triple (previous row is a contest target at the
  # same coordinates, logged to the opposing team).
  spoils <- chains_dt[
    description == "Spoil" &
    !is.na(player_id) &
    !(.prev_desc %chin% target_descs &
      x == .prev_x & y == .prev_y &
      !is.na(.prev_tid) & team_id != .prev_tid)
  ]

  cleanup <- function() {
    chains_dt[, grep("^\\.", names(chains_dt), value = TRUE) := NULL]
  }
  if (nrow(spoils) == 0) {
    cleanup()
    return(empty_dt)
  }

  # First Kick/Ground Kick within 5 rows back, and the team that kicked it.
  # The scan may only pass through in-flight annotation rows: if a possession
  # event (Handball, Mark, an earlier Spoil, ...) sits in between, the kick
  # further back belongs to a different play and crediting it would attribute
  # a spoil to a kick it never touched.
  inflight <- CHAINS_INFLIGHT_DESCS
  spoils[, `:=`(
    .clear2 = .lag1_desc %chin% inflight,
    .clear3 = .lag1_desc %chin% inflight & .lag2_desc %chin% inflight,
    .clear4 = .lag1_desc %chin% inflight & .lag2_desc %chin% inflight &
              .lag3_desc %chin% inflight,
    .clear5 = .lag1_desc %chin% inflight & .lag2_desc %chin% inflight &
              .lag3_desc %chin% inflight & .lag4_desc %chin% inflight
  )]
  spoils[, .kick_lag := data.table::fcase(
    .lag1_desc %chin% kick_descs, 1L,
    .clear2 & .lag2_desc %chin% kick_descs, 2L,
    .clear3 & .lag3_desc %chin% kick_descs, 3L,
    .clear4 & .lag4_desc %chin% kick_descs, 4L,
    .clear5 & .lag5_desc %chin% kick_descs, 5L,
    default = NA_integer_
  )]
  spoils[, `:=`(
    kick_display_order = data.table::fcase(
      .kick_lag == 1L, .lag1_do, .kick_lag == 2L, .lag2_do,
      .kick_lag == 3L, .lag3_do, .kick_lag == 4L, .lag4_do,
      .kick_lag == 5L, .lag5_do
    ),
    kick_team_id = data.table::fcase(
      .kick_lag == 1L, .lag1_tid, .kick_lag == 2L, .lag2_tid,
      .kick_lag == 3L, .lag3_tid, .kick_lag == 4L, .lag4_tid,
      .kick_lag == 5L, .lag5_tid
    )
  )]

  # A spoil is only meaningful against an opponent's kick. Same-team matches are
  # chain-logging artifacts (~16%) and are dropped rather than credited.
  spoils <- spoils[!is.na(kick_display_order) &
                   !is.na(kick_team_id) &
                   team_id != kick_team_id,
                   .(match_id, player_id, kick_display_order)]
  cleanup()
  if (nrow(spoils) == 0) return(empty_dt)

  spoils <- merge(
    spoils,
    pbp_dt[, .(match_id, display_order, delta_epv)],
    by.x = c("match_id", "kick_display_order"),
    by.y = c("match_id", "display_order"),
    all.x = TRUE, sort = FALSE
  )
  spoils <- spoils[!is.na(delta_epv)]
  if (nrow(spoils) == 0) return(empty_dt)

  # delta_epv is from the kicking team's perspective; flip it for the spoiler,
  # matching compute_contest_credit()'s defender convention exactly.
  spoils[, .credit := -delta_epv * contest_share]
  spoils[, .(
    spoil_epv_ctx = sum(.credit),
    spoils_priced = .N
  ), by = .(player_id, match_id)]
}


#' Backward-compatible wrapper (deprecated)
#' @keywords internal
compute_failed_recv_credit <- function(chains,
                                       weight_per_loss = EPV_RECV_FAILED_CONTEST_WT) {
  cli::cli_warn("Use {.fn compute_contest_credit} instead of {.fn compute_failed_recv_credit}")
  contests <- extract_contests(chains = chains, type = "aerial")
  if (nrow(contests) == 0) {
    return(data.table::data.table(
      player_id = character(), match_id = character(),
      failed_epv_recv = numeric(), failed_receptions = integer()
    ))
  }
  failed <- contests[outcome %in% c("spoil", "intercept_mark") & winner == "player2"]
  if (nrow(failed) == 0) {
    return(data.table::data.table(
      player_id = character(), match_id = character(),
      failed_epv_recv = numeric(), failed_receptions = integer()
    ))
  }
  failed[, .(failed_epv_recv = .N * weight_per_loss, failed_receptions = .N),
         by = .(player_id = player1_id, match_id)]
}


#' @rdname default_epv_params
#' @keywords internal
default_credit_params <- default_epv_params
