# EPV v3 — chain-native four-channel EPV.
#
# Design and gates: ../docs/plans/EPV-V3-CHAIN-NATIVE.md
#
# The whole engine follows from one decomposition of a kick's `delta_epv`, in
# the kicking team's frame:
#
#   delta_epv = (V_pre    - exp_pts )   disposal, to the kicker
#             + (V_branch - V_pre   )   contest surprise, split winner/loser
#             + (V_after  - V_branch)   subsequent play, paid by the NEXT row
#
# with  V_pre = (1 - p) * V_att + p * V_def  and  Delta = V_att - V_def, so the
# contest term is +p*Delta when the attack retains and -(1-p)*Delta when the
# defence wins. The winner banks |term| and the loser sheds it: zero-sum with no
# share parameter to argue about, and the payout scales with the SURPRISE, so
# beating a contest you were expected to lose is worth far more than winning a
# gimme.

#' Aerial outcome descriptions
#'
#' Kick outcomes resolved in the air by a named player. \code{Mark Fumbled} and
#' \code{Dropped Mark} are deliberately excluded: nobody secured the ball, so
#' there is no winner, and folding them into the attacking branch would drag
#' \code{V_att} down for every genuine mark. Those kicks fall through to the
#' ordinary non-aerial disposer/receiver split.
#' @keywords internal
EPV3_AERIAL_OUT <- c("Contested Mark", "Uncontested Mark", "Mark On Lead",
                     "Pack Mark (P)", "Pack Mark (O)", "Spoil",
                     "Spoil gaining possession", "Spoil ineffective")

#' Chain descriptions counting as CONTESTED aerial exposure
#'
#' The base for spreading contest debits whose loser chains never names. It has
#' to answer "did this player go up for contested balls in this zone", so it
#' includes a player who went up and fumbled, or who was the logged contest
#' target, even though neither is an outcome row.
#'
#' \strong{\code{Uncontested Mark} and \code{Mark On Lead} are excluded, and that
#' matters.} An uncontested mark means by definition that no contest happened, so
#' counting it as exposure charges small rebounding defenders for aerial duels
#' they were never in -- it put Caleb Daniel at -44.6 and Bradley Hill at -31.2
#' in the 2026 contest channel, both mark-and-kick players rather than aerial
#' contesters.
#' @keywords internal
EPV3_AERIAL_EXPOSURE_DESCS <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)",
                                "Spoil", "Spoil gaining possession",
                                "Spoil ineffective", "Mark Fumbled",
                                "Dropped Mark", "Contest Target",
                                "Kick Inside 50 Result", "Knock On",
                                "Contested Knock On", "Effective Knock On")

#' Wider exposure base: contested aerial involvement plus uncontested marks
#'
#' The two bases fail in opposite directions and neither is obviously right, so
#' \code{epv3_compare_alloc.R} scores them against each other rather than one
#' being assumed. Contested-only is nearly a win count, so it hands the biggest
#' debit to the best contester; adding uncontested marks dilutes that but charges
#' mark-and-kick defenders for duels they never entered.
#' @keywords internal
EPV3_AERIAL_EXPOSURE_WIDE <- c(EPV3_AERIAL_EXPOSURE_DESCS,
                               "Uncontested Mark", "Mark On Lead")

#' Build the aerial contest table from chains
#'
#' Anchors on the KICK, not on the \code{Contest Target} / \code{Spoil}
#' annotations. Anchoring on the annotations was tried first and is wrong twice
#' over: a spoil-anchored population is defence-wins-only by construction (it
#' read p = 75.1\% against the target-anchored 57.2\%, pure selection), and it
#' leaves a mark to be paid by both \code{epv_recv} and the contest channel.
#' Kick-anchoring makes the outcome always observed, so \code{p} is unbiased, and
#' makes a marker's reception credit and his contest credit the same quantity.
#'
#' \strong{Coordinates.} Raw chains stores every row of a chain in the CHAIN
#' team's attacking frame, including the opposition's rows -- which is why
#' \code{compute_contest_credit()} compares contest coordinates with
#' \code{x == .next_x} and no sign flip, while cleaned PBP (action-team frame)
#' needs \code{x == -.next_x}. A kick's chain belongs to the kicking team, so +x
#' on these rows already points at that team's goal. Asserted, not assumed.
#'
#' @param chains Raw chains data.
#' @param pbp_data Clean PBP carrying \code{exp_pts} and \code{delta_epv}.
#' @return A data.table, one row per aerial contest.
#' @keywords internal
build_aerial_contests <- function(chains, pbp_data) {
  ch <- data.table::as.data.table(chains)
  pbp <- data.table::as.data.table(pbp_data)
  detect_chains_columns(ch)

  score_rows <- ch[description %chin% c("Goal", "Behind", "Shot At Goal") & !is.na(team_id)]
  if (nrow(score_rows) > 0) {
    dir_tbl <- score_rows[, .(mean_x = mean(x, na.rm = TRUE)), by = .(match_id, team_id)]
    pos_share <- mean(dir_tbl$mean_x > 0, na.rm = TRUE)
    if (is.finite(pos_share) && pos_share < 0.98) {
      cli::cli_abort(c(
        "Chains coordinates are not in the chain team's attacking frame.",
        "x" = "Only {round(100 * pos_share, 1)}% of (match, team) scoring cells read positive mean x.",
        "i" = "Every contest feature below assumes +x points at the kicking team's goal."
      ))
    }
  }
  half <- as.numeric(stats::quantile(abs(ch$x), 0.995, na.rm = TRUE))

  data.table::setorder(ch, match_id, display_order)
  for (k in 1:6) {
    for (stem in c("description", "player_id", "team_id", "x", "y")) {
      ch[, (paste0(".f", k, "_", stem)) :=
           data.table::shift(get(stem), k, type = "lead"), by = match_id]
    }
  }

  kk <- ch[description %chin% c("Kick", "Ground Kick") &
             !is.na(player_id) & !is.na(team_id)]
  if (nrow(kk) == 0) return(kk[0])

  inflight <- CHAINS_INFLIGHT_DESCS
  kk[, .olag := data.table::fcase(
    !(.f1_description %chin% inflight), 1L,
    !(.f2_description %chin% inflight), 2L,
    !(.f3_description %chin% inflight), 3L,
    !(.f4_description %chin% inflight), 4L,
    !(.f5_description %chin% inflight), 5L,
    !(.f6_description %chin% inflight), 6L,
    default = NA_integer_
  )]
  pick <- function(stem) data.table::fcase(
    kk$.olag == 1L, kk[[paste0(".f1_", stem)]], kk$.olag == 2L, kk[[paste0(".f2_", stem)]],
    kk$.olag == 3L, kk[[paste0(".f3_", stem)]], kk$.olag == 4L, kk[[paste0(".f4_", stem)]],
    kk$.olag == 5L, kk[[paste0(".f5_", stem)]], kk$.olag == 6L, kk[[paste0(".f6_", stem)]]
  )
  kk[, `:=`(out_desc = pick("description"), out_pid = pick("player_id"),
            out_tid = pick("team_id"), out_x = pick("x"), out_y = pick("y"))]

  # The intended receiver is named only when a Contest Target row was logged in
  # the in-flight span. That is the only way a beaten target can be debited.
  tpid <- rep(NA_character_, nrow(kk))
  for (k in 1:5) {
    d <- kk[[paste0(".f", k, "_description")]]
    hit <- !is.na(kk$.olag) & k < kk$.olag & is.na(tpid) &
      d %chin% CHAINS_CONTEST_TARGET_DESCS
    tpid[hit] <- kk[[paste0(".f", k, "_player_id")]][hit]
  }
  kk[, target_pid := tpid]

  # Which outcomes count as a contest. Under EPV3_CONTEST_POPULATION = "duel"
  # this drops Uncontested Mark and Mark On Lead -- 68.5% of the rows and 53.9%
  # of the credit mass -- because they are receptions, not duels. See
  # epv_v3_duels.R. Kicks that stop being contests are NOT lost: they fall back
  # to the ordinary disposer/receiver split, because `aerial_kick_keys` (which
  # is what excludes them from that split) is built from this same table.
  .out_set <- epv3_aerial_out()
  cst <- kk[out_desc %chin% .out_set & !is.na(out_tid) & !is.na(out_pid), .(
    match_id, kick_do = display_order, kick_pid = player_id, kick_tid = team_id,
    kick_x = x, kick_y = y, out_desc, out_pid, out_tid, out_x, out_y, target_pid
  )]
  if (nrow(cst) == 0) return(cst)

  cst[, def_win := out_tid != kick_tid]
  # A Spoil logged to the KICKING team is a chain-logging artifact, not an
  # attacking win. v2 drops the same rows for the same reason (~16% of spoils).
  cst <- cst[!(grepl("^Spoil", out_desc) & def_win == FALSE)]

  cst <- merge(cst, pbp[, .(match_id, display_order, exp_pts, delta_epv)],
               by.x = c("match_id", "kick_do"),
               by.y = c("match_id", "display_order"),
               all.x = TRUE, sort = FALSE)
  cst <- cst[is.finite(exp_pts) & is.finite(delta_epv)]
  if (nrow(cst) == 0) return(cst)

  cst[, `:=`(
    V_after   = exp_pts + delta_epv,
    att_x     = out_x,
    abs_y     = abs(out_y),
    kick_len  = sqrt((out_x - kick_x)^2 + (out_y - kick_y)^2),
    fwd_gain  = out_x - kick_x
  )]
  cst[, `:=`(
    goal_dist = sqrt(pmax(0, half - att_x)^2 + abs_y^2),
    i50f      = factor(as.integer(att_x > half - 50), levels = c("0", "1"))
  )]
  data.table::setattr(cst, "half", half)
  cst[is.finite(goal_dist) & is.finite(kick_len) & is.finite(fwd_gain)]
}


#' Fit the three contest branch models
#'
#' \code{p} = P(defence wins), and the two branch values \code{V_att} /
#' \code{V_def} = E[post-event state | that side won]. \code{exp_pts} (the
#' pre-kick state) is a legitimate and by far the strongest feature: it describes
#' the situation BEFORE the kick, nothing about how the contest resolved. Without
#' it the fit missed badly in the tails while reading fine through the middle.
#'
#' @param cst Contest table from \code{build_aerial_contests()}.
#' @param train_idx Logical vector selecting the rows to fit on.
#' @return A list of three \code{bam} fits.
#' @keywords internal
fit_contest_models <- function(cst, train_idx = rep(TRUE, nrow(cst))) {
  tr <- cst[train_idx]
  # Every term here has to be knowable BEFORE the contest resolves. An earlier
  # draft carried a `lead_mark` indicator and it is leakage: "the mark was taken
  # on a lead" is only observable because the attack won it. It also all but
  # separated the binomial (19,031 attacking wins against 1 defensive), which is
  # how it surfaced -- a degenerate-contrast error in the defensive branch.
  rhs <- ~ s(att_x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
    s(exp_pts) + i50f
  fit <- function(f, d, ...) {
    # A factor with one observed level in a subset kills bam() with an opaque
    # "contrasts not defined for 0 degrees of freedom". Drop such terms rather
    # than let the whole engine fail on a thin season.
    for (fv in c("i50f")) {
      if (fv %in% all.vars(f) && length(unique(d[[fv]][!is.na(d[[fv]])])) < 2) {
        f <- stats::update(f, stats::as.formula(paste(". ~ . -", fv)))
      }
    }
    mgcv::bam(f, data = droplevels(d), discrete = TRUE, ...)
  }
  list(
    p   = fit(stats::update(rhs, def_win ~ .), tr, family = stats::binomial()),
    att = fit(stats::update(rhs, V_after ~ .), tr[def_win == FALSE]),
    def = fit(stats::update(rhs, V_after ~ .), tr[def_win == TRUE])
  )
}


#' Score contests and split the credit
#'
#' @param cst Contest table.
#' @param models From \code{fit_contest_models()}.
#' @return \code{cst} with \code{p_hat}, \code{Delta}, \code{V_pre},
#'   \code{disp_credit}, \code{winner_credit}, \code{loser_credit}.
#' @keywords internal
score_contests <- function(cst, models) {
  d <- data.table::copy(cst)
  d[, `:=`(
    p_hat     = as.numeric(stats::predict(models$p,   newdata = d, type = "response")),
    V_att_hat = as.numeric(stats::predict(models$att, newdata = d)),
    V_def_hat = as.numeric(stats::predict(models$def, newdata = d))
  )]
  d[, `:=`(V_pre = (1 - p_hat) * V_att_hat + p_hat * V_def_hat,
           Delta = V_att_hat - V_def_hat)]
  d[, cont_att := data.table::fifelse(def_win, -(1 - p_hat) * Delta, p_hat * Delta)]
  # The winner always banks |cont_att| and the loser always sheds it, whichever
  # side won, because cont_att is signed in the attacking frame and flips with
  # the outcome. Writing it this way rather than branching on def_win is not just
  # tidier -- the branching version had the debit sign inverted for defence wins.
  d[, `:=`(
    disp_credit   = V_pre - exp_pts,
    winner_credit = abs(cont_att),
    loser_credit  = -abs(cont_att),
    winner_pid    = out_pid,
    winner_tid    = out_tid,
    loser_pid     = data.table::fifelse(def_win, target_pid, NA_character_),
    loser_tid     = data.table::fifelse(def_win, kick_tid, out_tid)
  )]
  d
}


#' Distribute contest debits whose loser chains never names
#'
#' Chains names the beaten aerial opponent in only ~12\% of contests: never when
#' the attack retains (recon found 0 of 7,736 attacking mark wins carry an
#' opposing row at the same coordinates, including 0 of the 2,017 logged as
#' \emph{contested} marks), and only when a \code{Contest Target} row happened to
#' be logged when the defence wins. Left unallocated the channel becomes
#' upside-only: every player gains from contests he wins and nobody ever pays.
#'
#' The exposure weight is each player's count of AERIAL chain rows for his team
#' in that (match, zone). Two rejected alternatives, both tried:
#' \itemize{
#'   \item \emph{his contest wins} -- would make the best contester absorb the
#'     most debit, which is backwards.
#'   \item \emph{all his chain rows} -- this was the first draft and it is worse.
#'     It taxes ball-winning as contest-losing: high-possession players racked up
#'     chain rows and so absorbed their team's unnamed aerial debits despite
#'     never going up for the ball. It put Wanganeen-Milera at -52.7 and Bradley
#'     Hill at -46.4 in the 2026 contest channel, neither of whom is an aerial
#'     contester.
#' }
#' \strong{Neither exposure-weighted base survived measurement, and the default
#' is \code{"team"} — a flat share across the losing team.} Every weighting
#' scheme distorted in one direction or the other, because the weight has to be
#' built from the same events the credit is built from. A flat share makes no
#' claim about WHICH opponent was beaten, only that the team was, so it cannot
#' reorder players within a team. It also tied for the most repeatable
#' (r = 0.819 year-over-year). See \code{EPV_CONT_LOSS_ALLOC} for the full
#' five-way comparison; the rejected rules are kept implemented so the result can
#' be reproduced rather than taken on trust.
#'
#' @param scored Output of \code{score_contests()}.
#' @param chains Raw chains data.
#' @param half Half-ground extent.
#' @return A data.table of \code{player_id}, \code{match_id}, \code{cont_alloc}.
#' @keywords internal
allocate_contest_losses <- function(scored, chains, half,
                                    exposure_descs = EPV3_AERIAL_EXPOSURE_DESCS,
                                    player_stats = NULL) {
  ch <- data.table::as.data.table(chains)
  zone_of <- function(ax) data.table::fcase(
    ax > half / 3, "att", ax < -half / 3, "def", default = "mid")

  s <- data.table::copy(scored)
  s[, zone := zone_of(att_x)]
  mirror <- c(att = "def", def = "att", mid = "mid")
  unnamed <- s[is.na(loser_pid), .(debit = sum(loser_credit)),
               by = .(match_id, team_id = loser_tid, zone = mirror[zone])]
  if (nrow(unnamed) == 0) {
    return(data.table::data.table(player_id = character(), match_id = character(),
                                  cont_alloc = numeric()))
  }
  # --- "ledger": weight by the beaten player's OWN recorded losses ------------
  # The AFL API's playerStats extendedStats carry a per-player one-on-one contest
  # ledger -- contest_def_losses (aerial duels this defender lost) and
  # contest_off_one_on_ones minus contest_off_wins (duels this forward lost).
  # Verified populated in every season 2021-2026, `losses <= one_on_ones` on
  # every row, and the career leaderboard by defensive one-on-ones per game is a
  # clean roll-call of the competition's key defenders.
  #
  # It would have been the one place a box-score field touched v3 outside the
  # ruck carve-out, and defensibly so: the credit stays entirely delta_epv-derived
  # and the stat would only decide WHOSE ledger an already chain-derived debit
  # lands on. Chains cannot answer that -- recon found 0 of 7,736 attacking mark
  # wins name the beaten defender.
  #
  # *** MEASURED AND REJECTED (2026-08-03). NOT the default. ***
  # Its correlations collapse to near zero (contested marks 0.027, spoils 0.001),
  # which reads at first like the good outcome -- "measures ability, not volume".
  # Persistence says otherwise: r = 0.485 year-over-year against "team"'s 0.819,
  # second WORST of the five rules. Netting a player's wins against his own
  # recorded losses cancels most of the signal and leaves noise. Kept implemented
  # because the reasoning was sound and only the data settled it; do not re-adopt
  # it without a persistence number that beats 0.819.
  #
  # Its own caveat, still true: the ledger counts ONE-ON-ONE duels (~26 per
  # match) while chains sees ~254 aerial contests per match, so it is a weighting
  # key for who does the contested defending, not a complete census. Teams with
  # no recorded losses fall back to the flat share.
  if (identical(exposure_descs, "ledger")) {
    if (is.null(player_stats)) {
      cli::cli_abort(c(
        "{.val ledger} allocation needs {.arg player_stats}.",
        "x" = "Refusing to silently fall back to a different rule than the caller asked for."
      ))
    }
    ps <- data.table::as.data.table(player_stats)
    need <- c("contest_def_losses", "contest_off_one_on_ones", "contest_off_wins")
    if (!all(need %in% names(ps))) {
      cli::cli_abort("player_stats is missing the contest ledger: {setdiff(need, names(ps))}")
    }
    z <- function(v) data.table::fifelse(is.na(v), 0, as.numeric(v))
    key <- ps[, .(match_id, team_id, player_id,
                  w_def = z(contest_def_losses),
                  w_off = pmax(0, z(contest_off_one_on_ones) - z(contest_off_wins)))]

    # A contest the ATTACK retained leaves the defender unnamed; one the defence
    # won with no logged Contest Target leaves the forward unnamed. They weight
    # differently, so they are allocated separately rather than pooled.
    un <- s[is.na(loser_pid), .(debit = sum(loser_credit)),
            by = .(match_id, team_id = loser_tid, side = data.table::fifelse(def_win, "off", "def"))]
    a <- merge(key, un, by = c("match_id", "team_id"), allow.cartesian = TRUE)
    # player_stats and chains are different sources, so `match_id`/`team_id`
    # agreeing is an assumption, not a fact. A mismatch would empty this merge
    # and drop the whole debit silently -- the "guard degrades to a no-op"
    # failure this package keeps hitting -- so it is checked, not hoped for.
    if (nrow(a) == 0) {
      cli::cli_abort(c(
        "Ledger allocation matched no rows joining player_stats to chains.",
        "i" = "{nrow(un)} unnamed-debit cells, {nrow(key)} player_stats cells.",
        "x" = "Refusing to drop {round(abs(sum(un$debit)), 1)} points of contest debit silently."
      ))
    }
    a[, w := data.table::fifelse(side == "def", w_def, w_off)]
    a[, tw := sum(w), by = .(match_id, team_id, side)]
    # Flat fallback where a team recorded no losses at all on that side.
    a[, share := data.table::fifelse(tw > 0, w / tw, 1 / .N), by = .(match_id, team_id, side)]
    a[, cont_alloc := debit * share]
    out <- a[, .(cont_alloc = sum(cont_alloc)), by = .(player_id, match_id)]

    allocated <- sum(out$cont_alloc)
    owed <- sum(un$debit)
    if (abs(allocated - owed) > max(1e-6, 0.001 * abs(owed))) {
      cli::cli_abort(c(
        "Ledger allocation did not conserve: owed {round(owed, 2)}, allocated {round(allocated, 2)}.",
        "x" = "The point of allocating at all is that the debit lands somewhere."
      ))
    }
    cli::cli_alert_info(
      "Contest debits allocated by the one-on-one ledger: {round(owed, 1)} points across {format(nrow(out), big.mark = ',')} player-games")
    return(out)
  }

  if (identical(exposure_descs, "TEAM_EQUAL") || identical(exposure_descs, "team")) {
    # Every player who appeared for the losing team takes an equal share, and
    # zone is collapsed. This makes no claim about WHICH opponent was beaten --
    # it only says the team was. Conserves exactly, and because the share is flat
    # it cannot reorder players within a team, which is the distortion the
    # exposure-weighted rules could not avoid in either direction.
    unnamed <- s[is.na(loser_pid), .(debit = sum(loser_credit)),
                 by = .(match_id, team_id = loser_tid)]
    expo <- ch[!is.na(player_id) & !is.na(team_id),
               .(expo = 1), by = .(match_id, team_id, player_id)]
    a <- merge(expo, unnamed, by = c("match_id", "team_id"), allow.cartesian = TRUE)
    a[, share := expo / sum(expo), by = .(match_id, team_id)]
    a[, cont_alloc := debit * share]
    return(a[, .(cont_alloc = sum(cont_alloc)), by = .(player_id, match_id)])
  }

  expo <- ch[!is.na(player_id) & !is.na(team_id) &
               description %chin% exposure_descs][
    , zone := zone_of(x)][
    , .(expo = .N), by = .(match_id, team_id, player_id, zone)]
  if (nrow(expo) == 0) {
    cli::cli_warn("No aerial exposure rows found; contest debits left unallocated.")
    return(data.table::data.table(player_id = character(), match_id = character(),
                                  cont_alloc = numeric()))
  }
  a <- merge(expo, unnamed, by = c("match_id", "team_id", "zone"),
             allow.cartesian = TRUE)
  a[, share := expo / sum(expo), by = .(match_id, team_id, zone)]
  a[, cont_alloc := debit * share]
  a[, .(cont_alloc = sum(cont_alloc)), by = .(player_id, match_id)]
}


#' Per-player aerial contest credit
#'
#' Fits the branch models on a strictly EARLIER window than the season being
#' scored when \code{leak_safe} is TRUE, which it must be for anything feeding
#' the match-model gate: the ratings are features of that model, so branch models
#' fitted on the full history would leak the future into an OOS evaluation.
#'
#' @param chains Raw chains data.
#' @param pbp_data Clean PBP.
#' @param leak_safe Fit per season on earlier seasons only.
#' @param alloc Loser-allocation rule: \code{"team"} (default and the only one
#'   that survived measurement), \code{"none"}, \code{"prorata"} or
#'   \code{"ledger"}. See \code{EPV_CONT_LOSS_ALLOC}.
#' @param exposure_descs Chain descriptions counting as exposure, used only by
#'   \code{"prorata"}.
#' @param player_stats Required by \code{"ledger"}; ignored otherwise.
#' @return A data.table of \code{player_id}, \code{match_id},
#'   \code{epv_cont_aerial}, \code{epv_disp_aerial}, \code{contests_won},
#'   \code{contests_lost}.
#' @keywords internal
compute_aerial_credit <- function(chains, pbp_data, leak_safe = TRUE,
                                  alloc = EPV_CONT_LOSS_ALLOC,
                                  exposure_descs = EPV3_AERIAL_EXPOSURE_DESCS,
                                  player_stats = NULL) {
  cst <- build_aerial_contests(chains, pbp_data)
  if (nrow(cst) == 0) {
    return(data.table::data.table(
      player_id = character(), match_id = character(),
      epv_cont_aerial = numeric(), epv_disp_aerial = numeric(),
      contests_won = integer(), contests_lost = integer()))
  }
  half <- attr(cst, "half")
  # Season is parsed out of match_id ("CD_M20260140603" -> 2026). If that format
  # ever changes this yields all-NA, uniqueN() reads 1, and the leak-safe branch
  # below quietly falls through to a single in-sample fit -- i.e. the guard
  # becomes a no-op and every OOS number downstream is optimistic without
  # anything failing. Check it rather than discover it in a gate result.
  cst[, .season := as.integer(substr(match_id, 5, 8))]
  n_bad <- sum(is.na(cst$.season) | cst$.season < 2000 | cst$.season > 2100)
  if (isTRUE(leak_safe) && n_bad > 0) {
    cli::cli_abort(c(
      "Could not parse a plausible season from {n_bad} of {nrow(cst)} match_id{?s}.",
      "i" = "Example: {.val {utils::head(cst$match_id[is.na(cst$.season)], 1)}}",
      "x" = "Refusing to run leak-safe fitting on an unparsed season -- it would silently become an in-sample fit."
    ))
  }

  if (isTRUE(leak_safe) && data.table::uniqueN(cst$.season) > 1) {
    seasons <- sort(unique(cst$.season))
    scored <- data.table::rbindlist(lapply(seasons, function(s) {
      idx <- cst$.season < s
      # The earliest season has no history to fit on. Score it in-sample rather
      # than drop it, and say so -- a silently missing season is worse than a
      # flagged optimistic one.
      if (sum(idx) < 5000) {
        idx <- cst$.season == s
        cli::cli_alert_warning(
          "Season {s} contest models fitted IN-SAMPLE (no earlier season available).")
      }
      score_contests(cst[.season == s], fit_contest_models(cst, idx))
    }))
  } else {
    scored <- score_contests(cst, fit_contest_models(cst))
  }

  win <- scored[!is.na(winner_pid), .(cont = sum(winner_credit), won = .N),
                by = .(player_id = winner_pid, match_id)]
  los <- scored[!is.na(loser_pid), .(cont = sum(loser_credit), lost = .N),
                by = .(player_id = loser_pid, match_id)]
  dsp <- scored[!is.na(kick_pid), .(disp = sum(disp_credit)),
                by = .(player_id = kick_pid, match_id)]

  parts <- list(win[, .(player_id, match_id, cont, won, lost = 0L)],
                los[, .(player_id, match_id, cont, won = 0L, lost)])
  if (alloc %in% c("team", "prorata", "ledger")) {
    descs <- switch(alloc, team = "team", ledger = "ledger", exposure_descs)
    al <- allocate_contest_losses(scored, chains, half, descs, player_stats)
    parts <- c(parts, list(al[, .(player_id, match_id, cont = cont_alloc,
                                  won = 0L, lost = 0L)]))
  }
  cred <- data.table::rbindlist(parts, use.names = TRUE)[
    , .(epv_cont_aerial = sum(cont), contests_won = sum(won),
        contests_lost = sum(lost)), by = .(player_id, match_id)]

  out <- merge(cred, dsp[, .(player_id, match_id, epv_disp_aerial = disp)],
               by = c("player_id", "match_id"), all = TRUE)
  for (cc in c("epv_cont_aerial", "epv_disp_aerial")) {
    data.table::set(out, which(is.na(out[[cc]])), cc, 0)
  }
  for (cc in c("contests_won", "contests_lost")) {
    data.table::set(out, which(is.na(out[[cc]])), cc, 0L)
  }
  # The kick rows these contests consumed. The caller MUST exclude them from the
  # ordinary disposer/receiver split or the same swing is paid twice -- once as a
  # 50/50 split and again as disposal-plus-contest.
  data.table::setattr(out, "aerial_kick_keys",
                      unique(scored[, .(match_id, display_order = kick_do)]))
  out
}
