# Difficulty-weighted credit on EVERY disposal.
#
# v3 already implements Oliver's credit model, but only for the ~8% of kicks
# ending in an aerial contest. Everything else is split by a flat
# EPV_DISP_SCALE = EPV_RECV_SCALE = 0.5, regardless of how hard the disposal was.
#
# Measured, that flat split is applied across situations where the chance of
# retaining the ball runs from 18% to 96% (epv3_disposal_feasibility.R):
#
#   kick length   <15m 90.2%   15-25m 85.3%   25-35m 73.5%
#                 35-45m 56.0%  45-55m 45.2%   >55m 35.4%
#   type          handball 89.5%   kick 66.3%   ground kick 53.9%
#
# A 15m chip to an unmarked teammate and a 50m pass under pressure currently pay
# their receiver the same share. Oliver's whole argument is that they should not:
# credit belongs to the participant whose part was harder, and the counterfactual
# probability is what says which that was.
#
# THE IDENTITY, unchanged from the aerial path -- only its domain widens:
#
#   delta_epv = (V_pre    - exp_pts )   the DECISION      -> disposer
#             + (V_branch - V_pre   )   the SURPRISE      -> whoever resolved it
#             + (V_after  - V_branch)   subsequent play   -> the next row
#
# with V_pre = (1-p) * V_ret + p * V_turn and p = P(turnover). Difficulty
# weighting is not added on top; it falls out. On an easy disposal p is small, so
# V_pre sits close to V_ret and the receiver's surprise for completing it is
# near zero -- he did what was expected. On a hard one the disposer banks a large
# expected gain and the receiver banks a large surprise for pulling it off.
#
# This is the same shape as Oliver's (p_thrower - p_catcher + 1)/2, derived from
# state values rather than assumed, and it needs no share parameter.
#
# NOT ENABLED BY DEFAULT. It changes what every channel means, so it carries its
# own flag and its own gate.

#' Apply the contest identity to every disposal, not just aerial contests
#'
#' \code{FALSE} keeps the flat \code{EPV_DISP_SCALE}/\code{EPV_RECV_SCALE} split
#' on non-aerial disposals, which is production.
#' @keywords internal
EPV_DIFFICULTY_SPLIT <- FALSE

#' Disposal descriptions the difficulty split covers
#' @keywords internal
EPV_DIFFICULTY_DESCS <- c("Kick", "Handball", "Ground Kick")

#' Share of the SURPRISE paid to whoever resolved the disposal
#'
#' The rest goes to the disposer. Both contributed to beating expectation -- a
#' 50m pass hit on the chest is a good kick AND a good mark -- so handing the
#' whole surprise to the receiver is winner-take-all on the only term carrying
#' skill.
#'
#' \strong{0.5 is a placeholder and is the next thing to fit, not a finding.}
#' Oliver's simultaneous-team formula puts it at
#' \code{(p_disposer - p_receiver + 1) / 2}, i.e. the participant whose part was
#' harder takes more. We do not yet estimate the two participants' separate
#' success probabilities, only the joint one, so a flat half is the honest
#' interim. Fitting it is worth doing; asserting it is not.
#' @keywords internal
EPV_DIFFICULTY_SURPRISE_SHARE <- 0.5

#' Build the disposal-outcome table from chains
#'
#' One row per disposal, carrying what was knowable BEFORE it resolved plus the
#' outcome. 95.2\% of disposals resolve to an outcome row naming a team and a
#' player.
#'
#' @param chains Raw chains data.
#' @param pbp_data Clean PBP carrying \code{exp_pts} and \code{delta_epv}.
#' @return A data.table, one row per resolvable disposal.
#' @keywords internal
build_disposal_events <- function(chains, pbp_data) {
  ch <- data.table::as.data.table(chains)
  detect_chains_columns(ch)
  # Narrow before shifting. The full chains frame is 60+ columns and shifting on
  # it with get() inside [ is the documented data.table trap -- it broke the
  # fast column-reference path and made an earlier script unusably slow.
  p <- data.table::data.table(
    match_id = ch$match_id, display_order = ch$display_order,
    description = ch$description, player_id = ch$player_id,
    team_id = ch$team_id, x = ch$x, y = ch$y)
  rm(ch)
  data.table::setorder(p, match_id, display_order)

  for (k in 1:6) {
    for (s in c("description", "team_id", "player_id", "x", "y")) {
      p[, (paste0("f", k, "_", s)) :=
          data.table::shift(get(s), k, type = "lead"), by = match_id]
    }
  }
  half <- as.numeric(stats::quantile(abs(p$x), 0.995, na.rm = TRUE))

  d <- p[description %chin% EPV_DIFFICULTY_DESCS &
           !is.na(player_id) & !is.na(team_id)]
  if (nrow(d) == 0) return(d[0])

  inflight <- CHAINS_INFLIGHT_DESCS
  d[, .olag := data.table::fcase(
    !(f1_description %chin% inflight), 1L, !(f2_description %chin% inflight), 2L,
    !(f3_description %chin% inflight), 3L, !(f4_description %chin% inflight), 4L,
    !(f5_description %chin% inflight), 5L, !(f6_description %chin% inflight), 6L,
    default = NA_integer_)]
  pick <- function(stem) data.table::fcase(
    d$.olag == 1L, d[[paste0("f1_", stem)]], d$.olag == 2L, d[[paste0("f2_", stem)]],
    d$.olag == 3L, d[[paste0("f3_", stem)]], d$.olag == 4L, d[[paste0("f4_", stem)]],
    d$.olag == 5L, d[[paste0("f5_", stem)]], d$.olag == 6L, d[[paste0("f6_", stem)]])
  d[, `:=`(out_desc = pick("description"), out_tid = pick("team_id"),
           out_pid = pick("player_id"), out_x = pick("x"), out_y = pick("y"))]
  d <- d[!is.na(out_tid) & !is.na(out_pid)]
  if (nrow(d) == 0) return(d[0])

  pbp <- data.table::as.data.table(pbp_data)
  d <- merge(d, pbp[, .(match_id, display_order, exp_pts, delta_epv)],
             by = c("match_id", "display_order"), all.x = TRUE, sort = FALSE)
  d <- d[is.finite(exp_pts) & is.finite(delta_epv)]
  if (nrow(d) == 0) return(d)

  d[, `:=`(
    turnover = out_tid != team_id,
    V_after  = exp_pts + delta_epv,
    kick_len = sqrt((out_x - x)^2 + (out_y - y)^2),
    fwd_gain = out_x - x,
    abs_y    = abs(y),
    goal_dist = sqrt(pmax(0, half - x)^2 + abs(y)^2),
    is_handball = factor(as.integer(description == "Handball"), levels = c("0", "1")),
    i50f = factor(as.integer(x > half - 50), levels = c("0", "1"))
  )]
  data.table::setattr(d, "half", half)
  d[is.finite(kick_len) & is.finite(fwd_gain) & is.finite(goal_dist)]
}

#' Fit the three disposal branch models
#'
#' \code{p} = P(turnover), and the two branch values = E[post-state | branch].
#' Every term must be knowable BEFORE the disposal resolves -- \code{exp_pts}
#' describes the situation beforehand and is legitimate; the outcome
#' description is not and must never appear.
#'
#' @param de Disposal table from \code{build_disposal_events()}.
#' @param train_idx Logical vector selecting rows to fit on.
#' @return A list of three \code{bam} fits.
#' @keywords internal
fit_disposal_models <- function(de, train_idx = rep(TRUE, nrow(de))) {
  tr <- de[train_idx]
  rhs <- ~ s(x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
    s(exp_pts) + is_handball + i50f
  fit <- function(f, dd, ...) {
    for (fv in c("is_handball", "i50f")) {
      if (fv %in% all.vars(f) && length(unique(dd[[fv]][!is.na(dd[[fv]])])) < 2) {
        f <- stats::update(f, stats::as.formula(paste(". ~ . -", fv)))
      }
    }
    mgcv::bam(f, data = droplevels(dd), discrete = TRUE, ...)
  }
  list(
    p   = fit(stats::update(rhs, turnover ~ .), tr, family = stats::binomial()),
    ret = fit(stats::update(rhs, V_after ~ .), tr[turnover == FALSE]),
    trn = fit(stats::update(rhs, V_after ~ .), tr[turnover == TRUE])
  )
}

#' Score disposals and split the credit by difficulty
#'
#' @param de Disposal table.
#' @param models From \code{fit_disposal_models()}.
#' @return \code{de} with \code{p_hat}, \code{V_pre}, \code{disp_credit} and
#'   \code{recv_credit}.
#' @keywords internal
score_disposals <- function(de, models) {
  d <- data.table::copy(de)
  d[, `:=`(
    p_hat     = as.numeric(stats::predict(models$p,   newdata = d, type = "response")),
    V_ret_hat = as.numeric(stats::predict(models$ret, newdata = d)),
    V_trn_hat = as.numeric(stats::predict(models$trn, newdata = d))
  )]
  d[, V_pre := (1 - p_hat) * V_ret_hat + p_hat * V_trn_hat]

  # The DECISION term, V_pre - exp_pts, is the expected state the disposal
  # created before anyone resolved it.
  #
  # *** IT IS SITUATIONAL, NOT A SKILL TERM, AND GIVING THE DISPOSER ONLY THIS
  # DESTROYS HIS CHANNEL. *** Measured on the first build: any player kicking
  # from that spot with those options gets the same credit, so the channel
  # converted to margin at 0.100 (t 0.9) and carried 0.1% of the share. The
  # disposer's SKILL is in whether the disposal actually beat expectation --
  # which lives in the surprise term.
  #
  # So the surprise is SPLIT rather than handed whole to whoever resolved it.
  # Giving it all to the resolver is winner-take-all on the one term that
  # carries the skill, which is the specific error Oliver's essays are about.
  d[, surprise := data.table::fifelse(turnover, V_trn_hat - V_pre, V_ret_hat - V_pre)]
  ss <- EPV_DIFFICULTY_SURPRISE_SHARE
  d[, `:=`(
    disp_credit = (V_pre - exp_pts) + (1 - ss) * surprise,
    # Sign matters here and it was wrong on the first build. `V_after` and every
    # branch value are in the DISPOSING team's frame, so a turnover's surprise is
    # negative -- and `out_pid` on a turnover is the OPPONENT who intercepted.
    # Assigning it unflipped charged interceptors for intercepting, which cost
    # key defenders 7.2 points a game. The resolver's credit is always in HIS
    # frame: he gains when he takes it and gains when he wins it back.
    recv_credit = data.table::fifelse(turnover, -ss * surprise, ss * surprise)
  )]
  d
}

#' Per-player difficulty-weighted disposal credit
#'
#' @param chains Raw chains.
#' @param pbp_data Clean PBP.
#' @param leak_safe Fit each season on strictly earlier seasons.
#' @return A data.table of \code{player_id}, \code{match_id},
#'   \code{epv_disp_diff}, \code{epv_recv_diff}, plus the keys of the disposals
#'   consumed so the caller can exclude them from the flat split.
#' @keywords internal
compute_difficulty_credit <- function(chains, pbp_data, leak_safe = TRUE) {
  de <- build_disposal_events(chains, pbp_data)
  if (nrow(de) == 0) {
    return(data.table::data.table(player_id = character(), match_id = character(),
                                  epv_disp_diff = numeric(), epv_recv_diff = numeric()))
  }
  de[, .season := as.integer(substr(match_id, 5, 8))]
  n_bad <- sum(is.na(de$.season) | de$.season < 2000 | de$.season > 2100)
  if (isTRUE(leak_safe) && n_bad > 0) {
    cli::cli_abort(c(
      "Could not parse a plausible season from {n_bad} of {nrow(de)} match_id{?s}.",
      "x" = "Refusing to run leak-safe fitting on an unparsed season -- it would silently become an in-sample fit."
    ))
  }
  scored <- if (isTRUE(leak_safe) && data.table::uniqueN(de$.season) > 1) {
    seasons <- sort(unique(de$.season))
    data.table::rbindlist(lapply(seasons, function(s) {
      idx <- de$.season < s
      if (sum(idx) < 20000) {
        idx <- de$.season == s
        cli::cli_alert_warning(
          "Season {s} disposal models fitted IN-SAMPLE (no earlier season available).")
      }
      score_disposals(de[.season == s], fit_disposal_models(de, idx))
    }))
  } else {
    score_disposals(de, fit_disposal_models(de))
  }

  dsp <- scored[, .(epv_disp_diff = sum(disp_credit)), by = .(player_id, match_id)]
  rcv <- scored[, .(epv_recv_diff = sum(recv_credit)), by = .(player_id = out_pid, match_id)]
  out <- merge(dsp, rcv, by = c("player_id", "match_id"), all = TRUE)
  for (cc in c("epv_disp_diff", "epv_recv_diff")) {
    data.table::set(out, which(is.na(out[[cc]])), cc, 0)
  }
  data.table::setattr(out, "disposal_keys",
                      unique(scored[, .(match_id, display_order)]))
  data.table::setattr(out, "scored", scored)
  out
}
