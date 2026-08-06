# Player Rating Constants
# =======================
# Constants for player rating composition: EPR (Expected Possession Rating),
# EPV (Expected Possession Value) component weights, PSR (Player Skill Rating),
# TORP blending weights, WP credit allocation, stat-rating estimation, and
# position-based TOG (time on ground) priors.

# EPR (Expected Possession Rating) System Constants
# -------------------------------------------------

#' Default total predicted TOG for league-wide centering (18 players per team x 18 teams).
#' Used as fallback; actual centering adapts to the number of teams present.
#' @keywords internal
TOTAL_PRED_TOG <- 324L

#' Decay factor (in days) for PBP-level game recency weighting in add_epv_vars()
#' @keywords internal
EPV_WEIGHT_DECAY_DAYS <- 365

#' Decay factor (in days) for receiving component weighting
#' @keywords internal
EPR_DECAY_RECV <- 273

#' Decay factor (in days) for disposal component weighting
#' @keywords internal
EPR_DECAY_DISP <- 630

#' Decay factor (in days) for spoil component weighting
#' @keywords internal
EPR_DECAY_SPOIL <- 523

#' Decay factor (in days) for hitout component weighting
#' @keywords internal
EPR_DECAY_HITOUT <- 545

#' Default decay factor (in days) — legacy alias for backwards compatibility
#' @keywords internal
EPR_DECAY_DEFAULT_DAYS <- EPR_DECAY_RECV

#' Loading factor for TORP calculations
#' @keywords internal
EPR_LOADING_DEFAULT <- 1.0000


#' Prior games constant for hitout ratings
#' @keywords internal
EPR_PRIOR_GAMES_HITOUT <- 3.0000

# NOTE: these scale constants must stay ABOVE the EPR_PRIOR_RATE_* definitions
# that reference them. R sources a package file top to bottom, so defining them
# later gives 'object not found' at build time, not a runtime surprise.
#' Points-scale calibration for the EPV/EPR family
#'
#' One rating point should mean one scoreboard point. Measured on 1121 matches
#' (one row per match; team rows are mirrored pairs that double n without
#' adding information), regressing actual margin on the team rating difference:
#' EPR converted at 0.919 `[0.842, 0.996]` and PSR at 1.579 `[1.443, 1.715]`.
#' A nominal 0.5/0.5 TORP blend was therefore ~37/63 in scoreboard terms.
#'
#' Applied at the VALUE layer so it flows into the rating, per Pete
#' 2026-07-29 -- the same principle as the level fix. `new = slope * old`, so
#' EPV shrinks ~8% and PSV grows ~58%.
#'
#' \strong{This cannot be validated by match MAE, by construction.} A linear
#' rescale of a feature is a no-op for the model: the GAM absorbs it into its
#' coefficient and XGBoost splits are monotone-invariant. The change is about
#' what a published number MEANS, not about prediction.
#'
#' Caveat recorded because it bounds how much the constant is worth: the slope
#' wanders by season (EPR 0.87-1.02 excluding a thin 2021 at 0.579; PSR
#' 1.26-1.80). A single global constant is a deliberate simplification --
#' per-season calibration would be fitting noise.
#' @keywords internal
EPV_POINTS_SCALE <- 0.919

#' Use per-channel points scaling under v2 as well as v3
#'
#' \code{EPV_POINTS_SCALE} is ONE global factor for all four v2 channels, and
#' one factor cannot make each channel read one point per unit when they convert
#' at very different rates. Raw v2 \code{epv} conserves to margin at only
#' \strong{0.4778} as a result; the fitted per-channel EPR-layer scales are
#' recv 0.870, disp 0.502, spoil 2.892, hitout 4.033, and applying them takes
#' conservation to 1.0000.
#'
#' \strong{Correction 2026-08-06.} An earlier version of this note quoted
#' 0.893 / 1.556 / 0.344 as "the v2 frame". Those are the \emph{v3 ship} frame's
#' RAW-layer scales and do not belong here. v2's own raw-layer fit is
#' recv 0.611, disp 0.552, spoil \strong{-0.481} -- and that negative is real,
#' matching the long-recorded v2 contest coefficient of -0.41 (t -10.47).
#' \strong{It also means the raw-layer calibration cannot be applied to v2 as
#' fitted}: multiplying the spoil channel by a negative flips its sign, so a
#' spoil would subtract value from a player. Raw-layer calibration on v2 needs
#' that channel fixed or bounded first.
#'
#' \code{TRUE} makes \code{centre_epv_by_position()} read
#' \code{EPV3_POINTS_SCALE} regardless of engine, so the v2 channels get their
#' own factors. The name says v3 for historical reasons; the vector is engine
#' agnostic and \code{lbl} already maps all four v2 channels onto it.
#'
#' \strong{Set \code{EPR_PRIOR_RATE_*} to match.} Shrinkage pulls toward those
#' priors, so scaling a channel without scaling its prior changes the amount of
#' shrinkage rather than just the units -- the same coupling
#' \code{EPV_POINTS_SCALE} already documents.
#' @keywords internal
EPV_PER_CHANNEL_POINTS_SCALE <- FALSE

#' Which EPV engine computes the channels
#'
#' \code{"v2"} is production and is the default: box-score weights plus a
#' chain-derived part, four channels \code{recv / disp / spoil / hitout}.
#'
#' \code{"v3"} is the chain-native rebuild -- \code{delta_epv} arithmetic only,
#' with the ruck/hitout box terms as the single permitted carve-out because
#' \code{Centre Bounce} and \code{Ball Up Call} rows carry a \code{player_id}
#' 0.0\% of the time. Four channels \code{recv / disp / cont_aerial / cont_stop}.
#' See \code{../docs/plans/EPV-V3-CHAIN-NATIVE.md}.
#'
#' \strong{Flipping this changes every published rating.} It is a flag, not a
#' tunable: v3 has to clear the gates in that plan's section 6 first.
#' @keywords internal
EPV_ENGINE <- "v2"

#' How v3 distributes contest debits whose loser chains never names
#'
#' Chains names the beaten aerial opponent in only ~12\% of contests -- never
#' when the attack retains, and only when a \code{Contest Target} row happened to
#' be logged when the defence wins. So ~86\% of the contest credit mass carries a
#' debit with no name on it, and it has to go somewhere or the channel becomes
#' upside-only.
#'
#' Five rules were measured against each other (\code{epv3_compare_alloc.R},
#' 2026-08-03, 57,145 player-games). \code{"team"} wins and is the default.
#'
#' \tabular{lrrrrr}{
#'   \tab \strong{none} \tab \strong{contested} \tab \strong{wide} \tab \strong{team} \tab \strong{ledger} \cr
#'   cor contested_marks \tab  0.451 \tab -0.165 \tab  0.242 \tab  0.440 \tab  0.027 \cr
#'   cor spoils          \tab  0.576 \tab -0.046 \tab  0.238 \tab  0.582 \tab  0.001 \cr
#'   cor disposals       \tab  0.104 \tab  0.123 \tab -0.114 \tab  0.073 \tab  0.163 \cr
#'   \strong{r year-over-year} \tab 0.819 \tab 0.490 \tab 0.578 \tab \strong{0.819} \tab 0.485 \cr
#'   conserves           \tab  no    \tab  yes   \tab  yes   \tab  yes   \tab  yes   \cr
#' }
#'
#' \describe{
#'   \item{\code{"team"}}{Equal share across every player who appeared for the
#'     losing team. Makes no claim about WHICH opponent was beaten, only that the
#'     team was -- so it cannot reorder players within a team, which is the
#'     distortion every exposure-weighted rule introduced in one direction or the
#'     other. Conserves exactly and is the most repeatable.}
#'   \item{\code{"prorata"}}{Weight by aerial exposure in the zone. Rejected: the
#'     contested-only base is nearly a win count and INVERTS the channel (Harris
#'     Andrews, the best intercept defender in the competition, reads -284), and
#'     the wide base charges mark-and-kick rebounders for duels they never
#'     entered (Caleb Daniel -202).}
#'   \item{\code{"none"}}{Leave it unallocated. Scores as well as \code{"team"}
#'     but the channel becomes upside-only and it does not conserve -- 153,428
#'     points of debit simply vanish.}
#'   \item{\code{"ledger"}}{Weight by the player's OWN recorded one-on-one losses
#'     from the AFL API. Theoretically the best key and it is why the API dig
#'     happened -- \strong{but measurement rejected it.} Its near-zero
#'     correlations looked at first like "measures ability, not volume"; the
#'     persistence test says otherwise, at r = 0.485 year-over-year against
#'     \code{"team"}'s 0.819. Netting a player's wins against his own losses
#'     cancels most of the signal and leaves noise. Kept implemented because the
#'     reasoning was sound and only the data settled it.}
#' }
#' @keywords internal
EPV_CONT_LOSS_ALLOC <- "team"

#' How many channels v3 splits EPV into: 4 (default) or 3
#'
#' Pete's original framing was "3 subsections, maaaybe 4". Both are built.
#'
#' \code{4} keeps aerial contests and stoppage contests apart:
#' \code{recv / disp / cont_aerial / cont_stop}. \code{3} merges the two contest
#' channels into one: \code{recv / disp / cont}.
#'
#' \strong{The merge does not change \code{epv_adj} at all} -- the total is the
#' same sum either way. The entire difference is in EPR aggregation, because each
#' channel carries its own \code{EPR_DECAY_*}, \code{EPR_PRIOR_GAMES_*} and
#' \code{EPR_PRIOR_RATE_*}, so merging changes the decay and Bayesian shrinkage a
#' ruckman's stoppage value receives. That is the question, and it is why this
#' cannot be settled by looking at the columns.
#'
#' The prior evidence favours keeping them apart, but it is an argument rather
#' than a result: \code{cor(cont_aerial, cont_stop) = 0.004} (orthogonal, so
#' merging loses information rather than removing duplication), and
#' \code{cont_stop} is a ruck channel -- sd 1.480 for rucks against 0.112-0.504
#' everywhere else, versus \code{cont_aerial}'s 2.57 across the board. Merged, a
#' ruck's stoppage signal is swamped by a channel five times its size.
#'
#' Under \code{3}, the \code{hitout} slot is zeroed and
#' \code{EPR_PRIOR_RATE_HITOUT} goes to 0 so the empty slot contributes exactly
#' zero rather than shrinking toward a non-zero prior.
#'
#' \strong{Currently 4 on UNTUNED evidence, and under active comparison.} The
#' untuned gate favoured 4 on RMSE (32.535 vs 32.704), bits (0.2448 vs 0.2389),
#' Brier and tips, with MAE tied (+0.0057, CI spanning zero). The mechanism is
#' understood: merging buries \code{cont_stop}, the most predictive channel per
#' sd in the whole rating (+5.73 points of margin per sd, coefficient 9.60),
#' inside \code{cont_aerial}, which is fully redundant (-0.06 per sd, p = 0.954).
#'
#' \strong{But that comparison is not yet fair.} Both arms inherited decay and
#' shrinkage priors tuned for v2's box-score channels, and the 3-channel arm
#' inherits parameters shaped around a 4-channel structure. The optimiser
#' (\code{epv3_optimise_epr.R}) re-runs 3-vs-4 with each structure tuned on its
#' own terms, which is the only version of this comparison worth deciding on.
#' @keywords internal
EPV3_CHANNELS <- 3L

#' Per-channel points constants for v3 (1 unit = 1 point of margin)
#'
#' v3's channels are separately calibrated, replacing the single global
#' \code{EPV_POINTS_SCALE}.
#'
#' \strong{Fitted 2026-08-03 but deliberately NOT applied yet:}
#' \code{c(recv = 0.5969, disp = 0.6095, cont_aerial = 0.2656, cont_stop = 1.7680)}
#' (\code{epv3_calibrate_points.R}; the refit returns 1.0000 on every channel, so
#' the constants are what they claim). Left at 1 so the match-model gate compares
#' \emph{engines} with one variable moving, not engine-plus-rescale. Applying
#' them also requires threading each constant into its \code{EPR_PRIOR_RATE_*}
#' twin, exactly as \code{EPV_POINTS_SCALE} is -- \code{.bayesian_shrink()} adds
#' \code{prior_games * prior_rate} after the value, so a scaled channel with an
#' unscaled prior leaves EPR a blend of the two.
#'
#' \strong{The headline finding, which is not what was hoped for.} In raw EPV
#' units the contest channel carries 15.7\% of variance; converted to margin
#' points it carries \strong{3.3\%}, because a unit of contest is worth 0.27
#' points against a unit of disposal's 0.61. Contest value is real and much
#' larger than v2 allowed, but it converts to scoreboard margin at less than half
#' the rate possession value does. Equal thirds and "1 unit = 1 point" are not
#' simultaneously achievable, and per Pete's call the points constraint wins.
#'
#' \code{cont_stop} converts at 1.77 -- the highest of the four -- so calibration
#' would AMPLIFY the ruck channel 1.77x and take it from 0.5\% to 4.6\% of
#' variance.
#'
#' \strong{Prior warning.} Per-channel constants were tried on the v2 structure
#' on 2026-07-30 and failed: they reshuffled 5 of the served top 20, NARROWED
#' defender spread and dragged every position's thin-evidence quartile down
#' ~0.25. Under v3 this is the natural move, but it is a re-test, not a given,
#' and those are the three symptoms to look for.
#' \strong{FITTED AND APPLIED 2026-08-04}, on the structure that ships: 3
#' channels, raw contest merge, contest not standardised, ledger stoppage term,
#' measured shrinkage. Refitting was not optional -- a points constant is only
#' valid for the structure it was fitted on, and every one of those changed.
#'
#' Verified by REBUILD, not analytically, and iterated to a fixed point
#' (\code{epv3_verify_shipping_constants.R}): \strong{recv 1.00002, disp
#' 1.00016, contest 0.99599} -- worst residual 0.4\%.
#'
#' \strong{Do not chase that last 0.4\%.} The contest coefficient carries a
#' standard error of roughly 87\% of itself (t 1.12), so 0.4\% is orders of
#' magnitude below its own noise floor; earlier iterations oscillated between
#' 0.989 and 1.016 for exactly that reason. Two real causes:
#' \itemize{
#'   \item \code{adjust_epv_for_opponents()} is NOT linear in one channel -- it
#'     forms \code{.abs_total} as a sum of \code{abs()} ACROSS channels, so
#'     scaling channels by factors as different as 3.34 / 2.61 / 0.53 shifts the
#'     denominator. "Scaling a channel scales its rating by exactly k" is an
#'     approximation here, not an identity.
#'   \item contest value barely predicts team margin at all, which is a property
#'     of the game rather than a defect -- see \code{EPV3_CHANNELS} and
#'     \code{docs/reference/EPV-VALUE-ANATOMY.md} §5.
#' }
#' @keywords internal
EPV3_POINTS_SCALE <- if (identical(EPV_ENGINE, "v3")) {
  c(recv = 3.3413, disp = 2.6078, cont_aerial = 0.5226, cont_stop = 1)
} else {
  c(recv = 1, disp = 1, cont_aerial = 1, cont_stop = 1)
}

#' Which EPV channel maps onto which entry of EPV3_POINTS_SCALE
#'
#' The v3 scale vector is named for v3's channels, but the mapping is
#' engine-agnostic -- \code{EPV_PER_CHANNEL_POINTS_SCALE} uses it for v2's four
#' channels too. Named once here because every site that scales EPV into points
#' needs the identical mapping, and a second hand-written copy is how
#' \code{get_player_game_ratings()} drifted from the published pipeline.
#' @keywords internal
EPV_CHANNEL_SCALE_KEYS <- c(epv_recv = "recv", epv_disp = "disp",
                            epv_spoil = "cont_aerial", epv_hitout = "cont_stop")

# NOTE: EPR_PRIOR_GAMES_* live HERE, below EPV_ENGINE, and not up with the other
# EPR_* constants. They are engine-conditioned, R sources a package file top to
# bottom, and EPV_ENGINE is defined further down the file than their natural
# home -- putting them there gives "object 'EPV_ENGINE' not found" at BUILD
# time. Same hazard the note above EPV_POINTS_SCALE warns about.
#' How much evidence a channel needs before it stops being shrunk
#'
#' \code{prior_games} is the number of weighted games at which a player's own
#' record is worth as much as the prior. Production has run \strong{3.0} on every
#' channel, against a MEASURED requirement several times that -- it is a
#' within/between variance ratio (\code{sigma^2_within / tau^2_between}), and
#' v3's chain-delta channels are far noisier than v2's box-score counts, so they
#' need much more shrinkage.
#'
#' \strong{Keyed on \code{EPV_ENGINE}, and that is deliberate.} The v3 numbers
#' were measured on v3's channels and are meaningless for v2's; applying them to
#' v2 would change every published rating today for no reason. v2 keeps 3.0 so
#' production is byte-for-byte untouched while the engine flag stays at
#' \code{"v2"}.
#'
#' v2's own measured values are 7.75 / 9.12 / 12.19 / 4.18 -- also well above
#' 3.0, so production is under-shrinking too. That is a separate shippable
#' change and it has not been gated; do not fold it in here.
#'
#' Measured 2026-08-04 on the finished v3 structure over 1,032 players with 3+
#' games at TOG > 50 (\code{epv3_finalise.R} stage 4). An independent earlier
#' measurement on the 4-channel v3 build gave 14.38 / 24.35 / 11.37, so these
#' are stable to the structure change.
#' @keywords internal
EPR_PRIOR_GAMES_RECV <- if (identical(EPV_ENGINE, "v3")) 14.38 else 3.0000

#' Prior games constant for disposal ratings. See \code{EPR_PRIOR_GAMES_RECV}.
#' @keywords internal
EPR_PRIOR_GAMES_DISP <- if (identical(EPV_ENGINE, "v3")) 24.33 else 3.0000

#' Prior games constant for the spoil/contest slot. See \code{EPR_PRIOR_GAMES_RECV}.
#' @keywords internal
EPR_PRIOR_GAMES_SPOIL <- if (identical(EPV_ENGINE, "v3")) 11.09 else 3.0000

#' Sub-component scales applied BEFORE the two contest channels merge
#'
#' Only used when \code{EPV3_CHANNELS == 3L}. Pete's construction, and it is not
#' obvious: calibrate \code{cont_aerial} and \code{cont_stop} to one-point-per-
#' unit \emph{individually}, then add them. The sum then reads one point per
#' unit too, so the merged contest channel lands on the spec by construction
#' rather than by luck.
#'
#' Merging the RAW components does not get there. A raw sum blends by VARIANCE,
#' and the aerial part has roughly three times the spread while carrying no
#' margin signal of its own (t 0.91 against stoppage's 4.29) -- so the ruck
#' signal is diluted rather than carried, and the merged channel reads 0.570 at
#' t 1.87. Scaling first makes the merge blend by POINTS: 1.000 at t 4.43.
#'
#' This is also why the 3-channel arm previously lost the 3-vs-4 comparison on
#' bits. That comparison merged the raw components. Merging calibrated ones is a
#' different operation and has to be re-gated, not inherited.
#'
#' Fitted at the EPR layer against xmargin on the FOUR-channel build (the only
#' structure where the two components are separately visible to EPR), then
#' applied at the EPV layer, which is linear in each component so the ratio
#' carries. The overall level is re-fitted afterwards as
#' \code{EPV3_POINTS_SCALE["cont_aerial"]}, which scales the merged channel.
#' @keywords internal
EPV3_SUB_SCALE <- c(cont_aerial = 1, cont_stop = 1)

#' Correction factor sizing the ruck channel to the swing it stands in for
#'
#' \strong{1 = production. The 3.14x correction is NOT applied, but the reason
#' is NOT the one written here on 2026-08-04 -- that entry was wrong and is
#' retracted below.}
#'
#' \strong{RETRACTION (2026-08-05).} This constant was documented on 2026-08-04
#' as resting on an artifact: \code{exp_pts} is exactly 0.0000 on every
#' \code{Centre Bounce} row, and the +0.7912 \code{delta_epv} was called a
#' "scoreboard reset" that nobody created. Pete pushed back and he was right.
#'
#' A centre bounce IS a neutral state -- neither team has the ball -- so
#' \code{exp_pts = 0} is the correct value by symmetry, and winning the
#' clearance genuinely moves the winning team to +0.79. The value is real. The
#' gatherer correctly receives \code{EPV_RECV_SCALE} of it (0.3956 per bounce,
#' 10.01 points per match), and the DISPOSER half has nobody to go to because
#' the ball came from the umpire. Measured unallocated remainder:
#' \strong{0.3956 per centre bounce} against the documented 0.3925. The premise
#' behind 3.14x is sound; the "artifact" verdict was not.
#'
#' The one nuance that survives: \code{exp_pts} on \code{Centre Bounce} is
#' hard-coded (sd exactly 0, 100\% exactly zero) where every other description
#' has a distribution -- \code{Ball Up Call} reads sd 0.84. So the 0 is a
#' pipeline convention rather than an EP-model estimate. Defensible, but the
#' +0.79 depends on it.
#'
#' \strong{So why is the constant still 1?} Because "there is 0.3956 per bounce
#' unallocated" and "the RUCK should be paid it" are different claims, and only
#' the first is established. The increment measurement
#' (\code{epv3_ruck_increment.R}) says the ruck causes little of the outcome:
#' 0.0453 extra clearances per extra hitout over 1,242 matches (t 2.90, and
#' 0.0451 controlling for disposal differential), which prices a tap at
#' \strong{0.030 points against the 0.0742 already paid -- 0.4x}. The most
#' generous stoppage-side bound is 1.1x. Underneath it,
#' \code{cor(hitout differential, clearance differential) = 0.082}: winning the
#' ruck barely relates to winning the clearance.
#'
#' \strong{The reconciliation, and it points somewhere else entirely.} There is
#' ~10 points per match of genuinely unallocated stoppage value. It is real, and
#' it mostly does NOT belong to the ruck -- it belongs to the MIDFIELDER who
#' actually wins the ball, who today receives only the receiver's 50\% because a
#' stoppage has no disposer to pay the other half. Raising this constant would
#' hand the midfielder's share to the ruck. The open question is not "how much
#' more should rucks get" but "should a stoppage clearance pay 100\% to the
#' winner rather than 50\%".
#'
#' The regression route to a larger constant (7.22x historically, 7.50x refitted
#' 2026-08-04) remains rejected on its own evidence: it falls to 4.39 under PSR
#' control, reads 4.94 then 10.09 split-half, and \code{epv_cont_stop} is a
#' linear function of hitout COUNTS (\code{cor 0.9900}), so applying it made raw
#' hitout volume 12\% of the rating.
#' @keywords internal
EPV_RUCK_SWING_SCALE <- 1

#' Points-scale calibration for the PSV/PSR family
#'
#' See \code{EPV_POINTS_SCALE}. Applied to the shared coefficient vector, which
#' is what makes one constant reach BOTH products: PSR is not downstream of PSV
#' (they are parallel applications of the same betas to rated vs actual stats),
#' so scaling the betas is the only single point that moves them together.
#' @keywords internal
PSV_POINTS_SCALE <- 1.579

#' The points factor a channel's shrinkage prior has to carry
#'
#' \code{.bayesian_shrink()} adds \code{prior_games * prior_rate} AFTER the
#' channel value, so a scaled channel with an unscaled prior leaves EPR a blend
#' of scaled and unscaled parts rather than a clean rescale. Every
#' \code{EPR_PRIOR_RATE_*} therefore carries the same factor its channel does.
#'
#' Under v2 that factor is the single global \code{EPV_POINTS_SCALE}. Under v3
#' it is per channel, because v3's whole point is that one unit of each channel
#' is one point of margin and they convert at very different rates.
#'
#' \strong{Keyed on the constant, not on a per-call argument}, so a script that
#' passes \code{epv_engine = "v3"} while \code{EPV_ENGINE} still reads
#' \code{"v2"} gets v2 priors. That is the right default for production and the
#' wrong one for a two-engine gate, so gate scripts must
#' \code{assignInNamespace()} the prior rates for their v3 arm. The alternative
#' -- resolving the prior inside \code{.build_epr_season()} -- would make the
#' constant no longer a constant.
#' @keywords internal
.epr_prior_points_scale <- function(slot) {
  if (identical(EPV_ENGINE, "v3") && exists("EPV3_POINTS_SCALE")) {
    EPV3_POINTS_SCALE[[slot]]
  } else {
    EPV_POINTS_SCALE
  }
}

#' Prior rate for receiving component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_RECV <- -0.7000 * .epr_prior_points_scale("recv")

#' Prior rate for disposal component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_DISP <- -0.7000 * .epr_prior_points_scale("disp")

#' Prior rate for spoil component (shrinkage target per weighted game)
#'
#' Under v3 this slot holds CONTEST value. The shape (-0.3 against recv/disp's
#' -0.7) is inherited unchanged and only the points factor is threaded, which is
#' deliberate: whether a mostly zero-sum channel wants a negative prior at all
#' is a real question, but it is a separate change with its own effect and
#' bundling it here would make the calibration unattributable.
#' @keywords internal
EPR_PRIOR_RATE_SPOIL <- -0.3000 * .epr_prior_points_scale("cont_aerial")

#' Prior rate for hitout component (shrinkage target per weighted game)
#'
#' Carries \code{EPV_RUCK_SWING_SCALE} for the same reason the others carry
#' \code{EPV_POINTS_SCALE}: \code{.bayesian_shrink()} adds
#' \code{prior_games * prior_rate} AFTER the value, so an unscaled prior would leave
#' the hitout rating a blend of scaled and unscaled parts rather than a clean
#' rescale. If you change one, change both.
#' @keywords internal
#' Zeroed under the 3-channel v3 arm, where the hitout slot holds nothing. A
#' non-zero prior on an all-zero channel would shrink toward it and reintroduce a
#' level for a channel that does not exist. Conditioned on BOTH constants so v2
#' is untouched whatever `EPV3_CHANNELS` says.
EPR_PRIOR_RATE_HITOUT <- if (identical(EPV_ENGINE, "v3") && identical(EPV3_CHANNELS, 3L)) {
  0
} else {
  -0.3000 * .epr_prior_points_scale("cont_stop") * EPV_RUCK_SWING_SCALE
}

#' Decay factor (in days) for contest component weighting
#' @keywords internal
EPR_DECAY_CONTEST <- EPR_DECAY_RECV

#' Prior games constant for contest ratings
#' @keywords internal
EPR_PRIOR_GAMES_CONTEST <- 3.0000

#' Prior rate for contest component (shrinkage target per weighted game).
#' Zero because contest credit is zero-sum — average player should be 0.
#' @keywords internal
EPR_PRIOR_RATE_CONTEST <- 0.0000

#' Weight of EPR in TORP blend (0.5 = equal blend of EPR + PSR)
#' @keywords internal
TORP_EPR_WEIGHT <- 0.5

#' Whether the EPV position adjustment rescales as well as recentres
#'
#' The position adjustment in \code{create_player_game_data()} historically
#' subtracted a within-position mean and stopped, which fixes positional
#' *level* but leaves positional *spread* untouched. Since key defenders are
#' under-dispersed rather than under-levelled, rescaling is the layer where
#' that defect actually lives. When TRUE the adjustment becomes
#' \code{(p80 - mean_pos) / sd_pos * S * tog}, where \code{S} is the pooled
#' weighted SD for the channel, so overall units are preserved and only
#' between-position spread differences change.
#'
#' Evidence: FABLE-DEFENDER-VALUE-PLAN.md §7.18 — key-defender rating SD
#' 1.40 -> 1.60, max 3.42 -> 4.04, best-forward gap 1.96x -> 1.55x, and the
#' first paired bootstrap CI in that program to exclude zero.
#'
#' Assumption, stated because it is load-bearing: this asserts that every
#' position group *should* have the same spread of player value.
#' @keywords internal
EPV_POSITION_STANDARDISE <- TRUE

#' Centre EPV channels on their listed position's level, per round
#'
#' The positional level correction, at the layer that creates it. Turned on
#' 2026-07-29 after measurement showed the level gap originates at EPV:
#' \code{.position_adjust()} centres every channel to machine-precision zero by
#' \code{lineup_position}, yet on the listed taxonomy everything downstream uses,
#' \code{epv_adj} still spans 2.94 points (key_def -2.17, key_fwd +0.77 on 2026
#' per-game data). Role-centring removes the role effect; the player-type effect
#' survives it.
#'
#' Fixing it here flows to EPR, PSV blending, the match features and
#' \code{get_player_game_ratings()} at once. \code{EPR_POSITION_CENTRE} stays on
#' as a backstop rather than being replaced: \code{.bayesian_shrink()} pulls
#' toward a NON-ZERO \code{prior_rate} (-0.7 / -0.3) by an amount that depends on
#' \code{wt_gms}, so a zero EPV sum does not produce a zero EPR level.
#' @keywords internal
EPV_LEVEL_CENTRE <- TRUE

# ---------------------------------------------------------------------------
# The value/rating split (2026-07-29, Pete's design). All FALSE = production.
#
# The principle: the VALUE layer (EPV, PSV) describes one game, so it centres on
# the WEEKLY role -- he played CHF that day, judge him against the other CHFs
# that day. The RATING layer (EPR, PSR) accumulates across a season, so it takes
# a small SHRUNK adjustment on the STABLE listed position -- a rating must not
# lurch because a player was moved for one week.
#
# Today neither layer does exactly one job: EPV is adjusted TWICE (weekly role,
# then the per-match listing), PSV has no weekly stage at all, and the rating
# layer applies a FULL not shrunk correction. See docs/reference/POSITIONS.md.
#
# Each flag is separately scoreable so the arms can be attributed rather than
# shipped as one lump -- the mistake that let round 2's composite ship with
# individually-null parts.
# ---------------------------------------------------------------------------

#' Group the EPV role adjustment on `lineup_group` instead of raw `lineup_position`
#'
#' `.position_adjust()` groups by all 21 raw slots. Five of those are arbitrary
#' left/right mirrors (BPL/BPR, HBFL/HBFR, WL/WR, HFFL/HFFR, FPL/FPR) -- left
#' wing and right wing are the same job. Grouping them separately forces each
#' side to average zero INDEPENDENTLY, erasing any real difference between them
#' and halving the cell size for nothing. See `LINEUP_GROUP_MAP`.
#'
#' Note this is NOT the granularity question §7.15 closed. That compared a 6-way
#' vs 9-way collapse at the RATING layer and found nothing (Δ +0.013, P 0.417).
#' The value layer collapses nothing today, so it has never been measured.
#'
#' Applies to BOTH value layers -- EPV's `.position_adjust()` grouping and PSV's
#' role-centring pass -- because they must stay built the same way. An earlier
#' draft of this work assumed PSV had no weekly stage at all and tried to add
#' one; it already has one (`psr.R`, the `pos_col` block), keyed on the same raw
#' `lineup_position`. The two value layers are already symmetric; the open
#' question is only whether the mirrors should be merged.
#' @keywords internal
ROLE_USE_LINEUP_GROUP <- FALSE

#' Group PSV's role centring by `(season, round)` rather than pooling history
#'
#' PSV's role stage centres with `by = lineup_position` and NO season/round, so
#' every wing across all six seasons is centred against one pooled mean. That is
#' the same defect fixed for PSR in 2026-07-29's `PSR_CENTRE_BY_ROUND`: each
#' position averages zero across the dataset while any individual round stays
#' skewed, and a 2021 round-1 value is centred using 2026 games -- a backtest
#' leak. Found 2026-07-29 while checking whether PSV had a weekly stage at all.
#' @keywords internal
PSV_ROLE_CENTRE_BY_ROUND <- FALSE

#' Shrink the EPR position adjustment instead of applying it in full
#'
#' `centre_epr_by_position()` subtracts the full cell mean. Under the
#' value/rating split the rating layer should apply a SMALL adjustment on the
#' stable label, not a full one -- the value layer has already removed the role
#' effect, so a full second correction over-fits thin cells.
#'
#' Shrinkage is `n / (n + EPR_POSITION_SHRINK_PRIOR)` on the cell's weight, so a
#' well-populated cell is adjusted nearly in full and a thin one barely at all.
#' FALSE reproduces the full correction exactly.
#' @keywords internal
EPR_POSITION_SHRINK <- FALSE

#' Prior weight for `EPR_POSITION_SHRINK`, in the cell's own weight units
#'
#' A cell carrying this much weight gets half its measured position mean
#' subtracted. Set to 5 on MEASURED cell weights (2026-07-29), not the estimate
#' the first draft used -- that guessed 100-200 TOG-weighted player-games and
#' was wrong by ~3x. Actual distribution: median 43, 5th pctile 18, max 92.
#'
#' What 5 does, on the real distribution:
#'
#'   cell weight | fraction of the position mean subtracted
#'   18 (5th)    | 0.787
#'   43 (median) | 0.896
#'   82 (95th)   | 0.942
#'
#' So a normal cell keeps ~90% of the correction and only 0.6% of cells drop
#' below half. The cells it genuinely bites are the ones that deserve it --
#' 2021 round 1 carries cell weights of 0.7-2.1 because nobody has history yet
#' and pred_tog is near zero, and TODAY those get the FULL correction computed
#' off 0.7 units of evidence. Shrinkage cuts that to ~12%.
#'
#' \strong{FALSE -- shrinkage is right in principle but this is the WRONG
#' LAYER.} Measured 2026-07-29, correction size per (season, round, position)
#' cell:
#'
#'   EPV level centring (PRIMARY)  median 0.526, 90th 1.379, max 4.078
#'   EPR centring       (BACKSTOP) ~0.002
#'
#' The EPR correction is ~250x smaller because EPV_LEVEL_CENTRE has already
#' zeroed the level upstream -- this layer only mops up the residual
#' .bayesian_shrink() reintroduces. Shrinking a correction that is already
#' ~0.002 cannot do any work: enabling it moved published TORP by a mean of
#' 0.0044, left the served-round top 20 in an IDENTICAL order, and moved 2021
#' (the thinnest cells) LEAST of any season -- the opposite of the thin-cell
#' argument used to justify it.
#'
#' It is not free, either: it converts "bucket means are exactly zero" from a
#' checkable invariant into a tolerance judgement, which forced widening the
#' check_position_centring.R section 1 guard. Paying that for a 0.004 effect is
#' a bad trade.
#'
#' \strong{Where it DOES belong is the EPV layer}, where the correction is
#' median 0.526 and up to 4.078, and the thinnest cell carries weight 1.45 --
#' i.e. several points subtracted from every player in a cell on the strength
#' of 1.45 units of evidence. That is the real candidate; see NEXT-STEPS.md.
#'
#' The flag, the sweep and the tests are kept so the EPV version can reuse them.
#'
#' \strong{Not chosen on MAE either way.} A six-point sweep
#' (`ws11_shrink_prior_sweep.R`) put every arm inside the ~0.157 XGBoost noise
#' floor and produced a NON-MONOTONIC MAE curve (-0.162, -0.069, -0.150,
#' -0.122, +0.082, -0.021 for priors 5/10/25/50/100/250). A real effect would
#' show a smooth optimum; a scatter means the differences are noise. Do not
#' quote any of those deltas as a gain.
#' @keywords internal
EPR_POSITION_SHRINK_PRIOR <- 5

#' Shrink the EPV level correction toward the bucket's earlier mean
#'
#' The EPV layer is where the position-level correction is actually large:
#' measured per (season, round, listed bucket) cell on 2021-2026, the total
#' across the four channels has median 1.065 and reaches 8.178, against ~0.002
#' at the EPR layer. It is also where the evidence is thinnest -- and the thin
#' cells are not a smooth tail, they are the finals series and round 0:
#'
#'   season round bucket   players weight correction applied in FULL today
#'   2025   28    key_fwd  4       3.19   -7.046
#'   2023   28    rucks    3       2.14   -5.875
#'   2024   28    key_def  4       3.56   -2.911
#'
#' So a Grand Final key forward has seven points subtracted from him because the
#' other three key forwards on the ground that day happened to average high.
#' Only 22 of 966 cells carry weight below 5, but they are September.
#'
#' \strong{Shrinks toward the bucket's own earlier mean, NOT toward zero.} That
#' distinction is the whole design and it was measured, not assumed
#' (\code{data-raw/04-analysis/measure_epv_shrink_priors.R}, 2026-07-30).
#' Shrinking toward zero -- what the reverted \code{EPR_POSITION_SHRINK} did --
#' withholds correction, and every point withheld is a point of positional level
#' handed back to the published rating. At prior 5 that restored a spread of
#' 0.477 against the 2.94 the v2 level fix removed, and most of it came from
#' NORMAL cells (a median cell keeps only 0.891 of its correction), not from the
#' thin ones the shrinkage was for. Shrinking toward the earlier mean still
#' subtracts a full position level, so nothing is handed back; it only changes
#' WHICH games the level is measured on when the round's own cell is too thin to
#' measure it.
#'
#' The prior mean uses strictly EARLIER (season, round) cells. A whole-season
#' mean is the obvious choice and is wrong for the same reason the correction is
#' grouped per round: it would centre a round-1 value on games not yet played.
#' @keywords internal
EPV_POSITION_SHRINK <- FALSE

#' Which shrinkage rule `EPV_POSITION_SHRINK` uses: `"floor"` or `"prior"`
#'
#' \code{"prior"} is the smooth Bayesian blend, `lambda = wt / (wt + prior)`.
#' \strong{It FAILED its match-MAE gate on 2026-07-30} (`ws12_epv_shrink_prior.R`:
#' ΔMAE +0.232 / −0.051 / +0.090 at priors 1/2/5, non-monotonic, every arm inside
#' the ~0.157 noise floor, and every arm costing bits). The diagnosis is the
#' useful part: at prior 2 the blend moved \strong{55,758 of 56,162 rows}. It is
#' not a thin-cell fix -- it dilutes every normal cell by ~4.7% (lambda 0.953 at
#' the median cell weight) in order to reach the 22 cells that need it, so the
#' gate was measuring the dilution rather than the fix.
#'
#' \code{"floor"} is the answer to that: `lambda = min(1, wt / floor)`. Cells at
#' or above the floor are \strong{bit-identical to production}, and only cells
#' below it ramp toward their bucket's earlier mean. The change is then confined
#' to cells carrying ~0.5% of total weight -- i.e. the finals series and round 0 --
#' so match MAE cannot see it either way and it stands or falls on whether a Grand
#' Final key forward should be centred against the three other key forwards who
#' happened to play that day.
#'
#' Continuous at the boundary by construction (`lambda(floor) = 1`), so there is
#' no cliff between a cell just under and just over the threshold.
#' @keywords internal
EPV_POSITION_SHRINK_RULE <- "floor"

#' Cell weight at which `"floor"` shrinkage stops biting, in TOG-weighted games
#'
#' Set to 8 against the measured distribution: cell weights are min 1.45, 1st
#' pctile 3.57, \strong{5th pctile 8.17}, median 40.91. So a floor of 8 ramps
#' roughly the thinnest 5% of cells and leaves the other 95% untouched, which is
#' the region the finals cells occupy (2025 r28 key_fwd carries 3.19, giving
#' lambda 0.40; 2024 r28 rucks carries 1.45, giving 0.18).
#' @keywords internal
EPV_POSITION_SHRINK_FLOOR <- 8

#' Prior weight for `EPV_POSITION_SHRINK` under the `"prior"` rule
#'
#' A cell carrying this much time-on-ground splits its correction half from its
#' own round and half from the bucket's earlier history. \strong{Not comparable
#' to `EPR_POSITION_SHRINK_PRIOR`} -- that one is in accumulated rating weight.
#'
#' Measured EPV cell weights (2021-2026, 966 cells): min 1.45, 1st pctile 3.57,
#' 5th 8.17, median 40.91, max 89.87. What a prior does at those weights:
#'
#'   prior | weight 3.2 (a GF cell) | weight 40.9 (median round) |
#'   1     | 0.762                  | 0.976                      |
#'   2     | 0.615                  | 0.953                      |
#'   5     | 0.390                  | 0.891                      |
#'
#' Unlike the toward-zero version, a prior that touches normal cells is not
#' automatically a level cost here -- it just measures a normal cell's level
#' partly on earlier rounds. But it is still dilution, so prefer the smallest
#' prior that meaningfully damps a finals cell.
#' @keywords internal
EPV_POSITION_SHRINK_PRIOR <- 2

#' EPV channel stems the level centring applies to
#'
#' All four, unlike \code{EPV_STANDARDISE_CHANNELS} which excludes hitout --
#' dividing by a near-zero within-position SD amplifies without bound, but
#' subtracting a mean is safe at any spread.
#' @keywords internal
EPV_LEVEL_CENTRE_CHANNELS <- c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout")

#' Centre each EPR channel on its position's TOG-weighted mean
#'
#' \code{EPV_POSITION_STANDARDISE} equalises between-position *spread*, at the
#' player-game level, grouped by \code{lineup_position}. It does not equalise
#' *level*, and the correction it does make does not survive to the published
#' rating: the TOG weighting, opponent adjustment, decay and global prior that
#' follow are all position-blind. Read on the listed-position taxonomy, key
#' defenders still sat at median EPR -2.18 against medium forwards' +0.66
#' (FABLE-DEFENDER-VALUE-PLAN.md §8.2).
#'
#' This closes that gap directly, on the published rating, per channel, keyed on
#' \code{position_group} within each \code{(season, round)} cross-section --
#' collapsed first onto \code{MATCH_LISTED_POS_MAP}'s 6 buckets, the same
#' taxonomy the match model's position features use.
#'
#' \strong{This is a NORMALISATION, not a measurement.} Position levels are
#' unidentifiable from match margins -- on-field structure is rigid (every team
#' fields exactly one full-back), and although listed-position counts do vary
#' (2-9 midfielders), holding total EPR constant the positional mix explains
#' nothing: F(5, 1113) = 0.47, p = 0.80, every CI spanning about +/-3 points
#' (§8.3). Setting each position's mean to zero therefore ASSERTS that an
#' average key defender and an average midfielder contribute equally. That claim
#' cannot be checked against results.
#'
#' It is preferred anyway because the status quo embeds an assumption too --
#' that the uncentred levels are right -- and those levels are an artefact of
#' the pipeline rather than a value judgement. A deliberate, symmetric
#' assumption beats an accidental, asymmetric one.
#'
#' Cost: measured, and near zero. Alone it is dMAE +0.121, 95% CI
#' `[-0.250, +0.485]`; paired with the position-split match features it enables
#' (\code{MATCH_LISTED_POS_MAP} diffs) it is dMAE -0.026, CI `[-0.413, +0.358]` --
#' MAE-neutral (WS4/WS5, 2025-26 pooled, 387 games).
#' @keywords internal
EPR_POSITION_CENTRE <- TRUE

#' EPR channels the position centring applies to
#'
#' All four. Unlike the standardise step -- which divides by a within-position
#' SD and so had to exclude the ruck-exclusive \code{hitout} channel to avoid
#' amplifying near-zero outfield spreads -- centring only subtracts a mean, so
#' no channel can blow up and none needs excluding.
#' @keywords internal
EPR_CENTRE_CHANNELS <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")

#' Centre PSV on its listed position's level, per round
#'
#' The PSV-side twin of \code{EPV_LEVEL_CENTRE}, and the same finding one layer
#' across. \code{calculate_psv()} already centres to machine precision by
#' \code{lineup_position} (1.7e-15 across all 20 roles), but on the listed
#' taxonomy that PSR, TORP and the match model use, 2026 \code{psv_p80} still
#' spans 0.812 (key_fwd +0.517, med_def -0.295).
#'
#' Turned on 2026-07-29, immediately after the EPV fix, because fixing EPV alone
#' left per-game \code{torp_value = 0.5*epv + 0.5*psv} carrying ~0.41 of
#' positional bias sourced entirely from PSV. The two are NOT the same shape:
#' EPV penalised key defenders hardest, PSV penalises medium defenders and
#' rewards key forwards, so correcting one and not the other skews the blend
#' rather than halving the error.
#'
#' PSR's own centring stays on as the backstop, exactly as
#' \code{EPR_POSITION_CENTRE} does for EPR.
#' @keywords internal
PSV_LEVEL_CENTRE <- TRUE
#' Centre PSR within each (season, round) rather than pooled over all history
#'
#' PSR's position centring was grouped by position ALONE, so every position
#' averaged zero across the whole dataset while any individual round stayed
#' skewed -- on the served round, rucks sat +0.451 and key forwards -0.030, a
#' 0.481 spread that TORP inherited half of. Pooling is also a backtest leak: a
#' 2021 round-1 rating centred using games from 2026.
#'
#' Exposed as a flag rather than hardcoded so the old behaviour can be scored on
#' the match harness from the PRODUCTION code path, instead of a replica of it.
#' Every gate we have asks "is the new arm better"; none asks "is the arm I
#' scored the thing production runs", and a hand-rolled reconstruction of the
#' old centring is exactly where that gap opens.
#' @keywords internal
PSR_CENTRE_BY_ROUND <- TRUE

#' Centre PSR on the LISTED position taxonomy
#'
#' TRUE centres PSR on `.collapse_listed_position()` of a player's listed
#' position -- the same key EPV, EPR and PSV use -- instead of the stat-ratings
#' frame's `pos_group`, which is a PBP-derived PLAYSTYLE label.
#'
#' The two are different facts, not different vintages of one fact: the mappers
#' were verified to produce an identical partition of the same seven raw labels,
#' so the disagreement is entirely in the source data. They differ for 13.2% of
#' player-rounds overall and 20.3% in 2026. Centring on playstyle while TORP
#' reads the result under listed put ~0.30 of positional level back into TORP,
#' undoing half of what the 2026-07-29 program removed at EPV/EPR. 2021, where
#' the labels agree 100%, showed exactly 0.000 spread.
#'
#' **FALSE, and it must stay FALSE until something overturns the measurement.**
#' The listed arm was scored on 2026-07-29 (`ws9_psr_listed_centring.R`) and
#' REGRESSED: MAE 25.4335 -> 25.8152, dMAE +0.382 `[+0.081, +0.684]`, CI
#' entirely above zero. That is a real cost, not a null.
#'
#' Why it lost, most likely: the career-modal label PSR used is the most STABLE
#' of the three resolutions, and a rating's position key wants stability. Moving
#' it to the season listing made it lurch more, and centring PSR on the listed
#' label while EPV stayed on the per-match one made the two halves of TORP
#' agree LESS, not more.
#'
#' The positional level this was meant to remove (~0.30 in TORP) is real and
#' still there. The fix is the value/rating split in NEXT-STEPS.md, not this
#' flag. Setting this TRUE ships a measured regression.
#' @keywords internal
PSR_CENTRE_ON_LISTED <- FALSE

#' Whether PSR position centring rescales as well as recentres
#'
#' The mirror of \code{EPV_POSITION_STANDARDISE} on the PSR side.
#' \code{calculate_psr()} historically subtracted a positional mean and stopped,
#' so PSR carried the same under-dispersion defect as EPV.
#'
#' This is load-bearing and was missed once: FABLE-DEFENDER-VALUE-PLAN §7.18
#' scored its recommended arm with PSR standardised, but the first
#' implementation shipped only the EPV half plus weekly PSR centring — a
#' configuration that had never been scored. It left key-defender max at 3.74
#' against the 4.06 the scored arm produced, and the best-forward gap at 1.80×
#' against 1.52× (§7.24).
#'
#' Same degenerate-SD guard as the EPV side: a group whose within-position SD
#' is absent or ~zero falls back to centre-only rather than dividing by it.
#' @keywords internal
PSR_POSITION_STANDARDISE <- TRUE

#' Rating vintage produced by the current constants
#'
#' Bumped whenever a change alters historical ratings. Per decision D-DEF3 a
#' new vintage is published *alongside* the canonical one rather than
#' overwriting it, so a published number can always be traced to the definition
#' that produced it. See \code{docs/plans/RATING-VERSIONING-PLAN.md}.
#'
#' \code{"v1"} is every rating published before 2026-07-27. \code{"v2"} adds the
#' EPV position-variance standardisation and PSR standardisation.
#'
#' **Weekly PSR centring is NOT part of what v2 actually shipped**, despite
#' earlier descriptions of v2 (including this docstring and the published
#' manifest) saying so. The code is wired — \code{calculate_psr()} prefers
#' \code{lineup_pos_group} when present — but the released
#' \code{player_stat_ratings} frame carries no \code{lineup_position} column, so
#' the preference has never resolved and PSR still centres on the
#' season-constant \code{pos_group}. It activates by itself once the
#' \code{06-stat-ratings} pipeline joins \code{lineup_position} upstream, and
#' that will change historical ratings — so it needs its own vintage bump when
#' it does, not a silent activation under the v2 label.
#'
#' Likewise the corrected \code{LINEUP_POSITION_GROUP_MAP} has a narrower blast
#' radius than "v2 adds the corrected taxonomy" implies: EPV standardisation
#' groups on the raw 20-way \code{lineup_position}, not through the 6-way map,
#' so the correction reaches only \code{pos_group} derivations.
#' @keywords internal
RATING_VINTAGE <- "v2"

#' Map from the 20-way team-sheet lineup position to a 6-way position group
#'
#' Corrected 2026-07-27 after an audit of all 18 on-field codes against player
#' height, the clubs' listed positions, PBP-derived position groups and each
#' code's on-field statistical profile
#' (\code{data-raw/04-analysis/lineup_position_taxonomy_audit.R},
#' \code{position_source_provenance.R}). Three of the previous assignments were
#' contradicted by every source:
#'
#' \itemize{
#'   \item \code{CHF} was MEDIUM_FORWARD. A centre half forward averages 190.8cm
#'     and his club lists him KEY_FORWARD; PBP disagreed with the old mapping
#'     67% of the time. Now KEY_FORWARD.
#'   \item \code{FPL}/\code{FPR} were KEY_FORWARD. The forward pockets average
#'     187.0-187.3cm and are listed MEDIUM_FORWARD; PBP disagreed 72% and 69% of
#'     the time — the highest rates in the table. Now MEDIUM_FORWARD.
#'   \item \code{CHB} was MEDIUM_DEFENDER. This one is genuinely ambiguous —
#'     play profile places a CHB nearer the back pockets than the full-back,
#'     but the two central defensive posts are a coherent role pairing and the
#'     calibration evidence cannot separate the options (§7.15b). Grouped with
#'     FB as a football judgement, flagged as the taxonomy's softest call.
#' }
#'
#' Bench codes map to NA so they fall through to the modal-position resolution
#' in \code{.resolve_stat_rating_positions()}.
#' @keywords internal
LINEUP_POSITION_GROUP_MAP <- c(
  FB   = "KEY_DEFENDER",  CHB  = "KEY_DEFENDER",
  BPL  = "MEDIUM_DEFENDER", BPR  = "MEDIUM_DEFENDER",
  HBFL = "MEDIUM_DEFENDER", HBFR = "MEDIUM_DEFENDER",
  C    = "MIDFIELDER", WL   = "MIDFIELDER", WR   = "MIDFIELDER",
  R    = "MIDFIELDER", RR   = "MIDFIELDER",
  RK   = "RUCK",
  FF   = "KEY_FORWARD", CHF  = "KEY_FORWARD",
  FPL  = "MEDIUM_FORWARD", FPR  = "MEDIUM_FORWARD",
  HFFL = "MEDIUM_FORWARD", HFFR = "MEDIUM_FORWARD",
  INT  = NA_character_, SUB  = NA_character_, EMERG = NA_character_
)

#' Centre the hitout channel on ruck involvement rather than position
#'
#' \strong{Scheme C.} A channel's centring cell should be based on
#' PARTICIPATION IN THAT CHANNEL, not on a position label. The hitout channel
#' only exists for players who ruck, so celling it on where a player lined up
#' compares a part-time ruck with people who never contest a bounce.
#'
#' Measured 2026-08-06 on Grundy / Gawn / Cox, same method, three cells:
#' \tabular{lrrr}{
#'   \strong{scheme} \tab \strong{Grundy} \tab \strong{Gawn} \tab \strong{Cox} \cr
#'   lineup slot (current) \tab 1.310 \tab 0.588 \tab \strong{1.920} \cr
#'   listed position_group \tab 1.636 \tab 0.925 \tab -0.624 \cr
#'   \strong{ruck involvement} \tab \strong{2.668} \tab 1.994 \tab 0.103 \cr
#' }
#'
#' Listed position also fixes these three, but \strong{11 of the 44 players
#' averaging 15+ ruck contests a game are not listed as rucks} -- Rory Lobb
#' (KEY_DEFENDER, 29.2 a game), Mark Blicavs (MIDFIELDER, 23.9) -- so it leaves
#' them centred against defenders and midfielders. Involvement fixes those too.
#' @keywords internal
EPV_HITOUT_CENTRE_ON_RUCK <- FALSE

#' Ruck contests per game that count as "this player rucks"
#'
#' Splits the hitout centring into two cells rather than many. 10 is a
#' deliberately low bar: the point is to separate players who contest bounces at
#' all from those who never do, not to grade rucks against each other -- that is
#' what the centring itself does.
#' @keywords internal
EPV_RUCK_INVOLVEMENT_MIN <- 10

#' Width of the ramp between the two hitout cells
#'
#' \strong{0 keeps the hard threshold} at \code{EPV_RUCK_INVOLVEMENT_MIN},
#' which is the behaviour measured on 2026-08-06. A positive width blends the
#' two cell means instead of switching between them, ramping linearly from
#' \code{MIN - width/2} to \code{MIN + width/2}.
#'
#' \strong{Why.} A threshold puts a cliff exactly where players are densest.
#' Measured: mean \code{epv_hitout_adj} is \strong{+0.437} at nine contests a
#' game and \strong{−2.164} at ten — a swing of 2.6 for one extra contest.
#'
#' The two continuous alternatives both removed the cliff and cost the channel
#' its link to production, because they conditioned on a variable correlated
#' with output (contests, or share of contests) and so conditioned the output
#' away. Blending does not: the reference is still just two fixed cell means, and
#' only the WEIGHT between them varies. A partial ruck gets a partial cell, which
#' is the honest description of a ruck-forward and the thing every scheme so far
#' has failed to express.
#' @keywords internal
EPV_RUCK_BLEND_WIDTH <- 0

#' Remap bench starting slots to the role a player actually filled
#'
#' \code{lineup_position} is where a player STARTED, not what he did.
#' \code{INT} is not a position, and using it as a centring cell compares a
#' bench-starting specialist with the bench.
#'
#' Measured 2026-08-06 (\code{docs/reviews/INT-CENTRING-BUG-2026-08-06.md}):
#' the \code{INT} cell has a mean per-80 hitout of 0.378 against \code{RK}'s
#' 5.158. Sean Darcy rucks at 5.44 per 80 -- a normal ruck rate, near Max Gawn's
#' 5.96 -- but measured against the bench he lands 4.1 standard deviations above
#' the mean where Gawn lands 0.43 above his. Production has Darcy's
#' \code{epr_hitout} at 1.018 and Gawn's at -0.152 as a direct result.
#'
#' 1,798 interchange player-games in 2026, roughly a quarter of the total, sit in
#' that cell. Any bench-starting specialist is inflated the same way.
#'
#' \strong{This is step ONE only.} \code{centre_epv_by_position()} already
#' centres on the listed position, but it subtracts a MEAN -- a level shift
#' cannot undo a within-group ordering error created by standardising against
#' the wrong cell's sd. The two steps are independent and only this one is wrong.
#' @keywords internal
ROLE_REMAP_BENCH <- FALSE

#' Starting slots that are not roles
#' @keywords internal
ROLE_BENCH_SLOTS <- c("INT", "SUB", "EMERG")

#' Last-resort slot for a player with no non-bench game on record
#'
#' Only reached when a player has never started on the ground in any season. It
#' collapses such players onto one slot per listed position, which is its own
#' small distortion -- so the count reaching this tier is reported rather than
#' hidden.
#' @keywords internal
ROLE_FALLBACK_SLOT <- c(
  KEY_DEFENDER = "FB", MEDIUM_DEFENDER = "BPL", MIDFIELDER = "C",
  MIDFIELDER_FORWARD = "HFFL", MEDIUM_FORWARD = "HFFL",
  KEY_FORWARD = "FF", RUCK = "RK"
)

#' Lineup group: `lineup_position` with the left/right mirrors merged
#'
#' The middle rung between `lineup_position` (21 raw slots) and
#' `LINEUP_POSITION_GROUP_MAP` (6 broad groups). Five of the 21 slots are
#' arbitrary left/right mirrors of the same job -- BPL/BPR, HBFL/HBFR, WL/WR,
#' HFFL/HFFR, FPL/FPR. Left wing and right wing are the same role, and which
#' one a player is named in carries no information.
#'
#' Why merging them is the right default for centring: grouping by raw
#' `lineup_position` forces the left and right wing to average zero
#' INDEPENDENTLY. That silently erases any real difference between the two
#' sides instead of treating them as one role, and halves the cell size for
#' nothing. Merging gives 13 on-field groups.
#'
#' INT / SUB / EMERG are KEPT as their own groups here, unlike
#' `LINEUP_POSITION_GROUP_MAP` which sends them to NA. `INT` is the single most
#' common value in the teams data (10,305 rows vs ~2,468 for each on-field
#' slot), and `.position_adjust()` currently centres interchange players against
#' each other by grouping on raw `lineup_position`. Mapping them to NA would
#' change that behaviour as a side effect of a naming change, which is exactly
#' the kind of silent shift that has cost this repo real MAE.
#'
#' 21 slots -> 16 groups (13 on-field + INT + SUB + EMERG).
#' @keywords internal
LINEUP_GROUP_MAP <- c(
  FB   = "FB",
  BPL  = "BP",   BPR  = "BP",
  CHB  = "CHB",
  HBFL = "HBF",  HBFR = "HBF",
  C    = "C",
  WL   = "W",    WR   = "W",
  R    = "R",
  RR   = "RR",
  RK   = "RK",
  CHF  = "CHF",
  HFFL = "HFF",  HFFR = "HFF",
  FF   = "FF",
  FPL  = "FP",   FPR  = "FP",
  INT  = "INT",  SUB  = "SUB", EMERG = "EMERG"
)

#' Collapse `lineup_position` to the mirror-merged lineup group
#'
#' Mirrors \code{.collapse_listed_position()}: one place where a raw slot
#' becomes a group, and a loud warning for anything unmapped rather than a
#' silent NA. An unmapped slot means the AFL added or renamed a position, and
#' the rows would otherwise drop out of centring without anyone noticing.
#'
#' @param x Character vector of `lineup_position` values.
#' @return Character vector of lineup groups; NA for unmapped input.
#' @keywords internal
.collapse_lineup_group <- function(x) {
  xc <- as.character(x)
  unmapped <- setdiff(unique(xc[!is.na(xc)]), names(LINEUP_GROUP_MAP))
  if (length(unmapped) > 0) {
    # No bare `{?it/them}` on the second line: cli resolves a plural against a
    # quantity in the SAME string, and that line has none, so it errors rather
    # than pluralising. Third time this shape has bitten today -- see also the
    # `{.checked}` dot-prefix crash that took down the ratings pipeline.
    # alert_danger, not warn: an unmapped slot drops those rows out of centring
    # entirely, and a deferred warning is the reporting path that lost the
    # 2026-07-29 CSV divergence. Latent today (the flag defaults FALSE) -- so
    # was that bug, until it wasn't.
    cli::cli_alert_danger(
      "{length(unmapped)} unmapped lineup_position value{?s} left UNCENTRED: {paste(unmapped, collapse = ', ')}")
    cli::cli_warn(c(
      "{length(unmapped)} unmapped {.field lineup_position} value{?s}: {.val {unmapped}}",
      "i" = "Add them to {.code LINEUP_GROUP_MAP} -- unmapped rows are left uncentred."
    ))
  }
  unname(LINEUP_GROUP_MAP[xc])
}

#' EPV channels the position adjustment rescales (see EPV_POSITION_STANDARDISE)
#'
#' \code{hitout} is deliberately excluded. Standardising divides by the
#' within-position SD, which is only meaningful for a channel every position
#' participates in. Hitouts are ruck-exclusive: outfield positions carry hitout
#' SD 0.14-0.32 against a pooled SD of 1.241, so rescaling that channel
#' amplifies an outfield player's hitout deviation 4-9x (and 1.24 million-fold
#' for EMERG, where the within-position SD is exactly zero). In testing this
#' put a ruck named at nine different lineup positions into the overall top 10
#' at 4.06 against his true 1.12. Excluding the channel scores strictly better
#' than capping the amplifier (§7.18c).
#'
#' \strong{Under v3 the contest channel is excluded for the same reason}, and it
#' took a measurement to see it. Under \code{EPV3_CHANNELS = 3L} the
#' \code{spoil} slot holds aerial contest value MERGED with stoppage value, so
#' it is partly ruck-exclusive -- exactly the shape that made \code{hitout}
#' unstandardisable. Standardising it normalises away the only thing it
#' measures: with the two contest parts merged in points, the channel came out
#' at 0.023 points per unit, t 0.27, carrying \strong{0.0\%} of the rating's
#' variance. It had been standardised out of existence. Excluding it takes the
#' contest channel from t 1.83 to 2.24 and from 1.7\% of value to 2.6\%.
#'
#' Keyed on \code{EPV_ENGINE} rather than on \code{EPV3_CHANNELS} because v2's
#' \code{spoil} channel is a genuine all-position quantity (spoils, tackles,
#' pressure acts) that should keep being standardised.
#' @keywords internal
EPV_STANDARDISE_CHANNELS <- if (identical(EPV_ENGINE, "v3")) {
  c("recv", "disp")
} else {
  c("recv", "disp", "spoil")
}

#' PSR prior rate for replacement-level players
#'
#' Players without enough skill history to compute PSR are assigned this
#' replacement-level value. Based on empirical mean PSR of players with
#' 1-10 career games (~-1.5 to -2.0).
#' @keywords internal
PSR_PRIOR_RATE <- -2

# EPV (Expected Possession Value) Assignment Constants
# ----------------------------------------------------

#' Bounce weight in disposal credit
#' @keywords internal
EPV_BOUNCE_WT <- 0.0000

#' Disposal EPV offset when defending (pos_team == -1)
#' @keywords internal
EPV_DISP_NEG_OFFSET <- 0.0000

#' Disposal EPV offset when possessing (pos_team == 1)
#' @keywords internal
EPV_DISP_POS_OFFSET <- 0.0000

#' Disposal scaling factor
#' @keywords internal
EPV_DISP_SCALE <- 0.5000

#' Reception multiplier when defending (pos_team == -1)
#'
#' \strong{Stays at 1.0. Setting it to 0 was tried on 2026-08-04 and reverted
#' the same session} -- the case for 0 was real but incomplete, and the thing it
#' missed is worth recording.
#'
#' The multiplier sits on the \code{pos_team == -1} branch, so it is the
#' INTERCEPT branch. The case for zeroing it was that the term is noise:
#' game-to-game reception reliability rises 0.2507 -> 0.3086 (+23\%) while
#' count-dependence FALLS 0.53 -> 0.297.
#'
#' \strong{What that missed is what the term contains.} Interception is the
#' highest-value receiving act in the game by a factor of eight -- 168 per match
#' at mean +0.625 against ordinary receptions' 1,453 per match at +0.079. It is
#' 10\% of receiving events, 31\% of gross reception value and roughly half the
#' channel's net (\code{docs/reference/EPV-VALUE-ANATOMY.md} §4). So "drop a
#' noisy term" was actually "stop crediting interceptions".
#'
#' Three measurements, all against (\code{epv3_recv_neg_reexamine.R}):
#' \itemize{
#'   \item \strong{Conservation breaks.} Total EPV allocated per team-match falls
#'     +72.26 -> +38.69. The disposer is still CHARGED for the turnover and
#'     nobody is CREDITED the gain, so the metric systematically under-rates
#'     whoever does the gaining.
#'   \item \strong{The margin fit is worse}, recv t 9.65 -> 6.76.
#'   \item \strong{The two reliability measures disagree and the deciding one
#'     says no.} Game-to-game reliability rises, which removing any volatile term
#'     will do; YEAR-OVER-YEAR repeatability falls 0.6804 -> 0.6533. Persistence
#'     is the test that separates ability from noise, and it says signal was
#'     deleted. Named intercept defenders bear it out -- Tom Stewart 3.18 ->
#'     1.56, Jeremy Howe 4.14 -> 3.80, Steven May 3.54 -> 3.33.
#' }
#'
#' \strong{Also read the formula before assuming what this touches.} The
#' multiplier sits inside BOTH defensive branches, so at 0 the intercept-MARK
#' branch is zeroed as well and \code{EPV_RECV_INTERCEPT_MARK_SCALE} multiplies
#' a term that is now zero.
#' @keywords internal
EPV_RECV_NEG_MULT <- 1.0000

#' Reception offset when defending
#' @keywords internal
EPV_RECV_NEG_OFFSET <- 0.0000

#' Reception multiplier when possessing (pos_team == 1)
#' @keywords internal
EPV_RECV_POS_MULT <- 1.0000

#' Reception offset when possessing
#' @keywords internal
EPV_RECV_POS_OFFSET <- 0.0000

#' Reception scaling factor
#' @keywords internal
EPV_RECV_SCALE <- 0.5000

#' Reception scaling factor for intercept marks (pos_team == -1 AND mark in PBP)
#' @keywords internal
EPV_RECV_INTERCEPT_MARK_SCALE <- 1.0000

#' Penalty scale for failed aerial contest receptions (target who lost)
#' Applied per failed contest; negative credit = share of kicker's lost EPV
#' @keywords internal
EPV_RECV_FAILED_CONTEST_WT <- -0.3000

#' Spoil weight per spoil
#' @keywords internal
EPV_SPOIL_WT <- 0.0737

#' Tackle weight per tackle
#' @keywords internal
EPV_TACKLE_WT <- 0.2980

#' Pressure act weight
#' @keywords internal
EPV_PRESSURE_WT <- -0.0024

#' Defensive half pressure act weight (spoil component)
#' @keywords internal
EPV_DEF_PRESSURE_WT <- -0.1882

#' Hitout weight per hitout
#' @keywords internal
EPV_HITOUT_WT <- 0.0510 * EPV_RUCK_SWING_SCALE

#' Hitout to advantage weight
#' @keywords internal
EPV_HITOUT_ADV_WT <- 0.1748 * EPV_RUCK_SWING_SCALE

#' Ruck contest weight (hitout component)
#' @keywords internal
EPV_RUCK_CONTEST_WT <- 0.0232 * EPV_RUCK_SWING_SCALE

#' Make v3's stoppage channel zero-sum instead of pay-to-attend
#'
#' \strong{FALSE = v2 behaviour, i.e. inert.} Only read when
#' \code{EPV_ENGINE == "v3"}.
#'
#' Every other v3 channel is credit/debit: somebody gains and somebody sheds the
#' same amount. \code{cont_stop} is not. It pays
#' \code{hitouts * 0.0510 + hitouts_to_advantage * 0.1748 + ruck_contests * 0.0232},
#' and because \code{EPV_RUCK_CONTEST_WT} is POSITIVE a ruckman is paid 0.0232
#' for every contest he \emph{attends}, including the ones he loses. That is the
#' one place v3's own logic does not hold.
#'
#' A ruck contest has exactly two rucks and \code{hitouts} counts the ones this
#' ruck won, so \code{ruck_contests - hitouts} is how many he LOST. Turning that
#' term into a debit makes the channel behave like the other three without
#' leaving the hitout carve-out -- it is the same three box fields, read as a
#' win/loss ledger rather than an attendance count.
#'
#' \strong{ON since 2026-08-04.} Gated on channel quality rather than on match
#' MAE, which cannot see it -- the match model consumes every channel diff
#' separately and reweights them itself, so a cleaner channel carrying the same
#' information is absorbed.
#'
#' What it buys, against the attendance-count version:
#' \itemize{
#'   \item \strong{count-dependence falls}, \code{cor(contest, hitouts)} 0.370 ->
#'     0.252. This is the point of it: a COUNT can be padded by volume, a
#'     DIFFERENTIAL cannot, and that is what makes the channel safe to scale.
#'   \item year-over-year repeatability holds, 0.789 -> 0.792.
#'   \item two rucks in the 2026 top 40 rather than one.
#' }
#'
#' \strong{The reason for turning it on is not the reason previously written
#' here.} That said it would let the channel be amplified; measurement killed the
#' amplification instead (see \code{EPV_RUCK_SWING_SCALE} -- its justifying swing
#' figure is ~93\% centre-bounce reset artifact, and the regression route is
#' ~40\% team quality). The ledger stands on its own: paying a ruckman for
#' contests he LOST is indefensible whatever the channel is worth.
#' @keywords internal
EPV3_STOP_ZERO_SUM <- TRUE

#' Debit per ruck contest lost, under \code{EPV3_STOP_ZERO_SUM}
#'
#' Defaults to the magnitude of \code{EPV_RUCK_CONTEST_WT} so that attending and
#' splitting contests 50/50 is worth zero rather than positive. Kept separate so
#' the win and loss sides can be gated apart if the symmetric version fails.
#' @keywords internal
EPV_RUCK_LOSS_WT <- 0.0232 * EPV_RUCK_SWING_SCALE

#' Contested possessions weight (recv component)
#' @keywords internal
EPV_CONTESTED_POSS_WT <- 0.1642

#' Contested marks weight (recv component)
#' @keywords internal
EPV_CONTESTED_MARKS_WT <- 0.0259

#' Ground ball gets weight (recv component)
#' @keywords internal
EPV_GROUND_BALL_GETS_WT <- 0.2165

#' Marks inside 50 weight (recv component)
#' @keywords internal
EPV_MARKS_INSIDE50_WT <- 0.3464

#' Inside 50s weight (disp component)
#' @keywords internal
EPV_INSIDE50S_WT <- 0.2429

#' Clangers weight (disp component)
#' @keywords internal
EPV_CLANGERS_WT <- -0.0094

#' Score involvements weight (disp component)
#' @keywords internal
EPV_SCORE_INVOLVEMENTS_WT <- 0.2916

#' Intercepts weight (spoil component)
#' @keywords internal
EPV_INTERCEPTS_WT <- 0.0166

#' One percenters weight (spoil component)
#' @keywords internal
EPV_ONE_PERCENTERS_WT <- 0.1260

#' Rebound 50s weight (spoil component)
#' @keywords internal
EPV_REBOUND50S_WT <- -0.1763

#' Frees against weight (spoil component)
#' @keywords internal
EPV_FREES_AGAINST_WT <- 0.0428

#' Frees for weight (recv component)
#' @keywords internal
EPV_FREES_FOR_WT <- 0.2331

#' Goals weight (disp component)
#' @keywords internal
EPV_GOALS_WT <- 0.4262

#' Behinds weight (disp component)
#' @keywords internal
EPV_BEHINDS_WT <- 1.0899

#' Total marks weight (recv component)
#' @keywords internal
EPV_MARKS_WT <- 0.0160

#' Uncontested possessions weight (recv component)
#' @keywords internal
EPV_UNCONTESTED_POSS_WT <- 0.0344

#' Shots at goal weight (disp component)
#' @keywords internal
EPV_SHOTS_AT_GOAL_WT <- 0.4419

#' Kicks weight (disp component)
#' @keywords internal
EPV_KICKS_WT <- 0.0680

#' Handballs weight (disp component)
#' @keywords internal
EPV_HANDBALLS_WT <- 0.0629

#' Metres gained weight (disp component)
#' @keywords internal
EPV_METRES_GAINED_WT <- 0.0010

#' Turnovers weight (disp component)
#' @keywords internal
EPV_TURNOVERS_WT <- -0.0856

#' Goal assists weight (disp component)
#' @keywords internal
EPV_GOAL_ASSISTS_WT <- 0.2240

#' L2 (ridge) regularization lambda for count-based stat weights
#' @keywords internal
STAT_WEIGHT_LAMBDA <- 0.5


# WP Credit Constants
# -------------------

#' Default disposer share of WPA in win probability credit assignment
#' @keywords internal
WP_CREDIT_DISP_SHARE <- 0.5


# Stat Rating Estimation Constants
# --------------------------------

#' Default exponential decay rate for rate stats (per day)
#' Half-life = ln(2) / 0.0019 ~ 365 days
#' @keywords internal
STAT_RATING_LAMBDA_RATE_DEFAULT <- 0.0019

#' Default exponential decay rate for efficiency stats (per day)
#' Half-life = ln(2) / 0.0013 ~ 533 days
#' @keywords internal
STAT_RATING_LAMBDA_EFFICIENCY_DEFAULT <- 0.0013

#' Prior pseudo-games for Gamma-Poisson rate stats
#' @keywords internal
STAT_RATING_PRIOR_GAMES_DEFAULT <- 5

#' Prior pseudo-attempts for Beta-Binomial efficiency stats
#' @keywords internal
STAT_RATING_PRIOR_ATTEMPTS_DEFAULT <- 30

#' Minimum weighted games for a player to appear in stat rating output
#' @keywords internal
STAT_RATING_MIN_GAMES <- 0

#' Credible interval width (0.80 = 80% CI)
#' @keywords internal
STAT_RATING_CREDIBLE_LEVEL <- 0.80

# Backward compatibility aliases
#' @rdname STAT_RATING_LAMBDA_RATE_DEFAULT
#' @keywords internal
SKILL_LAMBDA_RATE_DEFAULT <- STAT_RATING_LAMBDA_RATE_DEFAULT

#' @rdname STAT_RATING_LAMBDA_EFFICIENCY_DEFAULT
#' @keywords internal
SKILL_LAMBDA_EFFICIENCY_DEFAULT <- STAT_RATING_LAMBDA_EFFICIENCY_DEFAULT

#' @rdname STAT_RATING_PRIOR_GAMES_DEFAULT
#' @keywords internal
SKILL_PRIOR_GAMES_DEFAULT <- STAT_RATING_PRIOR_GAMES_DEFAULT

#' @rdname STAT_RATING_PRIOR_ATTEMPTS_DEFAULT
#' @keywords internal
SKILL_PRIOR_ATTEMPTS_DEFAULT <- STAT_RATING_PRIOR_ATTEMPTS_DEFAULT

#' @rdname STAT_RATING_MIN_GAMES
#' @keywords internal
SKILL_MIN_GAMES <- STAT_RATING_MIN_GAMES

#' @rdname STAT_RATING_CREDIBLE_LEVEL
#' @keywords internal
SKILL_CREDIBLE_LEVEL <- STAT_RATING_CREDIBLE_LEVEL


# Position-Based TOG Constants
# ----------------------------

#' Average time-on-ground fraction by lineup_position (from load_teams())
#' Computed from historical data (2021-2025). Used to estimate per-player TOG
#' when lineups are announced but games haven't started.
#' `SUB = 0.33` is LIVE as of 2026-07-27: the team-rating build now keeps the
#' medical sub (previously filtered out, so this entry did nothing), and 0.33
#' matches his measured 32.5-32.7% TOG through 2025. From 2026 the AFL codes
#' that player `INT` instead, so he is weighted 0.73 — higher, but his measured
#' TOG also rises to ~55.6%, so it remains the closer of the two. `EMERG = 0.05`
#' is still unused: emergencies are filtered upstream.
#' Unknown positions fall back to 0.75 with a warning.
#' Run data-raw/debug/compute_position_tog.R to regenerate from current data.
#' @keywords internal
POSITION_AVG_TOG <- c(
  FB = 0.91, BPL = 0.86, CHB = 0.86, BPR = 0.85,
  FF = 0.84, CHF = 0.82, HBFL = 0.82, HBFR = 0.82, WR = 0.82,
  FPL = 0.81, FPR = 0.81, WL = 0.81,
  C = 0.80, HFFL = 0.80, R = 0.80, RR = 0.80,
  HFFR = 0.79, RK = 0.79,
  INT = 0.73, SUB = 0.33, EMERG = 0.05
)
