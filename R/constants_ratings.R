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

#' Prior games constant for receiving ratings
#' @keywords internal
EPR_PRIOR_GAMES_RECV <- 3.0000

#' Prior games constant for disposal ratings
#' @keywords internal
EPR_PRIOR_GAMES_DISP <- 3.0000

#' Prior games constant for spoil ratings
#' @keywords internal
EPR_PRIOR_GAMES_SPOIL <- 3.0000

#' Prior games constant for hitout ratings
#' @keywords internal
EPR_PRIOR_GAMES_HITOUT <- 3.0000

#' Prior rate for receiving component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_RECV <- -0.7000

#' Prior rate for disposal component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_DISP <- -0.7000

#' Prior rate for spoil component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_SPOIL <- -0.3000

#' Prior rate for hitout component (shrinkage target per weighted game)
#' @keywords internal
EPR_PRIOR_RATE_HITOUT <- -0.3000

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
#' @keywords internal
EPV_STANDARDISE_CHANNELS <- c("recv", "disp", "spoil")

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
EPV_HITOUT_WT <- 0.0510

#' Hitout to advantage weight
#' @keywords internal
EPV_HITOUT_ADV_WT <- 0.1748

#' Ruck contest weight (hitout component)
#' @keywords internal
EPV_RUCK_CONTEST_WT <- 0.0232

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
