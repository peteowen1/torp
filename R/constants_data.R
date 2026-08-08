# Data Processing Constants
# =========================
# Constants for cleaning PBP/chains data: validation thresholds, the EPV
# description whitelist, coordinate sign-flip detection, and contest extraction
# from raw chains.

# Data Validation Constants
# -------------------------

#' High missing data threshold (proportion)
#' @keywords internal
VALIDATION_HIGH_MISSING_THRESHOLD <- 0.5

#' Minimum observations for calibration bins
#' @keywords internal
VALIDATION_MIN_CALIBRATION_BIN_SIZE <- 10

#' How long a local copy of a HISTORICAL season stays usable, in days
#'
#' Historical files previously never expired, on the reasoning that past
#' seasons do not change. The published files do change -- a schema rename or a
#' rating-vintage regenerate rewrites them -- and a never-expire rule let an
#' April copy be served in July, silently corrupting two published rating
#' seasons (2026-07-27). See \code{local_max_age_for_url()}.
#'
#' 30 days trades a periodic re-download of unchanged history for the guarantee
#' that a regenerated history propagates at all. Override with
#' \code{options(torp.local_historical_max_age_days = N)}; \code{Inf} restores
#' never-expire for genuinely offline work.
#' @keywords internal
LOCAL_HISTORICAL_MAX_AGE_DAYS <- 30

#' Clock-skew tolerance (seconds) when comparing a GitHub release-asset
#' listing's `updated_at` against our own upload start time
#'
#' `save_to_release()`'s post-upload verify decides whether a larger-than-local
#' listing is a lagging read (stamped before our write) or a different write
#' that replaced ours (stamped at or after it). The two timestamps come from
#' different clocks, so allow a margin before calling a listing "newer than us".
#' @keywords internal
VB_VERIFY_CLOCK_SKEW_SECS <- 120


# EPV Model Constants
# -------------------

#' Descriptions relevant for EPV modeling (the 27-description whitelist)
#' Used in clean_model_data_epv_dt() and filter_relevant_descriptions()
#' @keywords internal
EPV_RELEVANT_DESCRIPTIONS <- c(
  "Ball Up Call", "Bounce", "Centre Bounce", "Contested Knock On", "Contested Mark",
  "Free Advantage", "Free For", "Free For: Before the Bounce", "Free For: In Possession",
  "Free For: Off The Ball", "Gather", "Gather From Hitout", "Gather from Opposition",
  "Ground Kick", "Handball", "Handball Received", "Hard Ball Get", "Hard Ball Get Crumb",
  "Kick", "Knock On", "Loose Ball Get", "Loose Ball Get Crumb", "Mark On Lead",
  "Out of Bounds", "Out On Full After Kick", "Ruck Hard Ball Get", "Uncontested Mark"
)


# Coordinate Cleaning Constants
# -----------------------------

#' Maximum reasonable Euclidean distance between consecutive PBP coordinates (metres).
#' Jumps exceeding this in pitch-relative space are smoothed via neighbor interpolation.
#' An AFL field is ~165m long and ~135m wide; 100m covers most realistic play distances.
#' @keywords internal
COORD_JUMP_THRESHOLD <- 100

#' Maximum distance (metres) after sign-flipping for a row to be considered
#' a sign-flip error. If negating a row's coordinates puts it within this
#' distance of the predecessor, the coordinates are likely in the wrong frame.
#' Set to 70m to cover the longest realistic kick distances (~65m displacement).
#' Safe because the 100m jump threshold already filters out all legitimate plays.
#' @keywords internal
COORD_FLIP_TOLERANCE <- 70


# Contest Extraction Constants
# ----------------------------
# Based on diagnostic analysis of raw CHAINS data (not cleaned PBP).
# Chains data has 87 unique descriptions including Spoil, Contest Target,
# Tackle, etc. that get stripped by clean_pbp(). Contests are identified by
# matching x,y coordinates on consecutive rows from opposing teams.

#' Chains descriptions for the contest target / kick result side
#' These appear in the row BEFORE the contest outcome (Spoil, Mark, etc.)
#' at the same x,y coordinates.
#'
#' \strong{`Kick Inside 50 Result` does not belong here and is left anyway.}
#' It is an ANNOTATION of where an inside-50 entry ended up, not a player who
#' contested anything: 90.4 rows per match of which \strong{34.7\% carry no
#' `player_id` at all}, against `Contest Target`'s 26.0 per match at 100\%. The
#' volume comparison is the tell -- `Contest Target` at 26.0 lines up almost
#' exactly with the AFL API's 26 defensive one-on-ones per match, two
#' independent sources counting the same thing, and an annotation firing 3.5x as
#' often can only dilute it.
#'
#' It is NOT removed here because this constant is read by FIVE call sites and
#' four of them are v2 production paths -- `clean_pbp()` (which sets
#' `contest_target_id`, feeding v2's `contest_epv` and the reduced disposal
#' scale), `contests.R`, and two in `player_credit.R`. Changing it would move
#' every published v2 rating with no gate. v3 uses
#' \code{EPV3_CONTEST_TARGET_DESCS} instead.
#'
#' \strong{Fixing it for v2 is a real candidate and needs its own gate.}
#' @keywords internal
CHAINS_CONTEST_TARGET_DESCS <- c("Contest Target", "Kick Inside 50 Result")

#' Chains descriptions v3 accepts as naming the beaten player in a duel
#'
#' `Contest Target` only. See \code{CHAINS_CONTEST_TARGET_DESCS} for why the
#' i50 annotation is excluded here and retained there.
#' @keywords internal
EPV3_CONTEST_TARGET_DESCS <- c("Contest Target")

#' Chains descriptions that may sit between a kick and its contest outcome
#'
#' These are annotation rows the AFL feed emits while the ball is still in
#' flight -- they do not represent anyone gaining possession. A backward scan
#' from a Spoil to the kick that produced it may pass through these and no
#' others; hitting a possession event (Handball, Mark, Loose Ball Get, an
#' earlier Spoil, ...) means the kick found further back is a different play
#' and must not be credited.
#'
#' Derived empirically from 2024 chains: every row observed between a kick and
#' a spoil at lags 2-5 is in this set.
#' @keywords internal
CHAINS_INFLIGHT_DESCS <- c(
  "Contest Target", "Kick Inside 50 Result", "Kick Into F50",
  "Shot At Goal", "Inside 50", "Rebound 50",
  "Kick In Ineffective", "Kickin long", "Kickin short", "Kickin play on",
  "Clearance (Operator)"
)

#' Chains descriptions indicating the opponent won the mark
#' @keywords internal
CHAINS_MARK_WIN_DESCS <- c("Contested Mark", "Uncontested Mark", "Mark On Lead")

#' Descriptions for ground ball contests (present in both chains and PBP)
#' Adjacent ground ball rows from opposing teams at same x,y indicate a contest
#' @keywords internal
CONTEST_GROUND_BALL_DESCS <- c(
  "Hard Ball Get", "Loose Ball Get",
  "Hard Ball Get Crumb", "Loose Ball Get Crumb",
  "Ruck Hard Ball Get"
)
