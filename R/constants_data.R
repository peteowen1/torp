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
#' @keywords internal
CHAINS_CONTEST_TARGET_DESCS <- c("Contest Target", "Kick Inside 50 Result")

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
