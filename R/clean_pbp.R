#' Clean Play-by-Play Data
#'
#' This function cleans and processes play-by-play data for AFL matches.
#'
#' @param df A dataframe containing raw play-by-play data.
#'
#' @return A cleaned and processed dataframe with additional variables.
#' @export
#'
#' @examples
#' \dontrun{
#' chains <- load_chains(2024, rounds = 1)
#' cleaned_data <- clean_pbp(chains)
#' }
#'
#' @importFrom dplyr mutate group_by ungroup row_number case_when if_else lead lag
#' @importFrom tidyr replace_na
#' @importFrom stringr str_starts str_detect
#' @importFrom data.table nafill setnafill as.data.table setDT setorder fifelse fcase shift copy
clean_pbp <- function(df) {
  # Use optimized data.table version
  clean_pbp_dt(df)
}

#' Clean Play-by-Play Data (data.table optimized)
#'
#' Optimized version of clean_pbp using data.table for better performance.
#' Consolidates all variable additions into fewer passes over the data.
#'
#' @param df A dataframe containing raw play-by-play data.
#' @return A cleaned and processed data.table with additional variables.
#' @keywords internal
#' @importFrom data.table as.data.table setDT setorder setkey fifelse fcase shift nafill copy `%chin%`
#' @importFrom stringr str_starts str_detect
clean_pbp_dt <- function(df) {
  # Clean names and convert to data.table in one step (avoid extra copy)
  dt <- data.table::as.data.table(torp_clean_names(df))

  # Normalise column names across API schema versions (CFS vs v2)
  .normalise_pbp_columns(dt)
  .normalise_team_values(dt)

  # Sort by match_id + display_order: all shift/lag/lead operations assume this order.
  # setkey(match_id) sets the key attribute for fast by-group operations while
  # preserving the within-match display_order sort (setkey is stable within key groups).
  data.table::setorder(dt, match_id, display_order)
  data.table::setkey(dt, match_id)

  # Apply transformations in sequence (each modifies dt by reference)
  add_torp_ids_dt(dt)
  add_team_id_mdl_dt(dt)
  fix_chain_coordinates_dt(dt)
  add_chain_vars_dt(dt)
  add_contest_vars_dt(dt)
  add_quarter_vars_dt(dt)
  add_game_vars_dt(dt)
  add_score_vars_dt(dt)

  return(dt)
}

#' Add TORP IDs and basic variables (Pass 1)
#'
#' Adds TORP identifiers and basic computed variables to the data.table.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_torp_ids_dt <- function(dt) {
  dt[, `:=`(
    # TORP IDs (use paste0 instead of glue for data.table compatibility)
    torp_match_id = paste0(season, "_", round_number, "_", home_team_abbr, "_", away_team_abbr),
    torp_row_id = paste0(
      season, "_", round_number, "_", home_team_abbr, "_", away_team_abbr,
      sprintf("%04d", display_order)
    ),
    # Basic variables
    team = torp_replace_teams(
      data.table::fifelse(team_id == home_team_id, home_team_name, away_team_name)
    ),
    opp_id = data.table::fifelse(team_id == home_team_id, away_team_id, home_team_id),
    y = -y,
    home_away = factor(data.table::fifelse(team_id == home_team_id, "Home", "Away")),
    goal_x = venue_length / 2 - x,
    throw_in = data.table::fcase(
      description %in% c("Centre Bounce", "Out of Bounds", "Ball Up Call"), 1L,
      default = 0L
    ),
    shot_row = data.table::fifelse(is.na(shot_at_goal), 0L, 1L),
    player_position_fac = fct_na_to_level(player_position, "Other"),
    player_name = fct_na_to_level(
      paste(player_name_given_name, player_name_surname), "Other"
    )
  )]
  invisible(NULL)
}

#' Add team_id_mdl and goal boundaries (Pass 1.5)
#'
#' Computes team_id_mdl (the possessing team for each row, handling throw-ins)
#' and goal boundary markers needed for coordinate fixing.
#' Extracted from add_quarter_vars_dt so it runs before fix_chain_coordinates_dt.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_team_id_mdl_dt <- function(dt) {
  # team_id_mdl: use lead for throw_in rows, then fill NAs
  dt[, team_id_mdl := data.table::fcase(
    throw_in == 1L, data.table::shift(team_id, n = 1L, type = "lead"),
    default = team_id
  ), by = .(match_id, period)]

  # Compute goal boundaries before filling so fill doesn't cross goal events
  dt[, is_goal_row := data.table::fifelse(description == "Goal", 1L, 0L)]
  dt[, tot_goals := cumsum(is_goal_row), by = .(match_id, period)]

  # Fill NAs in team_id_mdl (character column - use nafill_char)
  dt[, team_id_mdl := nafill_char(team_id_mdl, type = "nocb"), by = .(match_id, period, tot_goals)]
  dt[, team_id_mdl := nafill_char(team_id_mdl, type = "locf"), by = .(match_id, period, tot_goals)]

  dt[, home := data.table::fifelse(team_id_mdl == home_team_id, 1L, 0L)]

  invisible(NULL)
}

#' Fix chain coordinate jumps (Pass 1.6)
#'
#' Converts coordinates to pitch-relative (home-team) space, fixes unreasonable
#' jumps around throw-ins and ambiguous possession events, then converts back to
#' possession-team perspective.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
fix_chain_coordinates_dt <- function(dt) {
  # Warn if any rows have NA team_id_mdl (coordinates can't be fixed for these)
  na_team_count <- sum(is.na(dt$team_id_mdl))
  if (na_team_count > 0L) {
    warning("fix_chain_coordinates_dt: ", na_team_count,
            " rows have NA team_id_mdl; coordinates for these rows will not be fixed.")
  }

  # --- A) Convert to pitch-relative (home-team) coordinates ---
  # Use as.double() to avoid integer truncation when interpolating later
  dt[, `:=`(
    x_pitch = as.double(data.table::fifelse(team_id_mdl == home_team_id, x, -x)),
    y_pitch = as.double(data.table::fifelse(team_id_mdl == home_team_id, y, -y))
  )]

  # --- B) Fix throw-in/stoppage rows ---
  # For throw-in rows the position should match where play resumes.
  # Run twice: second pass fixes consecutive throw-in chains (e.g. OOB -> Ball Up)
  # where the first pass picked up an unfixed neighbor.
  for (pass in 1:2) {
    dt[, `:=`(
      lead_x_fix = data.table::shift(x_pitch, 1L, type = "lead"),
      lead_y_fix = data.table::shift(y_pitch, 1L, type = "lead")
    ), by = .(match_id, period)]

    dt[throw_in == 1L & !is.na(lead_x_fix), `:=`(
      x_pitch = lead_x_fix,
      y_pitch = lead_y_fix
    )]
    dt[, c("lead_x_fix", "lead_y_fix") := NULL]
  }

  # --- C) Fix sign-flipped coordinates ---
  # The AFL API sometimes delivers coordinates in the wrong team's frame,
  # producing coords approximately NEGATED relative to the predecessor.
  # The error can persist for multiple consecutive rows, so iterate until
  # no more flips are needed. A row is flipped when its jump from prev
  # exceeds COORD_JUMP_THRESHOLD (100m) but negating puts it within
  # COORD_FLIP_TOLERANCE (70m) of the predecessor.
  n_flipped <- 1L
  iter <- 0L
  max_flip_iters <- 50L
  while (n_flipped > 0L && iter < max_flip_iters) {
    iter <- iter + 1L
    dt[, `:=`(
      prev_x = data.table::shift(x_pitch, 1L, type = "lag"),
      prev_y = data.table::shift(y_pitch, 1L, type = "lag")
    ), by = .(match_id, period)]

    dt[!is.na(prev_x), `:=`(
      dist_as_is = sqrt((x_pitch - prev_x)^2 + (y_pitch - prev_y)^2),
      dist_flipped = sqrt((-x_pitch - prev_x)^2 + (-y_pitch - prev_y)^2)
    )]

    flip_rows <- dt[, which(
      dist_as_is > COORD_JUMP_THRESHOLD &
      dist_flipped < COORD_FLIP_TOLERANCE &
      !is.na(prev_x)
    )]
    n_flipped <- length(flip_rows)

    if (n_flipped > 0L) {
      dt[flip_rows, `:=`(x_pitch = -x_pitch, y_pitch = -y_pitch)]
    }

    dt[, c("prev_x", "prev_y", "dist_as_is", "dist_flipped") := NULL]
  }
  if (iter >= max_flip_iters) {
    warning("fix_chain_coordinates_dt: reached max iterations (", max_flip_iters,
            ") with ", n_flipped, " rows still flagged. Possible oscillating coordinates.")
  }

  # --- D) Both-neighbor sign-flip (looser tolerance) ---
  # Catches sign-flips missed by step C due to longer kick distances.
  # Safe to use looser tolerance because both neighbors must confirm the flip.
  dt[, `:=`(
    prev_x = data.table::shift(x_pitch, 1L, type = "lag"),
    prev_y = data.table::shift(y_pitch, 1L, type = "lag"),
    next_x = data.table::shift(x_pitch, 1L, type = "lead"),
    next_y = data.table::shift(y_pitch, 1L, type = "lead")
  ), by = .(match_id, period)]

  dt[!is.na(prev_x) & !is.na(next_x), `:=`(
    jump_dist = sqrt((x_pitch - prev_x)^2 + (y_pitch - prev_y)^2),
    flip_prev = sqrt((-x_pitch - prev_x)^2 + (-y_pitch - prev_y)^2),
    flip_next = sqrt((-x_pitch - next_x)^2 + (-y_pitch - next_y)^2),
    total_asis = sqrt((x_pitch - prev_x)^2 + (y_pitch - prev_y)^2) +
                 sqrt((x_pitch - next_x)^2 + (y_pitch - next_y)^2),
    total_flip = sqrt((-x_pitch - prev_x)^2 + (-y_pitch - prev_y)^2) +
                 sqrt((-x_pitch - next_x)^2 + (-y_pitch - next_y)^2)
  )]

  dt[jump_dist > COORD_JUMP_THRESHOLD &
     flip_prev < 60 & flip_next < 60 &
     total_flip < total_asis &
     !is.na(prev_x) & !is.na(next_x), `:=`(
    x_pitch = -x_pitch,
    y_pitch = -y_pitch
  )]

  dt[, c("flip_prev", "flip_next", "total_asis", "total_flip") := NULL]

  # --- E) Smooth remaining outliers via neighbor interpolation ---
  # Recompute neighbors fresh (step D may have flipped some rows)
  dt[, c("prev_x", "prev_y", "next_x", "next_y") := NULL]
  dt[, `:=`(
    prev_x = data.table::shift(x_pitch, 1L, type = "lag"),
    prev_y = data.table::shift(y_pitch, 1L, type = "lag"),
    next_x = data.table::shift(x_pitch, 1L, type = "lead"),
    next_y = data.table::shift(y_pitch, 1L, type = "lead")
  ), by = .(match_id, period)]

  dt[!is.na(prev_x) & !is.na(next_x), `:=`(
    jump_dist = sqrt((x_pitch - prev_x)^2 + (y_pitch - prev_y)^2),
    next_jump = sqrt((next_x - x_pitch)^2 + (next_y - y_pitch)^2)
  )]

  dt[jump_dist > COORD_JUMP_THRESHOLD & next_jump > COORD_JUMP_THRESHOLD &
     !is.na(prev_x) & !is.na(next_x), `:=`(
    x_pitch = (prev_x + next_x) / 2,
    y_pitch = (prev_y + next_y) / 2
  )]

  dt[, c("prev_x", "prev_y", "next_x", "next_y",
         "jump_dist", "next_jump") := NULL]

  # --- F) Paired sign-flip fix ---
  # Catches cases where TWO consecutive rows are both sign-flipped (e.g.
  # Out On Full + OOF Kick In, or Spoil + Kickin). Flipping the pair together
  # improves the jump from the predecessor without hurting the successor.
  # The !is.na(next2_x) guard (shift with by=match_id,period) ensures
  # pair_rows + 1 never crosses a match/period boundary.
  dt[, `:=`(
    prev_x = data.table::shift(x_pitch, 1L, type = "lag"),
    prev_y = data.table::shift(y_pitch, 1L, type = "lag"),
    next_x = data.table::shift(x_pitch, 1L, type = "lead"),
    next_y = data.table::shift(y_pitch, 1L, type = "lead"),
    next2_x = data.table::shift(x_pitch, 2L, type = "lead"),
    next2_y = data.table::shift(y_pitch, 2L, type = "lead")
  ), by = .(match_id, period)]

  dt[!is.na(prev_x) & !is.na(next2_x), `:=`(
    jump_dist = sqrt((x_pitch - prev_x)^2 + (y_pitch - prev_y)^2),
    flip_prev = sqrt((-x_pitch - prev_x)^2 + (-y_pitch - prev_y)^2),
    flip_pair_to_next2 = sqrt((-next_x - next2_x)^2 + (-next_y - next2_y)^2)
  )]

  pair_rows <- dt[, which(
    jump_dist > COORD_JUMP_THRESHOLD &
    flip_prev < 60 &
    flip_pair_to_next2 < 60 &
    !is.na(prev_x) & !is.na(next2_x)
  )]

  if (length(pair_rows) > 0L) {
    dt[pair_rows, `:=`(x_pitch = -x_pitch, y_pitch = -y_pitch)]
    next_rows <- pair_rows + 1L
    next_rows <- next_rows[next_rows <= nrow(dt)]
    if (length(next_rows) > 0L) {
      dt[next_rows, `:=`(x_pitch = -x_pitch, y_pitch = -y_pitch)]
    }
  }

  dt[, c("prev_x", "prev_y", "next_x", "next_y", "next2_x", "next2_y",
         "jump_dist", "flip_prev", "flip_pair_to_next2") := NULL]

  # --- G) Convert back to possession-team perspective ---
  dt[, `:=`(
    x = as.integer(round(data.table::fifelse(team_id_mdl == home_team_id, x_pitch, -x_pitch))),
    y = as.integer(round(data.table::fifelse(team_id_mdl == home_team_id, y_pitch, -y_pitch)))
  )]

  dt[, c("x_pitch", "y_pitch") := NULL]

  # --- H) Recalculate goal_x from fixed coordinates ---
  dt[, goal_x := venue_length / 2 - x]

  invisible(NULL)
}

#' Add chain variables (Pass 2)
#'
#' Adds chain-level variables grouped by match_id and chain_number.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_chain_vars_dt <- function(dt) {
  dt[, `:=`(
    end_of_chain = data.table::fifelse(max(display_order) == display_order, 1L, 0L),
    shot_display = data.table::fifelse(
      is.na(shot_at_goal),
      data.table::fifelse(description %in% c("Kick", "Ground Kick"), as.numeric(display_order), display_order / 2),
      as.numeric(display_order)
    )
  ), by = .(match_id, chain_number)]

  dt[, max_shot_display := max(shot_display), by = .(match_id, chain_number)]

  dt[, points_shot := data.table::fifelse(
    shot_display == max_shot_display,
    data.table::fcase(
      final_state %in% c("rushed", "rushedOpp", "behind"), 1,
      final_state == "goal", 6,
      default = NA_real_
    ),
    NA_real_
  )]
  invisible(NULL)
}


#' Add contest variables (Pass 2b)
#'
#' Collapses aerial contest information from adjacent rows onto the preceding
#' Kick row. Contest target rows ("Contest Target", "Kick Inside 50 Result")
#' and their outcome rows (Spoil, Mark, etc.) will be filtered out downstream
#' by \code{clean_model_data_epv_dt()}, but their information is preserved on
#' the Kick row via these columns.
#'
#' @param dt A data.table to modify (sorted by match_id, display_order)
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_contest_vars_dt <- function(dt) {
  target_descs <- CHAINS_CONTEST_TARGET_DESCS

  # Initialise contest columns
  dt[, `:=`(
    contest_target_id = NA_character_,
    contest_target_team_id = NA_character_,
    contest_defender_id = NA_character_,
    contest_defender_team_id = NA_character_,
    contest_outcome = NA_character_
  )]

  # Build adjacent-row info within each match
  dt[, `:=`(
    .next_desc = data.table::shift(description, 1L, type = "lead"),
    .next_pid  = data.table::shift(player_id, 1L, type = "lead"),
    .next_tid  = data.table::shift(team_id, 1L, type = "lead"),
    .next_x    = data.table::shift(x, 1L, type = "lead"),
    .next_y    = data.table::shift(y, 1L, type = "lead"),
    .prev_desc = data.table::shift(description, 1L, type = "lag"),
    .prev_do   = data.table::shift(display_order, 1L, type = "lag")
  ), by = match_id]

  # Find contest target rows where the next row is at same location from opposing team.
  # AFL API uses team-relative coordinates — opponent x,y are negated, so compare
  # with sign flip: x == -.next_x && y == -.next_y.
  contest_idx <- dt[
    description %in% target_descs &
    !is.na(.next_tid) &
    x == -.next_x & y == -.next_y &
    team_id != .next_tid,
    which = TRUE
  ]

  if (length(contest_idx) > 0) {
    # Determine contest outcome from the next row's description
    dt[contest_idx, contest_outcome := data.table::fcase(
      .next_desc == "Spoil", "spoil",
      .next_desc %in% CHAINS_MARK_WIN_DESCS, "intercept_mark",
      default = "other"
    )]

    # Store target and defender info on the contest target row itself
    dt[contest_idx, `:=`(
      contest_target_id = player_id,
      contest_target_team_id = team_id,
      contest_defender_id = .next_pid,
      contest_defender_team_id = .next_tid
    )]

    # Propagate contest info BACKWARDS onto the preceding Kick row (vectorized).
    # Use lag columns (already computed: .prev_desc) and build more for deeper lookback.
    kick_descs <- c("Kick", "Ground Kick")
    dt[, `:=`(
      .lag1_desc = data.table::shift(description, 1L, type = "lag"),
      .lag2_desc = data.table::shift(description, 2L, type = "lag"),
      .lag3_desc = data.table::shift(description, 3L, type = "lag"),
      .lag4_desc = data.table::shift(description, 4L, type = "lag"),
      .lag5_desc = data.table::shift(description, 5L, type = "lag")
    ), by = match_id]

    # For each contest target row, find how many rows back the kick is
    # Use global row index arithmetic: contest row at index ci → kick at ci - offset
    offsets <- data.table::fcase(
      dt$.lag1_desc[contest_idx] %chin% kick_descs, 1L,
      dt$.lag2_desc[contest_idx] %chin% kick_descs, 2L,
      dt$.lag3_desc[contest_idx] %chin% kick_descs, 3L,
      dt$.lag4_desc[contest_idx] %chin% kick_descs, 4L,
      dt$.lag5_desc[contest_idx] %chin% kick_descs, 5L,
      default = NA_integer_
    )

    valid <- !is.na(offsets)
    if (any(valid)) {
      valid_ci <- contest_idx[valid]
      kick_rows <- valid_ci - offsets[valid]

      # Guard against out-of-bounds or cross-match indices
      ok <- kick_rows > 0L
      if (any(ok)) {
        ok[ok] <- dt$match_id[kick_rows[ok]] == dt$match_id[valid_ci[ok]]
      }
      if (!all(ok)) {
        kick_rows <- kick_rows[ok]
        valid_ci <- valid_ci[ok]
      }

      if (length(kick_rows) > 0L) {
        # Batch-set contest info on the Kick rows
        data.table::set(dt, kick_rows, "contest_target_id", dt$contest_target_id[valid_ci])
        data.table::set(dt, kick_rows, "contest_target_team_id", dt$contest_target_team_id[valid_ci])
        data.table::set(dt, kick_rows, "contest_defender_id", dt$contest_defender_id[valid_ci])
        data.table::set(dt, kick_rows, "contest_defender_team_id", dt$contest_defender_team_id[valid_ci])
        data.table::set(dt, kick_rows, "contest_outcome", dt$contest_outcome[valid_ci])
      }
    }

    # Clean up lag columns
    lag_cols <- c(".lag1_desc", ".lag2_desc", ".lag3_desc", ".lag4_desc", ".lag5_desc")
    dt[, (lag_cols) := NULL]
  }

  # Clean up temp columns
  temp_cols <- c(".next_desc", ".next_pid", ".next_tid", ".next_x", ".next_y",
                 ".prev_desc", ".prev_do")
  dt[, (temp_cols) := NULL]

  invisible(NULL)
}


#' Add quarter variables (Pass 3)
#'
#' Adds quarter-level variables grouped by match_id and period.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_quarter_vars_dt <- function(dt) {
  dt[, end_of_qtr := data.table::fifelse(max(display_order) == display_order, 1L, 0L),
     by = .(match_id, period)]

  dt[, points_row := data.table::fcase(
    final_state %in% c("rushed", "rushedOpp", "behind") & end_of_chain == 1L, 1L,
    final_state == "goal" & end_of_chain == 1L, 6L,
    default = NA_integer_
  )]

  dt[, points_row_na := data.table::fifelse(is.na(points_row), 0L, points_row)]

  # team_id_mdl, is_goal_row, tot_goals, home already computed in add_team_id_mdl_dt()

  # scoring_team_id
  dt[, scoring_team_id := data.table::fifelse(
    end_of_chain == 1L,
    data.table::fcase(
      final_state == "behind", team_id,
      final_state == "goal", team_id,
      final_state == "rushed", team_id,
      final_state == "rushedOpp", opp_id,
      default = NA_character_
    ),
    NA_character_
  )]

  # points_team_id calculation
  dt[, lead_opp_id := data.table::shift(opp_id, n = 1L, type = "lead"), by = .(match_id, period)]
  dt[, lag_opp_id := data.table::shift(opp_id, n = 1L, type = "lag"), by = .(match_id, period)]
  dt[, lead_desc := data.table::shift(description, n = 1L, type = "lead"), by = .(match_id, period)]
  dt[, is_lead_kickin := data.table::fifelse(
    !is.na(lead_desc) & grepl("Kickin", lead_desc), TRUE, FALSE
  )]

  dt[, points_team_id := data.table::fcase(
    description == "Goal", team_id,
    description == "Behind", team_id,
    # Rushed/rushedOpp: use kick-in team's opponent when next row is a kick-in
    final_state %in% c("rushed", "rushedOpp") & is_lead_kickin, lead_opp_id,
    # Fallback when next row is not a kick-in: use description-based logic
    final_state == "rushedOpp", opp_id,
    final_state == "rushed" & description == "Spoil", opp_id,
    final_state == "rushed", team_id,
    default = NA_character_
  )]

  # Clean up temp columns
  dt[, c("lead_opp_id", "lag_opp_id", "lead_desc", "is_lead_kickin") := NULL]

  # Fix orphan kick-ins: behinds with wrong final_state in source data
  # A kick-in only happens after a behind, so if no scoring event precedes it,
  # a behind was missed. Assign points to the kick-in team's opponent (the attacker).
  dt[, lag_points_row := data.table::shift(points_row, n = 1L, type = "lag"), by = .(match_id, period)]
  orphan_idx <- dt[, which(grepl("Kickin", description) & (is.na(lag_points_row) | lag_points_row == 0L))]
  if (length(orphan_idx) > 0L) {
    prev_idx <- orphan_idx - 1L
    # Filter to valid prev indices (not row 0)
    keep <- prev_idx >= 1L
    orphan_idx <- orphan_idx[keep]
    prev_idx <- prev_idx[keep]
    if (length(prev_idx) > 0L) {
      # Vectorised guards: same match/period, prev not a kick-in, prev has no points
      valid <- dt$match_id[prev_idx] == dt$match_id[orphan_idx] &
        dt$period[prev_idx] == dt$period[orphan_idx] &
        !grepl("Kickin", dt$description[prev_idx]) &
        (is.na(dt$points_row[prev_idx]) | dt$points_row[prev_idx] == 0L)
      if (any(valid)) {
        data.table::set(dt, prev_idx[valid], "points_row", 1L)
        data.table::set(dt, prev_idx[valid], "points_team_id", dt$opp_id[orphan_idx[valid]])
      }
    }
  }
  dt[, lag_points_row := NULL]

  dt[, `:=`(
    home_points_row = data.table::fifelse(
      !is.na(points_team_id) & points_team_id == home_team_id & !is.na(points_row), points_row, 0L
    ),
    away_points_row = data.table::fifelse(
      points_team_id != home_team_id & !is.na(points_row) & !is.na(points_team_id), points_row, 0L
    )
  )]

  # Lag/lead descriptions and coordinates
  dt[, `:=`(
    lag_desc_tot = data.table::shift(description, n = 1L, type = "lag", fill = "Start of Quarter"),
    lag_x_tot = data.table::shift(x, n = 1L, type = "lag"),
    lag_goal_x_tot = data.table::shift(goal_x, n = 1L, type = "lag"),
    lag_y_tot = data.table::shift(y, n = 1L, type = "lag"),
    lead_desc_tot = data.table::shift(description, n = 1L, type = "lead"),
    lead_goal_x_tot = data.table::shift(goal_x, n = 1L, type = "lead"),
    rn_qtr = seq_len(.N)
  ), by = .(match_id, period)]

  # Phase of play and play type
  dt[, phase_of_play := factor(data.table::fcase(
    throw_in == 1L, "Hard Ball",
    stringr::str_starts(lag_desc_tot, "Free") | stringr::str_starts(lag_desc_tot, "OOF") |
      stringr::str_starts(lag_desc_tot, "Out on") | stringr::str_detect(lag_desc_tot, "ted Mark") |
      stringr::str_detect(lag_desc_tot, "Mark On"), "Set Shot",
    stringr::str_starts(description, "Free") | stringr::str_detect(description, "ted Mark") |
      stringr::str_detect(description, "Mark On"), "Set Shot",
    stringr::str_starts(lag_desc_tot, "Loose Ball") | stringr::str_starts(description, "Loose Ball"), "Loose Ball",
    stringr::str_starts(description, "Contested Knock On"), "Hard Ball",
    stringr::str_detect(lag_desc_tot, "Hard Ball") | stringr::str_detect(description, "Hard Ball"), "Hard Ball",
    stringr::str_detect(lag_desc_tot, "Handball"), "Handball Received",
    default = "Hard Ball"
  ))]

  dt[, play_type := factor(data.table::fcase(
    description == "Handball", "Handball",
    description == "Kick", "Kick",
    description == "Ground Kick", "Ground Kick",
    default = "Reception"
  ))]

  # pos_points and pos_points_team_id (NOCB fill)
  dt[, pos_points := data.table::nafill(points_row, type = "nocb"), by = .(match_id, period)]
  dt[, pos_points_team_id := nafill_char(scoring_team_id, type = "nocb"), by = .(match_id, period)]

  invisible(NULL)
}

#' Add game variables (Pass 4)
#'
#' Adds game-level variables grouped by match_id.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_game_vars_dt <- function(dt) {
  dt[, `:=`(
    home_points = cumsum(home_points_row),
    away_points = cumsum(away_points_row),
    rn = seq_len(.N)
  ), by = match_id]

  dt[, `:=`(
    pos_team_points = data.table::fifelse(home == 1L, home_points, away_points),
    opp_team_points = data.table::fifelse(home == 1L, away_points, home_points),
    total_seconds = (period - 1L) * AFL_QUARTER_DURATION + period_seconds
  )]

  dt[, points_diff := pos_team_points - opp_team_points]

  # --- Estimated game time (excludes clock stoppages) ---
  dt[, .play_delta := period_seconds - data.table::shift(period_seconds, 1L, type = "lag"),
     by = .(match_id, period)]
  dt[is.na(.play_delta) | .play_delta < 0, .play_delta := 0]

  dt[, .lag_desc := data.table::shift(description, 1L, type = "lag"), by = .(match_id, period)]
  dt[.lag_desc %chin% CLOCK_STOPPAGE_TRIGGERS | description %chin% CLOCK_RESTART_EVENTS,
     .play_delta := 0]
  dt[.play_delta > CLOCK_DELTA_CAP, .play_delta := CLOCK_DELTA_CAP]

  dt[, est_qtr_elapsed := cumsum(.play_delta), by = .(match_id, period)]
  dt[, est_qtr_remaining := pmax(0L, AFL_PLAY_QUARTER_SECONDS - est_qtr_elapsed)]
  dt[, est_match_elapsed := (period - 1L) * AFL_PLAY_QUARTER_SECONDS + est_qtr_elapsed]
  dt[, est_match_remaining := pmax(0L, AFL_PLAY_GAME_SECONDS - est_match_elapsed)]

  dt[, c(".play_delta", ".lag_desc") := NULL]

  dt[, model_points := data.table::fifelse(pos_points_team_id == team_id_mdl, pos_points, -pos_points)]

  dt[, `:=`(
    pos_is_goal = data.table::fifelse(pos_points == 6L, 1L, 0L),
    pos_team_shot = data.table::fifelse(model_points > 0, 1L, 0L),
    is_shot = data.table::fifelse(!is.na(model_points), 1L, 0L),
    next_score = data.table::fcase(
      model_points == -6L, 0L,
      model_points == -1L, 1L,
      model_points == 1L, 2L,
      model_points == 6L, 3L,
      default = 4L
    )
  )]

  dt[, label_ep := as.numeric(next_score)]

  invisible(NULL)
}

#' Add score variables (Pass 5)
#'
#' Adds final score-related variables grouped by match_id, period, and tot_goals.
#' Modifies the data.table by reference.
#'
#' @param dt A data.table to modify
#' @return Invisible NULL (modifies dt by reference)
#' @keywords internal
add_score_vars_dt <- function(dt) {
  dt[, label_wp := data.table::fcase(
    home_score > away_score & home == 1L, 1,
    home_score == away_score & home == 1L, 0.5,
    home_score < away_score & home == 1L, 0,
    home_score > away_score & home == 0L, 0,
    home_score == away_score & home == 0L, 0.5,
    home_score < away_score & home == 0L, 1,
    default = NA_real_
  )]

  # lead_x_tot and lead_y_tot with last value as default
  dt[, `:=`(
    lead_x_tot = data.table::fifelse(
      is.na(data.table::shift(x, n = 1L, type = "lead")),
      x[.N],
      data.table::shift(x, n = 1L, type = "lead")
    ),
    lead_y_tot = data.table::fifelse(
      is.na(data.table::shift(y, n = 1L, type = "lead")),
      y[.N],
      data.table::shift(y, n = 1L, type = "lead")
    )
  ), by = .(match_id, period, tot_goals)]

  invisible(NULL)
}

#' Fill NA values in character vectors
#'
#' A helper function to fill NA values in character vectors using LOCF or NOCB.
#' This provides similar functionality to zoo::na.locf0 but is optimized for use
#' within dplyr pipelines.
#'
#' @param x A character vector with potential NA values.
#' @param type Fill type: "locf" (last observation carried forward) or
#'   "nocb" (next observation carried backward).
#' @return A character vector with NA values filled.
#' @keywords internal
nafill_char <- function(x, type = "locf") {
  if (type == "locf") {
    # Last observation carried forward
    idx <- cummax(seq_along(x) * (!is.na(x)))
    idx[idx == 0] <- NA_integer_
    x[idx]
  } else if (type == "nocb") {
    # Next observation carried backward: reverse the vector, apply LOCF via
    # cummax index trick, then reverse back to original order
    n <- length(x)
    rev_idx <- cummax(seq_along(x) * (!is.na(rev(x))))
    rev_idx[rev_idx == 0] <- NA_integer_
    rev(rev(x)[rev_idx])
  } else {
    cli::cli_abort("type must be 'locf' or 'nocb'")
  }
}

#' Normalise PBP column names across API schema versions
#'
#' The AFL API changed its response schema for 2026+. The old CFS schema uses
#' names like `homeTeamScore.totalScore` (cleaned to `home_team_score_total_score`),
#' while the new schema uses `home.score.totalScore` (cleaned to `home_score_total_score`).
#' Both are normalised to the canonical name `home_score` via `PBP_COL_MAP`.
#'
#' @param dt A data.table with cleaned (snake_case) column names.
#' @return Invisible NULL (renames columns by reference).
#' @keywords internal
.normalise_pbp_columns <- function(dt) {
  .normalise_columns(dt, PBP_COL_MAP)

  # Validate critical columns exist after remapping
  critical <- c("home_team_id", "away_team_id", "home_team_name",
                 "away_team_name", "round_number")
  missing <- critical[!critical %in% names(dt)]
  if (length(missing) > 0) {
    cli::cli_warn(c(
      "PBP schema missing critical column{?s} after normalisation: {paste(missing, collapse = ', ')}.",
      "i" = "This usually means the AFL API response schema has changed.",
      "i" = "Check column mappings in {.fn .normalise_pbp_columns}."
    ))
  }

  invisible(NULL)
}
