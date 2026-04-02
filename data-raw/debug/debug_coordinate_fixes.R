# debug_coordinate_fixes.R
# ========================
# Compares chain x,y coordinates BEFORE and AFTER fix_chain_coordinates_dt()
# to show exactly which rows changed, why, and by how much.
#
# Usage:
#   source("data-raw/debug/debug_coordinate_fixes.R")
#
# Requires: devtools::load_all() or library(torp)

suppressPackageStartupMessages({
  library(data.table)
  library(cli)
})

devtools::load_all(quiet = TRUE)

# ---------------------------------------------------------------------------
# 1. Fetch a real match
# ---------------------------------------------------------------------------
cli::cli_h1("Fetching match data")

# Use a 2024 Round 1 match (or change to any match you want to debug)
chains <- get_match_chains(2024, 1)

# Pick first match
match_ids <- unique(chains$match_id)
target_match <- match_ids[1]
cli::cli_alert_info("Debugging match: {.val {target_match}}")

match_chains <- chains[match_id == target_match]
cli::cli_alert_info("Rows in match: {.val {nrow(match_chains)}}")

# ---------------------------------------------------------------------------
# 2. Run the pipeline up to JUST BEFORE coordinate fixing (the "before" state)
# ---------------------------------------------------------------------------
cli::cli_h1("Running pipeline: BEFORE coordinate fix")

dt_before <- data.table::as.data.table(torp_clean_names(data.table::copy(match_chains)))
torp:::.normalise_pbp_columns(dt_before)
torp:::.normalise_team_values(dt_before)
data.table::setorder(dt_before, match_id, display_order)
data.table::setkey(dt_before, match_id)

# Pass 1: basic vars (y=-y, goal_x, throw_in)
torp:::add_torp_ids_dt(dt_before)

# Pass 1.5: team_id_mdl (needed for pitch-relative conversion)
torp:::add_team_id_mdl_dt(dt_before)

# Save the "before" coordinates (after Pass 1 + 1.5, but BEFORE fix)
dt_before[, `:=`(
  x_before = x,
  y_before = y,
  goal_x_before = goal_x
)]

# Also compute pitch-relative "before" for comparison
dt_before[, `:=`(
  x_pitch_before = fifelse(team_id_mdl == home_team_id, x, -x),
  y_pitch_before = fifelse(team_id_mdl == home_team_id, y, -y)
)]

# ---------------------------------------------------------------------------
# 3. Run the full pipeline (the "after" state)
# ---------------------------------------------------------------------------
cli::cli_h1("Running pipeline: AFTER coordinate fix (full clean_pbp)")

dt_after <- clean_pbp(data.table::copy(match_chains))

# Compute pitch-relative "after"
dt_after[, `:=`(
  x_pitch_after = fifelse(team_id_mdl == home_team_id, x, -x),
  y_pitch_after = fifelse(team_id_mdl == home_team_id, y, -y)
)]

# ---------------------------------------------------------------------------
# 4. Merge before/after and identify changed rows
# ---------------------------------------------------------------------------
cli::cli_h1("Comparing before vs after")

# Join on display_order (unique within match)
compare <- merge(
  dt_before[, .(display_order, period, description, team_id, team_id_mdl,
                home_team_id, throw_in,
                x_before, y_before, goal_x_before,
                x_pitch_before, y_pitch_before)],
  dt_after[, .(display_order, x, y, goal_x,
               x_pitch_after, y_pitch_after)],
  by = "display_order"
)

setnames(compare, c("x", "y", "goal_x"), c("x_after", "y_after", "goal_x_after"))

# Flag rows where coordinates changed
compare[, `:=`(
  x_changed = abs(x_before - x_after) > 0.01,
  y_changed = abs(y_before - y_after) > 0.01,
  x_pitch_changed = abs(x_pitch_before - x_pitch_after) > 0.01,
  y_pitch_changed = abs(y_pitch_before - y_pitch_after) > 0.01,
  x_delta = round(x_after - x_before, 2),
  y_delta = round(y_after - y_before, 2),
  x_pitch_delta = round(x_pitch_after - x_pitch_before, 2),
  y_pitch_delta = round(y_pitch_after - y_pitch_before, 2)
)]

changed <- compare[x_changed == TRUE | y_changed == TRUE]

cli::cli_alert_info("Total rows in match: {.val {nrow(compare)}}")
cli::cli_alert_info("Rows with coordinate changes: {.val {nrow(changed)}}")

# ---------------------------------------------------------------------------
# 5. Classify WHY each row changed
# ---------------------------------------------------------------------------
# Two reasons a row can change:
#   A) throw_in == 1: coordinates replaced with lead row's pitch-relative coords
#   B) jump smoothing: row was >100m from both neighbors, got interpolated

changed[, change_reason := fcase(
  throw_in == 1L, "THROW_IN_FIX: Replaced with lead row position (where play resumes)",
  default = "JUMP_SMOOTH: Interpolated from neighbors (>100m from both)"
)]

# ---------------------------------------------------------------------------
# 6. Print the report
# ---------------------------------------------------------------------------
cli::cli_h1("Detailed Change Report")

# Summary by reason
reason_summary <- changed[, .N, by = change_reason]
cli::cli_h2("Changes by reason")
print(reason_summary)

# Summary by description
desc_summary <- changed[, .N, by = description]
cli::cli_h2("Changes by description type")
print(desc_summary[order(-N)])

# Show the actual changed rows
cli::cli_h2("All changed rows (possession-team perspective)")
changed_display <- changed[, .(
  display_order, period, description,
  team_id, team_id_mdl, home_team_id, throw_in,
  x_before = round(x_before, 1), x_after = round(x_after, 1), x_delta,
  y_before = round(y_before, 1), y_after = round(y_after, 1), y_delta,
  change_reason
)]
print(changed_display, nrows = 200)

# Show in pitch-relative frame (easier to see if jumps are fixed)
cli::cli_h2("All changed rows (pitch-relative / home-team perspective)")
changed_pitch <- changed[, .(
  display_order, period, description,
  team_id_mdl, throw_in,
  x_pitch_before = round(x_pitch_before, 1),
  x_pitch_after = round(x_pitch_after, 1),
  x_pitch_delta,
  y_pitch_before = round(y_pitch_before, 1),
  y_pitch_after = round(y_pitch_after, 1),
  y_pitch_delta,
  change_reason
)]
print(changed_pitch, nrows = 200)

# ---------------------------------------------------------------------------
# 7. Show context around each changed row (row before + changed + row after)
# ---------------------------------------------------------------------------
cli::cli_h2("Context windows (3-row view around each change)")

changed_orders <- changed$display_order
# Include neighbor rows for context
context_orders <- sort(unique(c(changed_orders - 1L, changed_orders, changed_orders + 1L)))
context_orders <- context_orders[context_orders >= min(compare$display_order) &
                                 context_orders <= max(compare$display_order)]

context <- compare[display_order %in% context_orders, .(
  display_order, period, description,
  team_id, team_id_mdl, throw_in,
  x_bef = round(x_before, 1), x_aft = round(x_after, 1),
  y_bef = round(y_before, 1), y_aft = round(y_after, 1),
  xp_bef = round(x_pitch_before, 1), xp_aft = round(x_pitch_after, 1),
  changed = fifelse(x_changed | y_changed, ">>> CHANGED", "")
)]

# Print in groups around each changed row
for (co in changed_orders) {
  window <- context[display_order >= co - 1L & display_order <= co + 1L]
  reason <- changed[display_order == co, change_reason]
  cli::cli_text("")
  cli::cli_alert_info("Display order {.val {co}}: {reason}")
  print(window, row.names = FALSE)
}

# ---------------------------------------------------------------------------
# 8. Before/after jump statistics
# ---------------------------------------------------------------------------
cli::cli_h1("Jump Statistics (pitch-relative frame)")

# Compute consecutive distance in pitch-relative frame
compare[, `:=`(
  lag_xp_before = shift(x_pitch_before, 1L, type = "lag"),
  lag_yp_before = shift(y_pitch_before, 1L, type = "lag"),
  lag_xp_after = shift(x_pitch_after, 1L, type = "lag"),
  lag_yp_after = shift(y_pitch_after, 1L, type = "lag")
), by = period]

compare[, `:=`(
  jump_before = sqrt((x_pitch_before - lag_xp_before)^2 + (y_pitch_before - lag_yp_before)^2),
  jump_after = sqrt((x_pitch_after - lag_xp_after)^2 + (y_pitch_after - lag_yp_after)^2)
)]

cli::cli_h2("BEFORE fix: jump distance distribution")
cat("  Max jump:     ", round(max(compare$jump_before, na.rm = TRUE), 1), "m\n")
cat("  Mean jump:    ", round(mean(compare$jump_before, na.rm = TRUE), 1), "m\n")
cat("  Median jump:  ", round(median(compare$jump_before, na.rm = TRUE), 1), "m\n")
cat("  P95 jump:     ", round(quantile(compare$jump_before, 0.95, na.rm = TRUE), 1), "m\n")
cat("  P99 jump:     ", round(quantile(compare$jump_before, 0.99, na.rm = TRUE), 1), "m\n")
cat("  Jumps > 100m: ", sum(compare$jump_before > 100, na.rm = TRUE), "rows\n")
cat("  Jumps > 120m: ", sum(compare$jump_before > 120, na.rm = TRUE), "rows\n")

cli::cli_h2("AFTER fix: jump distance distribution")
cat("  Max jump:     ", round(max(compare$jump_after, na.rm = TRUE), 1), "m\n")
cat("  Mean jump:    ", round(mean(compare$jump_after, na.rm = TRUE), 1), "m\n")
cat("  Median jump:  ", round(median(compare$jump_after, na.rm = TRUE), 1), "m\n")
cat("  P95 jump:     ", round(quantile(compare$jump_after, 0.95, na.rm = TRUE), 1), "m\n")
cat("  P99 jump:     ", round(quantile(compare$jump_after, 0.99, na.rm = TRUE), 1), "m\n")
cat("  Jumps > 100m: ", sum(compare$jump_after > 100, na.rm = TRUE), "rows\n")
cat("  Jumps > 120m: ", sum(compare$jump_after > 120, na.rm = TRUE), "rows\n")

# Show the worst jumps that STILL remain after fixing
cli::cli_h2("Largest remaining jumps AFTER fix (top 10)")
worst_remaining <- compare[order(-jump_after)][1:min(10, nrow(compare)), .(
  display_order, period, description,
  team_id_mdl, throw_in,
  xp_aft = round(x_pitch_after, 1),
  lag_xp = round(lag_xp_after, 1),
  yp_aft = round(y_pitch_after, 1),
  lag_yp = round(lag_yp_after, 1),
  jump_m = round(jump_after, 1)
)]
print(worst_remaining)

# Show the biggest improvements (before - after)
cli::cli_h2("Biggest jump reductions (top 10)")
compare[, jump_improvement := jump_before - jump_after]
best_fixes <- compare[order(-jump_improvement)][1:min(10, nrow(compare)), .(
  display_order, period, description,
  throw_in,
  jump_before_m = round(jump_before, 1),
  jump_after_m = round(jump_after, 1),
  improvement_m = round(jump_improvement, 1)
)]
print(best_fixes)

cli::cli_h1("Done")
cli::cli_alert_success("Debug script complete. Review the output above.")
