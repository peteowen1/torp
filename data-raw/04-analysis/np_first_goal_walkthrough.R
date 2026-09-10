# Full data for the "walk the whole match, row by row, to the first goal"
# artifact: every PBP row from kickoff to the first goal, its exp_pts/delta_epv,
# every real payment row from attr(np,"np_payments"), and the whole match's
# final net_points (proving conservation to the real margin).
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_first_goal_walkthrough.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE); library(jsonlite)})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
MATCH <- "CD_M20260140608"

pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON)); res[, match_id := as.character(match_id)]
tm  <- fread("data-raw/outputs/np_difficulty_terms_2025_2026.csv")
tm[, match_id := as.character(match_id)]
tm  <- tm[substr(match_id, 5, 8) == as.character(SEASON)]

say("Running build_net_points() on the real 2026 season (real algorithm, unmodified)...")
np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                       stoppages = "allocate", difficulty_terms = tm,
                       return_payments = TRUE)
pay <- as.data.table(attr(np, "np_payments"))
pay[, match_id := as.character(match_id)]
np_dt <- as.data.table(np)
np_dt[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]

first_dispord <- min(pbp[match_id == MATCH]$display_order)
first_goal <- min(ch[match_id == MATCH & description == "Goal"]$display_order)
say("Match ", MATCH, ": kickoff at ", first_dispord, ", first goal at ", first_goal)

nm <- unique(pbp[!is.na(player_id), .(player_id = as.character(player_id), player_name)])
ha <- unique(pbp[match_id == MATCH, .(team, home_away)])[!is.na(team)]

# --- PBP rows, kickoff to first goal ----------------------------------------
seg_pbp <- pbp[match_id == MATCH & display_order >= first_dispord & display_order <= first_goal,
              .(display_order, description, team, player_id, exp_pts, delta_epv, home_away)]
seg_pbp <- merge(seg_pbp, nm, by = "player_id", all.x = TRUE)
setorder(seg_pbp, display_order)

# --- chains rows for the same window (adds the Goal row PBP doesn't have) ---
seg_ch <- ch[match_id == MATCH & display_order >= first_dispord & display_order <= first_goal,
            .(display_order, description, team_id, player_id)]
seg_ch[, player_id := as.character(player_id)]
seg_ch <- merge(seg_ch, nm, by = "player_id", all.x = TRUE)
setorder(seg_ch, display_order)

# --- payments for the same window -------------------------------------------
# The raw build_net_points() ledger only pays each side HALF the row's swing
# on a stoppage (NP_STOPPAGE_LOSER_SHARE): winner gets +half, loser gets
# +half too (both positive in home frame, since the split doesn't negate the
# loser's share). It's .np_team_margin()'s row-level rescale -- applied to
# EVERY row, not just turnovers -- that brings each side up to the FULL row
# value with opposite signs, which is what actually gets published. Replicate
# that exact rescale here (epv_net_points.R:2352-2381) so the walkthrough
# shows what's real, not the intermediate pre-rescale ledger.
all_pay <- copy(pay)
ha_full <- unique(pbp[, .(match_id, team, home_away)])
all_pay <- merge(all_pay, ha_full, by = c("match_id", "team"), all.x = TRUE)
all_pay[, own := hm * data.table::fifelse(home_away == "Home", 1, -1)]
if (!"doubled" %in% names(all_pay)) all_pay[, doubled := FALSE]
all_pay[, v := sum(hm[doubled == FALSE]), by = .(match_id, display_order)]
all_pay <- all_pay[abs(v) > 1e-12]
all_pay[, gain_home := v > 0]
all_pay[, side := data.table::fifelse((home_away == "Home") == gain_home, "gain", "concede")]
all_pay[, side_sum := sum(own), by = .(match_id, display_order, side)]
all_pay[, target := data.table::fifelse(side == "gain", abs(v), -abs(v))]
all_pay[, scaled := data.table::fifelse(abs(side_sum) > 0.05 * abs(v), own * target / side_sum, NA_real_)]

seg_pay <- all_pay[match_id == MATCH & display_order >= first_dispord & display_order <= first_goal]
seg_pay[, player_id := as.character(player_id)]
seg_pay <- merge(seg_pay, nm, by = "player_id", all.x = TRUE)
seg_pay[is.na(player_id), player_name := "(pool)"]
setorder(seg_pay, display_order)
say("Rows where the row-rescale is guarded off (near-cancelling side): ",
    seg_pay[is.na(scaled), .N], " of ", nrow(seg_pay))

say("Payment rows in this window: ", nrow(seg_pay), " across ", uniqueN(seg_pay$display_order), " display_orders")
say("Sum of |hm| in this window: ", round(sum(abs(seg_pay$hm)), 3))
say("Sum of hm (home-frame, signed) in this window: ", round(sum(seg_pay$hm), 3))

# --- whole-match net points and the real margin, for the CONSERVATION proof -
# build_net_points() ALONE only pins the DIFFERENCE between the two teams to
# the margin (the older convention). The team-sum convention that's actually
# LIVE in production -- each team separately hits its own margin, +2/-2 not
# just a difference of 2 -- is a SEPARATE step, .np_team_margin(), applied by
# .np_engine_frame() on top of build_net_points()'s output. Apply it here too,
# or "checks out to the margin" will silently mean the wrong thing.
say("\nApplying .np_team_margin() -- the step that's actually live in production...")
np_final <- torp:::.np_team_margin(np, pbp, ps, res)
np_final <- as.data.table(np_final)
np_final[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]

whole_match_pre <- np_dt[match_id == MATCH]
whole_match_pre <- merge(whole_match_pre[, .(player_id, team, net_points_pre = round(net_points, 3))],
                         nm, by = "player_id")
whole_match_final <- np_final[match_id == MATCH]
whole_match_final <- merge(whole_match_final[, .(player_id, net_points_final = round(net_points, 3))],
                           nm, by = "player_id")
whole_match <- merge(whole_match_pre, whole_match_final, by = c("player_id", "player_name"))
setorder(whole_match, -net_points_final)

margin_row <- res[match_id == MATCH]
say("\nReal match margin: home ", margin_row$home_score, " away ", margin_row$away_score,
    " (", margin_row$home_score - margin_row$away_score, ")")
team_sums <- whole_match[, .(sum_pre_margin_convention = round(sum(net_points_pre), 3),
                             sum_published = round(sum(net_points_final), 3)), by = team]
print(team_sums)

out <- list(
  match_id = MATCH,
  home_team = as.character(ha[home_away == "Home"]$team),
  away_team = as.character(ha[home_away == "Away"]$team),
  home_score = margin_row$home_score, away_score = margin_row$away_score,
  margin = margin_row$home_score - margin_row$away_score,
  first_dispord = first_dispord, first_goal = first_goal,
  pbp_rows = seg_pbp, chains_rows = seg_ch,
  payment_rows = seg_pay[, .(player_id, match_id, display_order, role, team, player_name,
                             hm = round(hm, 6), own = round(own, 6),
                             scaled = round(scaled, 6), doubled)],
  whole_match_net_points = whole_match, team_sums = team_sums
)
write_json(out, "data-raw/outputs/np_first_goal_walkthrough.json", auto_unbox = TRUE,
          na = "null", digits = 6)
say("\nWrote data-raw/outputs/np_first_goal_walkthrough.json")
