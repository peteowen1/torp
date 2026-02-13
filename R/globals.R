# Global variable declarations for R CMD check
# =============================================
# This file declares variables used in non-standard evaluation (NSE) contexts
# such as data.table and dplyr operations to avoid "no visible binding" NOTEs.

#' @importFrom stats binomial coef complete.cases lm pchisq quantile sd var
#' @importFrom lubridate tz
NULL

# data.table special symbols
utils::globalVariables(c(".", ".N", ":="))

# data.table column names used in NSE
utils::globalVariables(c(
  # Match/game identifiers
  "match_id", "season", "round_number", "roundnum", "period", "display_order",

  # Team variables
  "team", "team_id", "home", "home_team", "home_team_id", "home_team_team_abbr",
  "home_team_team_name", "away_team", "away_team_id", "away_team_team_abbr",
  "away_team_team_name", "team_id_mdl",

  # Score variables
  "home_points", "away_points", "home_points_row", "away_points_row",
  "home_team_score_total_score", "away_team_score_total_score",
  "pos_team_points", "opp_team_points", "pos_points", "pos_points_team_id",
  "points_diff", "points_row", "points_row_na", "points_shot", "points_team_id",
  "model_points",

  # Position/location variables
  "x", "y", "goal_x", "venue_length",

  # Time variables
  "period_seconds", "time_remaining", "min_seconds", "max_seconds",

  # Chain/play variables
  "chain_number", "phase_of_play", "play_type", "description",
  "shot_at_goal", "shot_display", "max_shot_display",
  "end_of_qtr", "end_of_chain", "throw_in", "final_state",

  # Scoring/goal variables
  "tot_goals", "is_goal_row", "scoring_team_id", "opp_id",
  "lead_opp_id", "lag_opp_id", "lag_desc_tot",

  # Model labels
  "label_ep", "label_wp", "next_score",

  # Player variables
  "player_id", "player_given_name", "player_surname", "plyr_nm",
  "player_name_given_name", "player_name_surname", "player_position", "pos",

  # TORP rating variables
  "torp", "torp_shift", "torp_recv", "torp_disp", "torp_spoil", "torp_hitout",
  "torp_home_round", "torp_away_round", "home_torp", "away_torp",
  "tot_p_adj", "tot_p_sum", "recv_pts_adj", "recv_sum",
  "disp_pts_adj", "disp_sum", "spoil_pts_adj", "spoil_sum",
  "hitout_pts_adj", "hitout_sum", "weight_gm", "wt_gms", "utc_start_time",

  # data.table join prefixes
  "i.torp", "i.torp_shift_away", "i.torp_shift_home",

  # Model evaluation variables
  "auc", "log_loss", "bin", "pred_decile", "estimate", "result",
  "mean_predicted", "mean_actual", "se", "count",

  # Model objects
  "wp_model_ensemble",

  # Player profile aggregation variables
  "full_name_norm", "firstName", "surname", "providerId",
  "goals", "behinds", "shots_at_goal", "disposals", "kicks",
  "handballs", "inside50s", "marks", "tackles",
  "contested_possessions", "clearances_total_clearances",
  "disposal_efficiency", "time_on_ground_percentage",
  "games",

  # create_player_game_data variables
  "delta_epv", "pos_team", "wpa", "home_away", "lead_player", "lead_player_id",
  "round_week", "opp_tm", "recv_pts", "disp_pts", "spoil_pts", "hitout_pts",
  "recv_pts_wt", "disp_pts_wt", "spoil_pts_wt", "hitout_pts_wt",
  "tot_p", "tot_p_wt",
  "extended_stats_spoils", "extended_stats_pressure_acts",
  "extended_stats_def_half_pressure_acts", "extended_stats_hitouts_to_advantage",
  "extended_stats_ruck_contests", "bounces", "hitouts",
  "player_player_position", "position",
  "player_name", "player_player_player_player_id", "provider_id",

  # create_player_game_data stat columns
  "opp", "recvs", "uncontested_possessions", "marks_inside50",
  "contested_marks", "metres_gained", "intercepts", "rebound50s",
  "one_percenters", "frees_for", "frees_against", "clangers",
  "turnovers", "score_involvements", "goal_assists",
  "extended_stats_ground_ball_gets"
))
