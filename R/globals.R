# Global variable declarations for R CMD check
# =============================================
# This file declares variables used in non-standard evaluation (NSE) contexts
# such as data.table and dplyr operations to avoid "no visible binding" NOTEs.

#' @importFrom stats binomial coef complete.cases lm pchisq quantile sd var
#' @importFrom utils head
#' @importFrom lubridate tz
NULL

# data.table special symbols and env-variable patterns
utils::globalVariables(c(".", ".N", ":=", "..keep", "..cols_present"))

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
  "lead_opp_id", "lag_opp_id", "lag_desc_tot", "lead_desc",
  "is_lead_kickin", "lag_points_row",

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
  "pred_tog", "tog_safe", "tog_wt", "lineup_tog",

  # data.table join prefixes
  "i.torp", "i.torp_shift_away", "i.torp_shift_home",

  # Model evaluation variables
  "auc", "log_loss", "bin", "pred_decile", "estimate", "result",
  "mean_predicted", "mean_actual", "se", "count",

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
  "tot_p",
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
  "extended_stats_ground_ball_gets",

  # clean_model_data_epv_dt variables (data.table EPV pipeline)
  "lead_x_tot", "lead_y_tot", "lag_ti_flt", "lead_ti_flt",
  "mirror", "opp_points",
  "tmp_lag1_ti", "tmp_lag2_ti", "tmp_lag1_tm", "tmp_lag2_tm", "tmp_lag3_tm",

  "tmp_lag1_x", "tmp_lag2_x", "tmp_lead1_x", "tmp_lead2_x",
  "tmp_lead1_tm", "tmp_lead2_tm",
  "lv_lag1_x", "lv_lag1_y", "lv_lag1_gx", "lv_lag5_gx",
  "lv_lag1_ps", "lv_lag5_ps", "lv_lag1_tm", "lv_lag5_tm",
  "lv_lead1_x", "lv_lead1_y",
  "lv_lag1_desc", "lv_lead1_desc", "lv_lag1_pn", "lv_lead1_pn",
  "lag_goal_x", "lag_goal_x5", "lag_time5", "speed1", "speed5"
))

# Skill estimation variables
utils::globalVariables(c(
  "avail_only",
  "tog", "tog_denominator", "match_date_skill", "days_since", "decay_wt",
  "wt_events", "wt_exposure", "wt_successes", "wt_attempts",
  "alpha_post", "beta_post", "skill_estimate", "skill_lower", "skill_upper",
  "position_group", "wt_games_skill", "n_games_skill", "wt_games",
  "disposal_efficiency_pct_x_disposals", "disposal_efficiency_ps",
  "mu0", "alpha0", "w_num", "w_den",
  ".wnum", ".wden", ".w_col", ".w_rate", ".total_skill",
  ".eff_successes", ".eff_attempts", ".eff_w",
  ".raw_num", ".raw_den", ".raw_succ", ".raw_att", ".wt_att",
  ".raw_vals", ".raw_tog",
  "pos_group", "modal_pos", "i.pos_group", "i.modal_pos",
  "i.n_games", "i.wt_games",
  "..keep_cols", "..skill_cols", "..lower_cols", "..upper_cols",
  "..lower_present", "..upper_present", "..raw_cols"
))

# Contest extraction variables
utils::globalVariables(c(
  ".next_desc", ".next_player_id", ".next_team_id", ".next_x", ".next_y",
  "player1_id", "player2_id",
  "team1_id", "team2_id", "player1_desc", "player2_desc",
  "contest_type", "outcome", "winner", "p1_won",
  "p1_wins", "p1_win_pct", "total",
  "matchId", "displayOrder", "playerId", "teamId", "periodSeconds"
))

# Ladder / season simulation variables
utils::globalVariables(c(
  "sim_id", "played", "wins", "draws", "losses",
  "points_for", "points_against", "percentage", "ladder_points", "rank",
  "score_for", "score_against", "margin",
  "home_score", "away_score",
  "finals_finish", "finals_wins", "made_gf", "won_gf",
  "pred_xtotal",
  "avg_wins", "avg_losses", "avg_draws", "avg_percentage", "avg_rank",
 "top_8_pct", "top_4_pct", "top_2_pct", "top_1_pct",
  "made_finals_pct", "avg_finals_wins", "made_gf_pct", "won_gf_pct",
  "i.pred_xtotal",
  "injury", "estimated_return", "player_norm", "tm_rnk",
  "tog_frac", "total_p80", "recv_p80", "disp_p80", "spoil_p80", "hitout_p80",
  "avg_p80", "avg_tog"
))
