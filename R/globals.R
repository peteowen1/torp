# Global variable declarations for R CMD check
# =============================================
# This file declares variables used in non-standard evaluation (NSE) contexts
# such as data.table and dplyr operations to avoid "no visible binding" NOTEs.

#' @importFrom stats binomial coef complete.cases gaussian lm pchisq quantile sd var setNames
#' @importFrom utils head
#' @importFrom lubridate tz
NULL

# data.table special symbols and env-variable patterns
utils::globalVariables(c(".", ".N", ".I", ":=", "..keep", "..cols_present"))

# data.table column names used in NSE
utils::globalVariables(c(
  # Match/game identifiers
  "match_id", "season", "round_number", "roundnum", "period", "display_order",

  # Team variables
  "team", "team_id", "home", "home_team", "home_team_id", "home_team_abbr",
  "home_team_name", "away_team", "away_team_id", "away_team_abbr",
  "away_team_name", "team_id_mdl",

  # Score variables
  "home_points", "away_points", "home_points_row", "away_points_row",
  "pos_team_points", "opp_team_points", "pos_points", "pos_points_team_id",
  "points_diff", "points_row", "points_row_na", "points_shot", "points_team_id",
  "model_points",

  # Position/location variables
  "x", "y", "goal_x", "venue_length",

  # Time variables
  "period_seconds", "time_remaining", "min_seconds", "max_seconds",
  "est_qtr_elapsed", "est_qtr_remaining",
  "est_match_elapsed", "est_match_remaining",
  ".play_delta", ".lag_desc", "score_urgency",

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
  "player_id", "player_given_name", "player_surname",
  "player_name_given_name", "player_name_surname", "player_position", "listed_position",

  # TORP rating variables
  "torp", "torp_shift", "epr", "recv_epr", "disp_epr", "spoil_epr", "hitout_epr",
  "torp_home_round", "torp_away_round", "home_torp", "away_torp",
  "epv_adj", "recv_epv_adj", "recv_sum",
  "disp_epv_adj", "disp_sum", "spoil_epv_adj", "spoil_sum",
  "hitout_epv_adj", "hitout_sum", "weight_gm", "wt_gms", "wt_tog", "tog_sum", "utc_start_time",
  "days_diff", "wt_recv", "wt_disp", "wt_spoil", "wt_hitout",
  "wt_gms_recv", "wt_gms_disp", "wt_gms_spoil", "wt_gms_hitout",
  "pred_tog", "pred_selection", "pred_cond_tog",
  ".tog_safe", "tog_safe", "tog_wt", "lineup_tog",
  "i.squad_selection_rating", "i.cond_tog_rating", "i.n_80s", "i.wt_80s",
  "psv_raw", "psv", "osv", "dsv", "torp_value", "torp_value_p80",

  # data.table join prefixes
  "i.epr", "i.torp_shift_away", "i.torp_shift_home",

  # Model evaluation variables
  "auc", "log_loss", "bin", "pred_decile", "estimate", "result",
  "mean_predicted", "mean_actual", "se", "count",

  # Player profile aggregation variables
  "full_name_norm", "firstName", "surname", "providerId",
  "goals", "behinds", "shots_at_goal", "disposals", "kicks",
  "handballs", "inside50s", "marks", "tackles",
  "contested_possessions", "clearances",
  "disposal_efficiency", "time_on_ground_percentage",
  "games",

  # create_player_game_data variables
  "delta_epv", "pos_team", "wpa", "home_away", "lead_player", "lead_player_id", "is_intercept_mark", "lead_desc_tot",
  "round_week", "opp_tm", "recv_epv", "disp_epv", "spoil_epv", "hitout_epv",
  "epv", "receptions", "disposals_pbp", "opponent",
  "bounces", "hitouts",
  "position", "round_number",
  "player_name", "given_name", "surname", "jumper_number", "captain",

  # create_player_game_data stat columns
  "uncontested_possessions", "marks_inside50",
  "contested_marks", "metres_gained", "intercepts", "rebound50s",
  "one_percenters", "frees_for", "frees_against", "clangers",
  "turnovers", "score_involvements", "goal_assists",

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

# WP credit variables
utils::globalVariables(c(
  "wp_credit", "wp_disp_credit", "wp_recv_credit",
  "n_disposals", "n_receptions",
  "max_play_wpa", "max_play_display_order", "max_play_role",
  "abs_wpa", "disp_share",
  "has_receiver", "disp_wpa", "recv_wpa",
  "disp_peak_wpa", "disp_peak_do", "recv_peak_wpa", "recv_peak_do"
))

# Skill estimation variables
utils::globalVariables(c(
  "avail_only", ".played", "roster_pos_group",
  "round_idx", "first_season", "first_round",
  "tog", "tog_denominator", "match_date_rating", "days_since", "decay_wt",
  "wt_events", "wt_exposure", "wt_successes", "wt_attempts",
  "alpha_post", "beta_post", "skill_estimate", "skill_lower", "skill_upper",
  "position_group", "wt_games_rating", "n_games_rating", "wt_games",
  "disposal_efficiency_pct_x_disposals", "disposal_efficiency_ps",
  "mu0", "alpha0", "w_num", "w_den",
  ".wnum", ".wden", ".w_col", ".w_rate", ".total_rating",
  ".eff_successes", ".eff_attempts", ".eff_w",
  ".raw_num", ".raw_den", ".raw_succ", ".raw_att", ".wt_att",
  ".raw_vals", ".raw_tog",
  "pos_group", "modal_pos", "i.pos_group", "i.modal_pos",
  "i.n_games", "i.wt_games", "ref_date",
  "..keep_cols", "..skill_cols", "..rating_cols", "..lower_cols", "..upper_cols",
  "..lower_present", "..upper_present", "..raw_cols",
  ".d", ".base_w", "join_d", "ref_idx", "grp", "cum_num", "cum_den",
  "i.grp", "i.ref_idx", "i.player_name", "i.cum_num", "i.cum_den",
  "i.grand_mean", "i.pos_mean", "i.grand_prop", "i.pos_prop",
  "x.cum_num", "x.cum_den",
  "grand_mean", "pos_mean", "grand_prop", "pos_prop",
  "ref_d_val"
))

# Batch rating variables
utils::globalVariables(c(
  "match_idx", "match_idx_max", "match_ref", "date_val", "round_val",
  "wt_gms_raw"
))

# PSR variables
utils::globalVariables(c("psr_raw", "psr", "psr.x", "psr.y", "psr_diff",
                          "home_psr", "away_psr", "psr_week",
                          "osr", "dsr",
                          "effective_disposals", "effective_kicks",
                          "game_psr", "game_osr", "game_dsr"))

# explain_player_game / explain_player_plays variables
utils::globalVariables(c(
  "component", "raw_value", "stat", "rate", "team_status", "opp",
  ".row_idx", "is_play", "recv_credit", "disp_credit", "marker"
))

# player_credit.R stat columns
utils::globalVariables(c(
  "spoils", "pressure_acts", "def_half_pressure_acts",
  "hitouts_to_advantage", "ruck_contests", "ground_ball_gets"
))

# match_model.R columns
utils::globalVariables(c(
  "venue_name", "venue_timezone",
  "home_goals", "home_behinds", "away_goals", "away_behinds",
  "round_number.x"
))

# rlang .env pronoun
utils::globalVariables(c(".env"))

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
  "avg_pf_pg", "avg_pa_pg", "last_pct",
  "home_torp_eff", "away_torp_eff",
 "top_8_pct", "top_4_pct", "top_2_pct", "top_1_pct",
  "made_finals_pct", "avg_finals_wins", "made_gf_pct", "won_gf_pct",
  "i.pred_xtotal", "i.torp", "i.torp_boost", "i.pred_home_team",
  "return_round", "player_boost", "team_std", "..pr_cols", "player",
  "injury", "estimated_return", "player_norm", "tm_rnk",
  "tog_frac", "epv_p80", "recv_epv_p80", "disp_epv_p80", "spoil_epv_p80", "hitout_epv_p80",
  "epv_raw", "recv_epv_raw", "disp_epv_raw", "spoil_epv_raw", "hitout_epv_raw",
  "epv_c", "recv_epv_c", "disp_epv_c", "spoil_epv_c", "hitout_epv_c",
  "epv_pg", "avg_tog"
))

# Match model variables
utils::globalVariables(c(
  # Fixture/temporal features
  "utc_dt", "local_dt", "local_start_time_str", "venue.name", "venue.timezone",
  "game_year", "game_month", "game_yday", "game_mday", "game_wday", "game_wday_fac",
  "game_hour", "game_date_numeric", "timezone",
  "game_prop_through_year", "game_prop_through_month", "game_prop_through_week",
  "game_prop_through_day", "game_year_decimal",
  "compSeason.year", "team_name_season",

  # Position diff columns
  "BPL_diff", "BPR_diff", "FB_diff", "HBFL_diff", "HBFR_diff", "CHB_diff",
  "WL_diff", "WR_diff", "C_diff", "R_diff", "RR_diff", "RK_diff",
  "HFFL_diff", "HFFR_diff", "CHF_diff", "FPL_diff", "FPR_diff", "FF_diff",
  "int_diff",

  # Phase/group position columns
  "def", "mid", "fwd", "int",
  "backs", "half_backs", "midfielders", "followers", "half_forwards", "forwards",
  "key_def", "med_def", "midfield", "med_fwd", "key_fwd", "rucks", "other_pos",
  "CB", "BP", "HBF", "W", "MIDS", "HFF", "FP", "CF",

  # Individual position columns
  "BPL", "BPR", "FB", "HBFL", "HBFR", "CHB",
  "WL", "WR", "C", "R", "RR", "RK",
  "HFFL", "HFFR", "CHF", "FPL", "FPR", "FF",

  # TORP rating diffs
  "epr_diff", "epr_ratio", "epr_recv_diff", "epr_disp_diff",
  "epr_spoil_diff", "epr_hitout_diff",

  # Phase matchup columns
  "hoff_adef", "hmid_amid", "hdef_afwd", "hint_aint",

  # Score/result columns in match model
  "home_shots", "away_shots", "score_diff", "shot_diff", "team_shots",
  "harmean_shots", "shot_conv", "shot_conv_diff",
  "xscore_diff", "team_xscore", "win",
  "total_score", "total_shots", "total_xpoints", "total_points",
  "total_xpoints_adj",
  "home_xscore", "away_xscore",

  # xG join columns (results schema — kept via rename in .build_team_mdl_df)
  "homeTeamScore.matchScore.totalScore", "homeTeamScore.matchScore.goals",
  "homeTeamScore.matchScore.behinds",
  "awayTeamScore.matchScore.totalScore", "awayTeamScore.matchScore.goals",
  "awayTeamScore.matchScore.behinds",
  "match.matchId", "match.utcStartTime",

  # Fixture-schema score columns (from get_afl_results)
  "home.score.totalScore", "home.score.goals", "home.score.behinds",
  "away.score.totalScore", "away.score.goals", "away.score.behinds",
  "utcStartTime",

  # Distance/familiarity/rest
  "log_dist", "distance", "venue_lat", "venue_lon", "team_lat", "team_lon",
  "familiarity", "cum_total_games", "cum_venue_games",
  "days_rest", "log_dist_diff", "familiarity_diff",
  "days_rest_diff", "days_rest_diff_fac",

  # Weather columns
  "temp_avg", "wind_avg", "humidity_avg", "precipitation_total",
  "log_wind", "log_precip", "is_roof",

  # Weight columns
  "weightz", "shot_weightz",

  # GAM prediction columns
  "pred_tot_xscore", "pred_xscore_diff", "pred_conv_diff",
  "pred_score_diff", "pred_win",
  "bits", "tips", "mae",

  # Aggregation/join columns
  "team_type_fac", "venue_fac", "count",
  "team_name_adj", "home_ground", "venue_adj",

  # .x/.y suffixed columns from self-join
  "epr.x", "epr.y", "recv_epr.x", "recv_epr.y",
  "disp_epr.x", "disp_epr.y", "spoil_epr.x", "spoil_epr.y",
  "hitout_epr.x", "hitout_epr.y",
  "def.x", "def.y", "mid.x", "mid.y", "fwd.x", "fwd.y", "int.x", "int.y",
  "BPL.x", "BPL.y", "BPR.x", "BPR.y", "FB.x", "FB.y",
  "HBFL.x", "HBFL.y", "HBFR.x", "HBFR.y", "CHB.x", "CHB.y",
  "WL.x", "WL.y", "WR.x", "WR.y", "C.x", "C.y",
  "R.x", "R.y", "RR.x", "RR.y", "RK.x", "RK.y",
  "HFFL.x", "HFFL.y", "HFFR.x", "HFFR.y", "CHF.x", "CHF.y",
  "FPL.x", "FPL.y", "FPR.x", "FPR.y", "FF.x", "FF.y",
  "team_name.x", "team_name.y",
  "team_name_season.x", "team_name_season.y",
  "log_dist.x", "log_dist.y",
  "familiarity.x", "familiarity.y",
  "days_rest.x", "days_rest.y",
  "team_type_fac.x", "team_type_fac.y",
  "season.x", "season.y",
  "round.roundNumber.x", "round.roundNumber.y",
  "venue.x", "venue.y",
  "count.x", "count.y",
  "game_year_decimal.x", "game_year_decimal.y",
  "game_prop_through_year.x", "game_prop_through_year.y",
  "game_prop_through_month.x", "game_prop_through_month.y",
  "game_wday_fac.x", "game_wday_fac.y",
  "game_prop_through_day.x", "game_prop_through_day.y",

  # Lineup processing
  "lineup_tog", ".unknown_pos", "teamName", "teamType",
  "player.playerId", "position.x", "position.y",

  # Weather forecast
  "Latitude", "Longitude", "temperature_2m", "wind_speed_10m",
  "relative_humidity_2m", "kickoff_utc", "time",

  # Predictions pipeline specific
  "epr_week", "epr_recv_week", "epr_disp_week",
  "epr_spoil_week", "epr_hitout_week", "epr_boost", "total_sub",
  "n_players", "team_tog_sum",
  "home_boost", "away_boost", "boost",
  "type_anti", "Ground",

  # Predictions pipeline output
  "week", "source", "pred_xtotal", "pred_xmargin",

  # Backfill helper
  ".actual_margin",

  # Fixture/join schema columns
  "home.team.providerId", "away.team.providerId", "team.providerId",
  "team_name", "team_type", "venue", "match_date", "precipitation",

  # Predictions pipeline
  "home_rating", "away_rating", "start_time", "players",
  "pred_margin", "rating_diff",

  # Final ladder
  "win_prob", "is_played", "expected_wins", "expected_losses",
  "i.pred_win", "i.pred_margin", "i.pred_xtotal",
  "pred_home", "flipped"
))

# Team profile variables
utils::globalVariables(c(
  "pf", "pa", "loss", "draw", "round", "last_match",
  "..mean_cols", "..sum_cols", "..display_cols"
))

# Plot variables (NSE in ggplot2 aes/data manipulation)
utils::globalVariables(c(
  "total_seconds", "wp", "exp_pts", "home_wp",
  "game_date", "game_number", "rolling_avg",
  "percentile", "stat_name", "category",
  "avg_wins", "pct", "prob", "ladder_position",
  "metric_value",
  # Shot map
  "goal_prob", "behind_prob", "clanger_prob", "xscore", "outcome",
  # Player comparison
  "player_label", "season_fac", "avg_pct"
))
