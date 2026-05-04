# optimize_epr_ratings.R
# ======================
# Optimize EPR (Estimated Player Rating) parameters to minimize RMSE when
# team-sum EPR predicts next-game margin. Uses pre-computed raw components
# and incremental cumulative decay for speed.
#
# Usage: powershell.exe -Command 'Rscript "data-raw/03-ratings/optimize_epr_ratings.R"'

library(devtools)
library(data.table)
library(dplyr)
library(nloptr)
library(Rcpp)
devtools::load_all()

cat("=== EPR Parameter Optimization ===\n\n")

# 1. Load Data ----
cat("Loading data...\n")
tictoc::tic("Data loading")

pbp_data      <- load_pbp(TRUE)
chains_data   <- load_chains(TRUE)
player_stats  <- load_player_stats(TRUE)
teams_data    <- load_teams(TRUE)
fixtures      <- load_fixtures(TRUE)
results       <- load_results(TRUE)
all_grounds   <- file_reader("stadium_data", "reference-data")

tictoc::toc()

# 2. Pre-Compute Raw Components ----
cat("Pre-computing raw components...\n")
tictoc::tic("Pre-computation")

## 2a. Disposal raw components ----
disp_raw <- data.table::as.data.table(pbp_data)[
  , .(
    date = max(as.Date(utc_start_time)),
    # Raw delta_epv sums split by pos_team
    sum_depv_neg = sum(delta_epv[pos_team == -1], na.rm = TRUE),
    n_neg        = sum(pos_team == -1, na.rm = TRUE),
    sum_depv_pos = sum(delta_epv[pos_team == 1], na.rm = TRUE),
    n_pos        = sum(pos_team == 1, na.rm = TRUE),
    team         = data.table::last(team),
    position_group = data.table::last(player_position)
  ),
  by = .(player_id, match_id)
]

## 2b. Reception raw components ----
# Intercept marks: pos_team == -1 AND lead_desc_tot indicates a mark
pbp_dt <- data.table::as.data.table(pbp_data)
pbp_dt[, is_intercept_mark := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]
recv_raw <- pbp_dt[
  , .(
    # For reception: delta_epv * pos_team, split by pos_team
    # Non-intercept-mark defensive receptions
    sum_depv_pt_neg = sum((delta_epv * pos_team)[pos_team == -1 & !is_intercept_mark], na.rm = TRUE),
    n_recv_neg      = sum(pos_team == -1 & !is_intercept_mark, na.rm = TRUE),
    # Intercept mark defensive receptions (separate scale)
    sum_depv_pt_neg_im = sum((delta_epv * pos_team)[is_intercept_mark], na.rm = TRUE),
    n_recv_neg_im      = sum(is_intercept_mark, na.rm = TRUE),
    # Positive (offensive) receptions
    sum_depv_pt_pos = sum((delta_epv * pos_team)[pos_team == 1], na.rm = TRUE),
    n_recv_pos      = sum(pos_team == 1, na.rm = TRUE)
  ),
  by = .(lead_player_id, match_id)
]
setnames(recv_raw, "lead_player_id", "player_id")
pbp_dt[, is_intercept_mark := NULL]

## 2c. Spoil/hitout raw counts ----
# load_player_stats() now normalises column names (player_id, match_id, etc.)
ps_dt <- data.table::as.data.table(player_stats)

spoil_hitout_raw <- ps_dt[
  , .(
    spoils          = sum(spoils, na.rm = TRUE),
    tackles         = sum(tackles, na.rm = TRUE),
    pressure_acts   = sum(pressure_acts, na.rm = TRUE),
    def_pressure    = sum(def_half_pressure_acts, na.rm = TRUE),
    hitouts         = sum(hitouts, na.rm = TRUE),
    hitouts_adv     = sum(hitouts_to_advantage, na.rm = TRUE),
    ruck_contests   = sum(ruck_contests, na.rm = TRUE),
    bounces         = sum(bounces, na.rm = TRUE),
    contested_poss  = sum(contested_possessions, na.rm = TRUE),
    contested_marks = sum(contested_marks, na.rm = TRUE),
    ground_ball_gets = sum(ground_ball_gets, na.rm = TRUE),
    marks_inside50  = sum(marks_inside50, na.rm = TRUE),
    inside50s       = sum(inside50s, na.rm = TRUE),
    clangers        = sum(clangers, na.rm = TRUE),
    score_inv       = sum(score_involvements, na.rm = TRUE),
    intercepts      = sum(intercepts, na.rm = TRUE),
    one_percenters  = sum(one_percenters, na.rm = TRUE),
    rebound50s      = sum(rebound50s, na.rm = TRUE),
    frees_against   = sum(frees_against, na.rm = TRUE),
    clearances      = sum(clearances, na.rm = TRUE),
    frees_for       = sum(frees_for, na.rm = TRUE),
    goals           = sum(goals, na.rm = TRUE),
    behinds         = sum(behinds, na.rm = TRUE),
    marks_total     = sum(marks, na.rm = TRUE),
    uncontested_poss = sum(uncontested_possessions, na.rm = TRUE),
    shots_at_goal   = sum(shots_at_goal, na.rm = TRUE),
    kicks           = sum(kicks, na.rm = TRUE),
    handballs       = sum(handballs, na.rm = TRUE),
    metres_gained   = sum(metres_gained, na.rm = TRUE),
    turnovers_stat  = sum(turnovers, na.rm = TRUE),
    goal_assists    = sum(goal_assists, na.rm = TRUE)
  ),
  by = .(player_id, match_id)
]

## 2c3. Contest credit raw components ----
# Pre-compute contest_epv at default 1/3 share; optimizer scales via contest_scale
# Hard-fail here: optimizing without contest credit produces mismatched parameters
contest_raw <- compute_contest_credit(chains_data, pbp_data, contest_share = 1 / 3)
if (nrow(contest_raw) == 0 && nrow(chains_data) > 1000) {
  stop("Contest credit returned 0 rows from ", nrow(chains_data),
       " chain rows -- cannot optimize with contest_scale parameter")
}
cat(sprintf("  Contest credit rows: %d\n", nrow(contest_raw)))

## 2d. Merge all raw components ----
player_game_raw <- merge(disp_raw, recv_raw, by = c("player_id", "match_id"), all.x = TRUE)
player_game_raw <- merge(player_game_raw, spoil_hitout_raw, by = c("player_id", "match_id"), all.x = TRUE)
player_game_raw <- merge(player_game_raw, contest_raw[, .(player_id, match_id, contest_epv)],
                         by = c("player_id", "match_id"), all.x = TRUE)

# Join TOG from player_stats for per-80 normalisation
tog_dt <- ps_dt[
  , .(tog_pct = mean(time_on_ground_percentage, na.rm = TRUE)),
  by = .(player_id, match_id)
]
player_game_raw <- merge(player_game_raw, tog_dt, by = c("player_id", "match_id"), all.x = TRUE)
player_game_raw[is.na(tog_pct), tog_pct := 10]
player_game_raw[, tog_safe := pmax(tog_pct / 100, 0.1)]

# Remove rows with NA player_id (pbp rows where no player is attributed)
n_na <- sum(is.na(player_game_raw$player_id))
if (n_na > 0) cat(sprintf("Dropping %d rows with NA player_id\n", n_na))
player_game_raw <- player_game_raw[!is.na(player_id)]

# Replace NAs with 0
num_cols <- c("sum_depv_neg", "n_neg", "sum_depv_pos", "n_pos",
              "sum_depv_pt_neg", "n_recv_neg", "sum_depv_pt_pos", "n_recv_pos",
              "contest_epv",
              "spoils", "tackles", "pressure_acts", "def_pressure",
              "hitouts", "hitouts_adv", "ruck_contests",
              "contested_poss", "contested_marks", "ground_ball_gets", "marks_inside50",
              "inside50s", "clangers", "score_inv",
              "intercepts", "one_percenters", "rebound50s", "frees_against",
              "clearances", "frees_for",
              "goals", "behinds", "marks_total", "uncontested_poss", "shots_at_goal",
              "kicks", "handballs", "metres_gained", "turnovers_stat", "goal_assists")
for (col in num_cols) {
  data.table::set(player_game_raw, which(is.na(player_game_raw[[col]])), col, 0)
}

## 2c2. Normalize count-based stats by SD ----
# Dividing by SD makes all stat weights "per-SD" so L2 regularization treats them fairly.
# On writeback, weights are un-normalized back to per-raw-unit for player_credit.R.
stat_cols_to_normalize <- c(
  "spoils", "tackles", "pressure_acts", "def_pressure",
  "hitouts", "hitouts_adv", "ruck_contests",
  "contested_poss", "contested_marks", "ground_ball_gets", "marks_inside50",
  "inside50s", "clangers", "score_inv",
  "intercepts", "one_percenters", "rebound50s", "frees_against",
  "clearances", "frees_for",
  "goals", "behinds", "marks_total", "uncontested_poss", "shots_at_goal",
  "kicks", "handballs", "metres_gained", "turnovers_stat", "goal_assists"
)
stat_sds <- sapply(stat_cols_to_normalize, function(col) {
  s <- sd(player_game_raw[[col]], na.rm = TRUE)
  if (is.na(s) || s < 1e-6) 1 else s
})
cat("  Stat SDs for normalization:\n")
for (col in stat_cols_to_normalize) {
  cat(sprintf("    %-20s SD = %.4f\n", col, stat_sds[col]))
}
for (col in stat_cols_to_normalize) {
  player_game_raw[[col]] <- player_game_raw[[col]] / stat_sds[col]
}

# Derive season from date (needed for per-season position centering)
player_game_raw[, season := lubridate::year(date)]

# Sort chronologically
data.table::setorder(player_game_raw, date, match_id)

## 2e. Build match-level data ----
# Team map
team_map <- data.table::as.data.table(fixtures)[
  , .(team_name = names(sort(table(home_team_name), decreasing = TRUE))[1]),
  by = .(teamId = home_team_id)
][, team_name := torp_replace_teams(team_name)]

# Fixtures with results (columns normalised at load time)
fix_dt <- data.table::as.data.table(fixtures)[
  , .(match_id, season, round = round_number,
      home_teamId = home_team_id, away_teamId = away_team_id,
      utc_start_time, venue_name)
]

# Results are normalised at load time — canonical columns
results_dt <- data.table::as.data.table(results)[
  , .(match_id, home_score, away_score)
]

match_dt <- merge(fix_dt, results_dt, by = "match_id", all.x = TRUE)
match_dt[, `:=`(
  margin = home_score - away_score,
  date = as.Date(utc_start_time),
  venue = torp_replace_venues(venue_name)
)]
match_dt <- match_dt[!is.na(margin)]
data.table::setorder(match_dt, date, match_id)

# --- Lineups per team per match ---
teams_dt <- data.table::as.data.table(teams_data)
# Filter out EMERG/SUB
teams_dt <- teams_dt[is.na(lineup_position) | !(lineup_position %in% c("EMERG", "SUB"))]

lineups <- teams_dt[, .(player_ids = list(player_id),
                       position_xs = list(lineup_position)),
                    by = .(match_id, teamId = team_id)]

# --- Home ground / distance / familiarity ---
grounds_dt <- data.table::as.data.table(all_grounds)
grounds_dt[, venue := torp_replace_venues(as.character(Ground))]

# Find each team's home ground (mode of venue) — join fixtures for venue_name
home_ground <- merge(
  data.table::as.data.table(teams_data)[, .(match_id, team_id)],
  fix_dt[, .(match_id, venue_name)],
  by = "match_id"
)[, .(venue = torp_replace_venues(names(sort(table(venue_name), decreasing = TRUE))[1])),
  by = .(teamId = team_id)]
home_ground <- merge(home_ground, grounds_dt[, .(venue, home_lat = Latitude, home_lon = Longitude)],
                     by = "venue", all.x = TRUE)

# Add distance to match_dt for each team
match_dt <- merge(match_dt,
                  grounds_dt[, .(venue, venue_lat = Latitude, venue_lon = Longitude)],
                  by = "venue", all.x = TRUE)

# Home team distance
match_dt <- merge(match_dt,
                  home_ground[, .(teamId, home_lat_h = home_lat, home_lon_h = home_lon)],
                  by.x = "home_teamId", by.y = "teamId", all.x = TRUE)
# Away team distance
match_dt <- merge(match_dt,
                  home_ground[, .(teamId, home_lat_a = home_lat, home_lon_a = home_lon)],
                  by.x = "away_teamId", by.y = "teamId", all.x = TRUE)

# Haversine distances
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 6371000
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * R * asin(sqrt(a))
}

match_dt[, `:=`(
  dist_home = haversine(venue_lon, venue_lat, home_lon_h, home_lat_h),
  dist_away = haversine(venue_lon, venue_lat, home_lon_a, home_lat_a)
)]
match_dt[is.na(dist_home), dist_home := 0]
match_dt[is.na(dist_away), dist_away := 0]
match_dt[, log_dist_diff := log(dist_home + 10000) - log(dist_away + 10000)]

# --- Familiarity (proportion of past games at this venue) ---
# Pre-compute per team: cumulative games at each venue up to each match
# For speed, compute a simpler version: proportion of all historical games at venue
compute_familiarity <- function(match_dt, teams_dt) {
  # Get all team-match-venue records
  team_matches <- rbind(
    match_dt[, .(teamId = home_teamId, match_id, date, venue)],
    match_dt[, .(teamId = away_teamId, match_id, date, venue)]
  )
  data.table::setorder(team_matches, teamId, date)

  # O(n) per team: cumulative count per venue using a running counter table
  team_matches[, familiarity := {
    venue_counts <- integer(0)  # named integer vector: venue -> count
    fam <- numeric(.N)
    for (i in seq_len(.N)) {
      total_prior <- i - 1L
      if (total_prior == 0L) {
        fam[i] <- 0
      } else {
        v <- venue[i]
        fam[i] <- (venue_counts[v] %||% 0L) / total_prior
      }
      venue_counts[venue[i]] <- (venue_counts[venue[i]] %||% 0L) + 1L
    }
    fam
  }, by = teamId]

  return(team_matches[, .(teamId, match_id, familiarity)])
}

cat("  Computing familiarity...\n")
fam_dt <- compute_familiarity(match_dt, teams_dt)

# Merge familiarity for home and away
match_dt <- merge(match_dt,
                  fam_dt[, .(home_teamId = teamId, match_id, fam_home = familiarity)],
                  by = c("home_teamId", "match_id"), all.x = TRUE)
match_dt <- merge(match_dt,
                  fam_dt[, .(away_teamId = teamId, match_id, fam_away = familiarity)],
                  by = c("away_teamId", "match_id"), all.x = TRUE)
match_dt[is.na(fam_home), fam_home := 0]
match_dt[is.na(fam_away), fam_away := 0]
match_dt[, familiarity_diff := fam_home - fam_away]

# Merge lineups (with positions for lineup_tog weighting)
match_dt <- merge(match_dt,
                  lineups[, .(match_id, home_teamId = teamId, home_players = player_ids, home_positions = position_xs)],
                  by = c("match_id", "home_teamId"), all.x = TRUE)
match_dt <- merge(match_dt,
                  lineups[, .(match_id, away_teamId = teamId, away_players = player_ids, away_positions = position_xs)],
                  by = c("match_id", "away_teamId"), all.x = TRUE)

# Filter to evaluation window (2022+) and valid lineups
match_dt <- match_dt[season >= 2022 & !is.na(home_players) & !is.na(away_players)]
data.table::setorder(match_dt, date, match_id)

cat(sprintf("  %d matches in evaluation window (2022+)\n", nrow(match_dt)))

## 2f. Player-game index for fast lookup ----
# Create a lookup: for each player, ordered list of (match_id, date, raw_components)
pgr <- player_game_raw[match_id %in% unique(c(
  unlist(match_dt$home_players),
  player_game_raw$match_id
))]
data.table::setorder(pgr, player_id, date)
data.table::setkey(pgr, player_id, match_id)

# Also join lineup_position (20-way AFL lineup role: HBFR, INT, SUB, ...)
# from teams_data for any consumers that want the granular role. NOTE: position
# adjustment uses position_group (6-way class, already set from PBP's
# player_position at disp_raw step), NOT lineup_position — the 20-way groups
# are too granular for meaningful TOG-weighted centering.
lineup_dt <- data.table::as.data.table(teams_data)[
  , .(player_id, match_id, lineup_position)
]
pgr <- merge(pgr, lineup_dt, by = c("player_id", "match_id"), all.x = TRUE)

# Collapse MIDFIELDER_FORWARD → MEDIUM_FORWARD in position_group to match
# create_player_game_data() (keeps both pipelines on the same 6-way enum).
pgr[position_group == "MIDFIELDER_FORWARD", position_group := "MEDIUM_FORWARD"]

tictoc::toc()

## 2g. Pre-compute lineup expansion + eval data (reused across all stages) ----
cat("  Pre-computing lineup expansion...\n")
eval_matches_all <- match_dt  # already filtered to 2022+ with valid lineups

lineup_rows <- vector("list", nrow(eval_matches_all))
for (i in seq_len(nrow(eval_matches_all))) {
  m <- eval_matches_all[i]
  home_ids <- m$home_players[[1]]
  away_ids <- m$away_players[[1]]
  home_pos <- m$home_positions[[1]]
  away_pos <- m$away_positions[[1]]
  # Map field position to average TOG; default 0.75 for unknown positions
  home_tog <- ifelse(home_pos %in% names(POSITION_AVG_TOG), POSITION_AVG_TOG[home_pos], 0.75)
  away_tog <- ifelse(away_pos %in% names(POSITION_AVG_TOG), POSITION_AVG_TOG[away_pos], 0.75)
  lineup_rows[[i]] <- data.table::data.table(
    match_idx = i,
    player_id = c(home_ids, away_ids),
    is_home = c(rep(TRUE, length(home_ids)), rep(FALSE, length(away_ids))),
    lineup_tog = c(home_tog, away_tog),
    date_num = as.numeric(m$date) - 0.5
  )
}
lineup_dt_all <- data.table::rbindlist(lineup_rows)
cat(sprintf("  Lineup expansion: %d player-match rows for %d matches\n",
            nrow(lineup_dt_all), nrow(eval_matches_all)))

## 2h. Pre-compute lineup-level role positions (for balance penalty) ----
cat("  Pre-computing lineup positions for balance penalty...
")
pgr_pos_lookup <- pgr[!is.na(position), .(player_id, date_num = as.numeric(date), position)]
data.table::setkey(pgr_pos_lookup, player_id, date_num)
lu_pos_joined <- pgr_pos_lookup[lineup_dt_all, .(position = x.position),
                                 on = .(player_id, date_num), roll = TRUE]
lineup_dt_all[, role_position := lu_pos_joined$position]
rm(pgr_pos_lookup, lu_pos_joined)

# Pre-compute position group indices for lineup rows (reused in all stages)
lu_role_positions <- lineup_dt_all$role_position
lu_pos_levels <- unique(lu_role_positions[!is.na(lu_role_positions)])
lu_pos_idx <- lapply(lu_pos_levels, function(p) which(lu_role_positions == p))
names(lu_pos_idx) <- lu_pos_levels
cat(sprintf("  Position groups: %s
", paste(lu_pos_levels, collapse = ", ")))

# Position balance penalty: lambda * sd(position-group mean EPRs). ----
# Larger values produce more balanced positions at the cost of slightly
# worse margin prediction accuracy. Set to 0 to disable.
POSITION_BALANCE_LAMBDA <- 0.1

# L2 (ridge) penalty on count-based stat weights to prevent overfitting ----
STAT_WEIGHT_LAMBDA <- 0.1

# Names of count-based stat weight params subject to L2 penalty
L2_PARAM_NAMES <- c("spoil_wt", "tackle_wt", "pressure_wt", "def_pressure_wt",
                     "hitout_wt", "hitout_adv_wt", "ruck_contest_wt",
                     "contested_poss_wt", "contested_marks_wt", "ground_ball_gets_wt",
                     "marks_inside50_wt", "inside50s_wt", "clangers_wt",
                     "score_involvements_wt", "intercepts_wt", "one_percenters_wt",
                     "rebound50s_wt", "frees_against_wt",
                     "frees_for_wt",
                     "goals_wt", "behinds_wt", "marks_wt", "uncontested_poss_wt",
                     "shots_at_goal_wt", "kicks_wt", "handballs_wt",
                     "metres_gained_wt", "turnovers_wt", "goal_assists_wt")

# 3. Objective Function ----

# Compile Rcpp cumulative decay kernel for ~20-50x speedup over R for-loop
cat("Compiling Rcpp cumulative decay kernel...\n")
Rcpp::cppFunction('
Rcpp::List cumulative_decay_cpp(Rcpp::IntegerVector pids,
                                Rcpp::NumericVector dnums,
                                Rcpp::NumericVector recv_epv,
                                Rcpp::NumericVector disp_epv,
                                Rcpp::NumericVector spoil_epv,
                                Rcpp::NumericVector hitout_epv,
                                Rcpp::NumericVector tog,
                                double decay_r, double decay_d,
                                double decay_s, double decay_h) {
  int n = pids.size();
  Rcpp::NumericVector cr(n), cd(n), cs(n), ch(n);
  Rcpp::NumericVector cw_r(n), cw_d(n), cw_s(n), cw_h(n);

  // TOG-weighted: sums accumulate epv * tog (game total),
  // weights accumulate tog (weighted minutes)
  double t0 = tog[0];
  cr[0] = recv_epv[0] * t0; cd[0] = disp_epv[0] * t0;
  cs[0] = spoil_epv[0] * t0; ch[0] = hitout_epv[0] * t0;
  cw_r[0] = t0; cw_d[0] = t0; cw_s[0] = t0; cw_h[0] = t0;

  for (int i = 1; i < n; i++) {
    double ti = tog[i];
    if (pids[i] == pids[i-1]) {
      double gap = dnums[i] - dnums[i-1];
      double df_r = std::exp(-gap / decay_r);
      double df_d = std::exp(-gap / decay_d);
      double df_s = std::exp(-gap / decay_s);
      double df_h = std::exp(-gap / decay_h);
      cr[i] = cr[i-1] * df_r + recv_epv[i] * ti;
      cd[i] = cd[i-1] * df_d + disp_epv[i] * ti;
      cs[i] = cs[i-1] * df_s + spoil_epv[i] * ti;
      ch[i] = ch[i-1] * df_h + hitout_epv[i] * ti;
      cw_r[i] = cw_r[i-1] * df_r + ti;
      cw_d[i] = cw_d[i-1] * df_d + ti;
      cw_s[i] = cw_s[i-1] * df_s + ti;
      cw_h[i] = cw_h[i-1] * df_h + ti;
    } else {
      cr[i] = recv_epv[i] * ti; cd[i] = disp_epv[i] * ti;
      cs[i] = spoil_epv[i] * ti; ch[i] = hitout_epv[i] * ti;
      cw_r[i] = ti; cw_d[i] = ti; cw_s[i] = ti; cw_h[i] = ti;
    }
  }
  return Rcpp::List::create(
    Rcpp::Named("cr") = cr, Rcpp::Named("cd") = cd,
    Rcpp::Named("cs") = cs, Rcpp::Named("ch") = ch,
    Rcpp::Named("cw_r") = cw_r, Rcpp::Named("cw_d") = cw_d,
    Rcpp::Named("cw_s") = cw_s, Rcpp::Named("cw_h") = cw_h
  );
}
')

#' Soft quantile: differentiable approximation using kernel-weighted sorted values.
#' Unlike stats::quantile() which is a step function w.r.t. q, this provides
#' smooth gradients so optimizers explore the landscape cleanly.
soft_quantile <- function(x, q, bandwidth = 0.05) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(0)
  sorted <- sort(x)
  ranks <- (seq_len(n) - 0.5) / n
  weights <- dnorm(ranks, mean = q, sd = bandwidth)
  ws <- sum(weights)
  if (ws < 1e-12) return(sorted[max(1, round(q * n))])
  sum(sorted * weights) / ws
}


#' Compute EPV (expected point value) from raw components given params
#' Returns a data.table with player_id, match_id, date, disp_epv, recv_epv,
#' spoil_epv, hitout_epv (before position adjustment)
compute_epv <- function(pgr, params, verbose = FALSE) {
  out <- data.table::copy(pgr)
  p <- params

  # Disposal EPV
  out[, disp_epv := (sum_depv_neg + n_neg * p["disp_neg_offset"]) * p["disp_scale"] +
                    (sum_depv_pos + n_pos * p["disp_pos_offset"]) * p["disp_scale"] +
                    inside50s * p["inside50s_wt"] + clangers * p["clangers_wt"] +
                    score_inv * p["score_involvements_wt"] +
                    kicks * p["kicks_wt"] + handballs * p["handballs_wt"] +
                    metres_gained * p["metres_gained_wt"] + turnovers_stat * p["turnovers_wt"] +
                    goal_assists * p["goal_assists_wt"] +
                    goals * p["goals_wt"] + behinds * p["behinds_wt"] +
                    shots_at_goal * p["shots_at_goal_wt"]]

  # Reception EPV (intercept marks get separate scale)
  out[, recv_epv := (p["recv_neg_mult"] * sum_depv_pt_neg + n_recv_neg * p["recv_neg_offset"]) * p["recv_scale"] +
                    (p["recv_neg_mult"] * sum_depv_pt_neg_im + n_recv_neg_im * p["recv_neg_offset"]) * p["recv_intercept_mark_scale"] +
                    (p["recv_pos_mult"] * sum_depv_pt_pos + n_recv_pos * p["recv_pos_offset"]) * p["recv_scale"] +
                    contested_poss * p["contested_poss_wt"] + contested_marks * p["contested_marks_wt"] +
                    ground_ball_gets * p["ground_ball_gets_wt"] + marks_inside50 * p["marks_inside50_wt"] +
                    marks_total * p["marks_wt"] + uncontested_poss * p["uncontested_poss_wt"] +
                    frees_for * p["frees_for_wt"]]

  # Spoil EPV
  out[, spoil_epv := spoils * p["spoil_wt"] + tackles * p["tackle_wt"] +
                     pressure_acts * p["pressure_wt"] + def_pressure * p["def_pressure_wt"] +
                     intercepts * p["intercepts_wt"] + one_percenters * p["one_percenters_wt"] +
                     rebound50s * p["rebound50s_wt"] + frees_against * p["frees_against_wt"]]

  # Hitout EPV
  out[, hitout_epv := hitouts * p["hitout_wt"] + hitouts_adv * p["hitout_adv_wt"] +
                      ruck_contests * p["ruck_contest_wt"]]

  # Contest EPV (pre-computed, scaled by optimizer)
  contest_sc <- if ("contest_scale" %in% names(p)) p["contest_scale"] else 1.0
  out[, contest_epv_scaled := contest_epv * contest_sc]

  # Per-80 normalisation first: divide by actual TOG so ratings are per-full-game.
  # Must happen BEFORE position adjustment so it operates on per-80 rates
  # (otherwise low-TOG players have the per-game adjustment amplified by 1/tog_safe).
  out[, `:=`(
    recv_epv   = recv_epv   / tog_safe,
    disp_epv   = disp_epv   / tog_safe,
    spoil_epv  = spoil_epv  / tog_safe,
    hitout_epv = hitout_epv / tog_safe,
    contest_epv_scaled = contest_epv_scaled / tog_safe
  )]

  # Position-group mean subtraction (on per-80 rates)
  if (verbose) {
    cat(sprintf("
  Position mean adjustment on per-80 rates:
"))
    cat(sprintf("  %-18s %8s %8s %8s %8s
", "position_group", "recv", "disp", "spoil", "hitout"))
    for (pg in sort(unique(out$position_group[!is.na(out$position_group)]))) {
      idx <- out$position_group == pg & !is.na(out$position_group)
      cat(sprintf("  %-18s %+8.3f %+8.3f %+8.3f %+8.3f
", pg,
        mean(out$recv_epv[idx], na.rm = TRUE),
        mean(out$disp_epv[idx], na.rm = TRUE),
        mean(out$spoil_epv[idx], na.rm = TRUE),
        mean(out$hitout_epv[idx], na.rm = TRUE)))
    }
  }
  # TOG-weighted centering by (position_group, season) to match
  # create_player_game_data(). Groups on the 6-way position class
  # (KEY_DEFENDER, MIDFIELDER, ...), NOT the 20-way lineup_position
  # (HBFR, INT, SUB, ...) which is too granular for meaningful centering.
  # Weighting by tog_safe prevents low-TOG players with noisy per-80 rates
  # from swinging the mean.
  out[!is.na(position_group), `:=`(
    recv_epv   = recv_epv   - weighted.mean(recv_epv, tog_safe, na.rm = TRUE),
    disp_epv   = disp_epv   - weighted.mean(disp_epv, tog_safe, na.rm = TRUE),
    spoil_epv  = spoil_epv  - weighted.mean(spoil_epv, tog_safe, na.rm = TRUE),
    hitout_epv = hitout_epv - weighted.mean(hitout_epv, tog_safe, na.rm = TRUE),
    contest_epv_scaled = contest_epv_scaled - weighted.mean(contest_epv_scaled, tog_safe, na.rm = TRUE)
  ), by = .(position_group, season)]

  return(out)
}

#' Compute cumulative decay-weighted sums per player
#'
#' @param epv_dt EPV-assigned player-game data (with recv_epv, disp_epv, etc.)
#' @param decay_recv Decay factor in days for receiving
#' @param decay_disp Decay factor in days for disposal
#' @param decay_spoil Decay factor in days for spoil
#' @param decay_hitout Decay factor in days for hitout
#' @return data.table with cumulative sums per player per game
compute_cumulative <- function(epv_dt, decay_recv, decay_disp = decay_recv,
                               decay_spoil = decay_recv, decay_hitout = decay_recv,
                               decay_contest = decay_recv) {
  dt <- data.table::copy(epv_dt)
  data.table::setorder(dt, player_id, date)
  dt[, date_num := as.numeric(date)]

  # Ensure contest_epv_scaled exists (backward compat)
  if (!"contest_epv_scaled" %in% names(dt)) dt[, contest_epv_scaled := 0]

  # TOG-weighted: sums accumulate epv * tog (game total),
  # weights accumulate tog (weighted minutes)
  dt[, {
    n <- .N
    cr <- cd <- cs <- ch <- cc <- cw_r <- cw_d <- cw_s <- cw_h <- cw_c <- numeric(n)
    for (i in seq_len(n)) {
      ti <- tog_safe[i]
      if (i == 1) {
        cr[i] <- recv_epv[i] * ti; cd[i] <- disp_epv[i] * ti
        cs[i] <- spoil_epv[i] * ti; ch[i] <- hitout_epv[i] * ti
        cc[i] <- contest_epv_scaled[i] * ti
        cw_r[i] <- ti; cw_d[i] <- ti; cw_s[i] <- ti; cw_h[i] <- ti; cw_c[i] <- ti
      } else {
        gap <- date_num[i] - date_num[i - 1]
        df_r <- exp(-gap / decay_recv)
        df_d <- exp(-gap / decay_disp)
        df_s <- exp(-gap / decay_spoil)
        df_h <- exp(-gap / decay_hitout)
        df_c <- exp(-gap / decay_contest)
        cr[i] <- cr[i - 1] * df_r + recv_epv[i] * ti
        cd[i] <- cd[i - 1] * df_d + disp_epv[i] * ti
        cs[i] <- cs[i - 1] * df_s + spoil_epv[i] * ti
        ch[i] <- ch[i - 1] * df_h + hitout_epv[i] * ti
        cc[i] <- cc[i - 1] * df_c + contest_epv_scaled[i] * ti
        cw_r[i] <- cw_r[i - 1] * df_r + ti
        cw_d[i] <- cw_d[i - 1] * df_d + ti
        cw_s[i] <- cw_s[i - 1] * df_s + ti
        cw_h[i] <- cw_h[i - 1] * df_h + ti
        cw_c[i] <- cw_c[i - 1] * df_c + ti
      }
    }
    .(match_id = match_id, date_num = date_num,
      cum_recv = cr, cum_disp = cd, cum_spoil = cs, cum_hitout = ch, cum_contest = cc,
      cum_wt_recv = cw_r, cum_wt_disp = cw_d, cum_wt_spoil = cw_s, cum_wt_hitout = cw_h, cum_wt_contest = cw_c)
  }, by = player_id]
}

#' Compute cumulative decay-weighted sum of a SINGLE vector per player
#' Much faster than compute_cumulative() when only one component changed
compute_cumulative_single <- function(pgr_sorted, col_name, decay_days) {
  vals <- pgr_sorted[[col_name]]
  togs <- pgr_sorted$tog_safe
  pids <- pgr_sorted$player_id
  dnums <- pgr_sorted$date_num
  n <- length(vals)
  cum <- numeric(n)

  # TOG-weighted: accumulate val * tog (game total)
  cum[1] <- vals[1] * togs[1]
  for (i in 2:n) {
    if (pids[i] == pids[i - 1]) {
      df <- exp(-(dnums[i] - dnums[i - 1]) / decay_days)
      cum[i] <- cum[i - 1] * df + vals[i] * togs[i]
    } else {
      cum[i] <- vals[i] * togs[i]
    }
  }
  cum
}

#' Fast objective for Stages 2-3: works on pre-sorted pgr with pre-computed
#' lineup join indices and cached cumulative component sums.
#'
#' @param par Full parameter vector
#' @param env Pre-computed environment with pgr_s, lineup join data, etc.
#' @return RMSE value
objective_fn_fast <- function(par, env) {
  # L-BFGS-B enforces bounds natively; keep safety clamp for any other caller
  p <- par

  # --- Compute per-game EPV directly on sorted vectors (no copy) ---
  disp_epv <- (env$sum_depv_neg + env$n_neg * p["disp_neg_offset"]) * p["disp_scale"] +
              (env$sum_depv_pos + env$n_pos * p["disp_pos_offset"]) * p["disp_scale"] +
              env$inside50s * p["inside50s_wt"] +
              env$clangers * p["clangers_wt"] +
              env$score_inv * p["score_involvements_wt"] +
              env$kicks * p["kicks_wt"] +
              env$handballs * p["handballs_wt"] +
              env$metres_gained * p["metres_gained_wt"] +
              env$turnovers_stat * p["turnovers_wt"] +
              env$goal_assists * p["goal_assists_wt"] +
              env$goals * p["goals_wt"] +
              env$behinds * p["behinds_wt"] +
              env$shots_at_goal * p["shots_at_goal_wt"]

  recv_epv <- (p["recv_neg_mult"] * env$sum_depv_pt_neg + env$n_recv_neg * p["recv_neg_offset"]) * p["recv_scale"] +
              (p["recv_neg_mult"] * env$sum_depv_pt_neg_im + env$n_recv_neg_im * p["recv_neg_offset"]) * p["recv_intercept_mark_scale"] +
              (p["recv_pos_mult"] * env$sum_depv_pt_pos + env$n_recv_pos * p["recv_pos_offset"]) * p["recv_scale"] +
              env$contested_poss * p["contested_poss_wt"] +
              env$contested_marks * p["contested_marks_wt"] +
              env$ground_ball_gets * p["ground_ball_gets_wt"] +
              env$marks_inside50 * p["marks_inside50_wt"] +
              env$marks_total * p["marks_wt"] +
              env$uncontested_poss * p["uncontested_poss_wt"] +
              env$frees_for * p["frees_for_wt"]

  spoil_epv <- env$spoils * p["spoil_wt"] +
               env$tackles * p["tackle_wt"] +
               env$pressure_acts * p["pressure_wt"] +
               env$def_pressure * p["def_pressure_wt"] +
               env$one_percenters * p["one_percenters_wt"] +
               env$frees_against * p["frees_against_wt"] +
               env$intercepts * p["intercepts_wt"] +
               env$rebound50s * p["rebound50s_wt"]

  hitout_epv <- env$hitouts * p["hitout_wt"] +
                env$hitouts_adv * p["hitout_adv_wt"] +
                env$ruck_contests * p["ruck_contest_wt"]

  # Contest EPV (pre-computed, scaled by optimizer)
  contest_sc <- if ("contest_scale" %in% names(p)) p["contest_scale"] else 1.0
  contest_epv <- env$contest_epv_raw * contest_sc

  # Per-80 normalisation first (before position adjustment, so adjustment
  # operates on per-80 rates rather than inflating per-game adjustments)
  disp_epv <- disp_epv / env$tog_safe; recv_epv <- recv_epv / env$tog_safe
  spoil_epv <- spoil_epv / env$tog_safe; hitout_epv <- hitout_epv / env$tog_safe
  contest_epv <- contest_epv / env$tog_safe

  # Replace any NaN/NA EPV with 0
  disp_epv[is.na(disp_epv)] <- 0; recv_epv[is.na(recv_epv)] <- 0
  spoil_epv[is.na(spoil_epv)] <- 0; hitout_epv[is.na(hitout_epv)] <- 0
  contest_epv[is.na(contest_epv)] <- 0

  # TOG-weighted position-season mean subtraction
  for (pos in env$position_levels) {
    idx <- env$pos_indices[[pos]]
    if (length(idx) > 0) {
      w <- env$tog_safe[idx]
      ws <- sum(w)
      recv_epv[idx]   <- recv_epv[idx]   - sum(recv_epv[idx] * w) / ws
      disp_epv[idx]   <- disp_epv[idx]   - sum(disp_epv[idx] * w) / ws
      spoil_epv[idx]  <- spoil_epv[idx]  - sum(spoil_epv[idx] * w) / ws
      hitout_epv[idx] <- hitout_epv[idx] - sum(hitout_epv[idx] * w) / ws
      contest_epv[idx] <- contest_epv[idx] - sum(contest_epv[idx] * w) / ws
    }
  }

  # --- Cumulative decay sums via Rcpp (per-component decay) ---
  decay_r <- p["decay_recv"]; decay_d <- p["decay_disp"]
  decay_s <- p["decay_spoil"]; decay_h <- p["decay_hitout"]
  pids <- env$pgr_s$player_id
  dnums <- env$pgr_s$date_num

  cum <- cumulative_decay_cpp(pids, dnums, recv_epv, disp_epv,
                              spoil_epv, hitout_epv, env$tog_safe,
                              decay_r, decay_d, decay_s, decay_h)
  cr <- cum$cr; cd <- cum$cd; cs <- cum$cs; ch <- cum$ch
  cw_r <- cum$cw_r; cw_d <- cum$cw_d; cw_s <- cum$cw_s; cw_h <- cum$cw_h

  # Contest cumulative (R-based, since Rcpp only handles 4 components)
  # Use pre-computed contest cumulative weights if available, otherwise compute
  if (!is.null(env$cum_contest_raw)) {
    cc <- env$cum_contest_raw * contest_sc
    cw_c <- env$cum_wt_contest_raw
  } else {
    cc <- rep(0, length(cr))
    cw_c <- rep(0, length(cr))
  }

  # --- Map to lineup via pre-computed indices ---
  loading      <- 1.0  # fixed — unidentifiable when scale params are free
  prior_recv   <- p["prior_games_recv"]
  prior_disp   <- p["prior_games_disp"]
  prior_spoil  <- p["prior_games_spoil"]
  prior_hitout <- p["prior_games_hitout"]
  pr_recv      <- p["prior_rate_recv"]
  pr_disp      <- p["prior_rate_disp"]
  pr_spoil     <- p["prior_rate_spoil"]
  pr_hitout    <- p["prior_rate_hitout"]

  lu_idx <- env$lu_pgr_idx  # maps each lineup row to pgr_s row
  lu_cr <- cr[lu_idx]; lu_cd <- cd[lu_idx]; lu_cs <- cs[lu_idx]; lu_ch <- ch[lu_idx]
  lu_cc <- cc[lu_idx]
  lu_cw_r <- cw_r[lu_idx]; lu_cw_d <- cw_d[lu_idx]
  lu_cw_s <- cw_s[lu_idx]; lu_cw_h <- cw_h[lu_idx]
  lu_cw_c <- cw_c[lu_idx]

  # Replace NAs (unmatched players) with 0
  lu_cr[is.na(lu_cr)] <- 0; lu_cd[is.na(lu_cd)] <- 0
  lu_cs[is.na(lu_cs)] <- 0; lu_ch[is.na(lu_ch)] <- 0
  lu_cc[is.na(lu_cc)] <- 0
  lu_cw_r[is.na(lu_cw_r)] <- 0; lu_cw_d[is.na(lu_cw_d)] <- 0
  lu_cw_s[is.na(lu_cw_s)] <- 0; lu_cw_h[is.na(lu_cw_h)] <- 0
  lu_cw_c[is.na(lu_cw_c)] <- 0

  # EPR per player-match (per-component shrinkage with per-component wt_gms)
  # Contest uses recv prior as its shrinkage target (same ballpark)
  prior_contest <- prior_recv
  epr_vec <- (loading * lu_cr + prior_recv * pr_recv) / (lu_cw_r + prior_recv) +
              (loading * lu_cd + prior_disp * pr_disp) / (lu_cw_d + prior_disp) +
              (loading * lu_cs + prior_spoil * pr_spoil) / (lu_cw_s + prior_spoil) +
              (loading * lu_ch + prior_hitout * pr_hitout) / (lu_cw_h + prior_hitout) +
              (loading * lu_cc + prior_contest * 0) / (lu_cw_c + prior_contest)

  # Match-level aggregation (weight per-80 EPR by lineup_tog)
  # rowsum is a compiled C primitive — much faster than tapply for grouped sums
  epr_weighted <- epr_vec * env$lu_lineup_tog
  home_sum <- rowsum(epr_weighted * env$lu_is_home, env$lu_match_idx, reorder = FALSE, na.rm = TRUE)
  away_sum <- rowsum(epr_weighted * env$lu_not_home, env$lu_match_idx, reorder = FALSE, na.rm = TRUE)
  epr_diff <- as.numeric(home_sum - away_sum)

  # Position balance penalty: penalize systematic position-group bias
  pos_means <- vapply(env$lu_pos_levels, function(pos) {
    idx <- env$lu_pos_idx[[pos]]
    tog <- env$lu_lineup_tog[idx]
    sum(epr_vec[idx] * tog, na.rm = TRUE) / sum(tog, na.rm = TRUE)
  }, numeric(1))
  balance_penalty <- POSITION_BALANCE_LAMBDA * sd(pos_means, na.rm = TRUE)

  # L2 (ridge) penalty on count-based stat weights
  l2_penalty <- STAT_WEIGHT_LAMBDA * sum(p[L2_PARAM_NAMES]^2)

  fast_rmse_cv(epr_diff, env$eval_matches) + balance_penalty + l2_penalty
}

#' Fast RMSE from epr_diff vector and eval match data
#' Forces epr_diff coefficient = 1 so EPR points map directly to margin points.
#' Only fits intercept + distance + familiarity controls on the residual.
fast_rmse <- function(epr_diff, eval_matches) {
  residual <- eval_matches$margin - epr_diff
  X <- cbind(1, eval_matches$log_dist_diff, eval_matches$familiarity_diff)
  valid <- complete.cases(X, residual)
  if (sum(valid) < 50) return(999)
  fit <- .lm.fit(X[valid, , drop = FALSE], residual[valid])
  sqrt(mean(fit$residuals^2))
}

#' Leave-one-season-out CV RMSE
#' Computes epr_diff once, then for each fold: fit controls on 3 seasons, test on 1.
#' Returns mean test RMSE across folds.
fast_rmse_cv <- function(epr_diff, eval_matches, cv_seasons = 2022:2025) {
  residual <- eval_matches$margin - epr_diff
  X <- cbind(1, eval_matches$log_dist_diff, eval_matches$familiarity_diff)
  valid <- complete.cases(X, residual)
  seasons <- eval_matches$season

  fold_rmses <- numeric(length(cv_seasons))
  for (k in seq_along(cv_seasons)) {
    test_mask  <- valid & (seasons == cv_seasons[k])
    train_mask <- valid & (seasons != cv_seasons[k])
    if (sum(train_mask) < 50 || sum(test_mask) < 10) { fold_rmses[k] <- 999; next }
    fit <- .lm.fit(X[train_mask, , drop = FALSE], residual[train_mask])
    test_pred <- X[test_mask, , drop = FALSE] %*% fit$coefficients
    fold_rmses[k] <- sqrt(mean((residual[test_mask] - test_pred)^2))
  }
  mean(fold_rmses)
}

#' Rolling join cumulative data to lineup, then compute match-level epr_diff
#'
#' @param player_cum Cumulative sums per player (from compute_cumulative)
#' @param lineup_dt Pre-computed lineup expansion
#' @param eval_matches Evaluation match data
#' @param loading Loading factor
#' @param prior_recv Prior games for reception
#' @param prior_disp Prior games for disposal
#' @param prior_spoil Prior games for spoil
#' @param prior_hitout Prior games for hitout
#' @return Numeric vector of epr_diff per match
compute_epr_diff <- function(player_cum, lineup_dt, eval_matches,
                              loading, prior_recv, prior_disp, prior_spoil, prior_hitout,
                              prior_contest = prior_recv,
                              pr_recv = EPR_PRIOR_RATE_RECV, pr_disp = EPR_PRIOR_RATE_DISP,
                              pr_spoil = EPR_PRIOR_RATE_SPOIL, pr_hitout = EPR_PRIOR_RATE_HITOUT,
                              pr_contest = 0) {
  has_contest <- "cum_contest" %in% names(player_cum)
  if (has_contest) {
    lookup <- player_cum[, .(player_id, date_num, cum_recv, cum_disp, cum_spoil, cum_hitout, cum_contest,
                             cum_wt_recv, cum_wt_disp, cum_wt_spoil, cum_wt_hitout, cum_wt_contest)]
  } else {
    lookup <- player_cum[, .(player_id, date_num, cum_recv, cum_disp, cum_spoil, cum_hitout,
                             cum_wt_recv, cum_wt_disp, cum_wt_spoil, cum_wt_hitout)]
  }
  data.table::setkey(lookup, player_id, date_num)

  if (has_contest) {
    joined <- lookup[lineup_dt,
      .(match_idx, is_home, lineup_tog = i.lineup_tog,
        cr = x.cum_recv, cd = x.cum_disp,
        cs = x.cum_spoil, ch = x.cum_hitout, cc = x.cum_contest,
        cw_r = x.cum_wt_recv, cw_d = x.cum_wt_disp,
        cw_s = x.cum_wt_spoil, cw_h = x.cum_wt_hitout, cw_c = x.cum_wt_contest),
      on = .(player_id, date_num), roll = TRUE]
  } else {
    joined <- lookup[lineup_dt,
      .(match_idx, is_home, lineup_tog = i.lineup_tog,
        cr = x.cum_recv, cd = x.cum_disp,
        cs = x.cum_spoil, ch = x.cum_hitout,
        cw_r = x.cum_wt_recv, cw_d = x.cum_wt_disp,
        cw_s = x.cum_wt_spoil, cw_h = x.cum_wt_hitout),
      on = .(player_id, date_num), roll = TRUE]
    joined[, `:=`(cc = 0, cw_c = 0)]
  }
  joined[is.na(lineup_tog), lineup_tog := 0.75]

  # Replace NAs with 0 before formula (per-component priors handle the shrinkage target)
  joined[is.na(cr), `:=`(cr = 0, cd = 0, cs = 0, ch = 0, cc = 0,
                         cw_r = 0, cw_d = 0, cw_s = 0, cw_h = 0, cw_c = 0)]

  # Compute EPR per player (per-component shrinkage with per-component wt_gms)
  joined[, player_epr :=
    (loading * cr + prior_recv * pr_recv) / (cw_r + prior_recv) +
    (loading * cd + prior_disp * pr_disp) / (cw_d + prior_disp) +
    (loading * cs + prior_spoil * pr_spoil) / (cw_s + prior_spoil) +
    (loading * ch + prior_hitout * pr_hitout) / (cw_h + prior_hitout) +
    (loading * cc + prior_contest * pr_contest) / (cw_c + prior_contest)
  ]
  joined[is.na(player_epr), player_epr := pr_recv + pr_disp + pr_spoil + pr_hitout + pr_contest]

  # Aggregate to match-level (weight per-80 EPR by lineup_tog)
  match_epr <- joined[, .(
    home_epr = sum(player_epr[is_home] * lineup_tog[is_home], na.rm = TRUE),
    away_epr = sum(player_epr[!is_home] * lineup_tog[!is_home], na.rm = TRUE)
  ), by = match_idx]
  data.table::setorder(match_epr, match_idx)

  match_epr$home_epr - match_epr$away_epr
}

#' Objective function: compute RMSE of margin ~ epr_diff + controls
#'
#' @param par Named numeric vector of all parameters
#' @param pgr Pre-computed player-game raw data
#' @param match_dt Pre-computed match data with lineups
#' @param train_seasons Seasons to use for evaluation (default 2022:2025)
#' @param eval_matches Pre-computed eval matches (optional, for speed)
#' @param lineup_dt Pre-computed lineup expansion (optional, for speed)
#' @return RMSE value
objective_fn <- function(par, pgr, match_dt, train_seasons = 2022:2025,
                         eval_matches = NULL, lineup_dt = NULL) {
  loading      <- par["loading"]
  prior_recv   <- par["prior_games_recv"]
  prior_disp   <- par["prior_games_disp"]
  prior_spoil  <- par["prior_games_spoil"]
  prior_hitout <- par["prior_games_hitout"]

  # Compute EPV-assigned points
  epv_dt <- compute_epv(pgr, par)

  # Compute cumulative decay-weighted sums (per-component decay)
  player_cum <- compute_cumulative(epv_dt, par["decay_recv"], par["decay_disp"],
                                   par["decay_spoil"], par["decay_hitout"],
                                   decay_contest = par["decay_recv"])

  # Use pre-computed eval data if available, otherwise build
  if (is.null(eval_matches)) {
    eval_matches <- match_dt[season %in% train_seasons]
  }
  if (is.null(lineup_dt)) {
    lineup_rows <- vector("list", nrow(eval_matches))
    for (i in seq_len(nrow(eval_matches))) {
      m <- eval_matches[i]
      home_ids <- m$home_players[[1]]
      away_ids <- m$away_players[[1]]
      lineup_rows[[i]] <- data.table::data.table(
        match_idx = i,
        player_id = c(home_ids, away_ids),
        is_home = c(rep(TRUE, length(home_ids)), rep(FALSE, length(away_ids))),
        date_num = as.numeric(m$date) - 0.5
      )
    }
    lineup_dt <- data.table::rbindlist(lineup_rows)
  }

  epr_diff <- compute_epr_diff(player_cum, lineup_dt, eval_matches,
                                 loading, prior_recv, prior_disp, prior_spoil, prior_hitout,
                                 pr_recv = par["prior_rate_recv"], pr_disp = par["prior_rate_disp"],
                                 pr_spoil = par["prior_rate_spoil"], pr_hitout = par["prior_rate_hitout"])
  fast_rmse_cv(epr_diff, eval_matches)
}

# 4. Parameter Setup ----

# Read current defaults from package constants (loaded via devtools::load_all())
par_defaults <- c(
  # --- EPV/scale params (disp) ---
  disp_neg_offset        = EPV_DISP_NEG_OFFSET,
  disp_pos_offset        = EPV_DISP_POS_OFFSET,
  disp_scale             = EPV_DISP_SCALE,
  # --- EPV/scale params (recv) ---
  recv_neg_mult          = EPV_RECV_NEG_MULT,
  recv_neg_offset        = EPV_RECV_NEG_OFFSET,
  recv_pos_mult          = EPV_RECV_POS_MULT,
  recv_pos_offset        = EPV_RECV_POS_OFFSET,
  recv_scale             = EPV_RECV_SCALE,
  recv_intercept_mark_scale = EPV_RECV_INTERCEPT_MARK_SCALE,
  # --- Stat weights: disp component ---
  inside50s_wt           = EPV_INSIDE50S_WT,
  clangers_wt            = EPV_CLANGERS_WT,
  score_involvements_wt  = EPV_SCORE_INVOLVEMENTS_WT,
  kicks_wt               = EPV_KICKS_WT,
  handballs_wt           = EPV_HANDBALLS_WT,
  metres_gained_wt       = EPV_METRES_GAINED_WT,
  turnovers_wt           = EPV_TURNOVERS_WT,
  goal_assists_wt        = EPV_GOAL_ASSISTS_WT,
  # --- Stat weights: recv component ---
  contested_poss_wt      = EPV_CONTESTED_POSS_WT,
  contested_marks_wt     = EPV_CONTESTED_MARKS_WT,
  ground_ball_gets_wt    = EPV_GROUND_BALL_GETS_WT,
  goals_wt               = EPV_GOALS_WT,
  marks_inside50_wt      = EPV_MARKS_INSIDE50_WT,
  behinds_wt             = EPV_BEHINDS_WT,
  marks_wt               = EPV_MARKS_WT,
  uncontested_poss_wt    = EPV_UNCONTESTED_POSS_WT,
  shots_at_goal_wt       = EPV_SHOTS_AT_GOAL_WT,
  # --- Stat weights: spoil component ---
  spoil_wt               = EPV_SPOIL_WT,
  tackle_wt              = EPV_TACKLE_WT,
  pressure_wt            = EPV_PRESSURE_WT,
  def_pressure_wt        = EPV_DEF_PRESSURE_WT,
  intercepts_wt          = EPV_INTERCEPTS_WT,
  one_percenters_wt      = EPV_ONE_PERCENTERS_WT,
  rebound50s_wt          = EPV_REBOUND50S_WT,
  frees_against_wt       = EPV_FREES_AGAINST_WT,
  # --- Stat weights: hitout component ---
  hitout_wt              = EPV_HITOUT_WT,
  hitout_adv_wt          = EPV_HITOUT_ADV_WT,
  ruck_contest_wt        = EPV_RUCK_CONTEST_WT,
  frees_for_wt           = EPV_FREES_FOR_WT,
  # --- Contest credit scale ---
  contest_scale          = 1.0,
  # --- Aggregation params ---
  decay_recv             = EPR_DECAY_RECV,
  decay_disp             = EPR_DECAY_DISP,
  decay_spoil            = EPR_DECAY_SPOIL,
  decay_hitout           = EPR_DECAY_HITOUT,
  loading                = EPR_LOADING_DEFAULT,
  prior_games_recv       = EPR_PRIOR_GAMES_RECV,
  prior_games_disp       = EPR_PRIOR_GAMES_DISP,
  prior_games_spoil      = EPR_PRIOR_GAMES_SPOIL,
  prior_games_hitout     = EPR_PRIOR_GAMES_HITOUT,
  prior_rate_recv        = EPR_PRIOR_RATE_RECV,
  prior_rate_disp        = EPR_PRIOR_RATE_DISP,
  prior_rate_spoil       = EPR_PRIOR_RATE_SPOIL,
  prior_rate_hitout      = EPR_PRIOR_RATE_HITOUT
)

# Convert raw-scale defaults to normalized-scale (multiply by SD)
# Constants are in per-raw-unit; optimizer works in per-SD-unit
param_stat_map <- c(
  spoil_wt = "spoils", tackle_wt = "tackles",
  pressure_wt = "pressure_acts", def_pressure_wt = "def_pressure",
  hitout_wt = "hitouts", hitout_adv_wt = "hitouts_adv", ruck_contest_wt = "ruck_contests",
  contested_poss_wt = "contested_poss", contested_marks_wt = "contested_marks",
  ground_ball_gets_wt = "ground_ball_gets", marks_inside50_wt = "marks_inside50",
  inside50s_wt = "inside50s", clangers_wt = "clangers", score_involvements_wt = "score_inv",
  intercepts_wt = "intercepts", one_percenters_wt = "one_percenters",
  rebound50s_wt = "rebound50s", frees_against_wt = "frees_against",
  frees_for_wt = "frees_for",
  goals_wt = "goals", behinds_wt = "behinds", marks_wt = "marks_total",
  uncontested_poss_wt = "uncontested_poss", shots_at_goal_wt = "shots_at_goal",
  kicks_wt = "kicks", handballs_wt = "handballs", metres_gained_wt = "metres_gained",
  turnovers_wt = "turnovers_stat", goal_assists_wt = "goal_assists"
)
for (nm in names(param_stat_map)) {
  par_defaults[nm] <- par_defaults[nm] * stat_sds[param_stat_map[nm]]
}

# Lower bounds (widened; stat weight bounds now [-10, 10] since normalization changes scale)
par_lower <- c(
  # --- EPV/scale params (disp) ---
  disp_neg_offset        = 0,
  disp_pos_offset        = 0,
  disp_scale             = 0.5,
  # --- EPV/scale params (recv) ---
  recv_neg_mult          = 1,
  recv_neg_offset        = 0,
  recv_pos_mult          = 1,
  recv_pos_offset        = 0,
  recv_scale             = 0.5,
  recv_intercept_mark_scale = 1,
  # --- Stat weights: disp component (all [-10, 10]; L2 provides real constraint) ---
  inside50s_wt           = -10,
  clangers_wt            = -10,
  score_involvements_wt  = -10,
  kicks_wt               = -10,
  handballs_wt           = -10,
  metres_gained_wt       = -10,
  turnovers_wt           = -10,
  goal_assists_wt        = -10,
  # --- Stat weights: recv component ---
  contested_poss_wt      = -10,
  contested_marks_wt     = -10,
  ground_ball_gets_wt    = -10,
  marks_inside50_wt      = -10,
  goals_wt               = -10,
  behinds_wt             = -10,
  marks_wt               = -10,
  uncontested_poss_wt    = -10,
  shots_at_goal_wt       = -10,
  # --- Stat weights: spoil component ---
  spoil_wt               = -10,
  tackle_wt              = -10,
  pressure_wt            = -10,
  def_pressure_wt        = -10,
  intercepts_wt          = -10,
  one_percenters_wt      = -10,
  rebound50s_wt          = -10,
  frees_against_wt       = -10,
  # --- Stat weights: hitout component ---
  hitout_wt              = -10,
  hitout_adv_wt          = -10,
  ruck_contest_wt        = -10,
  frees_for_wt           = -10,
  # --- Contest credit scale (fixed) ---
  contest_scale          = 1,
  # --- Aggregation params ---
  decay_recv             = 100,
  decay_disp             = 100,
  decay_spoil            = 100,
  decay_hitout           = 100,
  loading                = 1.0,
  prior_games_recv       = 3,
  prior_games_disp       = 3,
  prior_games_spoil      = 3,
  prior_games_hitout     = 3,
  prior_rate_recv        = -3,
  prior_rate_disp        = -3,
  prior_rate_spoil       = -3,
  prior_rate_hitout      = -3
)

par_upper <- c(
  # --- EPV/scale params (disp) ---
  disp_neg_offset        = 0,
  disp_pos_offset        = 0,
  disp_scale             = 0.5,
  # --- EPV/scale params (recv) ---
  recv_neg_mult          = 1,
  recv_neg_offset        = 0,
  recv_pos_mult          = 1,
  recv_pos_offset        = 0,
  recv_scale             = 0.5,
  recv_intercept_mark_scale = 1,
  # --- Stat weights: disp component ---
  inside50s_wt           = 10,
  clangers_wt            = 10,
  score_involvements_wt  = 10,
  kicks_wt               = 10,
  handballs_wt           = 10,
  metres_gained_wt       = 10,
  turnovers_wt           = 10,
  goal_assists_wt        = 10,
  # --- Stat weights: recv component ---
  contested_poss_wt      = 10,
  contested_marks_wt     = 10,
  ground_ball_gets_wt    = 10,
  marks_inside50_wt      = 10,
  goals_wt               = 10,
  behinds_wt             = 10,
  marks_wt               = 10,
  uncontested_poss_wt    = 10,
  shots_at_goal_wt       = 10,
  # --- Stat weights: spoil component ---
  spoil_wt               = 10,
  tackle_wt              = 10,
  pressure_wt            = 10,
  def_pressure_wt        = 10,
  intercepts_wt          = 10,
  one_percenters_wt      = 10,
  rebound50s_wt          = 10,
  frees_against_wt       = 10,
  # --- Stat weights: hitout component ---
  hitout_wt              = 10,
  hitout_adv_wt          = 10,
  ruck_contest_wt        = 10,
  frees_for_wt           = 10,
  # --- Contest credit scale (fixed) ---
  contest_scale          = 1,
  # --- Aggregation params ---
  decay_recv             = 700,
  decay_disp             = 700,
  decay_spoil            = 700,
  decay_hitout           = 700,
  loading                = 1.0,
  prior_games_recv       = 15,
  prior_games_disp       = 15,
  prior_games_spoil      = 15,
  prior_games_hitout     = 15,
  prior_rate_recv        = 0,
  prior_rate_disp        = 0,
  prior_rate_spoil       = 0,
  prior_rate_hitout      = 0
)

# Clamp defaults to bounds (prevents x0 > ub errors when bounds are tightened)
par_defaults <- pmax(par_lower, pmin(par_upper, par_defaults))

cat("Current parameter values (from R/constants.R):\n")
cat(sprintf("  %s\n", paste(names(par_defaults), sprintf("%.4f", par_defaults), sep = " = ")))

# 4b. No-Ratings Baseline (LOOCV) ----
cat("\nComputing no-ratings baseline (distance + familiarity only, LOOCV)...\n")
no_rating_df <- match_dt[season %in% 2022:2025, .(season, margin, log_dist_diff, familiarity_diff)]
no_rating_df <- no_rating_df[complete.cases(no_rating_df)]
no_rating_rmses <- numeric(4)
for (k in seq_along(2022:2025)) {
  s <- (2022:2025)[k]
  train_nr <- no_rating_df[season != s]
  test_nr  <- no_rating_df[season == s]
  fit_nr <- lm(margin ~ log_dist_diff + familiarity_diff, data = train_nr)
  pred_nr <- predict(fit_nr, test_nr)
  no_rating_rmses[k] <- sqrt(mean((test_nr$margin - pred_nr)^2))
}
no_rating_rmse <- mean(no_rating_rmses)
cat(sprintf("No-ratings baseline RMSE: %.4f (%d matches, LOOCV)\n", no_rating_rmse, nrow(no_rating_df)))
cat(sprintf("  Per-fold: %s\n", paste(sprintf("%.2f", no_rating_rmses), collapse = ", ")))

# 5. Baseline RMSE ----
cat("\nComputing baseline RMSE with default parameters...\n")
tictoc::tic("Baseline")
baseline_rmse_pure <- objective_fn(par_defaults, pgr, match_dt, train_seasons = 2022:2025,
                                   eval_matches = eval_matches_all, lineup_dt = lineup_dt_all)
# Add same penalties used in optimization so comparisons are consistent
baseline_l2 <- STAT_WEIGHT_LAMBDA * sum(par_defaults[L2_PARAM_NAMES]^2)
# Position balance penalty can't be computed yet (needs lineup data from Stage 1 setup),
# but it's the same constant added to both baseline and grid search, so it cancels out.
# We include L2 here since it changes across stages when stat weights are optimized.
baseline_rmse <- baseline_rmse_pure + baseline_l2
tictoc::toc()
cat(sprintf("Default-params baseline RMSE: %.4f (pure: %.4f, L2 penalty: %.4f)\n\n",
            baseline_rmse, baseline_rmse_pure, baseline_l2))

# 6. Staged Optimization ----

## Stage 1: Grid search on aggregation params ----
cat("=== Stage 1: Grid search on aggregation params ===\n")
tictoc::tic("Stage 1")

agg_grid <- expand.grid(
  decay_shared       = c(350, 450, 550, 650),
  loading            = 1.0,
  prior_games_recv   = c(3, 5, 7, 9),
  prior_games_disp   = c(3, 5, 7, 9),
  prior_games_spoil  = c(3, 5, 7, 9),
  prior_games_hitout = c(3, 5, 7, 9)
)

cat(sprintf("  Grid has %d combinations\n", nrow(agg_grid)))

# --- Fast Stage 1: pre-compute EPV once + cumulative sums per decay ---
# Credit params are fixed during Stage 1, only agg params vary.
# All 4 decay components share one value during grid search (Nelder-Mead diverges them later).
cat("  Pre-computing EPV (fixed during Stage 1)...\n")
s1_epv_dt <- compute_epv(pgr, par_defaults)

decay_values <- sort(unique(agg_grid$decay_shared))
cat(sprintf("  Pre-computing cumulative sums for %d decay values...\n", length(decay_values)))

# For each decay value: compute cumulative sums (shared across components), rolling join to lineup
decay_precomp <- list()
for (d in decay_values) {
  cat(sprintf("    decay=%d...\n", d))
  pcum <- compute_cumulative(s1_epv_dt, d)  # all 4 components share same decay

  # Rolling join to pre-computed lineup
  lookup <- pcum[, .(player_id, date_num, cum_recv, cum_disp, cum_spoil, cum_hitout, cum_wt_recv)]
  data.table::setkey(lookup, player_id, date_num)

  joined <- lookup[lineup_dt_all,
    .(match_idx = i.match_idx, is_home = i.is_home, lineup_tog = i.lineup_tog,
      cr = x.cum_recv, cd = x.cum_disp, cs = x.cum_spoil,
      ch = x.cum_hitout, cw = x.cum_wt_recv),
    on = .(player_id, date_num), roll = TRUE]

  # Replace NAs with 0 for players not found
  for (col in c("cr", "cd", "cs", "ch", "cw"))
    data.table::set(joined, which(is.na(joined[[col]])), col, 0)
  joined[is.na(lineup_tog), lineup_tog := 0.75]

  decay_precomp[[as.character(d)]] <- joined
}

cat("  Running grid search...\n")
best_rmse <- baseline_rmse
best_par <- par_defaults

for (i in seq_len(nrow(agg_grid))) {
  d  <- as.character(agg_grid$decay_shared[i])
  ld <- agg_grid$loading[i]
  pr <- agg_grid$prior_games_recv[i]
  pd <- agg_grid$prior_games_disp[i]
  ps <- agg_grid$prior_games_spoil[i]
  ph <- agg_grid$prior_games_hitout[i]

  j <- decay_precomp[[d]]

  # Vectorized EPR per player-match (~37K elements)
  # Use default per-component priors for Stage 1 (optimized in Stage 2)
  # All 4 cum_wt columns are identical during Stage 1 (shared decay)
  s1_pr_r <- par_defaults["prior_rate_recv"]
  s1_pr_d <- par_defaults["prior_rate_disp"]
  s1_pr_s <- par_defaults["prior_rate_spoil"]
  s1_pr_h <- par_defaults["prior_rate_hitout"]
  epr_vec <- (ld * j$cr + pr * s1_pr_r) / (j$cw + pr) +
              (ld * j$cd + pd * s1_pr_d) / (j$cw + pd) +
              (ld * j$cs + ps * s1_pr_s) / (j$cw + ps) +
              (ld * j$ch + ph * s1_pr_h) / (j$cw + ph)

  # Aggregate to match-level epr_diff (weight per-80 EPR by lineup_tog)
  epr_weighted <- epr_vec * j$lineup_tog
  home_sum <- rowsum(epr_weighted * j$is_home, j$match_idx, reorder = FALSE, na.rm = TRUE)
  away_sum <- rowsum(epr_weighted * (!j$is_home), j$match_idx, reorder = FALSE, na.rm = TRUE)
  epr_diff <- as.numeric(home_sum - away_sum)

  # Position balance penalty
  pos_means_s1 <- vapply(lu_pos_levels, function(pos) {
    idx <- lu_pos_idx[[pos]]
    tog <- j$lineup_tog[idx]
    sum(epr_vec[idx] * tog, na.rm = TRUE) / sum(tog, na.rm = TRUE)
  }, numeric(1))
  balance_penalty_s1 <- POSITION_BALANCE_LAMBDA * sd(pos_means_s1, na.rm = TRUE)

  # Leave-one-season-out CV RMSE + balance penalty + L2 penalty
  # L2 is constant during Stage 1 (EPV params fixed), but must be included
  # so the baseline comparison is consistent with Stages 2-3
  rmse_i <- fast_rmse_cv(epr_diff, eval_matches_all) + balance_penalty_s1 + baseline_l2

  if (rmse_i < best_rmse) {
    best_rmse <- rmse_i
    best_par <- par_defaults
    best_par["decay_recv"]         <- agg_grid$decay_shared[i]
    best_par["decay_disp"]         <- agg_grid$decay_shared[i]
    best_par["decay_spoil"]        <- agg_grid$decay_shared[i]
    best_par["decay_hitout"]       <- agg_grid$decay_shared[i]
    best_par["loading"]            <- ld
    best_par["prior_games_recv"]   <- pr
    best_par["prior_games_disp"]   <- pd
    best_par["prior_games_spoil"]  <- ps
    best_par["prior_games_hitout"] <- ph
    cat(sprintf("  [%d/%d] New best RMSE: %.4f (decay=%.0f, pr=%.0f, pd=%.0f, ps=%.0f, ph=%.0f)\n",
                i, nrow(agg_grid), rmse_i,
                agg_grid$decay_shared[i], pr, pd, ps, ph))
  }

  if (i %% 500 == 0) cat(sprintf("  [%d/%d] Current best: %.4f\n", i, nrow(agg_grid), best_rmse))
}

# Clean up Stage 1 pre-computed data
rm(s1_epv_dt, decay_precomp)

tictoc::toc()
cat(sprintf("Stage 1 best RMSE: %.4f\n\n", best_rmse))

## Pre-compute environment for fast Stages 2-3 ----
cat("Pre-computing fast objective environment...\n")
tictoc::tic("Fast env setup")

# Sort pgr by player_id, date and extract columns as plain vectors
pgr_s <- data.table::copy(pgr[!is.na(player_id)])
data.table::setorder(pgr_s, player_id, date)
pgr_s[, date_num := as.numeric(date)]

fast_env <- list(
  # Raw component vectors (same order as pgr_s)
  sum_depv_neg   = pgr_s$sum_depv_neg,
  n_neg          = pgr_s$n_neg,
  sum_depv_pos   = pgr_s$sum_depv_pos,
  n_pos          = pgr_s$n_pos,
  sum_depv_pt_neg = pgr_s$sum_depv_pt_neg,
  n_recv_neg     = pgr_s$n_recv_neg,
  sum_depv_pt_neg_im = pgr_s$sum_depv_pt_neg_im,
  n_recv_neg_im  = pgr_s$n_recv_neg_im,
  sum_depv_pt_pos = pgr_s$sum_depv_pt_pos,
  n_recv_pos     = pgr_s$n_recv_pos,
  spoils         = pgr_s$spoils,
  tackles        = pgr_s$tackles,
  pressure_acts  = pgr_s$pressure_acts,
  def_pressure   = pgr_s$def_pressure,
  hitouts        = pgr_s$hitouts,
  hitouts_adv    = pgr_s$hitouts_adv,
  ruck_contests  = pgr_s$ruck_contests,
  contested_poss = pgr_s$contested_poss,
  contested_marks = pgr_s$contested_marks,
  ground_ball_gets = pgr_s$ground_ball_gets,
  marks_inside50 = pgr_s$marks_inside50,
  inside50s      = pgr_s$inside50s,
  clangers       = pgr_s$clangers,
  score_inv      = pgr_s$score_inv,
  intercepts     = pgr_s$intercepts,
  one_percenters = pgr_s$one_percenters,
  rebound50s     = pgr_s$rebound50s,
  frees_against  = pgr_s$frees_against,
  clearances     = pgr_s$clearances,
  frees_for      = pgr_s$frees_for,
  goals          = pgr_s$goals,
  behinds        = pgr_s$behinds,
  marks_total    = pgr_s$marks_total,
  uncontested_poss = pgr_s$uncontested_poss,
  shots_at_goal  = pgr_s$shots_at_goal,
  kicks          = pgr_s$kicks,
  handballs      = pgr_s$handballs,
  metres_gained  = pgr_s$metres_gained,
  turnovers_stat = pgr_s$turnovers_stat,
  goal_assists   = pgr_s$goal_assists,
  tog_safe       = pgr_s$tog_safe,
  contest_epv_raw = if ("contest_epv" %in% names(pgr_s)) pgr_s$contest_epv else rep(0, nrow(pgr_s)),
  cum_contest_raw = NULL,
  cum_wt_contest_raw = NULL,
  # Player/date vectors for cumulative loop
  pgr_s          = pgr_s[, .(player_id = as.integer(factor(player_id)), date_num)],
  # Position indices for quantile adjustment
  position_levels = NULL,
  pos_indices     = NULL,
  # Lineup mapping
  lu_pgr_idx     = NULL,
  lu_is_home     = NULL,
  lu_lineup_tog  = NULL,
  lu_match_idx   = NULL,
  eval_matches   = eval_matches_all
)

# Pre-compute position-season group indices (matches per-season centering in pipeline)
positions <- pgr_s$position
seasons_vec <- pgr_s$season
has_pos <- !is.na(positions)
pos_season_pairs <- unique(data.table::data.table(
  position = positions[has_pos],
  season = seasons_vec[has_pos]
))
fast_env$position_levels <- paste0(pos_season_pairs$position, ":", pos_season_pairs$season)
fast_env$pos_indices <- lapply(seq_len(nrow(pos_season_pairs)), function(i) {
  which(positions == pos_season_pairs$position[i] & seasons_vec == pos_season_pairs$season[i])
})
names(fast_env$pos_indices) <- fast_env$position_levels

# Pre-compute contest cumulative sums at scale=1 (fast path scales by contest_scale)
if ("contest_epv" %in% names(pgr_s) && any(pgr_s$contest_epv != 0, na.rm = TRUE)) {
  cat("  Pre-computing contest cumulative sums...\n")
  pgr_s_sorted <- data.table::copy(pgr_s)
  data.table::setorder(pgr_s_sorted, player_id, date_num)
  contest_vals <- pgr_s_sorted$contest_epv / pgr_s_sorted$tog_safe
  contest_vals[is.na(contest_vals)] <- 0
  # Position-center contest_epv (per-80, same as other components)
  for (pos in fast_env$position_levels) {
    idx <- fast_env$pos_indices[[pos]]
    if (length(idx) > 0) {
      w <- pgr_s_sorted$tog_safe[idx]
      contest_vals[idx] <- contest_vals[idx] - sum(contest_vals[idx] * w) / sum(w)
    }
  }
  # Compute cumulative decay (R-based, using recv decay as default)
  fast_env$cum_contest_raw <- compute_cumulative_single(pgr_s_sorted, "contest_epv", EPR_DECAY_RECV)
  fast_env$cum_wt_contest_raw <- compute_cumulative_single(
    data.table::data.table(player_id = pgr_s_sorted$player_id, date_num = pgr_s_sorted$date_num,
                           tog_safe = pgr_s_sorted$tog_safe, ones = 1),
    "ones", EPR_DECAY_RECV
  )
}

# Pre-compute lineup -> pgr_s rolling join index
# For each lineup row (player_id, date_num), find the last pgr_s row <= that date
pgr_s[, pgr_row := .I]  # row index in pgr_s
data.table::setkey(pgr_s, player_id, date_num)

lu_joined <- pgr_s[lineup_dt_all, .(pgr_row = x.pgr_row, match_idx = i.match_idx,
                                    is_home = i.is_home, lineup_tog = i.lineup_tog),
                   on = .(player_id, date_num), roll = TRUE]

fast_env$lu_pgr_idx    <- lu_joined$pgr_row
fast_env$lu_is_home    <- lu_joined$is_home
fast_env$lu_not_home   <- !lu_joined$is_home
fast_env$lu_lineup_tog <- lu_joined$lineup_tog
fast_env$lu_match_idx  <- lu_joined$match_idx
fast_env$lu_pos_levels <- lu_pos_levels
fast_env$lu_pos_idx    <- lu_pos_idx

tictoc::toc()
cat(sprintf("  Fast env: %d pgr rows, %d lineup rows\n", nrow(pgr_s), nrow(lu_joined)))

# Fast objective wrapper
obj_fast <- function(par) objective_fn_fast(par, fast_env)

## Stage 2: BOBYQA derivative-free optimization ----
# loading is fixed at 1.0 (unidentifiable when scales are free), excluded from optimization
cat("=== Stage 2: BOBYQA derivative-free optimization (all params except loading) ===
")
tictoc::tic("Stage 2 BOBYQA")

# Remove loading from optimization
# Exclude loading (unidentifiable) and prior_rate_* (set empirically from first-game EPV data)
fixed_params <- c("loading", "prior_rate_recv", "prior_rate_disp", "prior_rate_spoil", "prior_rate_hitout")
optim_names <- setdiff(names(best_par), fixed_params)
n_optim <- length(optim_names)
cat(sprintf("  Optimizing %d parameters (fixed: %s)
", n_optim, paste(fixed_params, collapse = ", ")))

# nloptr wrapper: convert unnamed vector <-> named vector
nloptr_fn <- function(x) {
  par <- best_par
  par[optim_names] <- x
  obj_fast(par)
}

# Track progress
s2_eval_count <- 0L
s2_best_rmse <- obj_fast(best_par)
s2_start_time <- proc.time()[3]

nloptr_fn_verbose <- function(x) {
  rmse <- nloptr_fn(x)
  s2_eval_count <<- s2_eval_count + 1L
  if (rmse < s2_best_rmse) s2_best_rmse <<- rmse
  if (s2_eval_count %% 50 == 0) {
    elapsed <- proc.time()[3] - s2_start_time
    cat(sprintf("  [%4d evals] best RMSE: %.4f | current: %.4f | %.0fs
",
                s2_eval_count, s2_best_rmse, rmse, elapsed))
  }
  rmse
}

# BOBYQA: quadratic model-based derivative-free optimizer
opt_bobyqa <- nloptr::nloptr(
  x0 = unname(best_par[optim_names]),
  eval_f = nloptr_fn_verbose,
  lb = unname(par_lower[optim_names]),
  ub = unname(par_upper[optim_names]),
  opts = list(
    algorithm = "NLOPT_LN_BOBYQA",
    maxeval = 5000,
    ftol_rel = 1e-7,
    xtol_rel = 1e-6
  )
)

best_par[optim_names] <- opt_bobyqa$solution
bobyqa_rmse <- obj_fast(best_par)
tictoc::toc()
cat(sprintf("Stage 2 BOBYQA RMSE: %.4f (started at %.4f, gain: %.4f)

",
            bobyqa_rmse, s2_best_rmse, s2_best_rmse - bobyqa_rmse))

## Stage 3: Subplex polish from BOBYQA solution ----
# Subplex uses a different search geometry (simplex-based) and often finds
# improvements that BOBYQA misses, especially in shallow valleys.
cat("=== Stage 3: Subplex polish (different search geometry) ===
")
tictoc::tic("Stage 3 Subplex")

s3_eval_count <- 0L
s3_best_rmse <- bobyqa_rmse
s3_start_time <- proc.time()[3]

nloptr_fn_verbose_s3 <- function(x) {
  rmse <- nloptr_fn(x)
  s3_eval_count <<- s3_eval_count + 1L
  if (rmse < s3_best_rmse) s3_best_rmse <<- rmse
  if (s3_eval_count %% 50 == 0) {
    elapsed <- proc.time()[3] - s3_start_time
    cat(sprintf("  [%4d evals] best RMSE: %.4f | current: %.4f | %.0fs
",
                s3_eval_count, s3_best_rmse, rmse, elapsed))
  }
  rmse
}

# Clamp to bounds (BOBYQA can return values at floating-point boundary)
sbplx_x0 <- pmin(pmax(opt_bobyqa$solution, unname(par_lower[optim_names])), unname(par_upper[optim_names]))

opt_sbplx <- nloptr::nloptr(
  x0 = sbplx_x0,
  eval_f = nloptr_fn_verbose_s3,
  lb = unname(par_lower[optim_names]),
  ub = unname(par_upper[optim_names]),
  opts = list(
    algorithm = "NLOPT_LN_SBPLX",
    maxeval = 5000,
    ftol_rel = 1e-7,
    xtol_rel = 1e-6
  )
)

best_par[optim_names] <- opt_sbplx$solution
sbplx_rmse <- obj_fast(best_par)
tictoc::toc()
cat(sprintf("Stage 3 Subplex RMSE: %.4f (was %.4f, gain: %.4f)

",
            sbplx_rmse, bobyqa_rmse, bobyqa_rmse - sbplx_rmse))

final_rmse <- sbplx_rmse
cat(sprintf("
Final optimized RMSE (LOOCV): %.4f

", final_rmse))

# 7. Per-fold RMSE breakdown ----
cat("=== Per-fold RMSE breakdown (leave-one-season-out) ===\n")

# Recompute epr_diff with optimized params for fold breakdown
# Use the fast objective internals to get epr_diff
# We already have final_rmse, now compute per-fold details

# Compute epr_diff using objective_fn's pipeline for reporting
epv_dt_final <- compute_epv(pgr, best_par, verbose = TRUE)
pcum_final <- compute_cumulative(epv_dt_final, best_par["decay_recv"], best_par["decay_disp"],
                                 best_par["decay_spoil"], best_par["decay_hitout"])
lookup_final <- pcum_final[, .(player_id, date_num, cum_recv, cum_disp, cum_spoil, cum_hitout,
                               cum_wt_recv, cum_wt_disp, cum_wt_spoil, cum_wt_hitout)]
data.table::setkey(lookup_final, player_id, date_num)
joined_final <- lookup_final[lineup_dt_all,
  .(match_idx = i.match_idx, is_home = i.is_home, lineup_tog = i.lineup_tog,
    cr = x.cum_recv, cd = x.cum_disp, cs = x.cum_spoil, ch = x.cum_hitout,
    cw_r = x.cum_wt_recv, cw_d = x.cum_wt_disp, cw_s = x.cum_wt_spoil, cw_h = x.cum_wt_hitout),
  on = .(player_id, date_num), roll = TRUE]
for (col in c("cr", "cd", "cs", "ch", "cw_r", "cw_d", "cw_s", "cw_h"))
  data.table::set(joined_final, which(is.na(joined_final[[col]])), col, 0)
joined_final[is.na(lineup_tog), lineup_tog := 0.75]

ld <- best_par["loading"]; pr <- best_par["prior_games_recv"]
pd <- best_par["prior_games_disp"]; ps <- best_par["prior_games_spoil"]
ph <- best_par["prior_games_hitout"]
prr <- best_par["prior_rate_recv"]; prd <- best_par["prior_rate_disp"]
prs <- best_par["prior_rate_spoil"]; prh <- best_par["prior_rate_hitout"]
epr_vec_final <- (ld * joined_final$cr + pr * prr) / (joined_final$cw_r + pr) +
                  (ld * joined_final$cd + pd * prd) / (joined_final$cw_d + pd) +
                  (ld * joined_final$cs + ps * prs) / (joined_final$cw_s + ps) +
                  (ld * joined_final$ch + ph * prh) / (joined_final$cw_h + ph)
epr_weighted_f <- epr_vec_final * joined_final$lineup_tog
home_sum_f <- tapply(epr_weighted_f * joined_final$is_home, joined_final$match_idx, sum, na.rm = TRUE)
away_sum_f <- tapply(epr_weighted_f * (!joined_final$is_home), joined_final$match_idx, sum, na.rm = TRUE)
epr_diff_final <- as.numeric(home_sum_f - away_sum_f)

# Per-fold RMSE for optimized params
residual_f <- eval_matches_all$margin - epr_diff_final
X_f <- cbind(1, eval_matches_all$log_dist_diff, eval_matches_all$familiarity_diff)
valid_f <- complete.cases(X_f, residual_f)

# Also compute default epr_diff for comparison
epv_dt_def <- compute_epv(pgr, par_defaults)
pcum_def <- compute_cumulative(epv_dt_def, par_defaults["decay_recv"], par_defaults["decay_disp"],
                               par_defaults["decay_spoil"], par_defaults["decay_hitout"])
lookup_def <- pcum_def[, .(player_id, date_num, cum_recv, cum_disp, cum_spoil, cum_hitout,
                           cum_wt_recv, cum_wt_disp, cum_wt_spoil, cum_wt_hitout)]
data.table::setkey(lookup_def, player_id, date_num)
joined_def <- lookup_def[lineup_dt_all,
  .(match_idx = i.match_idx, is_home = i.is_home, lineup_tog = i.lineup_tog,
    cr = x.cum_recv, cd = x.cum_disp, cs = x.cum_spoil, ch = x.cum_hitout,
    cw_r = x.cum_wt_recv, cw_d = x.cum_wt_disp, cw_s = x.cum_wt_spoil, cw_h = x.cum_wt_hitout),
  on = .(player_id, date_num), roll = TRUE]
for (col in c("cr", "cd", "cs", "ch", "cw_r", "cw_d", "cw_s", "cw_h"))
  data.table::set(joined_def, which(is.na(joined_def[[col]])), col, 0)
joined_def[is.na(lineup_tog), lineup_tog := 0.75]

ld_d <- par_defaults["loading"]; pr_d <- par_defaults["prior_games_recv"]
pd_d <- par_defaults["prior_games_disp"]; ps_d <- par_defaults["prior_games_spoil"]
ph_d <- par_defaults["prior_games_hitout"]
prr_d <- par_defaults["prior_rate_recv"]; prd_d <- par_defaults["prior_rate_disp"]
prs_d <- par_defaults["prior_rate_spoil"]; prh_d <- par_defaults["prior_rate_hitout"]
epr_vec_def <- (ld_d * joined_def$cr + pr_d * prr_d) / (joined_def$cw_r + pr_d) +
                (ld_d * joined_def$cd + pd_d * prd_d) / (joined_def$cw_d + pd_d) +
                (ld_d * joined_def$cs + ps_d * prs_d) / (joined_def$cw_s + ps_d) +
                (ld_d * joined_def$ch + ph_d * prh_d) / (joined_def$cw_h + ph_d)
epr_weighted_d <- epr_vec_def * joined_def$lineup_tog
home_sum_d <- tapply(epr_weighted_d * joined_def$is_home, joined_def$match_idx, sum, na.rm = TRUE)
away_sum_d <- tapply(epr_weighted_d * (!joined_def$is_home), joined_def$match_idx, sum, na.rm = TRUE)
epr_diff_def <- as.numeric(home_sum_d - away_sum_d)

residual_d <- eval_matches_all$margin - epr_diff_def
seasons_v <- eval_matches_all$season

cat(sprintf("  %-8s  %8s  %8s  %8s\n", "Season", "Optimized", "Defaults", "Improvement"))
for (s in 2022:2025) {
  test_m  <- valid_f & (seasons_v == s)
  train_m <- valid_f & (seasons_v != s)
  # Optimized
  fit_opt <- .lm.fit(X_f[train_m, , drop = FALSE], residual_f[train_m])
  pred_opt <- X_f[test_m, , drop = FALSE] %*% fit_opt$coefficients
  rmse_opt <- sqrt(mean((residual_f[test_m] - pred_opt)^2))
  # Defaults
  res_d <- eval_matches_all$margin - epr_diff_def
  X_d <- X_f  # same controls
  fit_def <- .lm.fit(X_d[train_m, , drop = FALSE], res_d[train_m])
  pred_def <- X_d[test_m, , drop = FALSE] %*% fit_def$coefficients
  rmse_def <- sqrt(mean((res_d[test_m] - pred_def)^2))
  cat(sprintf("  %-8d  %8.4f  %8.4f  %+8.4f\n", s, rmse_opt, rmse_def, rmse_def - rmse_opt))
}
rm(epv_dt_final, pcum_final, lookup_final, joined_final,
   epv_dt_def, pcum_def, lookup_def, joined_def)

# 8. Results ----
cat("\n=== OPTIMIZED PARAMETERS (normalized scale) ===\n")
cat("\n# Stat weights (normalized = per-SD; raw = per-count for constants.R):\n")
stat_categories <- list(
  "Scoring/Forward" = c("goals_wt", "behinds_wt", "shots_at_goal_wt",
                         "marks_inside50_wt", "inside50s_wt",
                         "score_involvements_wt", "goal_assists_wt"),
  "Contested/Ground" = c("contested_poss_wt", "contested_marks_wt",
                          "ground_ball_gets_wt"),
  "Defence" = c("spoil_wt", "tackle_wt", "pressure_wt", "def_pressure_wt",
                "intercepts_wt", "one_percenters_wt", "rebound50s_wt"),
  "Ruck" = c("hitout_wt", "hitout_adv_wt", "ruck_contest_wt"),
  "Disposal/Possession" = c("kicks_wt", "handballs_wt", "marks_wt",
                             "uncontested_poss_wt", "metres_gained_wt"),
  "Turnover/Discipline" = c("clangers_wt", "turnovers_wt",
                             "frees_against_wt", "frees_for_wt")
)
for (cat_name in names(stat_categories)) {
  cat(sprintf("\n  --- %s ---\n", cat_name))
  cat(sprintf("  %-28s %10s %10s %10s\n", "param", "normalized", "raw", "default_raw"))
  for (nm in stat_categories[[cat_name]]) {
    sd_col <- param_stat_map[nm]
    raw_wt <- best_par[nm] / stat_sds[sd_col]
    def_raw <- par_defaults[nm] / stat_sds[sd_col]
    cat(sprintf("  %-28s %+10.4f %+10.4f %+10.4f\n", nm, best_par[nm], raw_wt, def_raw))
  }
}
cat("\n# EPV/scale params:\n")
epv_params <- setdiff(names(par_defaults), c(names(param_stat_map),
  "decay_recv", "decay_disp", "decay_spoil", "decay_hitout", "loading",
  "prior_games_recv", "prior_games_disp", "prior_games_spoil", "prior_games_hitout",
  "prior_rate_recv", "prior_rate_disp", "prior_rate_spoil", "prior_rate_hitout"))
for (nm in epv_params) {
  cat(sprintf("  %-28s = %8.4f  (was %.4f)\n", nm, best_par[nm], par_defaults[nm]))
}
cat("\n# Aggregation params:\n")
agg_params <- c("decay_recv", "decay_disp", "decay_spoil", "decay_hitout", "loading",
  "prior_games_recv", "prior_games_disp", "prior_games_spoil", "prior_games_hitout",
  "prior_rate_recv", "prior_rate_disp", "prior_rate_spoil", "prior_rate_hitout")
for (nm in agg_params) {
  cat(sprintf("  %-28s = %8.4f  (was %.4f)\n", nm, best_par[nm], par_defaults[nm]))
}

cat("\n# Summary:\n")
# Compute pure (unpenalized) RMSE for the optimized params for fair comparison
final_rmse_pure <- fast_rmse_cv(epr_diff_final, eval_matches_all)
final_l2 <- STAT_WEIGHT_LAMBDA * sum(best_par[L2_PARAM_NAMES]^2)
cat(sprintf("  No-ratings RMSE (dist+fam only):    %.4f\n", no_rating_rmse))
cat(sprintf("  Default-params RMSE (pure):         %.4f\n", baseline_rmse_pure))
cat(sprintf("  Optimized RMSE (pure):              %.4f\n", final_rmse_pure))
cat(sprintf("  Optimized RMSE (penalized):         %.4f  (L2=%.4f)\n", final_rmse, final_l2))
cat(sprintf("  EPR value (no-ratings - default):  %.4f (%.1f%%)\n",
            no_rating_rmse - baseline_rmse_pure,
            100 * (no_rating_rmse - baseline_rmse_pure) / no_rating_rmse))
cat(sprintf("  Optimization gain (pure RMSE):      %.4f (%.2f%%)\n",
            baseline_rmse_pure - final_rmse_pure,
            100 * (baseline_rmse_pure - final_rmse_pure) / baseline_rmse_pure))

# 9. Save Results ----

# Map from optimizer param names -> constants.R variable names
param_to_constant <- c(
  disp_neg_offset   = "EPV_DISP_NEG_OFFSET",
  disp_pos_offset   = "EPV_DISP_POS_OFFSET",
  disp_scale        = "EPV_DISP_SCALE",
  recv_neg_mult     = "EPV_RECV_NEG_MULT",
  recv_neg_offset   = "EPV_RECV_NEG_OFFSET",
  recv_pos_mult     = "EPV_RECV_POS_MULT",
  recv_pos_offset   = "EPV_RECV_POS_OFFSET",
  recv_scale        = "EPV_RECV_SCALE",
  recv_intercept_mark_scale = "EPV_RECV_INTERCEPT_MARK_SCALE",
  spoil_wt          = "EPV_SPOIL_WT",
  tackle_wt         = "EPV_TACKLE_WT",
  pressure_wt       = "EPV_PRESSURE_WT",
  def_pressure_wt   = "EPV_DEF_PRESSURE_WT",
  hitout_wt         = "EPV_HITOUT_WT",
  hitout_adv_wt     = "EPV_HITOUT_ADV_WT",
  ruck_contest_wt        = "EPV_RUCK_CONTEST_WT",
  contested_poss_wt      = "EPV_CONTESTED_POSS_WT",
  contested_marks_wt     = "EPV_CONTESTED_MARKS_WT",
  ground_ball_gets_wt    = "EPV_GROUND_BALL_GETS_WT",
  marks_inside50_wt      = "EPV_MARKS_INSIDE50_WT",
  inside50s_wt           = "EPV_INSIDE50S_WT",
  clangers_wt            = "EPV_CLANGERS_WT",
  score_involvements_wt  = "EPV_SCORE_INVOLVEMENTS_WT",
  intercepts_wt          = "EPV_INTERCEPTS_WT",
  one_percenters_wt      = "EPV_ONE_PERCENTERS_WT",
  rebound50s_wt          = "EPV_REBOUND50S_WT",
  frees_against_wt       = "EPV_FREES_AGAINST_WT",
  frees_for_wt           = "EPV_FREES_FOR_WT",
  goals_wt               = "EPV_GOALS_WT",
  behinds_wt             = "EPV_BEHINDS_WT",
  marks_wt               = "EPV_MARKS_WT",
  uncontested_poss_wt    = "EPV_UNCONTESTED_POSS_WT",
  shots_at_goal_wt       = "EPV_SHOTS_AT_GOAL_WT",
  kicks_wt               = "EPV_KICKS_WT",
  handballs_wt           = "EPV_HANDBALLS_WT",
  metres_gained_wt       = "EPV_METRES_GAINED_WT",
  turnovers_wt           = "EPV_TURNOVERS_WT",
  goal_assists_wt        = "EPV_GOAL_ASSISTS_WT",
  decay_recv        = "EPR_DECAY_RECV",
  decay_disp        = "EPR_DECAY_DISP",
  decay_spoil       = "EPR_DECAY_SPOIL",
  decay_hitout      = "EPR_DECAY_HITOUT",
  loading           = "EPR_LOADING_DEFAULT",
  prior_games_recv  = "EPR_PRIOR_GAMES_RECV",
  prior_games_disp  = "EPR_PRIOR_GAMES_DISP",
  prior_games_spoil = "EPR_PRIOR_GAMES_SPOIL",
  prior_games_hitout = "EPR_PRIOR_GAMES_HITOUT"
  # prior_rate_* excluded — set empirically from first-game EPV, not optimized
)

## 9a. Un-normalize stat weights back to per-raw-unit for constants.R ----
# Optimizer works in per-SD scale; constants.R needs per-raw-unit weights
best_par_raw <- best_par
for (nm in names(param_stat_map)) {
  best_par_raw[nm] <- best_par[nm] / stat_sds[param_stat_map[nm]]
}

## 9b. Write optimized params back to R/constants_*.R ----
# constants.R was split into themed files in 1.3.3 (constants_afl,
# constants_ratings, constants_sim, constants_match, constants_data).
# Scan all of them so this script doesn't have to know which file owns
# which constant — robust against future re-organisation.
constants_files <- list.files("R", pattern = "^constants(_[a-z]+)?\\.R$",
                              full.names = TRUE)
if (length(constants_files) == 0) {
  stop("No R/constants*.R files found")
}
cat(sprintf("\nScanning %d constants file(s) for parameter updates...\n",
            length(constants_files)))

file_lines <- lapply(constants_files, readLines)
names(file_lines) <- constants_files
file_dirty <- setNames(rep(FALSE, length(constants_files)), constants_files)
n_updated <- 0L

for (par_name in names(param_to_constant)) {
  const_name <- param_to_constant[par_name]
  new_val <- best_par_raw[par_name]

  val_str <- if (grepl("^decay_", par_name)) sprintf("%.0f", new_val)
             else sprintf("%.4f", new_val)

  pattern <- paste0("^(", const_name, "\\s*<-\\s*).*$")
  hits <- vapply(file_lines, function(L) sum(grepl(pattern, L)), integer(1))
  total_hits <- sum(hits)

  if (total_hits == 0) {
    warning(sprintf("No definition found for %s in any constants_*.R file", const_name))
    next
  }
  if (total_hits > 1) {
    warning(sprintf("Multiple definitions of %s across constants_*.R files (%d hits)",
                    const_name, total_hits))
    next
  }

  target_file <- names(hits)[hits == 1L]
  L <- file_lines[[target_file]]
  match_idx <- grep(pattern, L)
  old_line <- L[match_idx]
  new_line <- sub(pattern, paste0("\\1", val_str), old_line)
  if (old_line != new_line) {
    file_lines[[target_file]][match_idx] <- new_line
    file_dirty[target_file] <- TRUE
    n_updated <- n_updated + 1L
    cat(sprintf("  %s [%s]: %s -> %s\n", const_name, basename(target_file),
                sub(paste0(const_name, "\\s*<-\\s*"), "", old_line), val_str))
  }
}

for (f in names(file_dirty)[file_dirty]) writeLines(file_lines[[f]], f)
cat(sprintf("Updated %d constants across %d file(s)\n",
            n_updated, sum(file_dirty)))

## 9c. Save params as CSV backup ----
optimized_params_df <- data.frame(
  param = names(best_par_raw),
  value_raw = unname(best_par_raw),
  value_normalized = unname(best_par[names(best_par_raw)])
)
utils::write.csv(optimized_params_df, "data-raw/03-ratings/optimized_torp_params.csv", row.names = FALSE)
cat("Backup saved to data-raw/03-ratings/optimized_torp_params.csv\n")

cat("\n=== Optimization complete ===\n")
