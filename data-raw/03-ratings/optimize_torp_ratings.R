# optimize_torp_ratings.R
# ======================
# Optimize TORP rating parameters to minimize RMSE when team-sum TORP predicts
# next-game margin. Uses pre-computed raw components and incremental cumulative
# decay for speed.
#
# Usage: powershell.exe -Command 'Rscript "data-raw/03-ratings/optimize_torp_ratings.R"'

library(devtools)
library(data.table)
library(dplyr)
library(fitzRoy)
devtools::load_all()

cat("=== TORP Ratings Parameter Optimization ===\n\n")

# 1. Load Data ----
cat("Loading data...\n")
tictoc::tic("Data loading")

pbp_data      <- load_pbp(TRUE)
player_stats  <- load_player_stats(TRUE)
teams_data    <- load_teams(TRUE)
fixtures      <- load_fixtures(TRUE)
results       <- load_results(TRUE)
all_grounds   <- readRDS("./data-raw/outputs/stadium_data.rds")

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
    tm           = data.table::last(team),
    pos          = data.table::last(player_position)
  ),
  by = .(player_id, match_id)
]

## 2b. Reception raw components ----
recv_raw <- data.table::as.data.table(pbp_data)[
  , .(
    # For reception: delta_epv * pos_team, split by pos_team
    sum_depv_pt_neg = sum((delta_epv * pos_team)[pos_team == -1], na.rm = TRUE),
    n_recv_neg      = sum(pos_team == -1, na.rm = TRUE),
    sum_depv_pt_pos = sum((delta_epv * pos_team)[pos_team == 1], na.rm = TRUE),
    n_recv_pos      = sum(pos_team == 1, na.rm = TRUE)
  ),
  by = .(lead_player_id, match_id)
]
setnames(recv_raw, "lead_player_id", "player_id")

## 2c. Spoil/hitout raw counts ----
spoil_hitout_raw <- data.table::as.data.table(player_stats)[
  , .(
    spoils          = sum(extended_stats_spoils, na.rm = TRUE),
    tackles         = sum(tackles, na.rm = TRUE),
    pressure_acts   = sum(extended_stats_pressure_acts, na.rm = TRUE),
    def_pressure    = sum(extended_stats_def_half_pressure_acts, na.rm = TRUE),
    hitouts         = sum(hitouts, na.rm = TRUE),
    hitouts_adv     = sum(extended_stats_hitouts_to_advantage, na.rm = TRUE),
    ruck_contests   = sum(extended_stats_ruck_contests, na.rm = TRUE),
    bounces         = sum(bounces, na.rm = TRUE)
  ),
  by = .(player_id = player_player_player_player_id, match_id = provider_id)
]

## 2d. Merge all raw components ----
player_game_raw <- merge(disp_raw, recv_raw, by = c("player_id", "match_id"), all.x = TRUE)
player_game_raw <- merge(player_game_raw, spoil_hitout_raw, by = c("player_id", "match_id"), all.x = TRUE)

# Replace NAs with 0
num_cols <- c("sum_depv_neg", "n_neg", "sum_depv_pos", "n_pos",
              "sum_depv_pt_neg", "n_recv_neg", "sum_depv_pt_pos", "n_recv_pos",
              "spoils", "tackles", "pressure_acts", "def_pressure",
              "hitouts", "hitouts_adv", "ruck_contests", "bounces")
for (col in num_cols) {
  data.table::set(player_game_raw, which(is.na(player_game_raw[[col]])), col, 0)
}

# Sort chronologically
data.table::setorder(player_game_raw, date, match_id)

## 2e. Build match-level data ----
# Team map
team_map <- data.table::as.data.table(fixtures)[
  , .(team_name = names(sort(table(home.team.name), decreasing = TRUE))[1]),
  by = .(teamId = home.team.providerId)
][, team_name := fitzRoy::replace_teams(team_name)]

# Fixtures with results
fix_dt <- data.table::as.data.table(fixtures)[
  , .(providerId, season = compSeason.year, round = round.roundNumber,
      home_teamId = home.team.providerId, away_teamId = away.team.providerId,
      utcStartTime, venue_name = venue.name)
]

results_dt <- data.table::as.data.table(results)[
  , .(providerId = match.matchId,
      home_score = homeTeamScore.matchScore.totalScore,
      away_score = awayTeamScore.matchScore.totalScore)
]

match_dt <- merge(fix_dt, results_dt, by = "providerId", all.x = TRUE)
match_dt[, `:=`(
  margin = home_score - away_score,
  date = as.Date(utcStartTime),
  venue = replace_venues(venue_name)
)]
match_dt <- match_dt[!is.na(margin)]
data.table::setorder(match_dt, date, providerId)

# --- Lineups per team per match ---
teams_dt <- data.table::as.data.table(teams_data)
# Filter out EMERG/SUB
teams_dt <- teams_dt[is.na(position) | !(position %in% c("EMERG", "SUB"))]

lineups <- teams_dt[, .(player_ids = list(player.playerId)),
                    by = .(match_id = providerId, teamId)]

# --- Home ground / distance / familiarity ---
grounds_dt <- data.table::as.data.table(all_grounds)
grounds_dt[, venue := replace_venues(as.character(Ground))]

# Find each team's home ground (mode of venue)
home_ground <- data.table::as.data.table(teams_data)[
  , .(venue = replace_venues(names(sort(table(venue.name), decreasing = TRUE))[1])),
  by = .(teamId)
]
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
    match_dt[, .(teamId = home_teamId, providerId, date, venue)],
    match_dt[, .(teamId = away_teamId, providerId, date, venue)]
  )
  data.table::setorder(team_matches, teamId, date)

  # For each team-match, compute proportion of prior games at this venue
  team_matches[, row_idx := .I]
  team_matches[, familiarity := {
    fam <- numeric(.N)
    for (i in seq_len(.N)) {
      if (i == 1) {
        fam[i] <- 0
      } else {
        prior <- venue[1:(i - 1)]
        fam[i] <- sum(prior == venue[i]) / length(prior)
      }
    }
    fam
  }, by = teamId]

  return(team_matches[, .(teamId, providerId, familiarity)])
}

cat("  Computing familiarity...\n")
fam_dt <- compute_familiarity(match_dt, teams_dt)

# Merge familiarity for home and away
match_dt <- merge(match_dt,
                  fam_dt[, .(home_teamId = teamId, providerId, fam_home = familiarity)],
                  by = c("home_teamId", "providerId"), all.x = TRUE)
match_dt <- merge(match_dt,
                  fam_dt[, .(away_teamId = teamId, providerId, fam_away = familiarity)],
                  by = c("away_teamId", "providerId"), all.x = TRUE)
match_dt[is.na(fam_home), fam_home := 0]
match_dt[is.na(fam_away), fam_away := 0]
match_dt[, familiarity_diff := fam_home - fam_away]

# Merge lineups
match_dt <- merge(match_dt,
                  lineups[, .(match_id, home_teamId = teamId, home_players = player_ids)],
                  by.x = c("providerId", "home_teamId"),
                  by.y = c("match_id", "home_teamId"), all.x = TRUE)
match_dt <- merge(match_dt,
                  lineups[, .(match_id, away_teamId = teamId, away_players = player_ids)],
                  by.x = c("providerId", "away_teamId"),
                  by.y = c("match_id", "away_teamId"), all.x = TRUE)

# Filter to evaluation window (2022+) and valid lineups
match_dt <- match_dt[season >= 2022 & !is.na(home_players) & !is.na(away_players)]
data.table::setorder(match_dt, date, providerId)

cat(sprintf("  %d matches in evaluation window (2022+)\n", nrow(match_dt)))

## 2f. Player-game index for fast lookup ----
# Create a lookup: for each player, ordered list of (match_id, date, raw_components)
pgr <- player_game_raw[match_id %in% unique(c(
  unlist(match_dt$home_players),
  player_game_raw$match_id
))]
data.table::setorder(pgr, player_id, date)
data.table::setkey(pgr, player_id, match_id)

# Also get position info from teams_data for position-group means
pos_dt <- data.table::as.data.table(teams_data)[
  , .(player_id = player.playerId, match_id = providerId, position)
]
pos_dt <- pos_dt[!is.na(position) & !(position %in% c("EMERG", "SUB"))]
pgr <- merge(pgr, pos_dt, by = c("player_id", "match_id"), all.x = TRUE)

tictoc::toc()

# 3. Objective Function ----

#' Compute credit-assigned points from raw components given params
#' Returns a data.table with player_id, match_id, date, disp_pts, recv_pts,
#' spoil_pts, hitout_pts (before position adjustment)
compute_credits <- function(pgr, params) {
  out <- data.table::copy(pgr)
  p <- params

  # Disposal points
  out[, disp_pts := (sum_depv_neg + n_neg * p["disp_neg_offset"]) * p["disp_scale"] +
                    (sum_depv_pos + n_pos * p["disp_pos_offset"]) * p["disp_scale"] -
                    bounces * p["bounce_penalty"]]

 # Reception points
  out[, recv_pts := (p["recv_neg_mult"] * sum_depv_pt_neg + n_recv_neg * p["recv_neg_offset"]) * p["recv_scale"] +
                    (p["recv_pos_mult"] * sum_depv_pt_pos + n_recv_pos * p["recv_pos_offset"]) * p["recv_scale"]]

  # Spoil points
  out[, spoil_pts := spoils * p["spoil_wt"] + tackles * p["tackle_wt"] +
                     pressure_acts * p["pressure_wt"] - def_pressure * p["def_pressure_wt"]]

  # Hitout points
  out[, hitout_pts := hitouts * p["hitout_wt"] + hitouts_adv * p["hitout_adv_wt"] -
                      ruck_contests * p["ruck_contest_wt"]]

  # Position-group adjustment (subtract quantile)
  q <- p["pos_adj_quantile"]
  out[!is.na(position), `:=`(
    disp_pts  = disp_pts  - stats::quantile(disp_pts, q, na.rm = TRUE),
    recv_pts  = recv_pts  - stats::quantile(recv_pts, q, na.rm = TRUE),
    spoil_pts = spoil_pts - stats::quantile(spoil_pts, q, na.rm = TRUE),
    hitout_pts = hitout_pts - stats::quantile(hitout_pts, q, na.rm = TRUE)
  ), by = position]

  return(out)
}

#' Objective function: compute RMSE of margin ~ torp_diff + controls
#'
#' Uses incremental cumulative decay to compute player TORPs at each match date.
#' @param par Named numeric vector of all 22 parameters
#' @param pgr Pre-computed player-game raw data
#' @param match_dt Pre-computed match data with lineups
#' @param train_seasons Seasons to use for evaluation (default 2022:2025)
#' @return RMSE value
objective_fn <- function(par, pgr, match_dt, train_seasons = 2022:2025) {

  # Extract aggregation params
  decay_days     <- par["decay_days"]
  loading        <- par["loading"]
  prior_recv     <- par["prior_games_recv"]
  prior_disp     <- par["prior_games_disp"]
  spoil_mult     <- par["spoil_multiplier"]

  # Compute credit-assigned points
  credit_dt <- compute_credits(pgr, par)
  credit_dt[, tot_pts := recv_pts + disp_pts + spoil_pts + hitout_pts]

  # --- Incremental cumulative decay ---
  # For each player, maintain running weighted sums
  data.table::setorder(credit_dt, player_id, date)

  # Pre-compute per player: cumulative weighted components using exponential decay
  # We process matches in chronological order and track cumulative sums
  eval_matches <- match_dt[season %in% train_seasons]

  # Build player TORP lookup at each match date using cumulative decay
  # Group by player, compute cumulative weighted sums
  credit_dt[, date_num := as.numeric(date)]

  # For each player, compute cumulative decay-weighted sums
  player_cum <- credit_dt[, {
    n <- .N
    cum_recv  <- numeric(n)
    cum_disp  <- numeric(n)
    cum_spoil <- numeric(n)
    cum_hitout <- numeric(n)
    cum_wt    <- numeric(n)

    for (i in seq_len(n)) {
      if (i == 1) {
        cum_recv[i]   <- recv_pts[i]
        cum_disp[i]   <- disp_pts[i]
        cum_spoil[i]  <- spoil_pts[i]
        cum_hitout[i] <- hitout_pts[i]
        cum_wt[i]     <- 1
      } else {
        decay_factor <- exp(-(date_num[i] - date_num[i - 1]) / decay_days)
        cum_recv[i]   <- cum_recv[i - 1] * decay_factor + recv_pts[i]
        cum_disp[i]   <- cum_disp[i - 1] * decay_factor + disp_pts[i]
        cum_spoil[i]  <- cum_spoil[i - 1] * decay_factor + spoil_pts[i]
        cum_hitout[i] <- cum_hitout[i - 1] * decay_factor + hitout_pts[i]
        cum_wt[i]     <- cum_wt[i - 1] * decay_factor + 1
      }
    }

    .(match_id = match_id, date = date,
      cum_recv = cum_recv, cum_disp = cum_disp,
      cum_spoil = cum_spoil, cum_hitout = cum_hitout,
      cum_wt = cum_wt)
  }, by = player_id]

  # Compute TORP from cumulative sums
  player_cum[, torp := loading * (
    cum_recv / (cum_wt + prior_recv) +
    cum_disp / (cum_wt + prior_disp) +
    spoil_mult * cum_spoil / (cum_wt + prior_recv) +
    cum_hitout / (cum_wt + prior_recv)
  )]

  # Build lookup table keyed by (player_id, date_num) for rolling joins
  player_torp_lookup <- player_cum[, .(player_id, date_num = as.numeric(date), torp)]
  data.table::setkey(player_torp_lookup, player_id, date_num)

  # Expand eval match lineups into flat table (one row per player-match)
  lineup_rows <- vector("list", nrow(eval_matches))
  for (i in seq_len(nrow(eval_matches))) {
    m <- eval_matches[i]
    home_ids <- m$home_players[[1]]
    away_ids <- m$away_players[[1]]
    lineup_rows[[i]] <- data.table::data.table(
      match_idx = i,
      player_id = c(home_ids, away_ids),
      is_home = c(rep(TRUE, length(home_ids)), rep(FALSE, length(away_ids))),
      date_num = as.numeric(m$date) - 0.5  # slightly before match for strict < lookup
    )
  }
  lineup_dt <- data.table::rbindlist(lineup_rows)

  # Rolling join: for each player-match, find their latest TORP before match date
  joined <- player_torp_lookup[lineup_dt, .(match_idx, is_home, torp = x.torp),
                                on = .(player_id, date_num), roll = TRUE]

  # Aggregate to match-level TORP diff
  match_torp <- joined[, .(
    home_torp = sum(torp[is_home], na.rm = TRUE),
    away_torp = sum(torp[!is_home], na.rm = TRUE)
  ), by = match_idx]
  data.table::setorder(match_torp, match_idx)

  torp_diff <- match_torp$home_torp - match_torp$away_torp
  valid <- rep(TRUE, nrow(eval_matches))

  # Fit evaluation model: margin ~ torp_diff + log_dist_diff + familiarity_diff
  eval_df <- data.frame(
    margin          = eval_matches$margin[valid],
    torp_diff       = torp_diff[valid],
    log_dist_diff   = eval_matches$log_dist_diff[valid],
    familiarity_diff = eval_matches$familiarity_diff[valid]
  )
  eval_df <- eval_df[complete.cases(eval_df), ]

  if (nrow(eval_df) < 50) return(999)

  fit <- lm(margin ~ torp_diff + log_dist_diff + familiarity_diff, data = eval_df)
  rmse <- sqrt(mean(fit$residuals^2))

  return(rmse)
}

# 4. Parameter Setup ----

# Current defaults (22 parameters)
par_defaults <- c(
  # Credit params (17)
  disp_neg_offset   = -0.04,
  disp_pos_offset   = 0.08,
  disp_scale        = 0.5,
  bounce_penalty    = 0.2,
  recv_neg_mult     = 1.5,
  recv_neg_offset   = 0.1,
  recv_pos_mult     = 1.0,
  recv_pos_offset   = 0.05,
  recv_scale        = 0.5,
  spoil_wt          = 0.6,
  tackle_wt         = 0.1,
  pressure_wt       = 0.1,
  def_pressure_wt   = 0.2,
  hitout_wt         = 0.15,
  hitout_adv_wt     = 0.25,
  ruck_contest_wt   = 0.06,
  pos_adj_quantile  = 0.4,
  # Aggregation params (5)
  decay_days        = 365,
  loading           = 1.5,
  prior_games_recv  = 4,
  prior_games_disp  = 6,
  spoil_multiplier  = 1.2
)

# Lower bounds
par_lower <- c(
  disp_neg_offset   = -0.2,
  disp_pos_offset   = -0.05,
  disp_scale        = 0.1,
  bounce_penalty    = 0.0,
  recv_neg_mult     = 0.5,
  recv_neg_offset   = -0.1,
  recv_pos_mult     = 0.3,
  recv_pos_offset   = -0.1,
  recv_scale        = 0.1,
  spoil_wt          = 0.0,
  tackle_wt         = 0.0,
  pressure_wt       = 0.0,
  def_pressure_wt   = 0.0,
  hitout_wt         = 0.0,
  hitout_adv_wt     = 0.0,
  ruck_contest_wt   = 0.0,
  pos_adj_quantile  = 0.1,
  decay_days        = 100,
  loading           = 0.5,
  prior_games_recv  = 1,
  prior_games_disp  = 1,
  spoil_multiplier  = 0.3
)

# Upper bounds
par_upper <- c(
  disp_neg_offset   = 0.1,
  disp_pos_offset   = 0.3,
  disp_scale        = 2.0,
  bounce_penalty    = 1.0,
  recv_neg_mult     = 3.0,
  recv_neg_offset   = 0.3,
  recv_pos_mult     = 3.0,
  recv_pos_offset   = 0.3,
  recv_scale        = 2.0,
  spoil_wt          = 2.0,
  tackle_wt         = 1.0,
  pressure_wt       = 1.0,
  def_pressure_wt   = 1.0,
  hitout_wt         = 1.0,
  hitout_adv_wt     = 1.0,
  ruck_contest_wt   = 0.5,
  pos_adj_quantile  = 0.7,
  decay_days        = 700,
  loading           = 4.0,
  prior_games_recv  = 15,
  prior_games_disp  = 15,
  spoil_multiplier  = 3.0
)

# 5. Baseline RMSE ----
cat("\nComputing baseline RMSE with default parameters...\n")
tictoc::tic("Baseline")
baseline_rmse <- objective_fn(par_defaults, pgr, match_dt, train_seasons = 2022:2025)
tictoc::toc()
cat(sprintf("Baseline RMSE: %.4f\n\n", baseline_rmse))

# 6. Staged Optimization ----

## Stage 1: Grid search on aggregation params ----
cat("=== Stage 1: Grid search on aggregation params ===\n")
tictoc::tic("Stage 1")

agg_grid <- expand.grid(
  decay_days       = c(200, 365, 500),
  loading          = c(1.0, 1.5, 2.0, 2.5),
  prior_games_recv = c(2, 4, 6),
  prior_games_disp = c(3, 6, 9),
  spoil_multiplier = c(0.8, 1.2, 1.6)
)

best_rmse <- baseline_rmse
best_par <- par_defaults

cat(sprintf("  Grid has %d combinations\n", nrow(agg_grid)))

for (i in seq_len(nrow(agg_grid))) {
  test_par <- par_defaults
  test_par["decay_days"]       <- agg_grid$decay_days[i]
  test_par["loading"]          <- agg_grid$loading[i]
  test_par["prior_games_recv"] <- agg_grid$prior_games_recv[i]
  test_par["prior_games_disp"] <- agg_grid$prior_games_disp[i]
  test_par["spoil_multiplier"] <- agg_grid$spoil_multiplier[i]

  rmse_i <- tryCatch(
    objective_fn(test_par, pgr, match_dt),
    error = function(e) 999
  )

  if (rmse_i < best_rmse) {
    best_rmse <- rmse_i
    best_par <- test_par
    cat(sprintf("  [%d/%d] New best RMSE: %.4f (decay=%.0f, loading=%.1f, prior_recv=%.0f, prior_disp=%.0f, spoil_mult=%.1f)\n",
                i, nrow(agg_grid), rmse_i,
                agg_grid$decay_days[i], agg_grid$loading[i],
                agg_grid$prior_games_recv[i], agg_grid$prior_games_disp[i],
                agg_grid$spoil_multiplier[i]))
  }

  if (i %% 50 == 0) cat(sprintf("  [%d/%d] Current best: %.4f\n", i, nrow(agg_grid), best_rmse))
}

tictoc::toc()
cat(sprintf("Stage 1 best RMSE: %.4f\n\n", best_rmse))

## Stage 2: Nelder-Mead on credit groups ----
cat("=== Stage 2: Nelder-Mead on credit groups ===\n")
tictoc::tic("Stage 2")

# Group A: Disposal params
disp_names <- c("disp_neg_offset", "disp_pos_offset", "disp_scale", "bounce_penalty")
cat("  Optimizing disposal params...\n")
opt_disp <- optim(
  par = best_par[disp_names],
  fn = function(sub_par) {
    test_par <- best_par
    test_par[disp_names] <- sub_par
    objective_fn(test_par, pgr, match_dt)
  },
  method = "Nelder-Mead",
  control = list(maxit = 200, trace = 1)
)
best_par[disp_names] <- opt_disp$par
cat(sprintf("  Disposal optimized: RMSE = %.4f\n", opt_disp$value))

# Group B: Reception params
recv_names <- c("recv_neg_mult", "recv_neg_offset", "recv_pos_mult", "recv_pos_offset", "recv_scale")
cat("  Optimizing reception params...\n")
opt_recv <- optim(
  par = best_par[recv_names],
  fn = function(sub_par) {
    test_par <- best_par
    test_par[recv_names] <- sub_par
    objective_fn(test_par, pgr, match_dt)
  },
  method = "Nelder-Mead",
  control = list(maxit = 200, trace = 1)
)
best_par[recv_names] <- opt_recv$par
cat(sprintf("  Reception optimized: RMSE = %.4f\n", opt_recv$value))

# Group C: Spoil/tackle params
spoil_names <- c("spoil_wt", "tackle_wt", "pressure_wt", "def_pressure_wt")
cat("  Optimizing spoil/tackle params...\n")
opt_spoil <- optim(
  par = best_par[spoil_names],
  fn = function(sub_par) {
    test_par <- best_par
    test_par[spoil_names] <- sub_par
    objective_fn(test_par, pgr, match_dt)
  },
  method = "Nelder-Mead",
  control = list(maxit = 200, trace = 1)
)
best_par[spoil_names] <- opt_spoil$par
cat(sprintf("  Spoil optimized: RMSE = %.4f\n", opt_spoil$value))

# Group D: Hitout params
hitout_names <- c("hitout_wt", "hitout_adv_wt", "ruck_contest_wt")
cat("  Optimizing hitout params...\n")
opt_hitout <- optim(
  par = best_par[hitout_names],
  fn = function(sub_par) {
    test_par <- best_par
    test_par[hitout_names] <- sub_par
    objective_fn(test_par, pgr, match_dt)
  },
  method = "Nelder-Mead",
  control = list(maxit = 200, trace = 1)
)
best_par[hitout_names] <- opt_hitout$par
cat(sprintf("  Hitout optimized: RMSE = %.4f\n", opt_hitout$value))

tictoc::toc()
cat(sprintf("Stage 2 best RMSE: %.4f\n\n", min(opt_disp$value, opt_recv$value, opt_spoil$value, opt_hitout$value)))

# Recompute overall RMSE with all Stage 2 updates
stage2_rmse <- objective_fn(best_par, pgr, match_dt)
cat(sprintf("Stage 2 combined RMSE: %.4f\n\n", stage2_rmse))

## Stage 3: Joint L-BFGS-B on all params ----
cat("=== Stage 3: Joint L-BFGS-B on all 22 params ===\n")
tictoc::tic("Stage 3")

opt_joint <- optim(
  par = best_par,
  fn = function(par) objective_fn(par, pgr, match_dt),
  method = "L-BFGS-B",
  lower = par_lower,
  upper = par_upper,
  control = list(maxit = 60, trace = 1)
)
best_par <- opt_joint$par
tictoc::toc()
cat(sprintf("Stage 3 RMSE: %.4f\n\n", opt_joint$value))

# 7. Validation (holdout test on 2025) ----
cat("=== Validation: Train 2022-2024, Test 2025 ===\n")

rmse_train <- objective_fn(best_par, pgr, match_dt, train_seasons = 2022:2024)
rmse_test  <- objective_fn(best_par, pgr, match_dt, train_seasons = 2025)
rmse_default_test <- objective_fn(par_defaults, pgr, match_dt, train_seasons = 2025)

cat(sprintf("  Train RMSE (2022-2024): %.4f\n", rmse_train))
cat(sprintf("  Test RMSE (2025) optimized: %.4f\n", rmse_test))
cat(sprintf("  Test RMSE (2025) defaults:  %.4f\n", rmse_default_test))
cat(sprintf("  Improvement: %.4f (%.1f%%)\n",
            rmse_default_test - rmse_test,
            100 * (rmse_default_test - rmse_test) / rmse_default_test))

# 8. Results ----
cat("\n=== OPTIMIZED PARAMETERS ===\n")
cat("\n# Credit params:\n")
for (nm in names(par_defaults)[1:17]) {
  cat(sprintf("  %-20s = %8.4f  (was %.4f)\n", nm, best_par[nm], par_defaults[nm]))
}
cat("\n# Aggregation params:\n")
for (nm in names(par_defaults)[18:22]) {
  cat(sprintf("  %-20s = %8.4f  (was %.4f)\n", nm, best_par[nm], par_defaults[nm]))
}

cat("\n# Summary:\n")
cat(sprintf("  Baseline RMSE:  %.4f\n", baseline_rmse))
cat(sprintf("  Optimized RMSE: %.4f\n", opt_joint$value))
cat(sprintf("  Improvement:    %.4f (%.1f%%)\n",
            baseline_rmse - opt_joint$value,
            100 * (baseline_rmse - opt_joint$value) / baseline_rmse))

# 9. Save Results ----
optimized_params <- as.list(best_par)
saveRDS(optimized_params, "data-raw/03-ratings/optimized_torp_params.rds")
cat("\nOptimized parameters saved to data-raw/03-ratings/optimized_torp_params.rds\n")

# Print code to update constants.R
cat("\n# To update constants.R with optimized values:\n")
cat(sprintf("CREDIT_DISP_NEG_OFFSET   <- %.4f\n", best_par["disp_neg_offset"]))
cat(sprintf("CREDIT_DISP_POS_OFFSET   <- %.4f\n", best_par["disp_pos_offset"]))
cat(sprintf("CREDIT_DISP_SCALE        <- %.4f\n", best_par["disp_scale"]))
cat(sprintf("CREDIT_BOUNCE_PENALTY    <- %.4f\n", best_par["bounce_penalty"]))
cat(sprintf("CREDIT_RECV_NEG_MULT     <- %.4f\n", best_par["recv_neg_mult"]))
cat(sprintf("CREDIT_RECV_NEG_OFFSET   <- %.4f\n", best_par["recv_neg_offset"]))
cat(sprintf("CREDIT_RECV_POS_MULT     <- %.4f\n", best_par["recv_pos_mult"]))
cat(sprintf("CREDIT_RECV_POS_OFFSET   <- %.4f\n", best_par["recv_pos_offset"]))
cat(sprintf("CREDIT_RECV_SCALE        <- %.4f\n", best_par["recv_scale"]))
cat(sprintf("CREDIT_SPOIL_WT          <- %.4f\n", best_par["spoil_wt"]))
cat(sprintf("CREDIT_TACKLE_WT         <- %.4f\n", best_par["tackle_wt"]))
cat(sprintf("CREDIT_PRESSURE_WT       <- %.4f\n", best_par["pressure_wt"]))
cat(sprintf("CREDIT_DEF_PRESSURE_WT   <- %.4f\n", best_par["def_pressure_wt"]))
cat(sprintf("CREDIT_HITOUT_WT         <- %.4f\n", best_par["hitout_wt"]))
cat(sprintf("CREDIT_HITOUT_ADV_WT     <- %.4f\n", best_par["hitout_adv_wt"]))
cat(sprintf("CREDIT_RUCK_CONTEST_WT   <- %.4f\n", best_par["ruck_contest_wt"]))
cat(sprintf("CREDIT_POS_ADJ_QUANTILE  <- %.4f\n", best_par["pos_adj_quantile"]))
cat(sprintf("RATING_DECAY_DEFAULT_DAYS <- %.0f\n", best_par["decay_days"]))
cat(sprintf("RATING_LOADING_DEFAULT   <- %.4f\n", best_par["loading"]))
cat(sprintf("RATING_PRIOR_GAMES_RECV  <- %.0f\n", best_par["prior_games_recv"]))
cat(sprintf("RATING_PRIOR_GAMES_DISP  <- %.0f\n", best_par["prior_games_disp"]))
cat(sprintf("RATING_SPOIL_MULTIPLIER  <- %.4f\n", best_par["spoil_multiplier"]))

cat("\n=== Optimization complete ===\n")
