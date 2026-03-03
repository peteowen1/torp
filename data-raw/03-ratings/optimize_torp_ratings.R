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

# Join TOG from player_stats for per-80 normalisation
tog_dt <- data.table::as.data.table(player_stats)[
  , .(tog_pct = mean(time_on_ground_percentage, na.rm = TRUE)),
  by = .(player_id = player_player_player_player_id, match_id = provider_id)
]
player_game_raw <- merge(player_game_raw, tog_dt, by = c("player_id", "match_id"), all.x = TRUE)
player_game_raw[is.na(tog_pct), tog_pct := 80]
player_game_raw[, tog_safe := pmax(tog_pct / 100, 0.1)]

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

lineups <- teams_dt[, .(player_ids = list(player.playerId),
                       position_xs = list(position)),
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

# Merge lineups (with positions for lineup_tog weighting)
match_dt <- merge(match_dt,
                  lineups[, .(match_id, home_teamId = teamId, home_players = player_ids, home_positions = position_xs)],
                  by.x = c("providerId", "home_teamId"),
                  by.y = c("match_id", "home_teamId"), all.x = TRUE)
match_dt <- merge(match_dt,
                  lineups[, .(match_id, away_teamId = teamId, away_players = player_ids, away_positions = position_xs)],
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
pos_dt[position == "MIDFIELDER_FORWARD", position := "MEDIUM_FORWARD"]
pgr <- merge(pgr, pos_dt, by = c("player_id", "match_id"), all.x = TRUE)

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

# 3. Objective Function ----

#' Compute credit-assigned points from raw components given params
#' Returns a data.table with player_id, match_id, date, disp_pts, recv_pts,
#' spoil_pts, hitout_pts (before position adjustment)
compute_credits <- function(pgr, params, verbose = FALSE) {
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

  # Per-80 normalisation first: divide by actual TOG so ratings are per-full-game.
  # Must happen BEFORE position-quantile so the quantile operates on per-80 rates
  # (otherwise low-TOG players have the per-game quantile amplified by 1/tog_safe).
  out[, `:=`(
    recv_pts   = recv_pts   / tog_safe,
    disp_pts   = disp_pts   / tog_safe,
    spoil_pts  = spoil_pts  / tog_safe,
    hitout_pts = hitout_pts / tog_safe
  )]

  # Position-group quantile adjustment (on per-80 rates)
  q <- p["pos_adj_quantile"]
  if (verbose) {
    cat(sprintf("\n  Position quantile adjustment on per-80 rates (q = %.2f%%):\n", q * 100))
    cat(sprintf("  %-18s %8s %8s %8s %8s\n", "Position", "recv", "disp", "spoil", "hitout"))
    for (pos in sort(unique(out$position[!is.na(out$position)]))) {
      idx <- out$position == pos & !is.na(out$position)
      cat(sprintf("  %-18s %+8.3f %+8.3f %+8.3f %+8.3f\n", pos,
        quantile(out$recv_pts[idx], q, na.rm = TRUE),
        quantile(out$disp_pts[idx], q, na.rm = TRUE),
        quantile(out$spoil_pts[idx], q, na.rm = TRUE),
        quantile(out$hitout_pts[idx], q, na.rm = TRUE)))
    }
  }
  out[!is.na(position), `:=`(
    recv_pts   = recv_pts   - stats::quantile(recv_pts, q, na.rm = TRUE),
    disp_pts   = disp_pts   - stats::quantile(disp_pts, q, na.rm = TRUE),
    spoil_pts  = spoil_pts  - stats::quantile(spoil_pts, q, na.rm = TRUE),
    hitout_pts = hitout_pts - stats::quantile(hitout_pts, q, na.rm = TRUE)
  ), by = position]

  return(out)
}

#' Compute cumulative decay-weighted sums per player
#'
#' @param credit_dt Credit-assigned player-game data (with recv_pts, disp_pts, etc.)
#' @param decay_days Decay factor in days
#' @return data.table with cumulative sums per player per game
compute_cumulative <- function(credit_dt, decay_days) {
  dt <- data.table::copy(credit_dt)
  data.table::setorder(dt, player_id, date)
  dt[, date_num := as.numeric(date)]

  dt[, {
    n <- .N
    cr <- cd <- cs <- ch <- cw <- numeric(n)
    for (i in seq_len(n)) {
      if (i == 1) {
        cr[i] <- recv_pts[i]; cd[i] <- disp_pts[i]
        cs[i] <- spoil_pts[i]; ch[i] <- hitout_pts[i]; cw[i] <- 1
      } else {
        df <- exp(-(date_num[i] - date_num[i - 1]) / decay_days)
        cr[i] <- cr[i - 1] * df + recv_pts[i]
        cd[i] <- cd[i - 1] * df + disp_pts[i]
        cs[i] <- cs[i - 1] * df + spoil_pts[i]
        ch[i] <- ch[i - 1] * df + hitout_pts[i]
        cw[i] <- cw[i - 1] * df + 1
      }
    }
    .(match_id = match_id, date_num = date_num,
      cum_recv = cr, cum_disp = cd, cum_spoil = cs, cum_hitout = ch, cum_wt = cw)
  }, by = player_id]
}

#' Compute cumulative decay-weighted sum of a SINGLE vector per player
#' Much faster than compute_cumulative() when only one component changed
compute_cumulative_single <- function(pgr_sorted, col_name, decay_days) {
  vals <- pgr_sorted[[col_name]]
  pids <- pgr_sorted$player_id
  dnums <- pgr_sorted$date_num
  n <- length(vals)
  cum <- numeric(n)

  cum[1] <- vals[1]
  for (i in 2:n) {
    if (pids[i] == pids[i - 1]) {
      df <- exp(-(dnums[i] - dnums[i - 1]) / decay_days)
      cum[i] <- cum[i - 1] * df + vals[i]
    } else {
      cum[i] <- vals[i]
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
  # Enforce bounds (Nelder-Mead is unconstrained)
  p <- pmax(par, par_lower[names(par)])
  p <- pmin(p, par_upper[names(par)])

  # --- Compute per-game credits directly on sorted vectors (no copy) ---
  disp_pts <- (env$sum_depv_neg + env$n_neg * p["disp_neg_offset"]) * p["disp_scale"] +
              (env$sum_depv_pos + env$n_pos * p["disp_pos_offset"]) * p["disp_scale"] -
              env$bounces * p["bounce_penalty"]

  recv_pts <- (p["recv_neg_mult"] * env$sum_depv_pt_neg + env$n_recv_neg * p["recv_neg_offset"]) * p["recv_scale"] +
              (p["recv_pos_mult"] * env$sum_depv_pt_pos + env$n_recv_pos * p["recv_pos_offset"]) * p["recv_scale"]

  spoil_pts <- env$spoils * p["spoil_wt"] + env$tackles * p["tackle_wt"] +
               env$pressure_acts * p["pressure_wt"] - env$def_pressure * p["def_pressure_wt"]

  hitout_pts <- env$hitouts * p["hitout_wt"] + env$hitouts_adv * p["hitout_adv_wt"] -
                env$ruck_contests * p["ruck_contest_wt"]

  # Per-80 normalisation first (before position-quantile, so quantile
  # operates on per-80 rates rather than inflating per-game adjustments)
  disp_pts <- disp_pts / env$tog_safe; recv_pts <- recv_pts / env$tog_safe
  spoil_pts <- spoil_pts / env$tog_safe; hitout_pts <- hitout_pts / env$tog_safe

  # Replace any NaN/NA credits with 0
  disp_pts[is.na(disp_pts)] <- 0; recv_pts[is.na(recv_pts)] <- 0
  spoil_pts[is.na(spoil_pts)] <- 0; hitout_pts[is.na(hitout_pts)] <- 0

  # Position-group quantile adjustment (on per-80 rates)
  q <- p["pos_adj_quantile"]
  for (pos in env$position_levels) {
    idx <- env$pos_indices[[pos]]
    if (length(idx) > 0) {
      recv_pts[idx]   <- recv_pts[idx]   - quantile(recv_pts[idx], q, na.rm = TRUE)
      disp_pts[idx]   <- disp_pts[idx]   - quantile(disp_pts[idx], q, na.rm = TRUE)
      spoil_pts[idx]  <- spoil_pts[idx]  - quantile(spoil_pts[idx], q, na.rm = TRUE)
      hitout_pts[idx] <- hitout_pts[idx] - quantile(hitout_pts[idx], q, na.rm = TRUE)
    }
  }

  # --- Cumulative decay sums (single-vector loop, no data.table overhead) ---
  decay_days <- p["decay_days"]
  pids <- env$pgr_s$player_id
  dnums <- env$pgr_s$date_num
  n <- length(disp_pts)
  cr <- cd <- cs <- ch <- cw <- numeric(n)
  cr[1] <- recv_pts[1]; cd[1] <- disp_pts[1]
  cs[1] <- spoil_pts[1]; ch[1] <- hitout_pts[1]; cw[1] <- 1
  for (i in 2:n) {
    if (pids[i] == pids[i - 1]) {
      df <- exp(-(dnums[i] - dnums[i - 1]) / decay_days)
      cr[i] <- cr[i - 1] * df + recv_pts[i]
      cd[i] <- cd[i - 1] * df + disp_pts[i]
      cs[i] <- cs[i - 1] * df + spoil_pts[i]
      ch[i] <- ch[i - 1] * df + hitout_pts[i]
      cw[i] <- cw[i - 1] * df + 1
    } else {
      cr[i] <- recv_pts[i]; cd[i] <- disp_pts[i]
      cs[i] <- spoil_pts[i]; ch[i] <- hitout_pts[i]; cw[i] <- 1
    }
  }

  # --- Map to lineup via pre-computed indices ---
  loading      <- p["loading"]
  prior_recv   <- p["prior_games_recv"]
  prior_disp   <- p["prior_games_disp"]
  prior_spoil  <- p["prior_games_spoil"]
  prior_hitout <- p["prior_games_hitout"]
  pr_recv      <- p["prior_rate_recv"]
  pr_disp      <- p["prior_rate_disp"]
  pr_spoil     <- p["prior_rate_spoil"]
  pr_hitout    <- p["prior_rate_hitout"]

  lu_idx <- env$lu_pgr_idx  # maps each lineup row to pgr_s row
  lu_cr <- cr[lu_idx]; lu_cd <- cd[lu_idx]; lu_cs <- cs[lu_idx]
  lu_ch <- ch[lu_idx]; lu_cw <- cw[lu_idx]

  # Replace NAs (unmatched players) with 0
  lu_cr[is.na(lu_cr)] <- 0; lu_cd[is.na(lu_cd)] <- 0
  lu_cs[is.na(lu_cs)] <- 0; lu_ch[is.na(lu_ch)] <- 0
  lu_cw[is.na(lu_cw)] <- 0

  # TORP per player-match (per-component shrinkage targets)
  torp_vec <- (loading * lu_cr + prior_recv * pr_recv) / (lu_cw + prior_recv) +
              (loading * lu_cd + prior_disp * pr_disp) / (lu_cw + prior_disp) +
              (loading * lu_cs + prior_spoil * pr_spoil) / (lu_cw + prior_spoil) +
              (loading * lu_ch + prior_hitout * pr_hitout) / (lu_cw + prior_hitout)

  # Match-level aggregation (weight per-80 TORP by lineup_tog)
  torp_weighted <- torp_vec * env$lu_lineup_tog
  home_sum <- tapply(torp_weighted * env$lu_is_home, env$lu_match_idx, sum, na.rm = TRUE)
  away_sum <- tapply(torp_weighted * (!env$lu_is_home), env$lu_match_idx, sum, na.rm = TRUE)
  torp_diff <- as.numeric(home_sum - away_sum)

  fast_rmse_cv(torp_diff, env$eval_matches)
}

#' Fast RMSE from torp_diff vector and eval match data
#' Forces torp_diff coefficient = 1 so TORP points map directly to margin points.
#' Only fits intercept + distance + familiarity controls on the residual.
fast_rmse <- function(torp_diff, eval_matches) {
  residual <- eval_matches$margin - torp_diff
  X <- cbind(1, eval_matches$log_dist_diff, eval_matches$familiarity_diff)
  valid <- complete.cases(X, residual)
  if (sum(valid) < 50) return(999)
  fit <- .lm.fit(X[valid, , drop = FALSE], residual[valid])
  sqrt(mean(fit$residuals^2))
}

#' Leave-one-season-out CV RMSE
#' Computes torp_diff once, then for each fold: fit controls on 3 seasons, test on 1.
#' Returns mean test RMSE across folds.
fast_rmse_cv <- function(torp_diff, eval_matches, cv_seasons = 2022:2025) {
  residual <- eval_matches$margin - torp_diff
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

#' Rolling join cumulative data to lineup, then compute match-level torp_diff
#'
#' @param player_cum Cumulative sums per player (from compute_cumulative)
#' @param lineup_dt Pre-computed lineup expansion
#' @param eval_matches Evaluation match data
#' @param loading Loading factor
#' @param prior_recv Prior games for reception
#' @param prior_disp Prior games for disposal
#' @param prior_spoil Prior games for spoil
#' @param prior_hitout Prior games for hitout
#' @return Numeric vector of torp_diff per match
compute_torp_diff <- function(player_cum, lineup_dt, eval_matches,
                              loading, prior_recv, prior_disp, prior_spoil, prior_hitout,
                              pr_recv = RATING_PRIOR_RATE_RECV, pr_disp = RATING_PRIOR_RATE_DISP,
                              pr_spoil = RATING_PRIOR_RATE_SPOIL, pr_hitout = RATING_PRIOR_RATE_HITOUT) {
  lookup <- player_cum[, .(player_id, date_num, cum_recv, cum_disp, cum_spoil, cum_hitout, cum_wt)]
  data.table::setkey(lookup, player_id, date_num)

  joined <- lookup[lineup_dt,
    .(match_idx, is_home, lineup_tog = i.lineup_tog,
      torp = x.cum_recv, cd = x.cum_disp,
      cs = x.cum_spoil, ch = x.cum_hitout, cw = x.cum_wt),
    on = .(player_id, date_num), roll = TRUE]
  joined[is.na(lineup_tog), lineup_tog := 0.75]

  # Replace NAs with 0 before formula (per-component priors handle the shrinkage target)
  joined[is.na(torp), `:=`(torp = 0, cd = 0, cs = 0, ch = 0, cw = 0)]

  # Compute TORP per player (per-component shrinkage targets)
  joined[, player_torp :=
    (loading * torp + prior_recv * pr_recv) / (cw + prior_recv) +
    (loading * cd + prior_disp * pr_disp) / (cw + prior_disp) +
    (loading * cs + prior_spoil * pr_spoil) / (cw + prior_spoil) +
    (loading * ch + prior_hitout * pr_hitout) / (cw + prior_hitout)
  ]
  joined[is.na(player_torp), player_torp := pr_recv + pr_disp + pr_spoil + pr_hitout]

  # Aggregate to match-level (weight per-80 TORP by lineup_tog)
  match_torp <- joined[, .(
    home_torp = sum(player_torp[is_home] * lineup_tog[is_home], na.rm = TRUE),
    away_torp = sum(player_torp[!is_home] * lineup_tog[!is_home], na.rm = TRUE)
  ), by = match_idx]
  data.table::setorder(match_torp, match_idx)

  match_torp$home_torp - match_torp$away_torp
}

#' Objective function: compute RMSE of margin ~ torp_diff + controls
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
  # Enforce bounds
  par <- pmax(par, par_lower[names(par)])
  par <- pmin(par, par_upper[names(par)])
  decay_days   <- par["decay_days"]
  loading      <- par["loading"]
  prior_recv   <- par["prior_games_recv"]
  prior_disp   <- par["prior_games_disp"]
  prior_spoil  <- par["prior_games_spoil"]
  prior_hitout <- par["prior_games_hitout"]

  # Compute credit-assigned points
  credit_dt <- compute_credits(pgr, par)

  # Compute cumulative decay-weighted sums
  player_cum <- compute_cumulative(credit_dt, decay_days)

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

  torp_diff <- compute_torp_diff(player_cum, lineup_dt, eval_matches,
                                 loading, prior_recv, prior_disp, prior_spoil, prior_hitout,
                                 pr_recv = par["prior_rate_recv"], pr_disp = par["prior_rate_disp"],
                                 pr_spoil = par["prior_rate_spoil"], pr_hitout = par["prior_rate_hitout"])
  fast_rmse_cv(torp_diff, eval_matches)
}

# 4. Parameter Setup ----

# Read current defaults from package constants (loaded via devtools::load_all())
par_defaults <- c(
  # Credit params (20) - from R/constants.R
  disp_neg_offset   = CREDIT_DISP_NEG_OFFSET,
  disp_pos_offset   = CREDIT_DISP_POS_OFFSET,
  disp_scale        = CREDIT_DISP_SCALE,
  bounce_penalty    = CREDIT_BOUNCE_PENALTY,
  recv_neg_mult     = CREDIT_RECV_NEG_MULT,
  recv_neg_offset   = CREDIT_RECV_NEG_OFFSET,
  recv_pos_mult     = CREDIT_RECV_POS_MULT,
  recv_pos_offset   = CREDIT_RECV_POS_OFFSET,
  recv_scale        = CREDIT_RECV_SCALE,
  spoil_wt          = CREDIT_SPOIL_WT,
  tackle_wt         = CREDIT_TACKLE_WT,
  pressure_wt       = CREDIT_PRESSURE_WT,
  def_pressure_wt   = CREDIT_DEF_PRESSURE_WT,
  hitout_wt         = CREDIT_HITOUT_WT,
  hitout_adv_wt     = CREDIT_HITOUT_ADV_WT,
  ruck_contest_wt        = CREDIT_RUCK_CONTEST_WT,
  pos_adj_quantile       = CREDIT_POS_ADJ_QUANTILE,
  # Aggregation params (10) - from R/constants.R
  decay_days         = RATING_DECAY_DEFAULT_DAYS,
  loading            = RATING_LOADING_DEFAULT,
  prior_games_recv   = RATING_PRIOR_GAMES_RECV,
  prior_games_disp   = RATING_PRIOR_GAMES_DISP,
  prior_games_spoil  = RATING_PRIOR_GAMES_SPOIL,
  prior_games_hitout = RATING_PRIOR_GAMES_HITOUT,
  prior_rate_recv    = RATING_PRIOR_RATE_RECV,
  prior_rate_disp    = RATING_PRIOR_RATE_DISP,
  prior_rate_spoil   = RATING_PRIOR_RATE_SPOIL,
  prior_rate_hitout  = RATING_PRIOR_RATE_HITOUT
)

cat("Current parameter values (from R/constants.R):\n")
cat(sprintf("  %s\n", paste(names(par_defaults), sprintf("%.4f", par_defaults), sep = " = ")))

# Lower bounds (widened where previous optima hit edges)
par_lower <- c(
  disp_neg_offset   = -0.5,
  disp_pos_offset   = -0.1,
  disp_scale        = 0.1,
  bounce_penalty    = 0.0,
  recv_neg_mult     = 0.3,
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
  ruck_contest_wt        = 0.03,
  pos_adj_quantile       = 0.2,
  decay_days         = 100,
  loading            = 1.0,
  prior_games_recv   = 3,
  prior_games_disp   = 3,
  prior_games_spoil  = 3,
  prior_games_hitout = 3,
  prior_rate_recv    = -3,
  prior_rate_disp    = -3,
  prior_rate_spoil   = -3,
  prior_rate_hitout  = -3
)

# Upper bounds (widened where previous optima hit edges)
par_upper <- c(
  disp_neg_offset   = 0.1,
  disp_pos_offset   = 0.5,
  disp_scale        = 2.0,
  bounce_penalty    = 1.0,
  recv_neg_mult     = 3.0,
  recv_neg_offset   = 0.5,
  recv_pos_mult     = 3.0,
  recv_pos_offset   = 0.5,
  recv_scale        = 2.0,
  spoil_wt          = 2.0,
  tackle_wt         = 2.0,
  pressure_wt       = 1.0,
  def_pressure_wt   = 1.5,
  hitout_wt         = 1.0,
  hitout_adv_wt     = 2.0,
  ruck_contest_wt        = 0.5,
  pos_adj_quantile       = 0.4,
  decay_days         = 700,
  loading            = 1.0,
  prior_games_recv   = 15,
  prior_games_disp   = 15,
  prior_games_spoil  = 15,
  prior_games_hitout = 15,
  prior_rate_recv    = -0.25,
  prior_rate_disp    = -0.25,
  prior_rate_spoil   = -0.25,
  prior_rate_hitout  = -0.25
)

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
baseline_rmse <- objective_fn(par_defaults, pgr, match_dt, train_seasons = 2022:2025,
                              eval_matches = eval_matches_all, lineup_dt = lineup_dt_all)
tictoc::toc()
cat(sprintf("Default-params baseline RMSE: %.4f\n\n", baseline_rmse))

# 6. Staged Optimization ----

## Stage 1: Grid search on aggregation params ----
cat("=== Stage 1: Grid search on aggregation params ===\n")
tictoc::tic("Stage 1")

agg_grid <- expand.grid(
  decay_days         = c(350, 450, 550, 650),
  loading            = 1.0,
  prior_games_recv   = c(3, 5, 7, 9),
  prior_games_disp   = c(3, 5, 7, 9),
  prior_games_spoil  = c(3, 5, 7, 9),
  prior_games_hitout = c(3, 5, 7, 9)
)

cat(sprintf("  Grid has %d combinations\n", nrow(agg_grid)))

# --- Fast Stage 1: pre-compute credits once + cumulative sums per decay ---
# Credit params are fixed during Stage 1, only agg params vary
cat("  Pre-computing credits (fixed during Stage 1)...\n")
s1_credit_dt <- compute_credits(pgr, par_defaults)

decay_values <- sort(unique(agg_grid$decay_days))
cat(sprintf("  Pre-computing cumulative sums for %d decay values...\n", length(decay_values)))

# For each decay value: compute cumulative sums, rolling join to lineup, store result
decay_precomp <- list()
for (d in decay_values) {
  cat(sprintf("    decay=%d...\n", d))
  pcum <- compute_cumulative(s1_credit_dt, d)

  # Rolling join to pre-computed lineup
  lookup <- pcum[, .(player_id, date_num, cum_recv, cum_disp, cum_spoil, cum_hitout, cum_wt)]
  data.table::setkey(lookup, player_id, date_num)

  joined <- lookup[lineup_dt_all,
    .(match_idx = i.match_idx, is_home = i.is_home, lineup_tog = i.lineup_tog,
      cr = x.cum_recv, cd = x.cum_disp, cs = x.cum_spoil,
      ch = x.cum_hitout, cw = x.cum_wt),
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
  d  <- as.character(agg_grid$decay_days[i])
  ld <- agg_grid$loading[i]
  pr <- agg_grid$prior_games_recv[i]
  pd <- agg_grid$prior_games_disp[i]
  ps <- agg_grid$prior_games_spoil[i]
  ph <- agg_grid$prior_games_hitout[i]

  j <- decay_precomp[[d]]

  # Vectorized TORP per player-match (~37K elements)
  # Use default per-component priors for Stage 1 (optimized in Stage 2)
  s1_pr_r <- par_defaults["prior_rate_recv"]
  s1_pr_d <- par_defaults["prior_rate_disp"]
  s1_pr_s <- par_defaults["prior_rate_spoil"]
  s1_pr_h <- par_defaults["prior_rate_hitout"]
  torp_vec <- (ld * j$cr + pr * s1_pr_r) / (j$cw + pr) +
              (ld * j$cd + pd * s1_pr_d) / (j$cw + pd) +
              (ld * j$cs + ps * s1_pr_s) / (j$cw + ps) +
              (ld * j$ch + ph * s1_pr_h) / (j$cw + ph)

  # Aggregate to match-level torp_diff (weight per-80 TORP by lineup_tog)
  torp_weighted <- torp_vec * j$lineup_tog
  home_sum <- tapply(torp_weighted * j$is_home, j$match_idx, sum, na.rm = TRUE)
  away_sum <- tapply(torp_weighted * (!j$is_home), j$match_idx, sum, na.rm = TRUE)
  torp_diff <- as.numeric(home_sum - away_sum)

  # Leave-one-season-out CV RMSE
  rmse_i <- fast_rmse_cv(torp_diff, eval_matches_all)

  if (rmse_i < best_rmse) {
    best_rmse <- rmse_i
    best_par <- par_defaults
    best_par["decay_days"]         <- agg_grid$decay_days[i]
    best_par["loading"]            <- ld
    best_par["prior_games_recv"]   <- pr
    best_par["prior_games_disp"]   <- pd
    best_par["prior_games_spoil"]  <- ps
    best_par["prior_games_hitout"] <- ph
    cat(sprintf("  [%d/%d] New best RMSE: %.4f (decay=%.0f, pr=%.0f, pd=%.0f, ps=%.0f, ph=%.0f)\n",
                i, nrow(agg_grid), rmse_i,
                agg_grid$decay_days[i], pr, pd, ps, ph))
  }

  if (i %% 500 == 0) cat(sprintf("  [%d/%d] Current best: %.4f\n", i, nrow(agg_grid), best_rmse))
}

# Clean up Stage 1 pre-computed data
rm(s1_credit_dt, decay_precomp)

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
  bounces        = pgr_s$bounces,
  sum_depv_pt_neg = pgr_s$sum_depv_pt_neg,
  n_recv_neg     = pgr_s$n_recv_neg,
  sum_depv_pt_pos = pgr_s$sum_depv_pt_pos,
  n_recv_pos     = pgr_s$n_recv_pos,
  spoils         = pgr_s$spoils,
  tackles        = pgr_s$tackles,
  pressure_acts  = pgr_s$pressure_acts,
  def_pressure   = pgr_s$def_pressure,
  hitouts        = pgr_s$hitouts,
  hitouts_adv    = pgr_s$hitouts_adv,
  ruck_contests  = pgr_s$ruck_contests,
  tog_safe       = pgr_s$tog_safe,
  # Player/date vectors for cumulative loop
  pgr_s          = pgr_s[, .(player_id, date_num)],
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

# Pre-compute position group indices
positions <- pgr_s$position
has_pos <- !is.na(positions)
fast_env$position_levels <- unique(positions[has_pos])
fast_env$pos_indices <- lapply(fast_env$position_levels, function(p) which(positions == p))
names(fast_env$pos_indices) <- fast_env$position_levels

# Pre-compute lineup -> pgr_s rolling join index
# For each lineup row (player_id, date_num), find the last pgr_s row <= that date
pgr_s[, pgr_row := .I]  # row index in pgr_s
data.table::setkey(pgr_s, player_id, date_num)

lu_joined <- pgr_s[lineup_dt_all, .(pgr_row = x.pgr_row, match_idx = i.match_idx,
                                    is_home = i.is_home, lineup_tog = i.lineup_tog),
                   on = .(player_id, date_num), roll = TRUE]

fast_env$lu_pgr_idx    <- lu_joined$pgr_row
fast_env$lu_is_home    <- lu_joined$is_home
fast_env$lu_lineup_tog <- lu_joined$lineup_tog
fast_env$lu_match_idx  <- lu_joined$match_idx

tictoc::toc()
cat(sprintf("  Fast env: %d pgr rows, %d lineup rows\n", nrow(pgr_s), nrow(lu_joined)))

# Fast objective wrapper
obj_fast <- function(par) objective_fn_fast(par, fast_env)

## Stage 2: Nelder-Mead on credit groups ----
cat("=== Stage 2: Nelder-Mead on credit groups ===\n")
tictoc::tic("Stage 2 total")

# Group A: Disposal params
disp_names <- c("disp_neg_offset", "disp_pos_offset", "disp_scale", "bounce_penalty")
cat("  Optimizing disposal params...\n")
tictoc::tic("  Group A: disposal")
opt_disp <- optim(
  par = best_par[disp_names],
  fn = function(sub_par) {
    test_par <- best_par
    test_par[disp_names] <- sub_par
    obj_fast(test_par)
  },
  method = "Nelder-Mead",
  control = list(maxit = 200, trace = 1)
)
best_par[disp_names] <- opt_disp$par
tictoc::toc()
cat(sprintf("  Disposal optimized: RMSE = %.4f\n", opt_disp$value))

# Group B: Reception params
recv_names <- c("recv_neg_mult", "recv_neg_offset", "recv_pos_mult", "recv_pos_offset", "recv_scale")
cat("  Optimizing reception params...\n")
tictoc::tic("  Group B: reception")
opt_recv <- optim(
  par = best_par[recv_names],
  fn = function(sub_par) {
    test_par <- best_par
    test_par[recv_names] <- sub_par
    obj_fast(test_par)
  },
  method = "Nelder-Mead",
  control = list(maxit = 200, trace = 1)
)
best_par[recv_names] <- opt_recv$par
tictoc::toc()
cat(sprintf("  Reception optimized: RMSE = %.4f\n", opt_recv$value))

# Group C: Spoil/tackle params
spoil_names <- c("spoil_wt", "tackle_wt", "pressure_wt", "def_pressure_wt")
cat("  Optimizing spoil/tackle params...\n")
tictoc::tic("  Group C: spoil/tackle")
opt_spoil <- optim(
  par = best_par[spoil_names],
  fn = function(sub_par) {
    test_par <- best_par
    test_par[spoil_names] <- sub_par
    obj_fast(test_par)
  },
  method = "Nelder-Mead",
  control = list(maxit = 200, trace = 1)
)
best_par[spoil_names] <- opt_spoil$par
tictoc::toc()
cat(sprintf("  Spoil optimized: RMSE = %.4f\n", opt_spoil$value))

# Group D: Hitout params
hitout_names <- c("hitout_wt", "hitout_adv_wt", "ruck_contest_wt")
cat("  Optimizing hitout params...\n")
tictoc::tic("  Group D: hitout")
opt_hitout <- optim(
  par = best_par[hitout_names],
  fn = function(sub_par) {
    test_par <- best_par
    test_par[hitout_names] <- sub_par
    obj_fast(test_par)
  },
  method = "Nelder-Mead",
  control = list(maxit = 200, trace = 1)
)
best_par[hitout_names] <- opt_hitout$par
tictoc::toc()
cat(sprintf("  Hitout optimized: RMSE = %.4f\n", opt_hitout$value))

# Group E: Aggregation params (priors + decay + prior rates)
agg_names <- c("pos_adj_quantile", "decay_days",
               "prior_games_recv", "prior_games_disp", "prior_games_spoil", "prior_games_hitout",
               "prior_rate_recv", "prior_rate_disp", "prior_rate_spoil", "prior_rate_hitout")
cat("  Optimizing aggregation params...\n")
tictoc::tic("  Group E: aggregation")
opt_agg <- optim(
  par = best_par[agg_names],
  fn = function(sub_par) {
    test_par <- best_par
    test_par[agg_names] <- sub_par
    obj_fast(test_par)
  },
  method = "Nelder-Mead",
  control = list(maxit = 300, trace = 1)
)
best_par[agg_names] <- opt_agg$par
tictoc::toc()
cat(sprintf("  Aggregation optimized: RMSE = %.4f\n", opt_agg$value))

tictoc::toc()
cat(sprintf("Stage 2 best RMSE: %.4f\n\n", opt_agg$value))

# Clamp best_par to bounds (Nelder-Mead is unconstrained)
best_par <- pmax(best_par, par_lower[names(best_par)])
best_par <- pmin(best_par, par_upper[names(best_par)])

# Recompute overall RMSE with all Stage 2 updates
stage2_rmse <- obj_fast(best_par)
cat(sprintf("Stage 2 combined RMSE: %.4f\n\n", stage2_rmse))

final_rmse <- stage2_rmse
cat(sprintf("\nFinal optimized RMSE (LOOCV): %.4f\n\n", final_rmse))

# 7. Per-fold RMSE breakdown ----
cat("=== Per-fold RMSE breakdown (leave-one-season-out) ===\n")

# Recompute torp_diff with optimized params for fold breakdown
# Use the fast objective internals to get torp_diff
# We already have final_rmse, now compute per-fold details

# Compute torp_diff using objective_fn's pipeline for reporting
credit_dt_final <- compute_credits(pgr, best_par, verbose = TRUE)
pcum_final <- compute_cumulative(credit_dt_final, best_par["decay_days"])
lookup_final <- pcum_final[, .(player_id, date_num, cum_recv, cum_disp, cum_spoil, cum_hitout, cum_wt)]
data.table::setkey(lookup_final, player_id, date_num)
joined_final <- lookup_final[lineup_dt_all,
  .(match_idx = i.match_idx, is_home = i.is_home, lineup_tog = i.lineup_tog,
    cr = x.cum_recv, cd = x.cum_disp, cs = x.cum_spoil,
    ch = x.cum_hitout, cw = x.cum_wt),
  on = .(player_id, date_num), roll = TRUE]
for (col in c("cr", "cd", "cs", "ch", "cw"))
  data.table::set(joined_final, which(is.na(joined_final[[col]])), col, 0)
joined_final[is.na(lineup_tog), lineup_tog := 0.75]

ld <- best_par["loading"]; pr <- best_par["prior_games_recv"]
pd <- best_par["prior_games_disp"]; ps <- best_par["prior_games_spoil"]
ph <- best_par["prior_games_hitout"]
prr <- best_par["prior_rate_recv"]; prd <- best_par["prior_rate_disp"]
prs <- best_par["prior_rate_spoil"]; prh <- best_par["prior_rate_hitout"]
torp_vec_final <- (ld * joined_final$cr + pr * prr) / (joined_final$cw + pr) +
                  (ld * joined_final$cd + pd * prd) / (joined_final$cw + pd) +
                  (ld * joined_final$cs + ps * prs) / (joined_final$cw + ps) +
                  (ld * joined_final$ch + ph * prh) / (joined_final$cw + ph)
torp_weighted_f <- torp_vec_final * joined_final$lineup_tog
home_sum_f <- tapply(torp_weighted_f * joined_final$is_home, joined_final$match_idx, sum, na.rm = TRUE)
away_sum_f <- tapply(torp_weighted_f * (!joined_final$is_home), joined_final$match_idx, sum, na.rm = TRUE)
torp_diff_final <- as.numeric(home_sum_f - away_sum_f)

# Per-fold RMSE for optimized params
residual_f <- eval_matches_all$margin - torp_diff_final
X_f <- cbind(1, eval_matches_all$log_dist_diff, eval_matches_all$familiarity_diff)
valid_f <- complete.cases(X_f, residual_f)

# Also compute default torp_diff for comparison
credit_dt_def <- compute_credits(pgr, par_defaults)
pcum_def <- compute_cumulative(credit_dt_def, par_defaults["decay_days"])
lookup_def <- pcum_def[, .(player_id, date_num, cum_recv, cum_disp, cum_spoil, cum_hitout, cum_wt)]
data.table::setkey(lookup_def, player_id, date_num)
joined_def <- lookup_def[lineup_dt_all,
  .(match_idx = i.match_idx, is_home = i.is_home, lineup_tog = i.lineup_tog,
    cr = x.cum_recv, cd = x.cum_disp, cs = x.cum_spoil,
    ch = x.cum_hitout, cw = x.cum_wt),
  on = .(player_id, date_num), roll = TRUE]
for (col in c("cr", "cd", "cs", "ch", "cw"))
  data.table::set(joined_def, which(is.na(joined_def[[col]])), col, 0)
joined_def[is.na(lineup_tog), lineup_tog := 0.75]

ld_d <- par_defaults["loading"]; pr_d <- par_defaults["prior_games_recv"]
pd_d <- par_defaults["prior_games_disp"]; ps_d <- par_defaults["prior_games_spoil"]
ph_d <- par_defaults["prior_games_hitout"]
prr_d <- par_defaults["prior_rate_recv"]; prd_d <- par_defaults["prior_rate_disp"]
prs_d <- par_defaults["prior_rate_spoil"]; prh_d <- par_defaults["prior_rate_hitout"]
torp_vec_def <- (ld_d * joined_def$cr + pr_d * prr_d) / (joined_def$cw + pr_d) +
                (ld_d * joined_def$cd + pd_d * prd_d) / (joined_def$cw + pd_d) +
                (ld_d * joined_def$cs + ps_d * prs_d) / (joined_def$cw + ps_d) +
                (ld_d * joined_def$ch + ph_d * prh_d) / (joined_def$cw + ph_d)
torp_weighted_d <- torp_vec_def * joined_def$lineup_tog
home_sum_d <- tapply(torp_weighted_d * joined_def$is_home, joined_def$match_idx, sum, na.rm = TRUE)
away_sum_d <- tapply(torp_weighted_d * (!joined_def$is_home), joined_def$match_idx, sum, na.rm = TRUE)
torp_diff_def <- as.numeric(home_sum_d - away_sum_d)

residual_d <- eval_matches_all$margin - torp_diff_def
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
  res_d <- eval_matches_all$margin - torp_diff_def
  X_d <- X_f  # same controls
  fit_def <- .lm.fit(X_d[train_m, , drop = FALSE], res_d[train_m])
  pred_def <- X_d[test_m, , drop = FALSE] %*% fit_def$coefficients
  rmse_def <- sqrt(mean((res_d[test_m] - pred_def)^2))
  cat(sprintf("  %-8d  %8.4f  %8.4f  %+8.4f\n", s, rmse_opt, rmse_def, rmse_def - rmse_opt))
}
rm(credit_dt_final, pcum_final, lookup_final, joined_final,
   credit_dt_def, pcum_def, lookup_def, joined_def)

# 8. Results ----
cat("\n=== OPTIMIZED PARAMETERS ===\n")
cat("\n# Credit params:\n")
for (nm in names(par_defaults)[1:17]) {
  cat(sprintf("  %-28s = %8.4f  (was %.4f)\n", nm, best_par[nm], par_defaults[nm]))
}
cat("\n# Aggregation params:\n")
for (nm in names(par_defaults)[18:length(par_defaults)]) {
  cat(sprintf("  %-28s = %8.4f  (was %.4f)\n", nm, best_par[nm], par_defaults[nm]))
}

cat("\n# Summary:\n")
cat(sprintf("  No-ratings RMSE (dist+fam only): %.4f\n", no_rating_rmse))
cat(sprintf("  Default-params RMSE:             %.4f\n", baseline_rmse))
cat(sprintf("  Optimized RMSE (LOOCV):          %.4f\n", final_rmse))
cat(sprintf("  TORP value (no-ratings - default): %.4f (%.1f%%)\n",
            no_rating_rmse - baseline_rmse,
            100 * (no_rating_rmse - baseline_rmse) / no_rating_rmse))
cat(sprintf("  Optimization gain (default - opt):  %.4f (%.1f%%)\n",
            baseline_rmse - final_rmse,
            100 * (baseline_rmse - final_rmse) / baseline_rmse))

# 9. Save Results ----

# Map from optimizer param names -> constants.R variable names
param_to_constant <- c(
  disp_neg_offset   = "CREDIT_DISP_NEG_OFFSET",
  disp_pos_offset   = "CREDIT_DISP_POS_OFFSET",
  disp_scale        = "CREDIT_DISP_SCALE",
  bounce_penalty    = "CREDIT_BOUNCE_PENALTY",
  recv_neg_mult     = "CREDIT_RECV_NEG_MULT",
  recv_neg_offset   = "CREDIT_RECV_NEG_OFFSET",
  recv_pos_mult     = "CREDIT_RECV_POS_MULT",
  recv_pos_offset   = "CREDIT_RECV_POS_OFFSET",
  recv_scale        = "CREDIT_RECV_SCALE",
  spoil_wt          = "CREDIT_SPOIL_WT",
  tackle_wt         = "CREDIT_TACKLE_WT",
  pressure_wt       = "CREDIT_PRESSURE_WT",
  def_pressure_wt   = "CREDIT_DEF_PRESSURE_WT",
  hitout_wt         = "CREDIT_HITOUT_WT",
  hitout_adv_wt     = "CREDIT_HITOUT_ADV_WT",
  ruck_contest_wt        = "CREDIT_RUCK_CONTEST_WT",
  pos_adj_quantile       = "CREDIT_POS_ADJ_QUANTILE",
  decay_days        = "RATING_DECAY_DEFAULT_DAYS",
  loading           = "RATING_LOADING_DEFAULT",
  prior_games_recv  = "RATING_PRIOR_GAMES_RECV",
  prior_games_disp  = "RATING_PRIOR_GAMES_DISP",
  prior_games_spoil = "RATING_PRIOR_GAMES_SPOIL",
  prior_games_hitout = "RATING_PRIOR_GAMES_HITOUT",
  prior_rate_recv    = "RATING_PRIOR_RATE_RECV",
  prior_rate_disp    = "RATING_PRIOR_RATE_DISP",
  prior_rate_spoil   = "RATING_PRIOR_RATE_SPOIL",
  prior_rate_hitout  = "RATING_PRIOR_RATE_HITOUT"
)

## 9a. Write optimized params back to R/constants.R ----
constants_path <- "R/constants.R"
cat(sprintf("\nUpdating %s with optimized parameters...\n", constants_path))

lines <- readLines(constants_path)
n_updated <- 0
for (par_name in names(param_to_constant)) {
  const_name <- param_to_constant[par_name]
  new_val <- best_par[par_name]

  # Format: integer-like values (decay_days) as integer, rest as 4-decimal
  if (par_name == "decay_days") {
    val_str <- sprintf("%.0f", new_val)
  } else {
    val_str <- sprintf("%.4f", new_val)
  }

  # Match the line: CONSTANT_NAME <- <value>
  pattern <- paste0("^(", const_name, "\\s*<-\\s*).*$")
  match_idx <- grep(pattern, lines)

  if (length(match_idx) == 1) {
    old_line <- lines[match_idx]
    new_line <- sub(pattern, paste0("\\1", val_str), old_line)
    if (old_line != new_line) {
      lines[match_idx] <- new_line
      n_updated <- n_updated + 1
      cat(sprintf("  %s: %s -> %s\n", const_name,
                  sub(paste0(const_name, "\\s*<-\\s*"), "", old_line), val_str))
    }
  } else {
    warning(sprintf("Could not find unique match for %s in %s", const_name, constants_path))
  }
}

writeLines(lines, constants_path)
cat(sprintf("Updated %d constants in %s\n", n_updated, constants_path))

## 9b. Save params as CSV backup ----
optimized_params_df <- data.frame(param = names(best_par), value = unname(best_par))
utils::write.csv(optimized_params_df, "data-raw/03-ratings/optimized_torp_params.csv", row.names = FALSE)
cat("Backup saved to data-raw/03-ratings/optimized_torp_params.csv\n")

cat("\n=== Optimization complete ===\n")
