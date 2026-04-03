# EPV Opponent Adjustment
# ========================
# Additive adjustment to per-game EPV based on opponent defensive quality.
# Teams that allow more EPV inflate their opponents' raw EPV — this corrects
# for that by adding/subtracting the gap between opponent EPV-allowed and
# league average, distributed by player TOG share.
#
# Uses rolling decay-weighted team defensive profiles with prior shrinkage,
# computed from player_game_data itself.

#' Compute rolling team EPV defensive profiles
#'
#' For each team at each match date, computes the decay-weighted average
#' total EPV that team concedes per game. Profiles are shrunk toward the
#' league mean via pseudo-games.
#'
#' @param pgd_dt data.table of player_game_data with match_id, team, opponent,
#'   utc_start_time, epv_adj, time_on_ground_percentage columns.
#' @param lambda_decay Numeric. Decay rate per day for defensive profile
#'   recency weighting.
#' @param prior_games Numeric. Pseudo-games at league average to add for
#'   shrinkage (handles early-season small samples).
#' @return data.table with columns: match_id, team (the defending team),
#'   epv_allowed (decay-weighted avg EPV conceded), league_avg_epv,
#'   epv_opp_adj (additive adjustment to apply to opponents).
#' @keywords internal
.compute_rolling_epv_profiles <- function(pgd_dt, lambda_decay, prior_games) {
  # Step 1: Aggregate EPV to team-game level
  team_game <- pgd_dt[, .(
    team_epv = sum(epv_adj * pmax(data.table::fifelse(
      is.na(time_on_ground_percentage), 100,
      as.numeric(time_on_ground_percentage)) / 100, 0.1), na.rm = TRUE),
    game_date = as.Date(utc_start_time[1])
  ), by = .(match_id, team, opponent)]

  data.table::setorder(team_game, game_date)

  # Step 2: "EPV allowed" = the EPV that the opponent's team generated
 # For each row: team T conceded team_epv to opponent O
  # So from T's perspective as a defence, they allowed this EPV
  # We need: for each (match_id, defending_team), the EPV they allowed
  # = the team_epv of the attacking team in that match
  allowed <- team_game[, .(match_id, defending_team = opponent,
                            attacking_team = team,
                            epv_allowed = team_epv,
                            game_date)]

  # Step 3: For each match, compute each defending team's rolling avg EPV allowed
  # using only matches BEFORE this one (strictly causal)
  all_matches <- unique(allowed[, .(match_id, game_date)])
  data.table::setorder(all_matches, game_date)

  # Get all unique defending teams
  all_teams <- unique(allowed$defending_team)

  # Build rolling profiles via cumulative decay-weighted sums
  profiles_list <- vector("list", nrow(all_matches))

  for (i in seq_len(nrow(all_matches))) {
    mid <- all_matches$match_id[i]
    mdate <- all_matches$game_date[i]

    # All prior games for each team
    prior <- allowed[game_date < mdate]

    if (nrow(prior) == 0) {
      # No prior data — all teams get league average (unknown)
      profiles_list[[i]] <- data.table::data.table(
        match_id = mid,
        defending_team = character(0),
        epv_allowed_avg = numeric(0)
      )
      next
    }

    # Compute decay-weighted average EPV allowed per defending team
    prior[, decay_wt := exp(-lambda_decay * as.numeric(mdate - game_date))]

    # League average (guarded against zero weights)
    total_prior_wt <- sum(prior$decay_wt, na.rm = TRUE)
    league_avg_val <- if (total_prior_wt > 0) {
      sum(prior$epv_allowed * prior$decay_wt, na.rm = TRUE) / total_prior_wt
    } else {
      0
    }

    team_profiles <- prior[, {
      wt_sum <- sum(decay_wt, na.rm = TRUE)
      n_games <- .N
      if (wt_sum == 0) {
        .(epv_allowed_avg = league_avg_val, n_def_games = n_games)
      } else {
        wt_mean <- sum(epv_allowed * decay_wt, na.rm = TRUE) / wt_sum
        shrunk <- (wt_sum * wt_mean + prior_games * league_avg_val) / (wt_sum + prior_games)
        .(epv_allowed_avg = shrunk, n_def_games = n_games)
      }
    }, by = defending_team]

    team_profiles[, match_id := mid]
    profiles_list[[i]] <- team_profiles
  }

  profiles <- data.table::rbindlist(profiles_list, fill = TRUE)

  # Step 4: Compute league average at each match date
  profiles[, league_avg := mean(epv_allowed_avg), by = match_id]

  # Step 5: Additive adjustment = league_avg - opponent_allowed
  # Positive = opponent is tougher than average (boost player EPV)
  # Negative = opponent is easier than average (deflate player EPV)
  profiles[, epv_opp_adj := league_avg - epv_allowed_avg]

  profiles[]
}


#' Apply additive EPV opponent adjustment to player game data
#'
#' For each player-game, computes opponent-quality-adjusted EPV columns
#' (`_oadj` suffix) alongside the existing position-adjusted `_adj` columns.
#' The adjustment is the gap between league-average EPV conceded and the
#' specific opponent's EPV conceded, distributed by player TOG share.
#'
#' The full adjustment chain visible in output:
#' \itemize{
#'   \item \code{recv_epv} — raw credit from PBP
#'   \item \code{recv_epv_adj} — position-centered (per-80-min)
#'   \item \code{recv_epv_oadj} — position + opponent adjusted
#' }
#'
#' @param player_game_data data.table of player game data with epv_adj,
#'   recv_epv_adj, disp_epv_adj, spoil_epv_adj, hitout_epv_adj columns.
#' @param lambda_decay Decay rate per day for opponent defensive profiles.
#' @param prior_games Pseudo-games at league average for shrinkage.
#' @return The input data with additional \code{_oadj} columns appended:
#'   \code{recv_epv_oadj}, \code{disp_epv_oadj}, \code{spoil_epv_oadj},
#'   \code{hitout_epv_oadj}, \code{epv_oadj}. Original columns unchanged.
#' @export
adjust_epv_for_opponents <- function(player_game_data,
                                      lambda_decay = EPV_OPP_LAMBDA_DECAY,
                                      prior_games = EPV_OPP_PRIOR_GAMES) {
  dt <- data.table::as.data.table(player_game_data)

  # Validate required columns
  required <- c("match_id", "team", "opponent", "utc_start_time",
                 "epv_adj", "time_on_ground_percentage")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    cli::cli_abort("Missing columns: {paste(missing, collapse = ', ')}")
  }

  cli::cli_inform("Computing rolling team EPV defensive profiles...")

  # Compute opponent profiles
  profiles <- .compute_rolling_epv_profiles(dt, lambda_decay, prior_games)

  if (nrow(profiles) == 0) {
    cli::cli_warn("No opponent profiles computed, copying _adj to _oadj unchanged")
    dt[, `:=`(
      recv_epv_oadj = recv_epv_adj, disp_epv_oadj = disp_epv_adj,
      spoil_epv_oadj = spoil_epv_adj, hitout_epv_oadj = hitout_epv_adj,
      epv_oadj = epv_adj
    )]
    return(dt)
  }

  # Join: for each player-game, look up their opponent's defensive profile
  dt <- merge(dt, profiles[, .(match_id, defending_team, epv_opp_adj)],
              by.x = c("match_id", "opponent"),
              by.y = c("match_id", "defending_team"),
              all.x = TRUE)

  # Compute each player's TOG share within their team for this game
  dt[, .tog_safe := pmax(data.table::fifelse(
    is.na(time_on_ground_percentage), 100,
    as.numeric(time_on_ground_percentage)) / 100, 0.1)]
  dt[, .team_tog := sum(.tog_safe, na.rm = TRUE), by = .(match_id, team)]
  dt[, .tog_share := .tog_safe / .team_tog]

  # Distribute adjustment by TOG share
  n_no_profile <- sum(is.na(dt$epv_opp_adj))
  if (n_no_profile > 0) {
    cli::cli_inform("{n_no_profile} player-games had no opponent profile (no adjustment applied)")
  }
  dt[is.na(epv_opp_adj), epv_opp_adj := 0]
  dt[, .player_adj := epv_opp_adj * .tog_share]

  # Create _oadj columns = _adj + opponent adjustment
  # Split adjustment across components by their absolute magnitude share
  epv_comps <- c("recv_epv", "disp_epv", "spoil_epv", "hitout_epv")
  adj_comps <- paste0(epv_comps, "_adj")
  oadj_comps <- paste0(epv_comps, "_oadj")

  if (all(adj_comps %in% names(dt))) {
    dt[, .abs_total := abs(recv_epv_adj) + abs(disp_epv_adj) +
                        abs(spoil_epv_adj) + abs(hitout_epv_adj)]
    dt[.abs_total == 0, .abs_total := 1]

    for (i in seq_along(adj_comps)) {
      dt[, (oadj_comps[i]) := get(adj_comps[i]) +
           .player_adj * abs(get(adj_comps[i])) / .abs_total]
    }
    dt[, epv_oadj := recv_epv_oadj + disp_epv_oadj + spoil_epv_oadj + hitout_epv_oadj]
  } else {
    dt[, epv_oadj := epv_adj + .player_adj]
  }

  # Clean up temp columns
  temp_cols <- c("epv_opp_adj", ".tog_safe", ".team_tog", ".tog_share",
                 ".player_adj", ".abs_total")
  dt[, (intersect(temp_cols, names(dt))) := NULL]

  cli::cli_inform("EPV opponent adjustment: {nrow(dt)} player-games, _oadj columns added")

  dt[]
}
