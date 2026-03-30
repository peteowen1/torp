# Player Network Centrality
#
# centrality-based quality adjustment for player ratings. Detects "isolated
# cluster inflation" where a player's rating looks inflated because they
# only played against weak opponents.
#
# Ported from bouncer (cricket) and adapted for AFL's team-based structure.
# Players are connected through shared matches (teammates + opponents).
# centrality measures how well-connected a player is in the competition network.
#
# Reference: Opsahl, Agneessens & Skvoretz (2010) 'Node centrality in weighted networks'

#' Calculate Player Centrality
#'
#' Builds a player interaction network from match data and computes
#' centrality centrality scores. Players who face diverse, high-quality
#' opponents get higher centrality. Players in isolated pools (e.g.,
#' few games, always same opponents) get lower centrality.
#'
#' @param player_matches Data frame with columns:
#'   - `player_id` or `playerId`: Player identifier
#'   - `team`: Player's team
#'   - `match_id` or `matchId`: Match identifier
#'   - `time_on_ground` (optional): Playing time as weight
#' @param min_matches Integer. Minimum matches for inclusion. Default 5.
#' @param damping Numeric. centrality damping factor (0-1). Default 0.85.
#'
#' @return Data frame with player_id, centrality (0-1 normalized),
#'   unique_opponents, matches_played, component_id, component_size
#' @export
#'
#' @examples
#' \dontrun{
#' # From player_stats data
#' ps <- load_player_stats(2024)
#' centrality <- calculate_player_centrality(ps)
#' }
calculate_player_centrality <- function(player_matches,
                                         min_matches = 5L,
                                         damping = 0.85) {

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg Matrix} is required for centrality calculation.")
  }

  # Normalize column names
  if ("playerId" %in% names(player_matches)) {
    names(player_matches)[names(player_matches) == "playerId"] <- "player_id"
  }
  if ("matchId" %in% names(player_matches)) {
    names(player_matches)[names(player_matches) == "matchId"] <- "match_id"
  }

  # Filter to players with enough matches
  match_counts <- stats::aggregate(
    match_id ~ player_id, data = player_matches,
    FUN = function(x) length(unique(x))
  )
  names(match_counts)[2] <- "n_matches"
  valid_players <- match_counts$player_id[match_counts$n_matches >= min_matches]

  if (length(valid_players) < 2) {
    cli::cli_warn("Fewer than 2 players meet minimum matches ({min_matches})")
    return(empty_centrality_result(valid_players, min_matches))
  }

  pm <- player_matches[player_matches$player_id %in% valid_players, ]

  # Build adjacency, find components, compute centrality
  adj <- build_adjacency(pm)
  components <- find_components(adj)
  scores <- compute_centrality_scores(adj, damping = damping)

  # Normalize to 0-1
  pr_range <- range(scores)
  if (pr_range[2] > pr_range[1]) {
    centrality <- (scores - pr_range[1]) / (pr_range[2] - pr_range[1])
  } else {
    centrality <- rep(1, length(scores))
  }

  # Count unique teams the player appeared on (transfers/trades)
  if ("team" %in% names(pm)) {
    opp <- stats::aggregate(
      team ~ player_id, data = pm,
      FUN = function(x) length(unique(x))
    )
    names(opp)[2] <- "unique_teams"
  } else {
    opp <- data.frame(player_id = names(centrality), unique_teams = NA_integer_)
  }

  player_ids <- names(centrality)
  result <- data.frame(
    player_id = player_ids,
    centrality = round(as.numeric(centrality[player_ids]), 4),
    unique_teams = opp$unique_teams[match(player_ids, opp$player_id)],
    matches_played = match_counts$n_matches[match(player_ids, match_counts$player_id)],
    component_id = components$membership[player_ids],
    component_size = components$sizes[as.character(components$membership[player_ids])],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Penalize small components
  main_size <- max(components$sizes)
  result$centrality <- result$centrality * pmin(1, result$component_size / main_size)

  cli::cli_alert_success(
    "Centrality: {nrow(result)} players, {components$n_components} components (main: {main_size})"
  )

  result[order(-result$centrality), ]
}


#' @keywords internal
empty_centrality_result <- function(players, min_matches) {
  data.frame(
    player_id = players,
    centrality = 1,
    unique_teams = 0L,
    matches_played = min_matches,
    component_id = 1L,
    component_size = length(players),
    stringsAsFactors = FALSE
  )
}


#' Build Player Adjacency Matrix
#' @keywords internal
build_adjacency <- function(pm) {
  player_ids <- sort(unique(pm$player_id))
  n <- length(player_ids)
  idx <- stats::setNames(seq_along(player_ids), player_ids)

  has_tog <- "time_on_ground" %in% names(pm)

  matches <- split(pm, pm$match_id)
  row_chunks <- vector("list", length(matches))
  col_chunks <- vector("list", length(matches))
  val_chunks <- vector("list", length(matches))

  for (m_idx in seq_along(matches)) {
    pids <- unique(matches[[m_idx]]$player_id)
    if (length(pids) < 2) next
    pidx <- idx[pids]
    pairs <- utils::combn(pidx, 2)
    np <- ncol(pairs)
    row_chunks[[m_idx]] <- c(pairs[1, ], pairs[2, ])
    col_chunks[[m_idx]] <- c(pairs[2, ], pairs[1, ])
    val_chunks[[m_idx]] <- rep(1, np * 2)
  }

  Matrix::sparseMatrix(
    i = unlist(row_chunks), j = unlist(col_chunks), x = unlist(val_chunks),
    dims = c(n, n), dimnames = list(player_ids, player_ids)
  )
}


#' Find Connected Components (Union-Find)
#' @keywords internal
find_components <- function(adj) {
  n <- nrow(adj)
  ids <- rownames(adj)
  parent <- seq_len(n)

  find_root <- function(i) {
    root <- i
    while (parent[root] != root) root <- parent[root]
    while (parent[i] != root) {
      nxt <- parent[i]; parent[i] <<- root; i <- nxt
    }
    root
  }

  s <- Matrix::summary(adj)
  for (k in seq_len(nrow(s))) {
    ri <- find_root(s$i[k])
    rj <- find_root(s$j[k])
    if (ri != rj) parent[ri] <- rj
  }

  membership <- vapply(seq_len(n), find_root, integer(1))
  names(membership) <- ids
  comp_table <- table(membership)
  sizes <- as.integer(comp_table)
  names(sizes) <- names(comp_table)
  list(membership = membership, sizes = sizes,
       n_components = length(comp_table))
}


#' Compute centrality via Power Iteration
#' @keywords internal
compute_centrality_scores <- function(adj, damping = 0.85, max_iter = 100L, tol = 1e-6) {
  n <- nrow(adj)
  ids <- rownames(adj)
  cs <- Matrix::colSums(adj)
  cs[cs == 0] <- 1
  trans <- adj %*% Matrix::Diagonal(n, 1 / cs)
  pr <- rep(1 / n, n)
  teleport <- (1 - damping) / n

  for (iter in seq_len(max_iter)) {
    pr_new <- as.numeric(damping * (trans %*% pr)) + teleport
    pr_new <- pr_new / sum(pr_new)
    if (max(abs(pr_new - pr)) < tol) break
    pr <- pr_new
  }
  names(pr) <- ids
  pr
}
