# Tests for R/centrality.R

test_that("calculate_player_centrality returns expected structure", {
  skip_if_not_installed("Matrix")

  # Realistic mock: each match has ~5 players per team, not all players in all matches
  pm <- rbind(
    data.frame(player_id = paste0("P", 1:5), match_id = "M1", team = "TeamA", stringsAsFactors = FALSE),
    data.frame(player_id = paste0("P", 6:10), match_id = "M1", team = "TeamB", stringsAsFactors = FALSE),
    data.frame(player_id = paste0("P", 1:5), match_id = "M2", team = "TeamA", stringsAsFactors = FALSE),
    data.frame(player_id = paste0("P", 11:15), match_id = "M2", team = "TeamC", stringsAsFactors = FALSE),
    data.frame(player_id = paste0("P", 6:10), match_id = "M3", team = "TeamB", stringsAsFactors = FALSE),
    data.frame(player_id = paste0("P", 11:15), match_id = "M3", team = "TeamC", stringsAsFactors = FALSE)
  )

  result <- calculate_player_centrality(pm, min_matches = 1L)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("player_id", "centrality", "unique_teams",
                     "matches_played", "component_id", "component_size") %in% names(result)))
  expect_true(nrow(result) > 0)
  expect_true(all(result$centrality >= 0 & result$centrality <= 1))
  expect_true(all(result$matches_played >= 1))
})

test_that("calculate_player_centrality respects min_matches filter", {
  skip_if_not_installed("Matrix")

  # P1 and P2 share 5+ matches, P3 only has 2
  pm <- rbind(
    data.frame(player_id = c("P1", "P2"), match_id = "M1", team = c("A", "B"), stringsAsFactors = FALSE),
    data.frame(player_id = c("P1", "P2"), match_id = "M2", team = c("A", "B"), stringsAsFactors = FALSE),
    data.frame(player_id = c("P1", "P2"), match_id = "M3", team = c("A", "B"), stringsAsFactors = FALSE),
    data.frame(player_id = c("P1", "P2"), match_id = "M4", team = c("A", "B"), stringsAsFactors = FALSE),
    data.frame(player_id = c("P1", "P2"), match_id = "M5", team = c("A", "B"), stringsAsFactors = FALSE),
    data.frame(player_id = c("P3", "P1"), match_id = "M6", team = c("A", "A"), stringsAsFactors = FALSE),
    data.frame(player_id = c("P3", "P2"), match_id = "M7", team = c("A", "B"), stringsAsFactors = FALSE)
  )

  result <- calculate_player_centrality(pm, min_matches = 5L)
  expect_false("P3" %in% result$player_id)
})

test_that("calculate_player_centrality handles camelCase column names", {
  skip_if_not_installed("Matrix")

  pm <- rbind(
    data.frame(playerId = paste0("P", 1:3), matchId = "M1", team = "A", stringsAsFactors = FALSE),
    data.frame(playerId = paste0("P", 1:3), matchId = "M2", team = "A", stringsAsFactors = FALSE),
    data.frame(playerId = paste0("P", 4:6), matchId = "M1", team = "B", stringsAsFactors = FALSE),
    data.frame(playerId = paste0("P", 4:6), matchId = "M2", team = "B", stringsAsFactors = FALSE)
  )

  result <- calculate_player_centrality(pm, min_matches = 1L)
  expect_s3_class(result, "data.frame")
  expect_true("player_id" %in% names(result))
})

test_that("calculate_player_centrality warns when too few players", {
  skip_if_not_installed("Matrix")

  pm <- data.frame(
    player_id = rep("P1", 3),
    match_id = paste0("M", 1:3),
    team = rep("TeamA", 3),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- calculate_player_centrality(pm, min_matches = 5L),
    "Fewer than 2"
  )
})

test_that("find_components preserves names on sizes", {
  skip_if_not_installed("Matrix")

  # Create a simple adjacency matrix
  adj <- Matrix::Matrix(0, nrow = 3, ncol = 3, sparse = TRUE)
  adj[1, 2] <- adj[2, 1] <- 1
  adj[1, 3] <- adj[3, 1] <- 1
  rownames(adj) <- colnames(adj) <- c("P1", "P2", "P3")

  components <- torp:::find_components(adj)

  expect_true(!is.null(names(components$sizes)))
  expect_equal(components$n_components, 1L)
  # All in one component of size 3
  expect_equal(unname(components$sizes[1]), 3L)
})

test_that("compute_centrality_scores returns named numeric vector", {
  skip_if_not_installed("Matrix")

  adj <- Matrix::Matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3, sparse = TRUE)
  rownames(adj) <- colnames(adj) <- c("P1", "P2", "P3")

  scores <- torp:::compute_centrality_scores(adj, damping = 0.85)

  expect_true(is.numeric(scores))
  expect_equal(length(scores), 3)
  expect_true(all(!is.na(scores)))
})
