test_that("build_adjacency returns an empty matrix when no players share a match", {
  pm <- data.frame(player_id = c("a", "b"), match_id = c(1, 2))
  adj <- torp:::build_adjacency(pm)
  expect_equal(dim(adj), c(2L, 2L))
  expect_equal(sum(adj), 0)
})

test_that("degenerate centrality result uses NA rather than fabricated values", {
  res <- torp:::empty_centrality_result(c("a", "b"), 5L)
  expect_true(all(is.na(res$unique_teams)))
  expect_true(all(is.na(res$matches_played)))
  expect_true(all(is.na(res$component_id)))
  expect_true(all(is.na(res$component_size)))
})
