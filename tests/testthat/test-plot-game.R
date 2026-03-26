test_that("plot_ep_wp returns ggplot for WP metric", {
  mock <- create_mock_ep_wp_data()
  p <- plot_ep_wp(match_id = "CD_M20240140101", data = mock)
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 1)
})

test_that("plot_ep_wp returns ggplot for EP metric", {
  mock <- create_mock_ep_wp_data()
  p <- plot_ep_wp(match_id = "CD_M20240140101", metric = "ep", data = mock)
  expect_s3_class(p, "ggplot")
})

test_that("plot_ep_wp errors on missing match_id", {
  mock <- create_mock_ep_wp_data()
  expect_error(plot_ep_wp(match_id = "NONEXISTENT", data = mock))
})

test_that("plot_ep_wp disables play markers", {
  mock <- create_mock_ep_wp_data()
  p <- plot_ep_wp(match_id = "CD_M20240140101", show_plays = FALSE, data = mock)
  expect_s3_class(p, "ggplot")
})
