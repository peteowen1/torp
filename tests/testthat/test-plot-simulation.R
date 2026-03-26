test_that("plot_simulation returns ggplot for ladder type", {
  mock_sim <- create_mock_sim_results()
  p <- plot_simulation(mock_sim, type = "ladder")
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 1)
})

test_that("plot_simulation returns ggplot for position type", {
  mock_sim <- create_mock_sim_results()
  p <- plot_simulation(mock_sim, type = "position")
  expect_s3_class(p, "ggplot")
})

test_that("plot_simulation returns ggplot for finals type", {
  mock_sim <- create_mock_sim_results()
  p <- plot_simulation(mock_sim, type = "finals")
  expect_s3_class(p, "ggplot")
})

test_that("plot_simulation filters teams", {
  mock_sim <- create_mock_sim_results()
  p <- plot_simulation(mock_sim, type = "ladder", teams = c("Adelaide Crows", "Brisbane Lions"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_simulation errors on wrong class", {
  expect_error(plot_simulation(list(a = 1)))
})
