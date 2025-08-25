test_that("fixture caching works correctly", {
  skip_if_offline()
  
  # Clear any existing cache
  clear_fixture_cache()
  
  # Test initial load (should be cache miss)
  expect_message(
    fixtures1 <- load_fixtures(seasons = 2021, verbose = TRUE),
    "Cache MISS"
  )
  
  # Test second load (should be cache hit)
  expect_message(
    fixtures2 <- load_fixtures(seasons = 2021, verbose = TRUE),
    "Cache HIT"
  )
  
  # Data should be identical
  expect_identical(fixtures1, fixtures2)
  
  # Test cache info
  cache_info <- get_cache_info()
  expect_true(nrow(cache_info) > 0)
  expect_true("fixtures_2021" %in% cache_info$cache_key)
})

test_that("cache expiration works", {
  skip_if_offline()
  
  # Clear cache
  clear_fixture_cache()
  
  # Load with very short TTL
  fixtures1 <- load_fixtures(seasons = 2021, cache_ttl = 0.1)
  
  # Wait for cache to expire
  Sys.sleep(0.2)
  
  # Should get fresh data (cache expired)
  expect_message(
    fixtures2 <- load_fixtures(seasons = 2021, verbose = TRUE, cache_ttl = 0.1),
    "Cache EXPIRED"
  )
})

test_that("cache can be disabled", {
  skip_if_offline()
  
  # Clear cache
  clear_fixture_cache()
  
  # Load without cache
  fixtures1 <- load_fixtures(seasons = 2021, use_cache = FALSE)
  fixtures2 <- load_fixtures(seasons = 2021, use_cache = FALSE)
  
  # Cache should still be empty
  cache_info <- get_cache_info()
  expect_equal(nrow(cache_info), 0)
})

test_that("all=TRUE caching works", {
  skip_if_offline()
  
  # Clear cache
  clear_fixture_cache()
  
  # Load all fixtures
  expect_message(
    fixtures1 <- load_fixtures(all = TRUE, verbose = TRUE),
    "Cache MISS"
  )
  
  # Second load should hit cache
  expect_message(
    fixtures2 <- load_fixtures(all = TRUE, verbose = TRUE),
    "Cache HIT"
  )
  
  expect_identical(fixtures1, fixtures2)
  
  # Check cache info
  cache_info <- get_cache_info()
  expect_true("fixtures_all" %in% cache_info$cache_key)
})