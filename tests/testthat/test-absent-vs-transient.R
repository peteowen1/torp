# A missing file must be recognised as MISSING, not as a network failure.
#
# WHY THIS FILE EXISTS. parquet_from_url() decided absent-vs-transient by
# grepping the error message for "404". On R's download.file() route the HTTP
# status arrives in a *warning* and the error itself reads only "cannot open
# URL '...'", so a real 404 scored as transient, got retried three times, and
# was raised as vb_error_transient. Callers handling vb_error_absent therefore
# never saw it: building a brand-new rating vintage aborted on all six seasons,
# because the not-yet-written torp_ratings_v3.parquet looked like a network
# failure rather than the legitimately-absent file it was (2026-08-18).
#
# Both directions are asserted. The fail-safe direction matters more than the
# fix: reading a blip as "absent" is what overwrites full-history ratings-data
# with one run's seasons (torp P1/P8), so ambiguity must resolve to transient.

test_that("a bare 'cannot open URL' error is absent when the resource is gone", {
  # The exact shape download.file() produces: no status code in the message.
  e <- simpleError("cannot open URL 'https://example.invalid/torp_ratings_v3.parquet'")
  testthat::local_mocked_bindings(.url_confirmed_absent = function(url) TRUE)
  expect_true(.error_is_absent(e, "https://example.invalid/torp_ratings_v3.parquet"))
})

test_that("an ambiguous failure is NOT absent", {
  # Same unhelpful message, but the resource is there (or unknown). This must
  # stay transient -- it is the direction that protects full-history data.
  e <- simpleError("cannot open URL 'https://example.invalid/torp_ratings.parquet'")
  testthat::local_mocked_bindings(.url_confirmed_absent = function(url) FALSE)
  expect_false(.error_is_absent(e, "https://example.invalid/torp_ratings.parquet"))
})

test_that("an explicit 404 message needs no network round-trip", {
  # The message check is a fast path and must stay one: arrow reports the
  # status properly, and confirming it again would cost a request per retry.
  called <- FALSE
  testthat::local_mocked_bindings(
    .url_confirmed_absent = function(url) { called <<- TRUE; FALSE })
  expect_true(.error_is_absent(simpleError("HTTP 404 Not Found"), "https://example.invalid/x"))
  expect_false(called)
})

test_that("confirmation fails safe when the network itself is down", {
  # curl throwing tells us nothing about whether the file exists, so the only
  # admissible answer is FALSE.
  testthat::local_mocked_bindings(
    new_handle = function(...) stop("no network"), .package = "curl")
  expect_false(.url_confirmed_absent("https://example.invalid/x.parquet"))
})

test_that("an error that names a 5xx never touches the network", {
  # Confirmation must not fire when the message already tells us existence is
  # not the problem. Making every failed read do a live HEAD is how error
  # classification -- and the tests over it -- become network-dependent, which
  # this file caught on itself: the retry test uses a real host whose HEAD
  # genuinely 404s, so a simulated 500 was classified absent and never retried.
  called <- FALSE
  testthat::local_mocked_bindings(
    .url_confirmed_absent = function(url) { called <<- TRUE; TRUE })
  expect_false(.error_is_absent(simpleError("simulated 500 from release CDN"),
                                "https://example.com/file.parquet"))
  expect_false(called)
})
