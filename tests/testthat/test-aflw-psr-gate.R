utc <- function(x) as.POSIXct(x, tz = "UTC")

gate_fx <- function() data.frame(
  season = c(2025L, 2026L, 2026L),
  status = c("CONCLUDED", "CONCLUDED", "CONCLUDED"),
  utc_start_time = c("2025-11-29T08:45:00.000+0000", "2026-11-21T05:05:00.000+0000",
                     "2026-11-27T08:45:00.000+0000"),
  stringsAsFactors = FALSE
)
gate_assets <- function(t) {
  stats::setNames(rep(utc(t), 2), c("aflw_psr_2025.parquet", "aflw_psr_2026.parquet"))
}

test_that("skips when files were published well after the last match and are recent", {
  d <- .aflw_psr_gate_decision(gate_assets("2027-03-10 06:20:00"), gate_fx(), utc("2027-03-20 06:00:00"))
  expect_true(d$skip)
  expect_match(d$reason, "no AFLW match since 2026-11-27 12:45")
})

test_that("runs inside the grace window after a match", {
  d <- .aflw_psr_gate_decision(gate_assets("2026-11-28 06:20:00"), gate_fx(), utc("2026-11-29 06:00:00"))
  expect_false(d$skip)
  expect_match(d$reason, "not 3 days after")
})

test_that("runs when a published file is older than the age limit", {
  d <- .aflw_psr_gate_decision(gate_assets("2026-12-05 06:20:00"), gate_fx(), utc("2027-03-20 06:00:00"))
  expect_false(d$skip)
  expect_match(d$reason, "days old")
})

test_that("runs when one season's file is older than the last match", {
  a <- gate_assets("2027-03-10 06:20:00")
  a["aflw_psr_2025.parquet"] <- utc("2026-11-20 06:20:00")
  expect_false(.aflw_psr_gate_decision(a, gate_fx(), utc("2027-03-20 06:00:00"))$skip)
})

test_that("runs when a season with played matches has no file", {
  d <- .aflw_psr_gate_decision(gate_assets("2027-03-10 06:20:00")[1], gate_fx(), utc("2027-03-20 06:00:00"))
  expect_false(d$skip)
  expect_match(d$reason, "aflw_psr_2026.parquet")
})

test_that("a new concluded match turns a skip into a run", {
  fx <- rbind(gate_fx(), data.frame(season = 2027L, status = "CONCLUDED",
                                    utc_start_time = "2027-08-14T05:05:00.000+0000"))
  a <- c(gate_assets("2027-08-10 06:20:00"),
         stats::setNames(utc("2027-08-10 06:20:00"), "aflw_psr_2027.parquet"))
  expect_false(.aflw_psr_gate_decision(a, fx, utc("2027-08-15 06:00:00"))$skip)
})

test_that("unplayed and cancelled fixtures do not count as matches", {
  fx <- rbind(gate_fx(), data.frame(season = 2027L, status = c("SCHEDULED", "CANCELLED"),
                                    utc_start_time = "2027-08-14T05:05:00.000+0000"))
  expect_true(.aflw_psr_gate_decision(gate_assets("2027-03-10 06:20:00"), fx, utc("2027-03-20 06:00:00"))$skip)
})

test_that("anything unreadable runs", {
  now <- utc("2027-03-20 06:00:00")
  ok <- gate_assets("2027-03-10 06:20:00")
  expect_false(.aflw_psr_gate_decision(ok, gate_fx()[0, ], now)$skip)
  expect_false(.aflw_psr_gate_decision(ok, data.frame(x = 1), now)$skip)
  expect_false(.aflw_psr_gate_decision(NULL, gate_fx(), now)$skip)
  expect_false(.aflw_psr_gate_decision(unname(ok), gate_fx(), now)$skip)
  bad <- gate_fx(); bad$utc_start_time[3] <- NA
  expect_false(.aflw_psr_gate_decision(ok, bad, now)$skip)
  na_time <- ok; na_time[1] <- NA
  expect_false(.aflw_psr_gate_decision(na_time, gate_fx(), now)$skip)
  expect_false(.aflw_psr_gate_decision(ok, gate_fx(), as.POSIXct(NA))$skip)
  expect_false(.aflw_psr_gate_decision(ok, gate_fx(), now, force = TRUE)$skip)
})

test_that("the wrapper runs on a fetch error and on the force variable", {
  local_mocked_bindings(.aflw_psr_asset_times = function() stop("network down"), .package = "torp")
  d <- .aflw_psr_gate()
  expect_false(d$skip)
  expect_match(d$reason, "gate error: network down")
  withr::local_envvar(TORP_FORCE_AFLW_PSR = "true")
  expect_match(.aflw_psr_gate()$reason, "forced")
})
