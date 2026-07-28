# adjust_epv_for_opponents() -- stale/mixed-vintage input guards.
#
# Regression coverage for the 2026-07-27 incident: the ratings rebuild's
# Stage 3 read a LOCAL torpdata/data copy that was stale for exactly 2022 and
# 2025 (2026-04-16, predating the metric-first column rename), so those seasons
# carried `recv_epv_adj` where fresh seasons carried `epv_recv_adj`. Their new-
# name columns were all NA; `.abs_total` (a sum of abs() across components) went
# NA; every _oadj for those seasons went NA; EPR counted the games toward
# wt_gms while adding nothing to the numerator; and both seasons' published epr
# SD deflated as 1/wt_gms (2.43 -> 1.09 and 2.40 -> 1.25, cor +0.99) with no
# warning anywhere.
#
# Network-free: operates on synthetic frames only.

.oadj_frame <- function(seasons = c(2021, 2022), n_per = 6, na_seasons = NULL) {
  rows <- do.call(rbind, lapply(seasons, function(s) {
    data.frame(
      match_id = paste0("M", s, "_", rep(seq_len(n_per / 2), each = 2)),
      team     = rep(c("A", "B"), n_per / 2),
      opponent = rep(c("B", "A"), n_per / 2),
      season   = s,
      utc_start_time = paste0(s, "-04-", sprintf("%02d", rep(seq_len(n_per / 2), each = 2)),
                              "T10:00:00.000+0000"),
      time_on_ground_percentage = 80,
      epv_recv_adj   = seq_len(n_per) * 0.5,
      epv_disp_adj   = seq_len(n_per) * 0.3,
      epv_spoil_adj  = seq_len(n_per) * 0.1,
      epv_hitout_adj = seq_len(n_per) * 0.05,
      stringsAsFactors = FALSE
    )
  }))
  rows$epv_adj <- rows$epv_recv_adj + rows$epv_disp_adj +
    rows$epv_spoil_adj + rows$epv_hitout_adj
  # Simulate a stale-vintage season: new-name columns absent (all NA), old-name
  # columns present instead -- exactly what a pre-rename parquet produces after
  # rbind with fresh seasons.
  for (s in na_seasons) {
    idx <- rows$season == s
    for (cc in c("epv_recv_adj", "epv_disp_adj", "epv_spoil_adj", "epv_hitout_adj")) {
      rows[[cc]][idx] <- NA_real_
    }
  }
  rows
}

test_that("a season with all-NA EPV components aborts, naming that season", {
  df <- .oadj_frame(na_seasons = 2022)
  expect_error(
    adjust_epv_for_opponents(df),
    class = "torp_error_stale_input"
  )
  expect_error(adjust_epv_for_opponents(df), "2022")
})

test_that("the abort points at a stale vintage when pre-rename columns are present", {
  # The diagnostic that matters operationally: say WHY the columns are empty,
  # so the operator refreshes the inputs instead of hunting a modelling bug.
  df <- .oadj_frame(na_seasons = 2022)
  df$recv_epv_adj <- 1  # legacy naming alongside -> the stale-vintage tell
  expect_error(adjust_epv_for_opponents(df), "STALE|stale")
})

test_that("clean multi-season input passes and produces no NA _oadj", {
  df <- .oadj_frame()
  out <- adjust_epv_for_opponents(df)
  expect_true("epv_oadj" %in% names(out))
  expect_equal(sum(is.na(out$epv_oadj)), 0L)
  # Both seasons must survive -- the incident wiped one while leaving the other.
  expect_setequal(unique(out$season), c(2021, 2022))
  for (s in c(2021, 2022)) {
    expect_false(all(is.na(out$epv_oadj[out$season == s])), info = paste("season", s))
  }
})

test_that("the adjustment preserves scale rather than deflating a season", {
  # The published symptom was a whole season shrinking toward zero. The
  # adjustment is additive, so per-season dispersion should be near-unchanged.
  df <- .oadj_frame(n_per = 10)
  out <- adjust_epv_for_opponents(df)
  for (s in c(2021, 2022)) {
    before <- stats::sd(df$epv_adj[df$season == s])
    after  <- stats::sd(out$epv_oadj[out$season == s])
    expect_gt(after / before, 0.5)
    expect_lt(after / before, 2.0)
  }
})

test_that("all-NA _oadj from any other path is caught by the output backstop", {
  # The input guard covers the known cause; this covers everything else. Force
  # the failure past the input check by nulling a component only after it.
  df <- .oadj_frame()
  testthat::local_mocked_bindings(
    .compute_rolling_epv_profiles = function(dt, ...) {
      # Corrupt epv_adj specifically: returning zero profiles takes the
      # no-profiles early return, which copies epv_adj straight into epv_oadj.
      # That path used to return before any output check, so an NA here escaped
      # silently -- this asserts the backstop now covers it too.
      dt[, epv_adj := NA_real_]
      data.table::data.table(match_id = character(0), defending_team = character(0),
                             epv_opp_adj = numeric(0))
    }
  )
  expect_error(
    suppressWarnings(adjust_epv_for_opponents(df)),
    class = "torp_error_na_oadj"
  )
})
