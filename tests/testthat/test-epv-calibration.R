test_that("channel scales are the coefficients, and both criteria hold at once", {
  # The claim this pins: scaling each channel by its own margin coefficient
  # makes every channel AND the total read 1.000. The reciprocal was applied
  # once by mistake and produced a confident, wrong conclusion that the two
  # criteria conflict, so the property is asserted rather than reasoned about.
  set.seed(7)
  n <- 2000
  teams <- c("A", "B")
  mid <- sprintf("M%04d", seq_len(n))

  # Three channels with genuinely different margin conversions, built so the
  # true coefficients are known: 2.0, 1.0 and 0.25.
  d_recv <- stats::rnorm(n, 0, 5)
  d_disp <- stats::rnorm(n, 0, 8)
  d_cont <- stats::rnorm(n, 0, 12)
  margin <- 2.0 * d_recv + 1.0 * d_disp + 0.25 * d_cont + stats::rnorm(n, 0, 4)

  # One "player" per team per match carrying the whole team total, which is all
  # the fit needs -- it aggregates by (match_id, team) regardless.
  pgd <- data.table::data.table(
    match_id = rep(mid, each = 2),
    team = rep(teams, times = n),
    epv_recv = as.vector(rbind(d_recv, 0)),
    epv_disp = as.vector(rbind(d_disp, 0)),
    epv_spoil = as.vector(rbind(d_cont, 0))
  )
  results <- data.frame(
    match_id = mid, home_team_name = "A", away_team_name = "B",
    home_score = margin, away_score = 0
  )

  s <- fit_epv_channel_scale(pgd, results)
  expect_named(s, c("epv_recv", "epv_disp", "epv_spoil"))
  # as.numeric() not unname(): the n_matches attribute rides along otherwise
  # and expect_equal compares attributes too.
  expect_equal(as.numeric(s), c(2.0, 1.0, 0.25), tolerance = 0.05)

  cal <- calibrate_epv_channels(pgd, results)
  expect_true(all(c("epv_recv_cal", "epv_disp_cal", "epv_spoil_cal", "epv_cal")
                  %in% names(cal)))
  # Existing columns untouched -- the function is additive by design.
  expect_equal(cal$epv_recv, pgd$epv_recv)
  expect_equal(cal$epv_spoil, pgd$epv_spoil)

  # The property. Both the per-channel fit and the total must read 1.000.
  ts <- cal[, lapply(.SD, sum),
            .SDcols = c("epv_recv_cal", "epv_disp_cal", "epv_spoil_cal"),
            by = .(match_id, team)]
  h <- ts[team == "A"]; a <- ts[team == "B"]
  m <- merge(h, a, by = "match_id", suffixes = c("_h", "_a"))
  m$margin <- margin[match(m$match_id, mid)]
  for (v in c("epv_recv_cal", "epv_disp_cal", "epv_spoil_cal")) {
    m[[paste0("d_", v)]] <- m[[paste0(v, "_h")]] - m[[paste0(v, "_a")]]
  }
  per_channel <- stats::coef(stats::lm(
    margin ~ 0 + d_epv_recv_cal + d_epv_disp_cal + d_epv_spoil_cal, data = m))
  expect_equal(unname(per_channel), c(1, 1, 1), tolerance = 1e-8)

  m$tot <- m$d_epv_recv_cal + m$d_epv_disp_cal + m$d_epv_spoil_cal
  total <- stats::coef(stats::lm(margin ~ 0 + tot, data = m))[[1]]
  expect_equal(total, 1, tolerance = 1e-8)

  # And the reciprocal -- the mistake -- must NOT have this property, so a
  # future edit that reintroduces it fails here rather than in a gate.
  inv <- 1 / s
  m$tot_inv <- inv[["epv_recv"]] * m$d_epv_recv_cal / s[["epv_recv"]] +
               inv[["epv_disp"]] * m$d_epv_disp_cal / s[["epv_disp"]] +
               inv[["epv_spoil"]] * m$d_epv_spoil_cal / s[["epv_spoil"]]
  total_inv <- stats::coef(stats::lm(margin ~ 0 + tot_inv, data = m))[[1]]
  expect_false(isTRUE(all.equal(total_inv, 1, tolerance = 0.05)))
})

test_that("calibration refuses to guess", {
  pgd <- data.table::data.table(match_id = "M1", team = "A", epv_recv = 1,
                                epv_disp = 1, epv_spoil = 1)
  expect_error(calibrate_epv_channels(pgd, results = NULL, scale = NULL),
               "scale.*results|results.*scale")
  expect_error(calibrate_epv_channels(pgd, scale = c(epv_recv = 1)),
               "must be named")
  # Too few matches to fit a constant on -- must abort, not return noise.
  res <- data.frame(match_id = "M1", home_team_name = "A", away_team_name = "B",
                    home_score = 10, away_score = 0)
  expect_error(fit_epv_channel_scale(pgd, res), "Refusing to fit")
})

test_that("EPV_RAW_CHANNEL_SCALE defaults to fitting, not to a stored number", {
  # A stored scale fitted on one build and applied to another is the exact
  # staleness trap that invalidated a gate on 2026-08-05.
  expect_null(EPV_RAW_CHANNEL_SCALE)
})
