# torp#250: the live path scores a match on the nightly's season fit (stoppage
# baseline, disposal-difficulty and contest models) instead of refitting them
# on that one match. These tests cover the plumbing: the fits are handed back
# by a season run, a supplied fit is used and nothing is refitted, and a bundle
# from another vintage is refused.
#
# What they cannot see: whether the season fit prices a live match the way the
# nightly priced it. That needs real data and is checked by scoring a played
# match through get_player_game_ratings() against the published frame.

fake_gam <- function() structure(list(), class = c("bam", "gam", "glm", "lm"))

fake_fit <- function() {
  cm <- if (identical(EPV3_CONTEST_OUTCOMES, "three")) {
    c("p_other", "p_att", "att", "def", "oth")
  } else {
    c("p", "att", "def")
  }
  list(
    stoppage_baseline = data.table::data.table(description = "Ball Up Call", band = 0,
                                               baseline = 0.1, n = 1L),
    disposal_models = stats::setNames(lapply(1:3, function(i) fake_gam()), c("p", "ret", "trn")),
    contest_models = stats::setNames(lapply(cm, function(i) fake_gam()), cm)
  )
}

fake_pgd <- function(fit = fake_fit()) {
  structure(data.frame(match_id = c("CD_M20260142901", "CD_M20260142901", "CD_M20260142801")),
            epv_engine = "v4", np_fit = fit)
}

test_that("the bundle is stamped with the vintage, version and season that built it", {
  b <- torp:::.np_fit_bundle(fake_pgd(), 2026)
  expect_s3_class(b, "torp_np_fit")
  expect_identical(b$meta$rating_vintage, RATING_VINTAGE)
  expect_identical(b$meta$torp_version, as.character(utils::packageVersion("torp")))
  expect_identical(b$meta$epv_engine, "v4")
  expect_identical(b$meta$season, 2026L)
  expect_identical(b$meta$n_matches, 2L)
  expect_identical(b$meta$last_match_id, "CD_M20260142901")
  expect_equal(b$stoppage_baseline, fake_fit()$stoppage_baseline)
})

test_that("a frame built without keep_np_fit has no bundle to give", {
  pgd <- fake_pgd()
  attr(pgd, "np_fit") <- NULL
  expect_error(torp:::.np_fit_bundle(pgd, 2026), "keep_np_fit")
})

test_that("a bundle from another vintage, engine or contest shape is refused", {
  b <- torp:::.np_fit_bundle(fake_pgd(), 2026)
  old <- b; old$meta$rating_vintage <- "v1"
  expect_error(torp:::.check_np_fit_bundle(old), "vintage")
  unstamped <- b; unstamped$meta$rating_vintage <- NULL
  expect_error(torp:::.check_np_fit_bundle(unstamped), "vintage")
  v3 <- b; v3$meta$epv_engine <- "v3"
  expect_error(torp:::.check_np_fit_bundle(v3), "v4")
  shape <- b; shape$meta$contest_outcomes <- "not-this"
  expect_error(torp:::.check_np_fit_bundle(shape), "contest")
  expect_error(torp:::.check_np_fit_bundle(unclass(b)), "Not a v4 season fit bundle")
})

test_that("a bundle missing any fitted piece is refused", {
  b <- torp:::.np_fit_bundle(fake_pgd(), 2026)
  no_trn <- b; no_trn$disposal_models$trn <- NULL
  expect_error(torp:::.check_np_fit_bundle(no_trn), "trn")
  not_model <- b; not_model$contest_models$def <- list()
  expect_error(torp:::.check_np_fit_bundle(not_model), "def")
  no_bl <- b; no_bl$stoppage_baseline <- no_bl$stoppage_baseline[0]
  expect_error(torp:::.check_np_fit_bundle(no_bl), "stoppage baseline")
  fit <- fake_fit(); fit$contest_models <- NULL
  expect_error(torp:::.np_fit_bundle(fake_pgd(fit), 2026), "contest_models")
})

test_that("a written bundle reads back whole, and a stale one on disk is refused", {
  b <- torp:::.np_fit_bundle(fake_pgd(), 2026)
  p <- file.path(withr::local_tempdir(), "np_fit_2026.rds")
  torp:::.write_np_fit_bundle(b, p)
  back <- suppressMessages(torp:::.load_np_fit_bundle(2026, path = p))
  expect_equal(back, b)
  old <- b; old$meta$rating_vintage <- "v1"
  saveRDS(old, p)
  expect_error(suppressMessages(torp:::.load_np_fit_bundle(2026, path = p)), "vintage")
  expect_error(torp:::.load_np_fit_bundle(2026, path = file.path(dirname(p), "nope.rds")), "No fit bundle")
})

test_that("the release bundle is fetched by season, checked and cached", {
  b <- torp:::.np_fit_bundle(fake_pgd(), 2026)
  cache <- torp:::.np_fit_cache
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  withr::defer(rm(list = ls(cache, all.names = TRUE), envir = cache))
  calls <- new.env(); calls$n <- 0L
  testthat::local_mocked_bindings(
    vb_download = function(repo, tag, name, dest, ...) {
      calls$n <- calls$n + 1L
      calls$tag <- tag
      calls$name <- name
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      saveRDS(b, dest)
      invisible(dest)
    },
    .package = "torp")
  got <- suppressMessages(torp:::.load_np_fit_bundle(2026))
  expect_equal(got, b)
  expect_identical(calls$tag, NP_FIT_RELEASE_TAG)
  expect_identical(calls$name, "np_fit_2026.rds")
  suppressMessages(torp:::.load_np_fit_bundle(2026))
  expect_identical(calls$n, 1L)
})

test_that("a missing release bundle aborts and says how to supply one", {
  cache <- torp:::.np_fit_cache
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  testthat::local_mocked_bindings(
    vb_download = function(...) stop("HTTP 404"),
    .package = "torp")
  expect_error(torp:::.load_np_fit_bundle(2031), "np_fit_2031.rds")
})

test_that("with no fit for the new season yet, the live path falls back to last season's", {
  cache <- torp:::.np_fit_cache
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  b <- torp:::.np_fit_bundle(fake_pgd(), 2026)
  asked <- character()
  testthat::local_mocked_bindings(
    vb_download = function(repo, tag, name, dest, ...) {
      asked <<- c(asked, name)
      if (name != "np_fit_2026.rds") stop("HTTP 404")
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      saveRDS(b, dest)
      invisible(dest)
    },
    .package = "torp")
  expect_message(got <- torp:::.resolve_np_fit(NULL, 2027), "falling back to the 2026 fit")
  expect_equal(got, b)
  expect_identical(asked, c("np_fit_2027.rds", "np_fit_2026.rds"))
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  testthat::local_mocked_bindings(vb_download = function(...) stop("HTTP 404"), .package = "torp")
  expect_error(suppressMessages(torp:::.resolve_np_fit(NULL, 2031)), "np_fit_2030.rds")
})

test_that("scoring a match from another season on a bundle says so", {
  b <- torp:::.np_fit_bundle(fake_pgd(), 2026)
  expect_identical(suppressMessages(torp:::.resolve_np_fit(b, 2026)), b)
  expect_message(torp:::.resolve_np_fit(b, 2027), "2027 match on the 2026 season fit")
})

test_that("supplied models are scored with and never refitted", {
  seen <- new.env()
  testthat::local_mocked_bindings(
    build_disposal_events = function(chains, pbp_data, half = NULL) {
      data.table::data.table(match_id = "CD_M20260142901", display_order = 1L, exp_pts = 0.2)
    },
    fit_disposal_models = function(...) stop("refitted the disposal models"),
    fit_contest_models = function(...) stop("refitted the contest models"),
    score_disposals = function(de, models) {
      seen$disposal <- models
      d <- data.table::copy(de)
      d[, `:=`(p_hat = 0.3, V_pre = 0.5, surprise = 0.5)]
      d
    },
    build_aerial_contests = function(chains, pbp_data, half = NULL) {
      data.table::data.table(match_id = "CD_M20260142901", kick_do = 3L)
    },
    score_contests = function(cst, models) {
      seen$contest <- models
      d <- data.table::copy(cst)
      d[, `:=`(p_hat = 0.4, V_pre = 0.1, exp_pts = 0, V_att_hat = 1, V_def_hat = -1,
               V_oth_hat = 0, out3 = "mark_def", V_after = -0.8, out_desc = "Contested Mark",
               def_win = TRUE, out_pid = "p9", target_pid = "p2")]
      d
    },
    .package = "torp")
  mods <- list(disposal = "the disposal fit", contest = "the contest fit")
  out <- suppressMessages(torp:::.np_difficulty_terms(NULL, NULL, models = mods,
                                                      return_models = TRUE))
  expect_identical(seen$disposal, "the disposal fit")
  expect_identical(seen$contest, "the contest fit")
  expect_setequal(out$display_order, c(1L, 3L))
  expect_identical(attr(out, "np_models"), c(mods, list(half = NULL)))
  plain <- suppressMessages(torp:::.np_difficulty_terms(NULL, NULL, models = mods))
  expect_null(attr(plain, "np_models"))
  expect_error(torp:::.np_difficulty_terms(NULL, NULL, models = list(disposal = "x")), "contest")
  expect_error(torp:::.np_difficulty_terms(NULL, NULL, models = mods, train_pbp = 1, train_chains = 1),
               "not both")
})

test_that("a season run hands back the models it fitted", {
  testthat::local_mocked_bindings(
    build_disposal_events = function(chains, pbp_data, half = NULL) {
      data.table::data.table(match_id = "CD_M20260142901", display_order = 1L, exp_pts = 0.2)
    },
    fit_disposal_models = function(de, ...) "fitted disposal",
    fit_contest_models = function(cst, ...) "fitted contest",
    score_disposals = function(de, models) {
      d <- data.table::copy(de)
      d[, `:=`(p_hat = 0.3, V_pre = 0.5, surprise = 0.5)]
      d
    },
    build_aerial_contests = function(chains, pbp_data, half = NULL) data.table::data.table(),
    .package = "torp")
  out <- suppressMessages(torp:::.np_difficulty_terms(NULL, NULL, return_models = TRUE))
  expect_identical(attr(out, "np_models")$disposal, "fitted disposal")
  # no contests in the data: nothing fitted, nothing handed back
  expect_null(attr(out, "np_models")$contest)
})

test_that("create_player_game_data refuses a fit it cannot use, before loading anything", {
  expect_error(create_player_game_data(epv_engine = "v2", np_fit = list()), "v4 only")
  expect_error(create_player_game_data(epv_engine = "v3", keep_np_fit = TRUE), "v4 only")
  expect_error(create_player_game_data(epv_engine = "v4", np_fit = list()), "needs .*chains")
  expect_error(create_player_game_data(epv_engine = "v4", np_fit = list(), chains = data.frame(),
                                       difficulty_terms = data.frame()), "not both")
})

# torp#308: a live call (one match) and the nightly (a season) must price the
# same disposal identically. Two batch-dependent things used to differ.

test_that("a pinned half-length overrides the batch's own, and rides along as an attribute", {
  # 200 matches' worth of rows so the 99.5th percentile of |x| is not the max.
  set.seed(1)
  n <- 4000
  ch <- data.table::data.table(
    match_id = rep(sprintf("CD_M2026%05d", 1:40), each = n / 40),
    display_order = rep(1:(n / 40), 40),
    description = rep(c("Kick", "Handball", "Handball", "Kick"), length.out = n),
    player_id = sample(paste0("p", 1:30), n, TRUE),
    team_id = rep(c("A", "B"), length.out = n),
    x = stats::runif(n, -80, 80), y = stats::runif(n, -40, 40))
  pbp <- ch[, .(match_id, display_order, exp_pts = 1, delta_epv = 0.1)]
  own <- suppressMessages(build_disposal_events(ch, pbp))
  expect_gt(nrow(own), 0)
  own_half <- attr(own, "half")
  expect_equal(own_half, as.numeric(stats::quantile(abs(ch$x), 0.995)))
  pinned <- suppressMessages(build_disposal_events(ch, pbp, half = 70))
  expect_identical(attr(pinned, "half"), 70)
  # goal_dist is computed from the pinned value, not the batch's.
  expect_equal(pinned$goal_dist,
               sqrt(pmax(0, 70 - pinned$x)^2 + abs(pinned$y)^2))
  expect_false(isTRUE(all.equal(own$goal_dist[1:50], pinned$goal_dist[1:50])))
})

test_that("a season fit bundle stores the half-length it was scored with", {
  fit <- fake_fit(); fit$half <- 78
  b <- torp:::.np_fit_bundle(fake_pgd(fit), 2026)
  expect_identical(b$half, 78)
  # A bundle built before this existed still loads; the live path then falls
  # back to estimating it from the batch, as it did.
  old <- torp:::.np_fit_bundle(fake_pgd(), 2026)
  expect_null(old$half)
  bad <- b; bad$half <- -3
  expect_error(torp:::.check_np_fit_bundle(bad), "half")
  bad$half <- c(78, 80)
  expect_error(torp:::.check_np_fit_bundle(bad), "half")
})

test_that("the bundle's half-length reaches the builders when a match is scored on it", {
  seen <- new.env()
  testthat::local_mocked_bindings(
    build_disposal_events = function(chains, pbp_data, half = NULL) {
      seen$d <- half
      d <- data.table::data.table(match_id = "CD_M20260142901", display_order = 1L, exp_pts = 0.2)
      data.table::setattr(d, "half", if (is.null(half)) 99 else half)
      d
    },
    score_disposals = function(de, models) {
      d <- data.table::copy(de)
      d[, `:=`(p_hat = 0.3, V_pre = 0.5, surprise = 0.5)]
      d
    },
    build_aerial_contests = function(chains, pbp_data, half = NULL) {
      seen$c <- half
      data.table::data.table()
    },
    .package = "torp")
  mods <- list(disposal = "d", contest = "c")
  out <- suppressMessages(torp:::.np_difficulty_terms(NULL, NULL, models = mods,
                                                      return_models = TRUE, half = 78))
  expect_identical(seen$d, 78)
  expect_identical(seen$c, 78)
  expect_identical(attr(out, "np_models")$half, 78)
})

test_that("scoring is exact: a season in one call equals the same rows match by match", {
  skip_if_not_installed("mgcv")
  set.seed(308)
  n <- 6000  # past the ~1,000 rows where predict.bam(discrete = TRUE) starts binning
  mk <- function(n) {
    d <- data.table::data.table(
      x = stats::runif(n, -80, 80), y = stats::runif(n, -40, 40),
      exp_pts = stats::rnorm(n, 1, 1),
      is_handball = factor(stats::rbinom(n, 1, 0.5), levels = c("0", "1")),
      i50f = factor(stats::rbinom(n, 1, 0.2), levels = c("0", "1")))
    d[, abs_y := abs(y)]
    d[, goal_dist := sqrt(pmax(0, 78 - x)^2 + abs_y^2)]
    d[, turnover := stats::rbinom(n, 1, stats::plogis(0.02 * goal_dist - 1.5)) == 1]
    d[, V_after := exp_pts + 0.01 * x + stats::rnorm(n)]
    d
  }
  de <- mk(n)
  m <- fit_disposal_models(de)
  season <- score_disposals(de, m)
  # Chunks of 300 rows sit under the binning threshold, so the pre-fix default
  # predicted these one by one and the season call binned them.
  chunk <- rep(seq_len(n / 300), each = 300)
  by_match <- data.table::rbindlist(lapply(split(seq_len(n), chunk),
                                           function(i) score_disposals(de[i], m)))
  expect_equal(by_match$p_hat, season$p_hat, tolerance = 1e-10)
  expect_equal(by_match$V_ret_hat, season$V_ret_hat, tolerance = 1e-10)
  expect_equal(by_match$V_trn_hat, season$V_trn_hat, tolerance = 1e-10)
})

test_that("contest scoring is exact: a season in one call equals the same rows match by match", {
  skip_if_not_installed("mgcv")
  skip_if_not(identical(EPV3_CONTEST_OUTCOMES, "three"))
  set.seed(309)
  n <- 6000
  cst <- data.table::data.table(
    att_x = stats::runif(n, -80, 80), abs_y = stats::runif(n, 0, 40),
    exp_pts = stats::rnorm(n, 1, 1), out3 = sample(c("mark_att", "mark_def", "other"), n, TRUE),
    kick_tid = "A", opp_tid = "B", out_tid = "A", out_pid = "p1", target_pid = "p2",
    def_win = sample(c(TRUE, FALSE), n, TRUE))
  cst[, `:=`(y1 = stats::rbinom(n, 1, stats::plogis(0.02 * att_x)), v = exp_pts + 0.01 * att_x + stats::rnorm(n))]
  b <- function(f, ...) mgcv::bam(f, data = cst, discrete = TRUE, ...)
  models <- list(
    p_other = b(y1 ~ s(att_x) + s(exp_pts), family = stats::binomial()),
    p_att   = b(y1 ~ s(att_x) + s(abs_y), family = stats::binomial()),
    att = b(v ~ s(att_x) + s(exp_pts)), def = b(v ~ s(att_x) + s(abs_y)),
    oth = b(v ~ s(att_x)))
  season <- score_contests(cst, models)
  chunk <- rep(seq_len(n / 300), each = 300)
  by_match <- data.table::rbindlist(lapply(split(seq_len(n), chunk),
                                           function(i) score_contests(cst[i], models)))
  for (col in c("p_other_hat", "p_att_hat", "V_att_hat", "V_def_hat", "V_oth_hat")) {
    expect_equal(by_match[[col]], season[[col]], tolerance = 1e-10, info = col)
  }
})
