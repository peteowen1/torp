# Guards for three helpers extracted 2026-08-11, each of which existed as
# copy-pasted arithmetic at multiple call sites before. The point of these
# tests is not that the arithmetic is right -- it was right in every copy --
# but that there is now exactly ONE copy, and that the copies stay gone.
#
# The three source-scanning "stays gone" guards are local-dev-only, same as
# test-versebus-sync.R: R CMD check runs tests against an installed package
# with no R/ source tree beside them, so they skip rather than fail there.

# Locate the package's R/ source dir, or NULL when running against an
# installed build (R CMD check).
.r_source_dir <- function() {
  for (d in c("R", file.path("..", "..", "R"), file.path("..", "R"))) {
    if (dir.exists(d)) return(normalizePath(d))
  }
  NULL
}

# Non-comment lines of every R/ source file, named by basename.
.r_source_code <- function(exclude = character(0)) {
  d <- .r_source_dir()
  if (is.null(d)) return(NULL)
  files <- list.files(d, pattern = "[.]R$", full.names = TRUE)
  files <- files[!basename(files) %in% exclude]
  stats::setNames(
    lapply(files, function(f) {
      lines <- readLines(f, warn = FALSE)
      lines[!grepl("^\\s*#", lines)]
    }),
    basename(files)
  )
}

# .strict_mode() ----

test_that(".strict_mode() is TRUE only for exactly \"1\"", {
  withr::with_envvar(c(VERSEBUS_STRICT = "1"), expect_true(.strict_mode()))
  withr::with_envvar(c(VERSEBUS_STRICT = ""), expect_false(.strict_mode()))
  # The whole reason the helper exists: check_vintage_alignment() used
  # nzchar(), which made every one of these strict at that site alone.
  withr::with_envvar(
    c(VERSEBUS_STRICT = "0"), expect_false(suppressWarnings(.strict_mode())))
  withr::with_envvar(
    c(VERSEBUS_STRICT = "true"), expect_false(suppressWarnings(.strict_mode())))
  withr::with_envvar(
    c(VERSEBUS_STRICT = "false"), expect_false(suppressWarnings(.strict_mode())))
})

test_that(".strict_mode() warns rather than silently reading a bad value as off", {
  # The rule is fail-open, so a plausible VERSEBUS_STRICT=true would leave
  # every abort path as a warning. Silence there is the failure mode this
  # whole commit is about, so it has to be audible.
  withr::with_envvar(c(VERSEBUS_STRICT = "true"), {
    expect_warning(.strict_mode(), "not.*1")
  })
  # The two legitimate values must stay quiet -- .strict_mode() is called on
  # every load, so a warning on the normal path would be unusable.
  withr::with_envvar(c(VERSEBUS_STRICT = "1"), expect_silent(.strict_mode()))
  withr::with_envvar(c(VERSEBUS_STRICT = ""), expect_silent(.strict_mode()))
})

test_that("versebus.R's inline copy still implements the same rule", {
  # versebus.R cannot call .strict_mode() (vendored + guarded, see above), so
  # nothing structural keeps the two equal. test-versebus-sync.R guards
  # torp-vs-torpmodels, not torp-vs-.strict_mode(). This is the missing edge:
  # if .strict_mode() is ever changed to accept more values, this fails and
  # points at the copy that has to move with it.
  d <- .r_source_dir()
  skip_if(is.null(d), "R/ source tree not present (installed build)")

  # Read the default off the PARSED expression -- no eval(). For a `function`
  # call object, [[2]] is the formals pairlist. Same approach as
  # test-versebus-sync.R's .parse_versebus_functions().
  exprs <- parse(file.path(d, "versebus.R"), keep.source = FALSE)
  formals_of <- NULL
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) &&
        length(e) == 3 && is.name(e[[2]]) &&
        identical(as.character(e[[2]]), "vb_download") &&
        is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
      formals_of <- e[[3]][[2]]
    }
  }
  expect_false(is.null(formals_of))

  inline <- paste(deparse(formals_of$require_manifest), collapse = " ")
  expect_equal(inline, "isTRUE(Sys.getenv(\"VERSEBUS_STRICT\") == \"1\")")

  # And the helper must agree with that literal on every value that matters.
  for (v in c("1", "", "0", "true")) {
    withr::with_envvar(c(VERSEBUS_STRICT = v), {
      expect_equal(
        suppressWarnings(.strict_mode()),
        isTRUE(Sys.getenv("VERSEBUS_STRICT") == "1"),
        info = paste("VERSEBUS_STRICT =", encodeString(v, quote = '"'))
      )
    })
  }
})

test_that("VERSEBUS_STRICT is parsed in one place (versebus.R excepted)", {
  # versebus.R is vendored into torpmodels and guarded function-by-function by
  # test-versebus-sync.R, so it cannot call a torp-local helper until the
  # sibling copy has one. load_utils.R is where the helper itself lives.
  # Every OTHER R/ file must go through .strict_mode().
  code <- .r_source_code(exclude = c("versebus.R", "load_utils.R"))
  skip_if(is.null(code), "R/ source tree not present (installed build)")

  offenders <- names(code)[vapply(
    code, function(x) any(grepl("Sys.getenv\\(\"VERSEBUS_STRICT\"\\)", x)),
    logical(1)
  )]
  expect_equal(
    offenders, character(0),
    info = "Call .strict_mode() instead of re-parsing VERSEBUS_STRICT"
  )
})

# .blend_gam_xgb() ----

test_that(".blend_gam_xgb() reproduces the literal it replaced", {
  gam <- c(10, -3.5, 0, 121.25)
  xgb <- c(20, 4.5, 7, -8.75)
  # Bit-for-bit against the arithmetic that was inline at three call sites.
  expect_identical(.blend_gam_xgb(gam, xgb), 0.5 * gam + 0.5 * xgb)
})

test_that(".blend_gam_xgb() weights are convex and honour the constant", {
  expect_identical(MATCH_BLEND_WEIGHT, 0.5)
  gam <- c(10, 20)
  xgb <- c(30, 40)
  expect_identical(.blend_gam_xgb(gam, xgb, weight = 1), gam)
  expect_identical(.blend_gam_xgb(gam, xgb, weight = 0), xgb)
  # A weight change must move the result, or a call site is ignoring it.
  expect_false(identical(.blend_gam_xgb(gam, xgb, weight = 0.75),
                         .blend_gam_xgb(gam, xgb, weight = 0.5)))
})

test_that("no R/ file writes the blend out by hand", {
  code <- .r_source_code()
  skip_if(is.null(code), "R/ source tree not present (installed build)")

  # Collapse each file to one string first: the match_model.R copy was
  # wrapped across two lines, so a per-line scan would have missed it.
  pattern <- paste0(
    "[0-9.]+\\s*\\*\\s*[A-Za-z_$.]*gam_pred_[a-z_]+",
    "\\s*\\+\\s*",
    "[0-9.]+\\s*\\*\\s*[A-Za-z_$.]*xgb_pred_"
  )
  offenders <- names(code)[vapply(code, function(x) {
    grepl(pattern, paste(x, collapse = " "))
  }, logical(1))]
  expect_equal(
    offenders, character(0),
    info = "Call .blend_gam_xgb() instead of writing w*gam + (1-w)*xgb inline"
  )
})

# .build_week_ratings() ----

test_that(".build_week_ratings() applies the horizon discount from min(target_weeks)", {
  tr <- data.frame(
    team = rep("Geelong", 3), season = 2026L, round = 20L,
    player_id = 1:3,
    epr = c(1, 2, 3), epr_recv = c(1, 1, 1), epr_disp = c(1, 1, 1),
    epr_spoil = c(1, 1, 1), epr_hitout = c(1, 1, 1), psr = c(1, 1, 1),
    pred_tog = c(80, 80, 80),
    injury = NA_character_, return_round = NA_real_,
    stringsAsFactors = FALSE
  )

  near <- .build_week_ratings(tr, w = 21, target_weeks = 21:23)
  far  <- .build_week_ratings(tr, w = 23, target_weeks = 21:23)

  expect_equal(nrow(near), 1L)
  expect_equal(near$round, 21)
  expect_equal(far$round, 23)
  # 0.99 at the nearest week, dropping 0.01 per week out.
  expect_equal(near$epr_week / far$epr_week, 0.99 / 0.97, tolerance = 1e-12)
})

test_that(".build_week_ratings() excludes players still injured that week", {
  # Vary only return_round, holding the target week (and so the horizon
  # discount) fixed, so the difference is inclusion and nothing else.
  roster <- function(return_round) {
    data.frame(
      team = rep("Carlton", 2), season = 2026L, round = 20L,
      player_id = 1:2,
      epr = c(2, 10), epr_recv = c(1, 1), epr_disp = c(1, 1),
      epr_spoil = c(1, 1), epr_hitout = c(1, 1), psr = c(1, 1),
      pred_tog = c(80, 80),
      injury = c(NA, "Hamstring"), return_round = c(NA, return_round),
      stringsAsFactors = FALSE
    )
  }

  still_out <- .build_week_ratings(roster(24), w = 21, target_weeks = 21)
  back      <- .build_week_ratings(roster(21), w = 21, target_weeks = 21)

  # Only the epr-2 player counts while the epr-10 player is out.
  expect_equal(still_out$epr_week, 2 * 18 * 0.99, tolerance = 1e-12)
  # Both count once he returns; tog_wt splits the 18 player-equivalents.
  expect_equal(back$epr_week, (2 * 9 + 10 * 9) * 0.99, tolerance = 1e-12)
  expect_gt(back$epr_week, still_out$epr_week)
})

test_that(".build_week_ratings() normalises each team to 18 player-equivalents", {
  # Non-obvious and worth pinning: the roster rating is a per-18 weighted
  # average, not a sum, so squad size does not inflate it.
  squad <- function(n) {
    data.frame(
      team = rep("Sydney", n), season = 2026L, round = 20L,
      player_id = seq_len(n),
      epr = rep(3, n), epr_recv = rep(1, n), epr_disp = rep(1, n),
      epr_spoil = rep(1, n), epr_hitout = rep(1, n), psr = rep(1, n),
      pred_tog = rep(80, n),
      injury = NA_character_, return_round = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  small <- .build_week_ratings(squad(18), w = 21, target_weeks = 21)
  large <- .build_week_ratings(squad(40), w = 21, target_weeks = 21)
  expect_equal(small$epr_week, large$epr_week, tolerance = 1e-12)
  expect_equal(small$epr_week, 3 * 18 * 0.99, tolerance = 1e-12)
})

test_that(".build_week_ratings() is defined once, not inlined by its callers", {
  # It was pasted verbatim into match_model.R and matchup_table.R until
  # 2026-08-11; the two differed only in a line wrap.
  code <- .r_source_code()
  skip_if(is.null(code), "R/ source tree not present (installed build)")

  defs <- sum(vapply(code, function(x) {
    sum(grepl("^\\s*\\.build_week_ratings\\s*<-\\s*function", x))
  }, integer(1)))
  expect_equal(defs, 1L)
})
