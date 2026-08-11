# `.frame_epv_engine()` -- the single read of the engine a frame was priced with.
#
# Before 2026-08-11 three sites each wrote `attr(x, "epv_engine")` and each was
# independently responsible for noticing a NULL. None did. R drops attributes
# on merge()/rbind()/most dplyr verbs, which is why
# `adjust_epv_for_opponents()` re-attaches by hand and why the v3 experiment
# scripts call setattr() six-plus times.
#
# The accessor does NOT change what is returned -- that is the whole point, and
# the first test pins it. What it adds is a warning in the one case where the
# silent fallback is wrong.

test_that("the returned value is exactly what attr() returned before", {
  # Output-neutrality. If this ever differs, every EPV price could move.
  d <- data.frame(x = 1:3)
  expect_null(.frame_epv_engine(d))
  expect_identical(.frame_epv_engine(d), attr(d, "epv_engine"))

  attr(d, "epv_engine") <- "v2"
  expect_identical(.frame_epv_engine(d), "v2")
  expect_identical(.frame_epv_engine(d), attr(d, "epv_engine"))

  attr(d, "epv_engine") <- "v3"
  expect_identical(.frame_epv_engine(d), "v3")
  expect_identical(.frame_epv_engine(d), attr(d, "epv_engine"))
})

test_that("a missing stamp is silent while the package is configured for v2", {
  # Today's state. A frame with no stamp and a v2 frame mean the same thing, so
  # warning here would be pure noise on every ordinary run.
  d <- data.frame(x = 1)
  expect_silent(res <- .frame_epv_engine(d, configured = "v2"))
  expect_null(res)
})

test_that("a missing stamp WARNS once the package is configured for v3", {
  # The case the accessor exists for. v3 configured plus no stamp is far more
  # likely a dropped attribute than a genuine v2 frame, and it would otherwise
  # price silently as v2.
  d <- data.frame(x = 1)
  expect_warning(res <- .frame_epv_engine(d, configured = "v3"), "no .*epv_engine")
  # ...and still returns NULL, so behaviour is unchanged. Audible, not different.
  expect_null(suppressWarnings(.frame_epv_engine(d, configured = "v3")))
})

test_that("a present stamp never warns, under either configuration", {
  d <- data.frame(x = 1)
  attr(d, "epv_engine") <- "v3"
  expect_silent(.frame_epv_engine(d, configured = "v3"))
  expect_silent(.frame_epv_engine(d, configured = "v2"))
  attr(d, "epv_engine") <- "v2"
  expect_silent(.frame_epv_engine(d, configured = "v3"))
})

test_that("the warning names the transform that loses the attribute", {
  # A warning that does not tell you what to do gets suppressed rather than
  # fixed. This one has to name merge/rbind and the setattr remedy.
  d <- data.frame(x = 1)
  w <- tryCatch(.frame_epv_engine(d, configured = "v3"),
                warning = function(e) conditionMessage(e))
  expect_match(w, "merge", fixed = TRUE)
  expect_match(w, "setattr", fixed = TRUE)
})

test_that("merge() really does drop the attribute -- the premise, not folklore", {
  # If R ever stopped dropping it, this whole accessor would be unnecessary.
  # Pin the behaviour the design rests on rather than trusting the comment.
  a <- data.frame(id = 1:3, v = 1:3)
  attr(a, "epv_engine") <- "v3"
  b <- data.frame(id = 1:3, w = 4:6)
  merged <- merge(a, b, by = "id")
  expect_null(attr(merged, "epv_engine"))

  dm <- data.table::as.data.table(a)
  data.table::setattr(dm, "epv_engine", "v3")
  merged_dt <- merge(dm, data.table::as.data.table(b), by = "id")
  expect_null(attr(merged_dt, "epv_engine"))
})

test_that(".use_per_channel_scale() treats a dropped stamp exactly as v2", {
  # The consequence chain, stated end to end: NULL and "v2" must reach the same
  # pricing decision, or a dropped attribute would already be changing numbers.
  expect_identical(.use_per_channel_scale(NULL), .use_per_channel_scale("v2"))
})
