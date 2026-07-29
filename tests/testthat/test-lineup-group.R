# Tests for LINEUP_GROUP_MAP / .collapse_lineup_group()
#
# The middle rung between lineup_position (21 raw slots) and
# LINEUP_POSITION_GROUP_MAP (6 broad groups). Merges only the arbitrary
# left/right mirrors.

test_that("every lineup_position in the teams data is mapped", {
  # The 21 values observed in load_teams(TRUE) on 2026-07-29. Hardcoded rather
  # than loaded so the test stays offline and fails loudly if the AFL adds a
  # slot -- an unmapped slot silently drops rows out of centring.
  observed <- c("FB", "BPL", "BPR", "CHB", "HBFL", "HBFR", "C", "WL", "WR",
                "R", "RR", "RK", "CHF", "HFFL", "HFFR", "FF", "FPL", "FPR",
                "INT", "SUB", "EMERG")
  expect_length(observed, 21)
  expect_true(all(observed %in% names(LINEUP_GROUP_MAP)))
  expect_false(any(is.na(.collapse_lineup_group(observed))))
})

test_that("the five left/right mirrors merge, and nothing else does", {
  mirrors <- list(c("BPL", "BPR"), c("HBFL", "HBFR"), c("WL", "WR"),
                  c("HFFL", "HFFR"), c("FPL", "FPR"))
  for (m in mirrors) {
    g <- .collapse_lineup_group(m)
    expect_equal(g[1], g[2], info = paste(m, collapse = "/"))
  }
  # 21 slots -> 16 groups: 13 on-field + INT + SUB + EMERG
  expect_equal(length(unique(unname(LINEUP_GROUP_MAP))), 16L)
})

test_that("distinct roles are NOT merged", {
  # The point is to merge arbitrary left/right, not to collapse real roles.
  # R (rover) and RR (ruck rover) are different jobs; so are FB/CHB and FF/CHF.
  distinct <- list(c("R", "RR"), c("FB", "CHB"), c("FF", "CHF"),
                   c("C", "W"), c("RK", "R"))
  for (d in distinct) {
    g <- .collapse_lineup_group(d)
    expect_false(isTRUE(g[1] == g[2]), info = paste(d, collapse = " vs "))
  }
})

test_that("INT/SUB/EMERG keep their own groups, unlike the 6-way map", {
  # LINEUP_POSITION_GROUP_MAP sends these to NA. LINEUP_GROUP_MAP must not:
  # .position_adjust() groups on raw lineup_position today, so interchange
  # players are centred against each other. Mapping them to NA would change
  # that as a side effect of a rename.
  bench <- c("INT", "SUB", "EMERG")
  expect_false(any(is.na(.collapse_lineup_group(bench))))
  expect_equal(.collapse_lineup_group(bench), bench)
  expect_true(all(is.na(LINEUP_POSITION_GROUP_MAP[bench])))
})

test_that("lineup_group is strictly between lineup_position and the 6-way group", {
  # Every lineup_group must sit inside exactly one 6-way group, or the two maps
  # disagree about what a position is -- the precise failure that put ~0.30 of
  # positional level back into TORP via PSR.
  onfield <- setdiff(names(LINEUP_GROUP_MAP), c("INT", "SUB", "EMERG"))
  tab <- table(.collapse_lineup_group(onfield),
               unname(LINEUP_POSITION_GROUP_MAP[onfield]))
  # each lineup_group row maps into exactly one 6-way column
  expect_true(all(rowSums(tab > 0) == 1))
  expect_equal(length(unique(.collapse_lineup_group(onfield))), 13L)
})

test_that("unmapped values warn rather than silently returning NA", {
  expect_warning(out <- .collapse_lineup_group(c("FB", "NOT_A_POSITION")),
                 "unmapped")
  expect_equal(out[1], "FB")
  expect_true(is.na(out[2]))
})

test_that("NA in, NA out, without a warning", {
  expect_silent(out <- .collapse_lineup_group(c("FB", NA_character_)))
  expect_equal(out[1], "FB")
  expect_true(is.na(out[2]))
})
