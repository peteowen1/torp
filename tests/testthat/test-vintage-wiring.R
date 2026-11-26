# Published-vintage vs deployed-code guard (torp's instance of citius's
# calibration-wiring pattern -- see
# citiusverse/citius/tests/testthat/test-calibration-wiring.R for the sibling).
# Design: docs/plans/FABLE-VINTAGE-GUARD-PLAN.md.
#
# WHY THIS FILE EXISTS. torp 2026-07-27/28: v2 was published as canonical
# while `main` still computed v1, and the daily pipeline silently rewrote 2026
# rows with v1 logic into the v2 table. Publishing a vintage and deploying the
# code that produces it are separate acts, and nothing compared them before a
# write. citius closed its own version of "adopted but never wired" with
# test-calibration-wiring.R after THREE recurrences (indoor, season,
# coasting_trait); this closes torp's before a first recurrence.
#
# Two failure directions:
#   UNWIRED -- a rating-defining constant exists but .rating_defining_constants()
#     never captures it, so changing it silently changes published ratings with
#     no manifest trace and no vintage bump required.
#   DRIFT -- a wired constant's live value no longer matches what the manifest
#     recorded for the canonical vintage, meaning RATING_VINTAGE should have
#     been bumped and was not. check_vintage_alignment() catches this at
#     runtime; the round-trip tests below prove the comparison itself works.
#
# Entirely offline: every check here either scans the loaded package namespace
# for constant names or calls check_vintage_alignment() with an injected
# `manifest =`, never read_ratings_manifest()'s real network default.

# ---------------------------------------------------------------------------
# THE REGISTER. Every entry needs a reason and a date. Prune it once wired.
# ---------------------------------------------------------------------------

# Rating-defining-family constants (scanned below) that
# .rating_defining_constants() does not capture: measured inert on published
# numbers, never read into arithmetic, or (NP_DISPOSAL_DESCS,
# NP_EXCLUDED_DESCS) still unmeasured.
#
# Until 2026-11-26 this also held 63 constants it called rating-defining (the
# raw EPV per-event weights, the EPR/EPV/PSR centring and shrink flags, the
# points-scale family and EPR_LOADING_DEFAULT), registered on 2026-08-09
# because widening the manifest was out of scope for FABLE-VINTAGE-GUARD-PLAN.
# v18 item (3) wired all of them; see the block at the end of
# .rating_defining_constants(). What remains here is not a backlog.
KNOWN_NON_DEFINING <- c(
  # -- Legacy alias. EPR_DECAY_DEFAULT_DAYS is `<- EPR_DECAY_RECV`, so its
  # value can never differ from the wired EPR_DECAY_RECV leaf. (2026-08-09)
  # (EPV_ENGINE and EPV_CONT_LOSS_ALLOC sat here until 2026-08-18; the EPR_DECAY
  # and EPR_PRIOR_* leaves are still captured under v2's recv/disp/spoil/hitout
  # labels -- the values are captured, only the naming is stale.)
  "EPR_DECAY_DEFAULT_DAYS",

  # -- The NP_ family, widened into this scan on 2026-09-08 and audited on
  # 2026-09-10 (the stoppage pricing unit, the offence pool and the contest
  # winner table all moved to .rating_defining_constants(), each with a measured
  # magnitude recorded there). Two of the rest are genuinely inert under
  # production's credit = "difficulty" path. The first is NP_RECEIVER_SHARE, checked
  # per-player-game (not a season-total signed sum, which cancels): 1.7 points
  # of absolute movement across the WHOLE 2026 season, 2 of 512 players
  # touched at all. NP_DEFENSIVE_SHARE and the NP_BALL_WINNER_SHARE_BY_ACT
  # table were ALSO first claimed inert here on that same season-signed-sum
  # method and were both wrong -- both are wired into
  # .rating_defining_constants() now, with the correction explained there.
  # NP_BALL_WINNER_SHARE (the flat scalar, not the by-act table) really is
  # dead: .np_defensive_pool() overwrites it unconditionally whenever
  # by_act = TRUE, which credit = "difficulty" always sets, confirmed by
  # tracing the code rather than by summing a season total.
  "NP_RECEIVER_SHARE", "NP_BALL_WINNER_SHARE",

  # -- The pool-SPREAD dials. Production runs spread = "matchup"
  # (build_net_points()'s match.arg default; .np_engine_frame() never passes
  # `spread`, traced 2026-09-10), so NP_MIRROR_SHARE and NP_POSITION_MIRROR
  # are read on every team pool -- but MEASURED INERT on published numbers
  # (2026-11-26, v18 item 7): on 2026 rounds 0-4 (1,655 player-games),
  # NP_MIRROR_SHARE 0.5 -> 0 moves the published epv* columns at most
  # 5.3e-15, and reversing NP_POSITION_MIRROR's map at most 5.8e-15, because
  # .np_team_margin() re-splits the pool (see NP_STOPPAGE_LOSER_SHARE below).
  # They move build_net_points()'s raw ledger only, so they stay, labelled
  # display-only. NP_CONTEXT_WEIGHTS is read only under spread = "context".
  # NP_DISPOSAL_DESCS and NP_EXCLUDED_DESCS are still unmeasured.
  "NP_MIRROR_SHARE", "NP_POSITION_MIRROR", "NP_CONTEXT_WEIGHTS",
  "NP_DISPOSAL_DESCS", "NP_EXCLUDED_DESCS",
  # -- MEASURED INERT, 2026-09-10, and the reason it can be trusted as inert
  # rather than merely small. .np_team_margin() rescales each SIDE of every row
  # up to that row's FULL value (own * target / side_sum), so any constant whose
  # only job is splitting a row's value BETWEEN the two teams is divided
  # straight back out. NP_STOPPAGE_LOSER_SHARE is exactly that and nothing else
  # -- the within-side ruck/player/pool split is NP_STOPPAGE_SPLIT's job, and
  # that one is wired. Swept 0.50 -> 0.20 over 2026: the RAW ledger moves up to
  # 9.68 points a player-game (mean 0.72), and the PUBLISHED numbers move
  # 8.0e-15 -- 0 of 9,794 player-matches change at 1e-6. Not "small": zero.
  # This also closes NEXT-STEPS.md's "two dials never swept, unaudited either
  # way" for this dial; there is nothing to sweep.
  # Caution for whoever revisits: the same algebra predicted NP_BLAME_SHARE
  # inert and was WRONG (it moves 100% of players, max 6.25), because
  # NP_BLAME_POOL gives the conceding side its own pool, making blame_share a
  # within-side split as well as a cross-team one. Measure, do not infer.
  "NP_STOPPAGE_LOSER_SHARE",
  # A different category again: these two are the bounds of a sanity check on
  # the fitted stoppage win rate. They can abort a run or print a warning; they
  # are never read into any arithmetic, so no value of either can change a
  # published number. (2026-09-09) The third, the minimum cell size the check
  # reads (torp#225), is the same kind: it decides which cells the check looks
  # at and nothing else; every row still goes into the ledger. (2026-09-23)
  "NP_STOPPAGE_WIN_WARN", "NP_STOPPAGE_WIN_ABORT", "NP_STOPPAGE_WIN_MIN_N",
  # -- Audited 2026-10-31 (the four of torp#287's thirteen not wired). None
  # reaches a published number: EPV_RAW_CHANNEL_SCALE is read only by
  # calibrate_epv_channels(), which only analysis scripts and
  # benchmark_suite.R call; EPV_BOX_TERMS shapes player_epv_breakdown()'s
  # display; NP_FIT_RELEASE_TAG names the release the fit bundle lives on;
  # TORP_RATINGS_COL_MAP renames columns at load (load_data.R).
  "EPV_RAW_CHANNEL_SCALE", "EPV_BOX_TERMS", "NP_FIT_RELEASE_TAG",
  "TORP_RATINGS_COL_MAP"
)

# ---------------------------------------------------------------------------
# Scanners
# ---------------------------------------------------------------------------

#' Every top-level constant NAME in the loaded torp namespace matching the
#' rating-defining families.
#'
#' Reads the namespace, not R/constants_ratings.R as text: `R CMD check` runs
#' tests against a BUILT package from `<pkg>.Rcheck/tests/`, where no `R/` source
#' tree exists and the code lives in a lazy-load database, so a file read threw
#' (then skipped) on every CI run and the scan never ran there. The namespace is
#' the same under `devtools::load_all()` and `R CMD check`. The match is on
#' all-caps names whose value is not a function, so a `#' @keywords internal`
#' doc line (the hazard the old text scan stripped comments to avoid) cannot
#' appear here at all.
vintage_scan_family_constants <- function() {
  ns <- asNamespace("torp")
  nm <- ls(ns, all.names = TRUE)
  nm <- nm[grepl("^[A-Z][A-Z0-9_]*$", nm) &
             grepl("^(TORP_|EPR_|EPV_|PSR_|LINEUP_POSITION_|NP_)", nm)]
  is_fn <- vapply(nm, function(n) is.function(get(n, envir = ns)), logical(1))
  sort(unique(nm[!is_fn]))
}

#' The deparsed source of .rating_defining_constants(), used for wiring-by-
#' REFERENCE checks. The function groups several constants under shorthand
#' list keys (EPR_DECAY_RECV becomes EPR_DECAY$recv), so matching against the
#' function's OUTPUT names would miss them; matching whether the identifier is
#' mentioned in the function's own code does not.
vintage_defining_constants_source <- function() {
  paste(deparse(.rating_defining_constants), collapse = "\n")
}

#' Whether NAME is referenced as a whole identifier inside `source_text`.
#' `\\b` works here because every candidate is a full R identifier and
#' underscore is a word character, so this cannot partial-match e.g.
#' EPR_DECAY_RECV inside a longer name.
vintage_is_wired <- function(name, source_text = vintage_defining_constants_source()) {
  grepl(paste0("\\b", name, "\\b"), source_text)
}

# ---------------------------------------------------------------------------
# The checks
# ---------------------------------------------------------------------------

test_that("the constants scan and the defining-constants flatten find something to check", {
  # A "count the violations, pass if zero" guard passes vacuously on an empty
  # scan -- exactly what a broken scanner or an accidentally-emptied
  # constants file would produce. Floors are set well below the current
  # counts (138 family constants, 137 flattened leaves on 2026-10-31) so a real deletion
  # trips them rather than every routine edit.
  scanned <- vintage_scan_family_constants()
  expect_gte(length(scanned), 15L)

  flat <- .flatten_named_list(.rating_defining_constants())
  expect_gte(length(flat), 10L)
})

test_that("every rating-defining-family constant is wired or registered", {
  scanned <- vintage_scan_family_constants()
  src <- vintage_defining_constants_source()
  wired <- vapply(scanned, vintage_is_wired, logical(1), source_text = src)
  orphans <- sort(scanned[!wired])

  # A register entry that has since been wired fails this too -- there is no
  # way to pass with an entry in KNOWN_NON_DEFINING that vintage_is_wired()
  # would now find, because the two sides must match EXACTLY.
  registered <- KNOWN_NON_DEFINING
  expect_equal(
    orphans, sort(registered),
    info = paste0(
      "A rating-defining-family constant is neither referenced inside ",
      ".rating_defining_constants() nor registered in KNOWN_NON_DEFINING. ",
      "Changing it would silently change published ratings with no manifest ",
      "trace and no vintage bump. Wire it into .rating_defining_constants(), ",
      "or register it in KNOWN_NON_DEFINING with a reason and a date.\n",
      "Newly unregistered: ", paste(setdiff(orphans, registered), collapse = ", "), "\n",
      "Stale register entries (already wired, prune them): ",
      paste(setdiff(registered, orphans), collapse = ", ")
    )
  )
})

# ---------------------------------------------------------------------------
# Round-trip: the comparison check_vintage_alignment() runs actually works
# ---------------------------------------------------------------------------

test_that("round-trip: a manifest recording the CURRENT constants reports aligned", {
  entry <- .build_rating_vintage_entry(n_rows = 10, version = RATING_VINTAGE,
                                       file = "torp_ratings.parquet",
                                       generated_utc = "2026-08-09T00:00:00Z")
  # Mirror the real path exactly: publish_ratings_manifest() writes with
  # toJSON(auto_unbox = TRUE, null = "null"); read_ratings_manifest() reads
  # with fromJSON(simplifyVector = FALSE). A manifest hand-built any other way
  # would not exercise the same type coercions the real comparison must
  # survive (integers -> doubles, NA -> null -> NULL, named vectors -> lists).
  manifest_json <- jsonlite::toJSON(
    list(canonical = RATING_VINTAGE,
         vintages = stats::setNames(list(entry), RATING_VINTAGE)),
    auto_unbox = TRUE, null = "null"
  )
  manifest <- jsonlite::fromJSON(manifest_json, simplifyVector = FALSE)

  res <- check_vintage_alignment(strict = TRUE, manifest = manifest)
  expect_true(res$aligned)
  expect_equal(res$canonical, RATING_VINTAGE)

  # Non-strict must agree -- alignment success does not depend on strictness.
  res2 <- check_vintage_alignment(strict = FALSE, manifest = manifest)
  expect_true(res2$aligned)
})

test_that("round-trip: a planted single-leaf drift is caught and named", {
  entry <- .build_rating_vintage_entry(n_rows = 10, version = RATING_VINTAGE,
                                       file = "torp_ratings.parquet",
                                       generated_utc = "2026-08-09T00:00:00Z")
  manifest_json <- jsonlite::toJSON(
    list(canonical = RATING_VINTAGE,
         vintages = stats::setNames(list(entry), RATING_VINTAGE)),
    auto_unbox = TRUE, null = "null"
  )
  manifest <- jsonlite::fromJSON(manifest_json, simplifyVector = FALSE)

  # Nudge one decay value the manifest recorded for the canonical vintage --
  # e.g. EPR_DECAY_RECV edited without a RATING_VINTAGE bump.
  manifest$vintages[[RATING_VINTAGE]]$defining_constants$EPR_DECAY$recv <-
    manifest$vintages[[RATING_VINTAGE]]$defining_constants$EPR_DECAY$recv + 1

  err <- tryCatch(check_vintage_alignment(strict = TRUE, manifest = manifest),
                  error = function(e) e)
  expect_s3_class(err, "torp_error_vintage_constants_drift")
  expect_match(conditionMessage(err), "EPR_DECAY.recv", fixed = TRUE)
})

test_that(".diff_defining_constants() reports no differences for identical live constants", {
  live_rt <- .round_trip_constants(.rating_defining_constants())
  diffs <- .diff_defining_constants(live_rt)
  expect_length(diffs, 0L)
})

test_that(".diff_defining_constants() reports an absent leaf as <absent>, not a crash", {
  live_rt <- .round_trip_constants(.rating_defining_constants())
  live_rt$TORP_EPR_WEIGHT <- NULL   # simulate a manifest missing a leaf entirely
  diffs <- .diff_defining_constants(live_rt)
  expect_true("TORP_EPR_WEIGHT" %in% names(diffs))
  expect_equal(diffs$TORP_EPR_WEIGHT$manifest, "<absent>")
})

# ---------------------------------------------------------------------------
# check_vintage_alignment() -- unit tests with an injected manifest (offline)
# ---------------------------------------------------------------------------

test_that("manifest is injectable, defaulting to read_ratings_manifest()", {
  # This is what makes every test in this file able to run without network --
  # the plan calls for exactly this parameter.
  expect_equal(deparse(formals(check_vintage_alignment)$manifest), "read_ratings_manifest()")
})

test_that("strict defaults to VERSEBUS_STRICT=1, like every other call site", {
  # eval() here is safe: the expression being evaluated is the function's own
  # formal default (`.strict_mode()`), read from the loaded package's static
  # source via formals() -- not external/user input.
  withr::with_envvar(c(VERSEBUS_STRICT = "1"), {
    expect_true(eval(formals(check_vintage_alignment)$strict))
  })
  withr::with_envvar(c(VERSEBUS_STRICT = ""), {
    expect_false(eval(formals(check_vintage_alignment)$strict))
  })
  # Regression, 2026-08-11: this default tested nzchar(), so "0" -- which the
  # four other call sites read as lenient -- went STRICT here alone.
  withr::with_envvar(c(VERSEBUS_STRICT = "0"), {
    expect_false(eval(formals(check_vintage_alignment)$strict))
  })
})

test_that("an unreadable (NULL) manifest aborts under strict", {
  # Same rule as preserve_rating_vintage(): ambiguity is never "absent". An
  # unreadable manifest cannot license a production write.
  err <- tryCatch(check_vintage_alignment(strict = TRUE, manifest = NULL),
                  error = function(e) e)
  expect_s3_class(err, "torp_error_vintage_manifest_unreadable")
})

test_that("an unreadable (NULL) manifest warns and grandfathers under non-strict", {
  # Pre-manifest releases (published before D-DEF3 existed) must not be
  # blocked forever.
  expect_warning(
    res <- check_vintage_alignment(strict = FALSE, manifest = NULL),
    "Grandfathering"
  )
  expect_true(is.na(res$aligned))
})

test_that("a canonical/RATING_VINTAGE mismatch aborts ALWAYS -- this is incident 1", {
  # torp 2026-07-27/28: manifest canonical says "v1", deployed code computes
  # RATING_VINTAGE. Writing now would relabel v1 rows with v2 logic. This must
  # abort regardless of strict, because it is not an ambiguity to grandfather
  # -- it is a definite, detected mismatch.
  manifest <- list(canonical = "v1", vintages = list())

  withr::with_envvar(c(GITHUB_REF_NAME = "main"), {
    err_strict <- tryCatch(check_vintage_alignment(strict = TRUE, manifest = manifest),
                           error = function(e) e)
    expect_s3_class(err_strict, "torp_error_vintage_mismatch")
    expect_match(conditionMessage(err_strict), "v1", fixed = TRUE)
    expect_match(conditionMessage(err_strict), RATING_VINTAGE, fixed = TRUE)
    expect_match(conditionMessage(err_strict), "main", fixed = TRUE)

    err_nonstrict <- tryCatch(check_vintage_alignment(strict = FALSE, manifest = manifest),
                              error = function(e) e)
    expect_s3_class(err_nonstrict, "torp_error_vintage_mismatch")
  })
})

test_that("branch name falls back to 'local' outside CI", {
  manifest <- list(canonical = "v1", vintages = list())
  withr::with_envvar(c(GITHUB_REF_NAME = NA), {
    err <- tryCatch(check_vintage_alignment(strict = TRUE, manifest = manifest),
                    error = function(e) e)
    expect_match(conditionMessage(err), "local", fixed = TRUE)
  })
})

test_that("a canonical vintage with no recorded defining_constants aborts under strict", {
  # Canonical-with-no-definition is a provenance gap, not a licence to write
  # (the preserve_rating_vintage() rule, applied here too).
  manifest <- list(canonical = RATING_VINTAGE,
                   vintages = stats::setNames(
                     list(list(defining_constants = NULL)), RATING_VINTAGE))
  err <- tryCatch(check_vintage_alignment(strict = TRUE, manifest = manifest),
                  error = function(e) e)
  expect_s3_class(err, "torp_error_vintage_undefined")
})

test_that("a canonical vintage with no recorded defining_constants warns under non-strict", {
  manifest <- list(canonical = RATING_VINTAGE,
                   vintages = stats::setNames(
                     list(list(defining_constants = NULL)), RATING_VINTAGE))
  expect_warning(
    res <- check_vintage_alignment(strict = FALSE, manifest = manifest),
    "no recorded defining_constants"
  )
  expect_true(is.na(res$aligned))
  expect_equal(res$canonical, RATING_VINTAGE)
})

test_that("a canonical vintage absent from vintages entirely is treated as undefined, not a crash", {
  manifest <- list(canonical = RATING_VINTAGE, vintages = list())
  err <- tryCatch(check_vintage_alignment(strict = TRUE, manifest = manifest),
                  error = function(e) e)
  expect_s3_class(err, "torp_error_vintage_undefined")
})

test_that("check_vintage_alignment stays internal -- no pkgdown index entry required", {
  expect_false("check_vintage_alignment" %in% getNamespaceExports("torp"))
})
