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
# Entirely offline: every check here either scans R/constants_ratings.R as
# text or calls check_vintage_alignment() with an injected `manifest =`, never
# read_ratings_manifest()'s real network default.

# ---------------------------------------------------------------------------
# THE REGISTER. Every entry needs a reason and a date. Prune it once wired.
# ---------------------------------------------------------------------------

# Rating-defining-family constants (scanned below) that
# .rating_defining_constants() does not capture.
#
# Registering these is NOT a claim they are safe to change. Several read TRUE
# / non-default TODAY and are directly applied in run_ratings_pipeline.R Stage
# 3 (EPR_POSITION_CENTRE, EPV_LEVEL_CENTRE, PSR_CENTRE_BY_ROUND,
# PSR_POSITION_STANDARDISE, EPV_HITOUT_CENTRE_ON_RUCK, EPV_POINTS_SCALE) --
# changing any of them changes every historical rating with no vintage bump
# and no trace in ratings_manifest.json. This is a pre-existing gap in
# D-DEF3's original wiring (`.rating_defining_constants()` only ever captured
# the blend weight, the two v2 standardisation flags and the EPR
# decay/prior/rate family), surfaced by writing this scan rather than created
# by it, and widening what the manifest captures is explicitly out of scope
# for FABLE-VINTAGE-GUARD-PLAN ("Out of scope: Any change to what counts as a
# defining constant"). Fixing it is a separate, deliberate change -- queued,
# not silently absorbed here.
KNOWN_NON_DEFINING <- c(
  # -- Raw EPV per-event assignment weights: the constants that directly
  # compute epv_recv/epv_disp/epv_spoil/epv_hitout in add_epv_vars(). Each one
  # genuinely IS rating-defining; none is threaded into the manifest. (2026-08-09)
  "EPV_BOUNCE_WT", "EPV_DISP_NEG_OFFSET", "EPV_DISP_POS_OFFSET", "EPV_DISP_SCALE",
  "EPV_RECV_NEG_MULT", "EPV_RECV_NEG_OFFSET", "EPV_RECV_POS_MULT", "EPV_RECV_POS_OFFSET",
  "EPV_RECV_SCALE", "EPV_RECV_INTERCEPT_MARK_SCALE", "EPV_RECV_FAILED_CONTEST_WT",
  "EPV_SPOIL_WT", "EPV_TACKLE_WT", "EPV_PRESSURE_WT", "EPV_DEF_PRESSURE_WT",
  "EPV_HITOUT_WT", "EPV_HITOUT_ADV_WT", "EPV_RUCK_CONTEST_WT", "EPV_RUCK_LOSS_WT",
  "EPV_CONTESTED_POSS_WT", "EPV_CONTESTED_MARKS_WT", "EPV_GROUND_BALL_GETS_WT",
  "EPV_MARKS_INSIDE50_WT", "EPV_INSIDE50S_WT", "EPV_CLANGERS_WT",
  "EPV_SCORE_INVOLVEMENTS_WT", "EPV_INTERCEPTS_WT", "EPV_ONE_PERCENTERS_WT",
  "EPV_REBOUND50S_WT", "EPV_FREES_AGAINST_WT", "EPV_FREES_FOR_WT", "EPV_GOALS_WT",
  "EPV_BEHINDS_WT", "EPV_MARKS_WT", "EPV_UNCONTESTED_POSS_WT", "EPV_SHOTS_AT_GOAL_WT",
  "EPV_KICKS_WT", "EPV_HANDBALLS_WT", "EPV_METRES_GAINED_WT", "EPV_TURNOVERS_WT",
  "EPV_GOAL_ASSISTS_WT",

  # -- Position centring / standardisation / shrink flags at the EPR, EPV and
  # PSR layers. Several are live TRUE today and directly gate blocks in
  # run_ratings_pipeline.R Stage 3 (EPR_POSITION_CENTRE, EPV_LEVEL_CENTRE,
  # PSR_CENTRE_BY_ROUND, PSR_POSITION_STANDARDISE, EPV_HITOUT_CENTRE_ON_RUCK).
  # Same gap as above. (2026-08-09)
  "EPR_POSITION_CENTRE", "EPR_POSITION_SHRINK", "EPR_POSITION_SHRINK_PRIOR",
  "EPR_CENTRE_CHANNELS", "EPV_LEVEL_CENTRE", "EPV_LEVEL_CENTRE_CHANNELS",
  "EPV_POSITION_SHRINK", "EPV_POSITION_SHRINK_RULE", "EPV_POSITION_SHRINK_FLOOR",
  "EPV_POSITION_SHRINK_PRIOR", "EPV_HITOUT_CENTRE_ON_RUCK", "EPV_RUCK_INVOLVEMENT_MIN",
  "EPV_RUCK_BLEND_WIDTH", "PSR_CENTRE_BY_ROUND", "PSR_CENTRE_ON_LISTED",
  "PSR_POSITION_STANDARDISE", "PSR_PRIOR_RATE",

  # -- Points-scale / conversion / recency-decay constants applied at the
  # value layer. EPV_POINTS_SCALE is live per its own docstring ("Applied at
  # the VALUE layer so it flows into the rating"); EPV_WEIGHT_DECAY_DAYS feeds
  # add_epv_vars()'s recency weighting directly. EPV_RUCK_SWING_SCALE is
  # currently =1 ("1 = production... NOT applied" per its docstring) but is
  # registered rather than assumed permanently inert -- flipping it would be
  # exactly this kind of silent change. (2026-08-09)
  "EPV_POINTS_SCALE", "EPV_PER_CHANNEL_POINTS_SCALE", "EPV_CHANNEL_SCALE_KEYS",
  "EPV_RUCK_SWING_SCALE", "EPV_WEIGHT_DECAY_DAYS",

  # -- v3 chain-native engine selector and its exclusively-v3 machinery.
  # EPV_ENGINE == "v2" is the production default; flipping it changes every
  # published rating and would need both its own vintage bump AND its own
  # wiring into .rating_defining_constants() (which today hardcodes v2's
  # recv/disp/spoil/hitout shape) -- a known limitation for when v3 ships (see
  # docs/plans/EPV-V3-CHAIN-NATIVE.md), not fixed here. EPV_CONT_LOSS_ALLOC
  # only affects v3's contest-loss allocation rule. (2026-08-09)
  "EPV_ENGINE", "EPV_CONT_LOSS_ALLOC",

  # -- The "contest" EPR slot (decay/prior-games/prior-rate) is defined but
  # referenced nowhere outside constants_ratings.R itself (verified by a
  # repo-wide grep 2026-08-09) -- dead until a consumer reads it, most likely
  # a future v3 3-channel merge. Not rating-defining while nothing reads it.
  "EPR_DECAY_CONTEST", "EPR_PRIOR_GAMES_CONTEST", "EPR_PRIOR_RATE_CONTEST",

  # -- Legacy/derived, not independent degrees of freedom.
  # EPR_DECAY_DEFAULT_DAYS is a straight alias (`<- EPR_DECAY_RECV`), so its
  # value can never differ from the wired EPR_DECAY_RECV leaf.
  # EPR_LOADING_DEFAULT is a real, used `loading` default (player_ratings.R,
  # psr.R) currently at the identity value 1.0 -- registered rather than
  # assumed inert for the same reason as EPV_RUCK_SWING_SCALE above. (2026-08-09)
  "EPR_DECAY_DEFAULT_DAYS", "EPR_LOADING_DEFAULT"
)

# ---------------------------------------------------------------------------
# Scanners
# ---------------------------------------------------------------------------

#' Path to the source file the completeness scan parses, or NA if it is not
#' reachable.
#'
#' An earlier version of this comment claimed `test_path()` reaches the package
#' root "under both devtools::test() and R CMD check". It does not: `R CMD check`
#' runs tests against a BUILT package from `<pkg>.Rcheck/tests/`, where no `R/`
#' source tree exists, and the installed package stores code in a lazy-load
#' database rather than as `.R` files. So `readLines()` threw
#' `cannot open the connection` and both tests below ERRORed on every CI run.
#'
#' Returns NA rather than a non-existent path so callers skip explicitly,
#' matching `test-versebus-sync.R`'s local-dev-only precedent. This is a
#' source-text scan by design (see `vintage_scan_family_constants()`), so it
#' cannot fall back to the loaded namespace without losing the property that
#' makes it correct.
vintage_constants_source_path <- function() {
  candidates <- c(
    testthat::test_path("..", "..", "R", "constants_ratings.R"),
    testthat::test_path("..", "R", "constants_ratings.R"),
    file.path("R", "constants_ratings.R")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) return(NA_character_)
  normalizePath(hit[[1]], winslash = "/", mustWork = FALSE)
}

#' Skip helper: these scans need the package SOURCE, not the installed package.
skip_if_no_constants_source <- function() {
  testthat::skip_if(is.na(vintage_constants_source_path()),
                    "R/constants_ratings.R not reachable (R CMD check runs against a built package)")
}

#' Every top-level constant NAME in R/constants_ratings.R matching the
#' rating-defining families, scanned from source TEXT rather than from the
#' loaded package -- a `#' @keywords internal` documentation line must never
#' be mistaken for an assignment (mirrors citius's wiring_script_setters()
#' comment-stripping).
vintage_scan_family_constants <- function(path = vintage_constants_source_path()) {
  lines <- readLines(path, warn = FALSE)
  code_lines <- sub("#.*$", "", lines)
  pat <- "^([A-Z][A-Z0-9_]*) *<-"
  hit <- grepl(pat, code_lines)
  nm <- sub(paste0(pat, ".*$"), "\\1", code_lines[hit])
  families <- "^(TORP_|EPR_|EPV_|PSR_|LINEUP_POSITION_)"
  sort(unique(nm[grepl(families, nm)]))
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
  # counts (86 family constants, 38 flattened leaves) so a real deletion
  # trips them rather than every routine edit.
  skip_if_no_constants_source()
  scanned <- vintage_scan_family_constants()
  expect_gte(length(scanned), 15L)

  flat <- .flatten_named_list(.rating_defining_constants())
  expect_gte(length(flat), 10L)
})

test_that("every rating-defining-family constant is wired or registered", {
  skip_if_no_constants_source()
  scanned <- vintage_scan_family_constants()
  src <- vintage_defining_constants_source()
  wired <- vapply(scanned, vintage_is_wired, logical(1), source_text = src)
  orphans <- sort(scanned[!wired])

  # A register entry that has since been wired fails this too -- there is no
  # way to pass with an entry in KNOWN_NON_DEFINING that vintage_is_wired()
  # would now find, because the two sides must match EXACTLY.
  expect_equal(
    orphans, sort(KNOWN_NON_DEFINING),
    info = paste0(
      "A rating-defining-family constant is neither referenced inside ",
      ".rating_defining_constants() nor registered in KNOWN_NON_DEFINING. ",
      "Changing it would silently change published ratings with no manifest ",
      "trace and no vintage bump. Wire it into .rating_defining_constants(), ",
      "or register it in KNOWN_NON_DEFINING with a reason and a date.\n",
      "Newly unregistered: ", paste(setdiff(orphans, KNOWN_NON_DEFINING), collapse = ", "), "\n",
      "Stale register entries (already wired, prune them): ",
      paste(setdiff(KNOWN_NON_DEFINING, orphans), collapse = ", ")
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

test_that("strict defaults to whether VERSEBUS_STRICT is set", {
  # eval() here is safe: the expression being evaluated is the function's own
  # formal default (`nzchar(Sys.getenv("VERSEBUS_STRICT"))`), read from the
  # loaded package's static source via formals() -- not external/user input.
  withr::with_envvar(c(VERSEBUS_STRICT = "1"), {
    expect_true(eval(formals(check_vintage_alignment)$strict))
  })
  withr::with_envvar(c(VERSEBUS_STRICT = ""), {
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
