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

  # -- EPV_ENGINE and EPV_CONT_LOSS_ALLOC sat here until 2026-08-18, deferred on
  # the entry's own stated grounds: flipping the engine "changes every published
  # rating and would need both its own vintage bump AND its own wiring into
  # .rating_defining_constants() -- a known limitation for when v3 ships".
  # v3 has now shipped, so both are wired and this entry is pruned rather than
  # carried forward. While they sat here the drift guard was blind to the
  # largest rating-defining change available: an engine flip produced
  # byte-identical defining_constants and passed the check unremarked.
  #
  # STILL UNRESOLVED, recorded rather than left implicit: the EPR_DECAY and
  # EPR_PRIOR_* leaves are captured under v2's recv/disp/spoil/hitout channel
  # names, so a v3 vintage records its priors under v2's labels. The VALUES are
  # captured and drift is still detected -- only the naming is stale.


  # -- Legacy/derived, not independent degrees of freedom.
  # EPR_DECAY_DEFAULT_DAYS is a straight alias (`<- EPR_DECAY_RECV`), so its
  # value can never differ from the wired EPR_DECAY_RECV leaf.
  # EPR_LOADING_DEFAULT is a real, used `loading` default (player_ratings.R,
  # psr.R) currently at the identity value 1.0 -- registered rather than
  # assumed inert for the same reason as EPV_RUCK_SWING_SCALE above. (2026-08-09)
  "EPR_DECAY_DEFAULT_DAYS", "EPR_LOADING_DEFAULT",

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

  # -- STILL UNAUDITED. These five are the pool-SPREAD dials, a different
  # sub-family from the stoppage pricing unit that was wired on 2026-09-10.
  # Worth knowing before they are swept: production runs spread = "matchup"
  # (build_net_points()'s match.arg default -- .np_engine_frame() never passes
  # `spread`, traced 2026-09-10), so NP_MIRROR_SHARE and NP_POSITION_MIRROR are
  # LIVE on every team pool, while NP_CONTEXT_WEIGHTS is read only under
  # spread = "context" and therefore cannot move a published number today.
  # None has been measured; that is the next audit session.
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
  "NP_STOPPAGE_WIN_WARN", "NP_STOPPAGE_WIN_ABORT", "NP_STOPPAGE_WIN_MIN_N"
)

# ---------------------------------------------------------------------------
# NOT AUDITED. Constants defined OUTSIDE R/constants_ratings.R (epv_difficulty.R,
# epv_v3.R, epv_net_points.R, ...). The scan used to read that one source file
# as text, which does not exist under R CMD check, so it skipped on CI; it now
# reads the loaded namespace, which sees these too. Nobody has decided whether
# they are rating-defining, and several look it (EPV_RAW_CHANNEL_SCALE,
# EPV_MODEL_FEATURES, EPV_OPP_LAMBDA_DECAY). They are NOT in KNOWN_NON_DEFINING
# because that list is a claim of safety. Wiring one is not free either: a new
# leaf in .rating_defining_constants() reads as drift against the published
# manifest, so check_vintage_alignment(strict = TRUE) refuses canonical writes
# until the manifest records it. That is Pete's call per constant.
#
# Until then each one's VALUE is pinned below, so an edit fails this file
# instead of silently moving published ratings. To change one: decide whether
# it is rating-defining, then wire it (and update the manifest) or move it to
# KNOWN_NON_DEFINING with a reason, and drop its pin. (2026-10-31, torp#287)
# ---------------------------------------------------------------------------
UNAUDITED_PINNED <- c(
  EPV_BOX_TERMS                   = "ae0fdb3ee6c835dc36be20e66e065d46",
  EPV_DIFFICULTY_DESCS            = "b75411abf5cd44608c78e7b3bb760bcf",
  EPV_DIFFICULTY_SPLIT            = "68934a3e9455fa72420237eb05902327",
  EPV_DIFFICULTY_SURPRISE_BY_TYPE = "68934a3e9455fa72420237eb05902327",
  EPV_DIFFICULTY_SURPRISE_SHARE   = "d310cb367d993fb6fb584b198a2fd72c",
  EPV_DIFFICULTY_SURPRISE_TABLE   = "8e17363a4a0bb049b3c57bfce0957112",
  EPV_MODEL_FEATURES              = "67a596debb4e2fe4fe64bb66cbc8e34e",
  EPV_OPP_LAMBDA_DECAY            = "50a0f3f37dd7e3799f7ef18846ee1587",
  EPV_OPP_PRIOR_GAMES             = "e4da3b7fbbce2345d7772b0674a318d5",
  EPV_RAW_CHANNEL_SCALE           = "37a6259cc0c1dae299a7866489dff0bd",
  EPV_RELEVANT_DESCRIPTIONS       = "a808db549b5cb69fd47ad65807238e89",
  NP_FIT_RELEASE_TAG              = "543c433c4ae2a8d3cca25304839fdaea",
  TORP_RATINGS_COL_MAP            = "dc8117f9c608901b758230c20daff634"
)

#' A stable text fingerprint of a constant's value: deparsed for language
#' objects (formulas), full-precision JSON otherwise, then md5. writeBin keeps
#' Windows from turning "\n" into "\r\n", so the digest is the same everywhere.
#' jsonlite writes a named atomic vector as a bare array, so names are carried
#' explicitly (recursing into lists): re-keying EPV_DIFFICULTY_SURPRISE_TABLE
#' or TORP_RATINGS_COL_MAP must change the digest.
vintage_constant_fingerprint <- function(name) {
  v <- get(name, envir = asNamespace("torp"))
  keep_names <- function(x) {
    if (is.list(x)) return(list(n = names(x), v = lapply(unname(x), keep_names)))
    if (!is.null(names(x))) return(list(n = names(x), v = unname(x)))
    x
  }
  txt <- if (is.language(v) || inherits(v, "formula")) {
    paste(deparse(v), collapse = " ")
  } else {
    as.character(jsonlite::toJSON(keep_names(v), auto_unbox = TRUE, null = "null", digits = NA))
  }
  f <- tempfile()
  on.exit(unlink(f))
  writeBin(charToRaw(enc2utf8(txt)), f)
  unname(tools::md5sum(f))
}

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
  # counts (138 family constants, 38 flattened leaves on 2026-10-31) so a real deletion
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
  registered <- c(KNOWN_NON_DEFINING, names(UNAUDITED_PINNED))
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

test_that("no constant is both known-safe and unaudited", {
  expect_length(intersect(KNOWN_NON_DEFINING, names(UNAUDITED_PINNED)), 0L)
})

test_that("unaudited constants keep their pinned values", {
  # A failure here means one of the UNAUDITED_PINNED constants changed. It may
  # be rating-defining; see the block above UNAUDITED_PINNED before re-pinning.
  live <- vapply(names(UNAUDITED_PINNED), vintage_constant_fingerprint, character(1))
  changed <- names(live)[live != UNAUDITED_PINNED]
  expect_identical(
    changed, character(0),
    info = paste0("Unaudited constant(s) changed: ", paste(changed, collapse = ", "),
                  ". Decide whether each is rating-defining before re-pinning.")
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
