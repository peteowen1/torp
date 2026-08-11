# `explain_epr()` used to retype `.bayesian_shrink()`'s body four times rather
# than call it (psr.R:1217-1220 before 2026-08-11). An explainer that
# reimplements the thing it explains keeps printing a confident, plausible
# trace of the OLD formula the day the real one changes, and nothing fails.
#
# These tests do not run explain_epr() -- it needs the AFL API and a resolved
# player. They pin the two things the change actually turned on: that the
# production shrinkage is what gets called, and that the call is spelled the
# way the four sites spell it.

test_that(".bayesian_shrink() is the formula explain_epr() prints", {
  # The literal expression that used to be inline, four times over. If
  # .bayesian_shrink() is ever rewritten, this fails and the explainer's
  # printed derivation has to be revisited with it -- which is the whole point
  # of making the explainer call it.
  set.seed(3)
  n <- 200
  sum_val <- stats::rnorm(n, 40, 15)
  wt_gms <- stats::runif(n, 0, 60)
  loading <- 1.4
  prior_games <- 3
  prior_rate <- 0.35

  expect_identical(
    .bayesian_shrink(sum_val, wt_gms, loading, prior_games, prior_rate),
    (loading * sum_val + prior_games * prior_rate) / (wt_gms + prior_games)
  )
})

test_that("explain_epr() no longer spells the shrinkage out by hand", {
  # Drift guard. Local-dev only, like the other source scans -- R CMD check
  # runs against an installed package with no R/ tree beside it.
  code <- .r_source_code()
  skip_if(is.null(code), "R/ source tree not present (installed build)")
  psr <- code[["psr.R"]]
  expect_false(is.null(psr))

  # The shape of the inline form: a division whose numerator multiplies a
  # prior_games term by a prior rate.
  inline <- grepl("loading \\* [a-z_]+_sum\\s*\\+\\s*prior_gms", paste(psr, collapse = " "))
  expect_false(inline,
               info = "call .bayesian_shrink() instead of retyping its body")
  expect_true(any(grepl("\\.bayesian_shrink\\(", psr)))
})

test_that("named subsetting does not leak a name into the result", {
  # The latent bug the rewrite exposed. `prior_gms["recv"]` keeps its name, so
  # the computed value carried name "recv", and `c(recv = value)` then produced
  # "recv.recv" -- meaning res$shrinkage$epr_raw[["recv"]] did not resolve.
  # `[[` drops the name and the wrapper's own names stand.
  prior_gms <- c(recv = 3, disp = 3)

  leaky <- (1 * 10 + prior_gms["recv"] * 2) / (5 + prior_gms["recv"])
  clean <- (1 * 10 + prior_gms[["recv"]] * 2) / (5 + prior_gms[["recv"]])

  expect_identical(unname(leaky), clean)          # same number
  expect_identical(names(c(recv = leaky)), "recv.recv")  # the old bug
  expect_identical(names(c(recv = clean)), "recv")       # what we want now
})

test_that("the explainer says its numbers are pre-centring", {
  # explain_epr() reports epr_RAW. The published EPR is that, then
  # position-centred, so the two do not match and the output has to say so or
  # someone will file a bug against the leaderboard.
  code <- .r_source_code()
  skip_if(is.null(code), "R/ source tree not present (installed build)")
  psr <- paste(code[["psr.R"]], collapse = " ")
  expect_match(psr, "centre_epr_by_position", fixed = TRUE)
  expect_match(psr, "PUBLISHED EPR", fixed = TRUE)
})

test_that("explain_epr() selects channels the way calculate_epr_stats() does", {
  # Second divergence, found in review of the first. Production prefers the
  # opponent-adjusted `_oadj` channels when present and falls back to `_adj`;
  # the pipeline runs adjust_epv_for_opponents() before EPR, so published
  # numbers ARE opponent-adjusted. explain_epr() hardcoded `_adj` while a
  # comment claimed it matched calculate_epr_stats(). It now does the same
  # has_oadj selection.
  code <- .r_source_code()
  skip_if(is.null(code), "R/ source tree not present (installed build)")

  psr <- paste(code[["psr.R"]], collapse = " ")
  expect_match(psr, "has_oadj", fixed = TRUE)

  # And the selection rule must be the same one, not a lookalike: both files
  # gate on all four _oadj channels being present.
  pr <- paste(code[["player_ratings.R"]], collapse = " ")
  for (ch in c("epv_recv_oadj", "epv_disp_oadj", "epv_spoil_oadj", "epv_hitout_oadj")) {
    expect_match(psr, ch, fixed = TRUE)
    expect_match(pr, ch, fixed = TRUE)
  }
})

test_that("explain_epr() no longer hardcodes the _adj channels in its aggregates", {
  # The specific line shape that was wrong: sum(dt$epv_<ch>_adj * ...).
  code <- .r_source_code()
  skip_if(is.null(code), "R/ source tree not present (installed build)")
  psr <- paste(code[["psr.R"]], collapse = " ")
  expect_false(grepl("sum\\(dt\\$epv_[a-z]+_adj \\*", psr),
               info = "aggregate channels must follow the has_oadj selection")
})

test_that("the note names the ACTUAL callers of centre_epr_by_position()", {
  # A first draft of that note said the centring happens "via calculate_epr()".
  # It does not -- calculate_epr() never calls it. The real callers are
  # build_ratings_history() (ratings_build.R:210) and run_ratings_pipeline.R.
  # Pointing a reader at the wrong function is worse than the vague note it
  # replaced, so this pins the call graph rather than the prose.
  code <- .r_source_code()
  skip_if(is.null(code), "R/ source tree not present (installed build)")

  callers <- names(code)[vapply(code, function(x) {
    any(grepl("(?<!function\\()\\bcentre_epr_by_position\\(", x, perl = TRUE)) &&
      !any(grepl("^centre_epr_by_position <- function", x))
  }, logical(1))]
  # ratings_build.R is where the package-internal call lives.
  expect_true("ratings_build.R" %in% callers)

  # And calculate_epr() must NOT be claimed as the caller while it isn't one.
  pr <- code[["player_ratings.R"]]
  start <- grep("^calculate_epr <- function", pr)
  if (length(start) == 1) {
    end <- start + which(grepl("^}", pr[start:length(pr)]))[1] - 1
    body_txt <- paste(pr[start:end], collapse = " ")
    calls_it <- grepl("centre_epr_by_position\\(", body_txt)
    psr_txt <- paste(code[["psr.R"]], collapse = " ")
    claims_it <- grepl("via calculate_epr", psr_txt)
    expect_false(claims_it && !calls_it)
  }
})
