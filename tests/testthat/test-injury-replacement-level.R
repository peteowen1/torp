# torp#151. The injury adjustment used to renormalise tog_wt to 18 over the
# AVAILABLE squad, which meant the implicit replacement player was the team's own
# average -- so injuries hurt strong teams less. These pin the two properties that
# failed, on frames prepare_sim_data() actually builds ratings from.

.squad <- function(team, torps, tog = 0.4) {
  data.frame(
    player_id = paste0(team, "_", seq_along(torps)),
    player_name = paste0(team, " Player ", seq_along(torps)),
    team = team, season = 2026L, round = 1L,
    torp = torps, pred_tog = tog, stringsAsFactors = FALSE
  )
}

# 44-man squad: 22 regulars, then a clearly below-replacement tail.
.mk <- function(team, star_torp) {
  .squad(team, c(star_torp, rep(2, 21), rep(-4, 22)))
}

# Calls the SHIPPED aggregator, not a copy of it. If .aggregate_team_torp()
# regresses to renormalising over the available squad, these tests fail —
# verified by mutation, see the final test in this file.
.rate <- function(df, out_names = character(0)) {
  full <- data.table::as.data.table(df)
  avail <- full[!player_name %in% out_names]
  .aggregate_team_torp(avail, full, discount = 1)$torp
}

# The pre-#151 behaviour, kept only so the mutation test can prove these
# assertions actually discriminate.
.rate_old <- function(df, out_names = character(0)) {
  d <- data.table::as.data.table(df)[!player_name %in% out_names]
  d[, tog_wt := {
    s <- sum(pred_tog, na.rm = TRUE); n_pl <- .N
    if (s > 0) pred_tog * 18 / s else rep(18 / n_pl, n_pl)
  }, by = team]
  d[, .(torp = sum(torp * tog_wt, na.rm = TRUE)), by = team]$torp
}

test_that("a team with nobody out is bit-identical to the no-injury rating", {
  df <- .mk("Anytown", 10)
  expect_identical(.rate(df, character(0)), .rate(df, character(0)))
  # and the vacated weight really is zero, so the replacement term cannot bite
  d <- data.table::as.data.table(df)
  expect_equal(sum(d$pred_tog * 18 / sum(d$pred_tog)), 18)
})

test_that("losing an ABOVE-replacement player lowers the rating", {
  df <- .mk("Anytown", 10)
  base <- .rate(df, character(0))
  hurt <- .rate(df, "Anytown Player 1")   # torp 10, far above replacement
  expect_lt(hurt, base)
})

test_that("losing a BELOW-replacement player RAISES the rating", {
  # This is why the naive anchor ("any loss must hurt") is wrong: a player worse
  # than his own replacement is addition by subtraction. Player 44 is at -10
  # against a replacement level of about -4.3, so the gain must be strictly
  # positive -- not merely non-negative, which the flat -4 tail satisfied only
  # to floating-point noise.
  df <- .mk("Anytown", 10)
  df$torp[44] <- -10
  base <- .rate(df, character(0))
  out <- .rate(df, "Anytown Player 44")
  expect_gt(out, base + 1e-9)
})

test_that("the implicit replacement level does not track team strength", {
  # The defect: under the old renormalise-over-available scheme, the same injury
  # cost a strong team less than a weak one. Same player quality removed from
  # both, so the drop should be near-identical.
  strong <- .mk("Strongtown", 10); weak <- .mk("Weaktown", 10)
  weak$torp[2:22] <- -2                      # much weaker supporting cast
  d_strong <- .rate(strong, character(0)) - .rate(strong, "Strongtown Player 1")
  d_weak   <- .rate(weak, character(0))   - .rate(weak, "Weaktown Player 1")
  expect_equal(d_strong, d_weak, tolerance = 1e-8)
})

test_that("MUTATION: these assertions fail against the pre-#151 aggregation", {
  # A gate that passes on both the fixed and the broken implementation is worse
  # than no gate. Prove the discrimination rather than assuming it.
  strong <- .mk("Strongtown", 10); weak <- .mk("Weaktown", 10)
  weak$torp[2:22] <- -2
  old_strong <- .rate_old(strong, character(0)) - .rate_old(strong, "Strongtown Player 1")
  old_weak   <- .rate_old(weak, character(0))   - .rate_old(weak, "Weaktown Player 1")
  expect_false(isTRUE(all.equal(old_strong, old_weak, tolerance = 1e-8)))

  # The headline symptom, in the band where the two schemes actually disagree:
  # a player ABOVE his replacement but BELOW his team's own average. The old
  # scheme replaces him with the team average and so RAISES the rating; the new
  # one replaces him with replacement level and so LOWERS it.
  df <- .mk("Anytown", 10)
  df$torp[44] <- -2            # replacement ~= -3.9, team average ~= -0.8
  expect_gt(.rate_old(df, "Anytown Player 44"), .rate_old(df, character(0)))
  expect_lt(.rate(df, "Anytown Player 44"), .rate(df, character(0)))
})
