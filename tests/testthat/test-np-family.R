# The four Net Points family columns (np_disp / np_recv / np_contest /
# np_team) are sums of the play-type breakdown (torp#263, R/np_breakdown.R).

test_that("every play type .np_play_type can return has a family", {
  roles <- c("actor", "receiver", "contest_winner", "ball_winner", "error_blame",
             "pressure_back", "stoppage_ruck", "stoppage_player")
  descs <- c("Kick", "Handball", "Ground Kick", "Bounce", "Free For", "Free For: In Possession",
             "Handball Received", "Loose Ball Get", "Hard Ball Get", "Crumb", "Gather",
             "Uncontested Mark", "Contested Mark", "Pack Mark", "Centre Bounce",
             "Ball Up Call", "Throw In", "Spoil", "something else", NA)
  g <- expand.grid(role = roles, d = descs, stringsAsFactors = FALSE)
  labels <- unique(torp:::.np_play_type(g$role, g$d))
  pools <- c("Team pool share", "Anchor to the real margin",
             "Re-spread from a team-mate with no act")
  fam <- torp:::.NP_PLAY_TYPE_FAMILY
  expect_true(all(labels %in% names(fam)))
  # and the map carries nothing the breakdown cannot produce
  expect_setequal(names(fam), c(labels, pools))
  expect_false(anyDuplicated(names(fam)) > 0)
  expect_setequal(unique(unname(fam)), torp:::.NP_FAMILY_COLS)
  # Pete's grouping (2026-10-25), spot-checked where it is easy to get wrong
  expect_equal(unname(fam[c("Blame passed back (turnover)", "Charged for an error",
                            "Taking a handball", "Ruck: ball-up",
                            "Re-spread from a team-mate with no act")]),
               c("np_disp", "np_recv", "np_recv", "np_contest", "np_team"))
})

bd_fixture <- function() {
  data.table::data.table(
    match_id  = c("M1", "M1", "M1", "M1", "M1", "M2"),
    player_id = c("A",  "A",  "A",  "B",  "B",  "A"),
    category  = c("Kick", "Receiving a kick", "Team pool share",
                  "Ruck: centre bounce", "Anchor to the real margin", "Charged for an error"),
    value     = c(2, 1, -0.5, 3, 0.25, -1))
}

test_that(".np_family_sums adds each player-match's categories into four columns", {
  out <- torp:::.np_family_sums(bd_fixture())
  data.table::setkey(out, match_id, player_id)
  expect_equal(out$np_disp,    c(2, 0, 0))
  expect_equal(out$np_recv,    c(1, 0, -1))
  expect_equal(out$np_contest, c(0, 3, 0))
  expect_equal(out$np_team,    c(-0.5, 0.25, 0))
})

test_that(".np_family_sums refuses a play type it does not map", {
  bd <- bd_fixture()
  bd$category[1] <- "Brand new play type"
  expect_error(torp:::.np_family_sums(bd), "no family")
})

test_that(".np_family_gate passes the right columns and catches every kind of drift", {
  bd <- bd_fixture()
  fr <- data.frame(match_id = c("M1", "M1", "M2"), player_id = c("A", "B", "A"),
                   net_points = c(2.5, 3.25, -1),
                   np_disp = c(2, 0, 0), np_recv = c(1, 0, -1),
                   np_contest = c(0, 3, 0), np_team = c(-0.5, 0.25, 0))
  expect_invisible(torp:::.np_family_gate(fr, bd))
  # a column off by more than rounding
  bad <- fr; bad$np_recv[1] <- 1 + 1e-6; bad$np_disp[1] <- 2 - 1e-6
  expect_error(torp:::.np_family_gate(bad, bd), "do not match")
  # the four miss net_points
  bad <- fr; bad$net_points[2] <- 4
  expect_error(torp:::.np_family_gate(bad, bd), "miss net_points")
  # a player-match in the breakdown but not the frame, even one netting to zero
  bd0 <- rbind(bd, data.table::data.table(match_id = "M2", player_id = "C",
                                          category = c("Kick", "Team pool share"),
                                          value = c(1, -1)))
  expect_error(torp:::.np_family_gate(fr, bd0), "do not match")
  # a duplicated row
  expect_error(torp:::.np_family_gate(rbind(fr, fr[1, ]), bd), "more than one row")
})

test_that(".np_play_type's own abort on an unknown role formats", {
  expect_error(torp:::.np_play_type("brand_new_role", "Kick"), "Unknown Net Points payment role")
})
