# Restricting the aerial contest population to genuine DUELS.
#
# THE DEFECT THIS ADDRESSES. `EPV3_AERIAL_OUT` -- the list that DEFINES which
# kicks become contests -- includes `Uncontested Mark` and `Mark On Lead`.
# `EPV3_AERIAL_EXPOSURE_DESCS`, which defines who was exposed to a contest,
# excludes both and says why: "An uncontested mark means by definition that no
# contest happened". Both cannot be right, and measurement says the exposure
# list is the one telling the truth.
#
# Measured over 291,908 scored contests (epv3_duel_population.R):
#
#   68.5% of the population is Uncontested Mark (61.9%) or Mark On Lead (6.6%)
#   53.9% of the CREDIT MASS sits in those non-duels -- mean 0.487 against
#         genuine duels' 0.902, so the "p is near zero so it self-neutralises"
#         argument the design rested on is simply false
#   60.2% of the UNNAMED DEBIT comes from them: 92,241 points charged to
#         defending teams, spread flat over 22 players, for duels nobody entered
#
# And restricting improves signal density rather than merely shrinking the
# channel: gross per team-match falls 72.64 -> 33.47 (-54%) while the SURPLUS
# sd, which is the only part that can ever reach a margin, falls 9.49 -> 7.82
# (-18%). Surplus-to-gross nearly doubles, 0.131 -> 0.234.
#
# The named-loser rate nearly triples too, 11.6% -> 31.3%, which is what Pete's
# original objection predicted: if a duel by definition has a loser, a
# population where 88% have no loser is not a population of duels.

#' Kick outcomes that represent a genuine aerial DUEL
#'
#' Two players could plausibly have contested the ball. Excludes
#' \code{Uncontested Mark} and \code{Mark On Lead}, which
#' \code{EPV3_AERIAL_OUT} contains and which are receptions rather than duels --
#' \code{Mark On Lead} records a defence win 0.0\% of the time across 19,247
#' events, which is what "nobody contested it" looks like in the data.
#'
#' \code{Spoil} variants are 100\% defence-wins by construction and
#' \code{Contested Mark} 43.9\%; those are the two that carry the population.
#' @keywords internal
EPV3_DUEL_OUT <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)",
                   "Spoil", "Spoil gaining possession", "Spoil ineffective")

#' Which aerial outcomes count as a contest
#'
#' \code{"duel"} restricts to \code{EPV3_DUEL_OUT}; \code{"all"} reproduces the
#' original population including uncontested and leading marks.
#'
#' \strong{This changes what the contest channel MEASURES}, so it is a flag with
#' its own gate rather than a tunable. Note it is not a pure restriction of the
#' channel: kicks that stop being contests fall back to the ordinary
#' disposer/receiver split, so value moves into \code{recv} and \code{disp}
#' rather than disappearing.
#' @keywords internal
EPV3_CONTEST_POPULATION <- "all"

#' The outcome set the contest path should use, given the population setting
#' @keywords internal
epv3_aerial_out <- function(population = EPV3_CONTEST_POPULATION) {
  switch(population,
    duel = EPV3_DUEL_OUT,
    all  = EPV3_AERIAL_OUT,
    cli::cli_abort(c(
      "Unknown {.code EPV3_CONTEST_POPULATION}: {.val {population}}",
      "x" = "Refusing to guess a contest population -- expected {.val duel} or {.val all}."
    ))
  )
}
