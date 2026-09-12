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
EPV3_CONTEST_POPULATION <- "evidence"

#' Judge a contest by what happened, not by who won it (torp#220)
#'
#' The two-way contest path filters its scored rows a second time, after the
#' population has already been decided. The shipped expression is
#' \code{def_win == TRUE | out_desc \%chin\% EPV3_DUEL_OUT}, which keeps every
#' defensive win unconditionally but an attacking win only when the outcome is
#' in the narrower duel list --- so a \code{Mark Fumbled} won by the defence is
#' priced and the identical outcome won by the attack is discarded.
#'
#' The intent stated one line above it is symmetric: "applied only where a
#' contest was actually fought". The implementation is not, and that is the
#' defect.
#'
#' Measured on 2026: the asymmetric version drops 1,483 of 17,078 contests (8.7
#' percent) and \strong{100 percent of them are attacking wins} --- 1,149
#' \code{Mark Fumbled} and 339 \code{Free For}. It lifts the defensive win rate
#' from 78.7 to 86 percent and the defence's share of contest credit from 48.3
#' to 60.1 percent.
#'
#' \code{TRUE} asks the same question the population filter already asked: was a
#' target logged, or is the outcome self-evidently a duel. Expect it to
#' \emph{widen} the forward-defender gap --- the inflated number is the current
#' one, not the corrected one.
#' @keywords internal
NP_CONTEST_FILTER_SYMMETRIC <- FALSE

#' How many outcomes the contest model prices
#'
#' \code{"two"} is the shipped v11 behaviour: the outcome is either a defensive
#' win or an attacking one, and \code{V_pre = (1 - p) V_att + p V_def}.
#'
#' \code{"three"} prices Pete's three outcomes (2026-09-12) --- a mark to the
#' attack, a mark to the defence, or nobody marking it --- so
#' \code{V_pre = p_att V_att + p_def V_def + p_other V_other}.
#'
#' \strong{Why the two-way version is wrong, measured on 2026.} Under
#' \code{"two"} a spoil has nowhere to go but the defensive-win bin, and spoils
#' are 88.4 percent of all defensive wins. So \code{V_def} is fitted almost
#' entirely on spoils, and an intercept mark --- genuinely worth -0.722 expected
#' points to the attacking side --- is priced at +0.270. About one full point
#' understated, on each of 1,602 intercept marks. The fitted probability is also
#' blind to the distinction: mean \code{p_hat} is 0.787 on spoils, 0.786 on
#' attacking marks and 0.791 on defensive marks, because two of the three are
#' the same label to it.
#'
#' \strong{What else moves with this switch}, all decided with Pete on real
#' rows the same day. A free kick is a possession win for whoever won it, so it
#' joins the mark branches rather than sitting in \code{other}. The asymmetric
#' second filter in \code{np_difficulty_terms()} is dropped: it kept every
#' defensive win unconditionally but attacking wins only when the outcome was
#' in \code{EPV3_DUEL_OUT}, discarding 1,483 rows of which 100 percent were
#' attacking wins and inflating the defensive win rate from 78.7 to 86 percent.
#' @keywords internal
EPV3_CONTEST_OUTCOMES <- "two"

#' Outcomes where a player took possession cleanly, for the three-way split
#'
#' A mark, or a free kick --- Pete's rule (2026-09-12): "a free is a win for the
#' team that won the free, which is basically the same as a mark". Everything
#' else is \code{other}: the ball hit the ground and is still live.
#' @keywords internal
EPV3_CONTEST_MARK_OUTS <- c("Contested Mark", "Uncontested Mark", "Pack Mark (P)",
                            "Pack Mark (O)", "Mark On Lead")

#' Drop kicks that were shots at goal from the contest population
#'
#' A defender touching a shot on the goal line is not a marking contest. Sampled
#' 2026: the sequence reads \code{Shot At Goal} then \code{Spoil} at x around 76
#' then \code{Kickin play on} --- a rushed behind. 396 of 11,302 spoils (3.5
#' percent), 493 of 17,571 contests overall.
#'
#' Kept separate from \code{EPV3_CONTEST_OUTCOMES} so the two can be measured
#' one at a time; both are part of the same design decision.
#' @keywords internal
EPV3_CONTEST_EXCLUDE_SHOTS <- FALSE

#' Outcomes that are self-evidently a duel, whatever chains annotated
#'
#' Used by \code{EPV3_CONTEST_POPULATION = "evidence"}. A contested or pack mark
#' is a duel by name; a spoil is a defender beating someone to the ball; a
#' fumbled or dropped mark is a contest someone lost by not holding it.
#'
#' \code{Mark Fumbled} and \code{Mark Dropped} are Pete's additions (2026-09-11)
#' and were excluded entirely before. Measured, \code{Mark Fumbled} runs at
#' 37.9 percent defence-win over 1,840 rows -- genuinely two-sided, not a
#' rounding error. \code{Mark Dropped} deduplicates against it: the pair is one
#' event on two rows at the same coordinates.
#' @keywords internal
EPV3_DUEL_EVIDENCE_OUTS <- c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)",
                             "Spoil", "Spoil gaining possession",
                             "Spoil ineffective", "Mark Fumbled",
                             "Mark Dropped", "Dropped Mark")

#' The outcome set the contest path should use, given the population setting
#'
#' \code{"evidence"} returns the WIDE set deliberately. It cannot express Pete's
#' rule on its own, because that rule is row-level -- a contest is a kick where
#' chains logged a target OR the outcome is self-evidently a duel -- and an
#' outcome whitelist has no way to say "or a target was logged". The row-level
#' filter lives in \code{build_aerial_contests()}; this function's job under
#' \code{"evidence"} is only to let every candidate row through so that filter
#' can see it.
#' @keywords internal
epv3_aerial_out <- function(population = EPV3_CONTEST_POPULATION) {
  switch(population,
    duel     = EPV3_DUEL_OUT,
    all      = EPV3_AERIAL_OUT,
    # "Free For" joins the CANDIDATE set but is NOT in EPV3_DUEL_EVIDENCE_OUTS:
    # a free kick is not self-evidently a duel, so it survives only through the
    # target clause of the row-level filter. That is Pete's call (2026-09-11) --
    # a contest that drew a free kick is still a contest, and chains named both
    # players. It admits ~556 of the 2,842 Free For outcomes; the rest have no
    # logged contest and drop out.
    evidence = unique(c(EPV3_AERIAL_OUT, EPV3_DUEL_EVIDENCE_OUTS, "Free For")),
    cli::cli_abort(c(
      "Unknown {.code EPV3_CONTEST_POPULATION}: {.val {population}}",
      "x" = "Refusing to guess a contest population -- expected {.val duel}, {.val all} or {.val evidence}."
    ))
  )
}
