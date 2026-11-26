# Constants shared by the trade-grade scripts (05, 10, 12, 13).

# A player received in a trade counts at this share of his TORP value in every
# grade; picks count in full. Stands in for what clubs pay players, which the
# value leaves out, until contracts are modelled (Pete, 19 Nov 2026).
#
# Measured: over 2021-2026, clubs taking on more player value than they gave
# paid on average 0.32 of it in picks (131 sides; 0.25-0.38 a year bar 2021's
# 0.17 on 12 sides), so 0.32 makes the two sides even on average. Pete chose
# the softer 0.5 after reading real trades before and after: players still win
# more often than picks, but the big swings shrink. Traded players' forecasts
# were accurate (9,253 forecast, 9,636 as it turned out), so the factor is not
# a forecast correction. A draftee from a pick is not scaled: he is on a
# rookie contract, which is the case this factor is a stand-in for.
PLAYER_VALUE_FACTOR <- 0.5

# Pick value: share of the long-history games shape blended into the TORP
# curve (Pete, 26 Nov 2026). On 1991-2015 drafts, star value (Brownlow votes +
# All-Australian) matches the TORP curve and the AFL index, but career games
# rate picks 7-15 at ~70% of a top-3 pick, not ~50% (paired gap -31 to -5
# points), and picks 31-45 sit above the index on both measures. Pete chose to
# move halfway toward games: durable role players are worth paying for.
# Evidence: docs/plans/TRADE-VALUE.md "Long-history quality check". 0 = the old
# curve (v8_direct). 04_pick_curve.R writes the blend as v8_value.
PICK_GAMES_BLEND <- 0.5
