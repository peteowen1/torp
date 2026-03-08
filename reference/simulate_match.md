# Simulate a single match (internal helper)

Uses the same formula as the regular season simulation. For finals,
`allow_draw = FALSE` re-draws until a decisive result.

## Usage

``` r
simulate_match(
  home_torp,
  away_torp,
  home_advantage = SIM_HOME_ADVANTAGE,
  allow_draw = TRUE
)
```

## Arguments

- home_torp:

  Numeric home team TORP rating.

- away_torp:

  Numeric away team TORP rating.

- home_advantage:

  Numeric home advantage in points.

- allow_draw:

  Logical; if FALSE, re-simulates ties.

## Value

A list with `result` (margin from home perspective), `home_score`,
`away_score`, `estimate`.
