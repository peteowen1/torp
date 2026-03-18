# Parse Estimated Return into Round Number

Vectorized function that converts free-text estimated return strings
from the AFL injury list into numeric round numbers for use in
simulation scheduling.

## Usage

``` r
parse_return_round(estimated_return, season, current_round = 1L)
```

## Arguments

- estimated_return:

  Character vector of return estimates (e.g., "Round 14", "TBC", "2027",
  "Mid-season").

- season:

  Numeric season year (e.g. 2026). Used to detect next-year returns.

- current_round:

  Integer current round number. Used to compute TBC fallback.

## Value

Numeric vector. `Inf` = out for season. `NA` = not injured / already
available. Finite values = round the player is expected to return.
