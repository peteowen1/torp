# Calculate xGs for AFL Matches

Calculate xGs for AFL Matches

## Usage

``` r
calculate_match_xgs(
  season = get_afl_season(),
  round = get_afl_week(),
  quarter = 1:4
)
```

## Arguments

- season:

  AFL season

- round:

  AFL round

- quarter:

  AFL match quarter

## Value

A data frame with xG statistics for the specified matches
