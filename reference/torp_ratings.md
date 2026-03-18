# TORP Ratings (Total Over Replacement Predictive-value)

High-level convenience function that computes the full TORP rating for
each player: EPR (possession-value rating) + PSR (skill rating) blended
together. Returns `epr`, `psr`, `osr`, `dsr`, and `torp` columns.

## Usage

``` r
torp_ratings(
  season_val = get_afl_season(type = "current"),
  round_val = get_afl_week(type = "next"),
  ...
)
```

## Arguments

- season_val:

  Season to calculate ratings for.

- round_val:

  Round to calculate ratings for.

- ...:

  Additional arguments passed to
  [`calculate_epr`](https://peteowen1.github.io/torp/reference/calculate_epr.md).

## Value

A data frame with columns: `player_id`, `player_name`, `epr`,
`recv_epr`, `disp_epr`, `spoil_epr`, `hitout_epr`, `psr`, `osr`, `dsr`,
`torp`, plus metadata columns.

## See also

[`epr_ratings`](https://peteowen1.github.io/torp/reference/epr_ratings.md)
for possession-value ratings only,
[`psr_ratings`](https://peteowen1.github.io/torp/reference/psr_ratings.md)
for skill ratings only.

## Examples

``` r
if (FALSE) { # \dontrun{
  tr <- torp_ratings(2026, 1)
} # }
```
