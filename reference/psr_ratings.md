# PSR Ratings (Player Skill Rating)

Computes skill-based ratings for each player-round: `psr` (margin-based
total), plus `osr` (offensive) and `dsr` (defensive) components that sum
to `psr`.

## Usage

``` r
psr_ratings(
  season_val = get_afl_season(type = "current"),
  round_val = get_afl_week(type = "next"),
  psr_coef_path = NULL
)
```

## Arguments

- season_val:

  Season to compute PSR for. Default is current season.

- round_val:

  Round to filter to. If NULL (default), returns all rounds.

- psr_coef_path:

  Optional path to the margin PSR coefficient CSV. If NULL, uses the
  bundled `inst/extdata/psr_coefficients.csv`.

## Value

A data.table with columns: `player_id`, `player_name`, `season`,
`round`, `pos_group`, `psr_raw`, `psr`, `osr`, `dsr`.

## See also

[`torp_ratings`](https://peteowen1.github.io/torp/reference/torp_ratings.md)
for the full blended rating,
[`epr_ratings`](https://peteowen1.github.io/torp/reference/epr_ratings.md)
for possession-value ratings only.

## Examples

``` r
if (FALSE) { # \dontrun{
  psr_df <- psr_ratings(2026, 1)
  psr_df <- psr_ratings(2026)      # all rounds
} # }
```
