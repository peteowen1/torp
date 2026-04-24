# TBC Listing Played-Rate

Per-round analogue of
[`test_played_rate()`](https://peteowen1.github.io/torp/reference/test_played_rate.md)
for players listed as "TBC" (estimated return unknown). For each
completed round, finds players whose latest pre-kickoff weekly listing
says "TBC" and checks whether they were named in the selected 22. The
overall played fraction here directly calibrates the `current_round + 3`
fallback used by
[`parse_return_round()`](https://peteowen1.github.io/torp/reference/parse_return_round.md)
for TBC entries.

## Usage

``` r
tbc_played_rate(season = NULL, round = NULL)
```

## Arguments

- season:

  Season year. Defaults to current via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md).

- round:

  Optional round number (or vector) to filter to.

## Value

A tibble with one row per TBC listing: `round`, `player`, `team`,
`injury`, `scraped_at`, `played` (logical). Attached `summary` attribute
is a list with elements:

- `n_listings` — total listing-round rows

- `overall_played_pct` — fraction of listings where the player played

- `per_round` — tibble of `round`, `n`, `played`, `played_pct`

## Details

Also returns a `summary` attribute with the overall and per-round played
rate – useful for dashboards and for feeding back into simulation
defaults.
