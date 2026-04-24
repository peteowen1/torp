# Empirical Return Distribution for TBC Listings

For every weekly TBC listing in the season's injury history, measures
rounds-until-first-senior-game after the listing's scrape. Returns a
tibble of listing-level observations plus an `attr(x, "cdf")` with the
empirical P(returned by +N rounds) and median rounds-to-return (NA if
more than half are still censored).

## Usage

``` r
tbc_return_survival(season = NULL)
```

## Arguments

- season:

  Season year. Defaults to current via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md).

## Value

A tibble with one row per TBC listing episode: `player`, `team`,
`injury`, `first_tbc_round` (round at scrape), `return_round` (first
lineup appearance, `NA` if censored), `rounds_out` (observed, with
censoring flag), `censored` (logical). Attached attributes:

- `cdf` — tibble of `offset`, `returned`, `total`, `p_returned`
  (cumulative empirical P(returned by +offset rounds))

- `median_rounds_out` — median of observed (non-censored) returns,
  `NA_real_` when fewer than half of episodes have returned

- `n_returned` — count of episodes where the player has returned

- `n_censored` — count of episodes still out at `last_completed`

## Details

Players who have not yet returned are right-censored: their observed
rounds-out is a lower bound. The CDF uses a simple observed / total
estimator – it under-estimates the true return probability at higher
offsets (because some censored players will eventually return), so treat
it as a conservative floor.

Used to audit the `parse_return_round("TBC")` default of
`current_round + 3` – if the median actual return is 1-2 rounds, the
simulation is treating TBC as more severe than the data supports.
