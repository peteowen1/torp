# Injury-Return Accuracy (Predicted vs Actual Rounds Out)

For every round with injury-history coverage, take the latest
pre-kickoff weekly listing for each player, compute the predicted return
round via
[`parse_return_round()`](https://peteowen1.github.io/torp/reference/parse_return_round.md),
and compare to whether the player actually played that round. Produces a
listing-level tibble plus a per-band summary (`attr(x, "by_band")`)
showing calibration of each `estimated_return` band (e.g. "1-2 weeks",
"TBC", "Test").

## Usage

``` r
injury_return_accuracy(season = NULL, round = NULL)
```

## Arguments

- season:

  Season year. Defaults to current via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md).

- round:

  Optional round number (or vector) to filter to.

## Value

A tibble with one row per listing-round: `round`, `player`, `team`,
`injury`, `estimated_return`, `predicted_return_round` (may be `Inf` =
out for season), `predicted_rounds_out` (predicted - round, `Inf` for
season-out), `played` (actual appearance in selected 22), `scraped_at`.
Attached `by_band` attribute is a tibble of calibration stats per
`estimated_return` value, with columns:

- `band` — lowercased trimmed estimate string (e.g. `"1-2 weeks"`,
  `"tbc"`, `"test"`)

- `n` — count of listing-rounds in this band

- `mean_predicted_out` — mean `predicted_rounds_out` (finite only)

- `played_pct` — fraction where the player actually played

## Details

Interpretation of `played_pct` varies by the sign of
`predicted_rounds_out`:

- Among rows with `predicted_rounds_out > 0` (model says "still out"),
  `played_pct` is a **false-positive rate** — fraction who played
  despite being flagged out.

- Among rows with `predicted_rounds_out <= 0` (model says "back this
  round"), `played_pct` is a **true-positive rate** — recall of
  predicted-back listings.
