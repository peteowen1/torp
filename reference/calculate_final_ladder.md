# Calculate expected final ladder from actual results + predictions

Combines played game results with model predictions for remaining games
to produce a single deterministic expected end-of-season ladder. Uses
fractional wins (pred_win) for unplayed games, giving a smooth expected
ladder that properly reflects match uncertainty.

## Usage

``` r
calculate_final_ladder(
  season = get_afl_season(),
  fixtures = NULL,
  predictions = NULL,
  injuries = NULL
)
```

## Arguments

- season:

  Numeric season year (e.g. 2026).

- fixtures:

  Optional fixture data. If NULL, loads via
  [`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md).

- predictions:

  Optional predictions data. If NULL, loads via
  [`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md).

- injuries:

  Optional injury data.frame from
  [`get_all_injuries()`](https://peteowen1.github.io/torp/reference/get_all_injuries.md).
  When provided, team predictions are adjusted for injured players
  returning during the season.

## Value

A data.table with one row per team, sorted by expected ladder position.
Columns: `team`, `played`, `wins`, `draws`, `losses`, `expected_wins`,
`expected_losses`, `points_for`, `points_against`, `percentage`,
`ladder_points`, `rank`.

## Details

When `injuries` is provided, predicted margins for unplayed games are
adjusted to reflect injured players returning mid-season. The injury
schedule is built using
[`torp_ratings()`](https://peteowen1.github.io/torp/reference/torp_ratings.md),
[`parse_return_round()`](https://peteowen1.github.io/torp/reference/parse_return_round.md),
and
[`build_injury_schedule()`](https://peteowen1.github.io/torp/reference/build_injury_schedule.md),
and cumulative TORP boosts are applied per round to shift `pred_margin`
and recalculate `pred_win`.
