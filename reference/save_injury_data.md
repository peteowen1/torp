# Save Injury Data to GitHub Release

Appends the current scrape to the season's injury history on torpdata,
deduping to one row per state change. Weekly-source rows are collapsed
on (player_norm, team, injury, estimated_return, updated) so you get one
row each time a player's status changes (e.g. Test -\> Out). Preseason
rows are collapsed on (player_norm, team).

## Usage

``` r
save_injury_data(injuries_df, season)
```

## Arguments

- injuries_df:

  A data.frame of injuries (e.g., from
  [`get_all_injuries()`](https://peteowen1.github.io/torp/reference/get_all_injuries.md)).

- season:

  Numeric season year (e.g. 2026).

## Value

Invisible NULL. Called for side effects (upload).
