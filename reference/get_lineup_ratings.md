# Get player-level ratings for match lineups

Returns individual player EPR, PSR, and TORP ratings for selected
lineups, with TOG-weighting applied (same logic as match predictions).

## Usage

``` r
get_lineup_ratings(season = NULL, round = NULL, match_id = NULL)
```

## Arguments

- season:

  Season year (default: current via get_afl_season())

- round:

  Round number (default: next week via get_afl_week("next"))

- match_id:

  Optional match ID to filter to a single match

## Value

Tibble with one row per player, columns: season, round, match_id,
team_name, player_name, lineup_position, position_group, epr, recv_epr,
disp_epr, spoil_epr, hitout_epr, psr, torp (blended EPR+PSR)
