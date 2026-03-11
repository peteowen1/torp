# AFL Season Fixtures

Complete fixture list for the current AFL season including match
scheduling, team matchups, venues, and round information. Useful as
offline fallback when torpdata is unavailable.

## Usage

``` r
fixtures
```

## Format

A data frame with fixture information including match IDs, start times,
round details, home/away team information, venue details, and season
metadata. Key columns include `match_id`, `utc_start_time`, `status`,
`round_number`, `home_team_name`, `away_team_name`, `venue_name`, and
`season`.

## Source

AFL fixture data via AFL API

## Note

For live/current fixtures, use
[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md)
from torpdata. This bundled copy serves as offline fallback reference.
