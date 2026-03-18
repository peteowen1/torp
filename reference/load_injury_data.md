# Load Injury Data from GitHub Releases

Loads historical injury list snapshots from the torpdata repository.
Each snapshot is a parquet file saved by
[`save_injury_data()`](https://peteowen1.github.io/torp/reference/save_injury_data.md)
during the predictions pipeline.

## Usage

``` r
load_injury_data(seasons = get_afl_season(), columns = NULL)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years. Defaults to the current AFL season
  via
  [`get_afl_season()`](https://peteowen1.github.io/torp/reference/get_afl_season.md).

- columns:

  Optional character vector of column names to read.

## Value

A data frame containing injury snapshots with columns including
`player`, `team`, `injury`, `estimated_return`, `updated`,
`player_norm`, `source`, and `scraped_date`.

## See also

[`get_all_injuries()`](https://peteowen1.github.io/torp/reference/get_all_injuries.md),
[`save_injury_data()`](https://peteowen1.github.io/torp/reference/save_injury_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
try({
  load_injury_data(2026)
})
} # }
```
