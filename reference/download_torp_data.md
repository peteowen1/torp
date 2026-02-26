# Download TORP Data for Local Storage

Downloads parquet files from GitHub releases to the local
`torpdata/data/` directory for fast offline access. Subsequent calls to
[`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md),
[`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md),
etc. will read from local files instead of downloading.

## Usage

``` r
download_torp_data(data_types = "all", seasons = TRUE, overwrite = FALSE)
```

## Arguments

- data_types:

  Character vector of data types to download. Options: `"pbp"`,
  `"chains"`, `"ep_wp_chart"`, `"fixtures"`, `"player_stats"`,
  `"player_details"`, `"player_game"`, `"results"`, `"xg"`, `"teams"`,
  `"predictions"`, `"player_game_ratings"`, `"player_season_ratings"`,
  `"torp_ratings"`, `"team_ratings"`. Default is `"all"` which downloads
  all types.

- seasons:

  A numeric vector of 4-digit years, or `TRUE` for all available seasons
  (2021 to current). Default is `TRUE`.

- overwrite:

  Logical. If `FALSE` (default), skips files that already exist locally.

## Value

Invisible NULL. Called for side effects (file downloads).

## Examples

``` r
if (FALSE) { # \dontrun{
# Download all PBP data
download_torp_data("pbp")

# Download multiple types for specific seasons
download_torp_data(c("pbp", "chains"), seasons = 2024:2025)

# Download everything
download_torp_data()
} # }
```
