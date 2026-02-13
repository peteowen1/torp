# Get a Player Profile

Combines raw stats, TORP season ratings, and current TORP rating for a
player into a single object. Accepts partial name matches (e.g.
"Heeney").

## Usage

``` r
player_profile(player_name, seasons = TRUE)
```

## Arguments

- player_name:

  A character string of the player's name (full or partial).

- seasons:

  Seasons to include. Numeric vector of years, or `TRUE` for all
  available.

## Value

A list of class `torp_player_profile` with elements:

- player_info:

  1-row tibble with player_id, name, team, position.

- yearly_stats:

  Per-season aggregated raw stats from
  [`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md).

- torp_season:

  Per-season TORP ratings from
  [`player_season_ratings()`](https://peteowen1.github.io/torp/reference/player_season_ratings.md).

- current_torp:

  Current TORP rating from
  [`calculate_torp_ratings()`](https://peteowen1.github.io/torp/reference/calculate_torp_ratings.md).

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  player_profile("Heeney")
  player_profile("Heeney", seasons = 2022:2024)
})
#> Loading 1/6 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/player_details-data/player_details_2026.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/player_details-data/player_details_2026.parquet>
#> - check internet connection
#> Loading 1/6 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/player_stats-data/player_stats_2026.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/player_stats-data/player_stats_2026.parquet>
#> - check internet connection
#> Loading 1/6 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/player_game-data/player_game_2026.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/player_game-data/player_game_2026.parquet>
#> - check internet connection
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/player_details-data/player_details_2026.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/player_details-data/player_details_2026.parquet>
#> - check internet connection
#> Loading 1/6 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/player_game-data/player_game_2026.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/player_game-data/player_game_2026.parquet>
#> - check internet connection
#> Loading 1/6 files...
#> Joining with `by = join_by(season, round)`
#> TORP ratings as at 2026 round 0
#> Loading 1/3 files...
#> Loading 1/3 files...
#> Loading 1/6 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/player_game-data/player_game_2026.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/player_game-data/player_game_2026.parquet>
#> - check internet connection
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/player_details-data/player_details_2026.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/player_details-data/player_details_2026.parquet>
#> - check internet connection
#> Loading 1/6 files...
#> Warning: downloaded length 0 != reported length 9
#> Warning: cannot open URL 'https://github.com/peteowen1/torpdata/releases/download/player_game-data/player_game_2026.parquet': HTTP status was '404 Not Found'
#> Warning: Failed to connect to
#> <https://github.com/peteowen1/torpdata/releases/download/player_game-data/player_game_2026.parquet>
#> - check internet connection
#> Joining with `by = join_by(season, round)`
#> TORP ratings as at 2026 round 0
#> === Isaac Heeney (Sydney Swans - MIDFIELDER) ===
#> 
#> --- Yearly Stats ---
#> Key: <season>
#>  season games goals behinds shots_at_goal disposals kicks handballs inside50s
#>   <int> <int> <num>   <num>         <num>     <num> <num>     <num>     <num>
#>    2022    25    49      28            88       410   251       159        69
#>    2023    23    30      19            64       357   214       143        55
#>    2024    24    34      20            59       602   328       274       108
#>  marks tackles contested_possessions clearances_total_clearances
#>  <num>   <num>                 <num>                       <num>
#>    117     118                   203                          54
#>    110      99                   194                          41
#>    108     113                   287                         139
#>  disposal_efficiency time_on_ground_percentage
#>                <num>                     <num>
#>                 70.1                      85.4
#>                 63.9                      84.6
#>                 67.8                      81.5
#> 
#> --- TORP Season Ratings ---
#> # A tibble: 3 × 13
#>   season player_name  player_id  team_id team       position games season_points
#>    <dbl> <chr>        <chr>      <chr>   <chr>      <chr>    <int>         <dbl>
#> 1   2024 Isaac Heeney CD_I298539 CD_T160 Sydney Sw… MIDFIEL…    24          277.
#> 2   2022 Isaac Heeney CD_I298539 CD_T160 Sydney Sw… MEDIUM_…    25          277.
#> 3   2023 Isaac Heeney CD_I298539 CD_T160 Sydney Sw… MEDIUM_…    23          172.
#> # ℹ 5 more variables: season_recv <dbl>, season_disp <dbl>, season_spoil <dbl>,
#> #   season_hitout <dbl>, ppg <dbl>
#> 
# }
```
