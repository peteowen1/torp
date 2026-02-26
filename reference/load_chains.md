# Load Chains Data

Loads chains data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_chains(
  seasons = get_afl_season(),
  rounds = TRUE,
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- rounds:

  A numeric vector associated with given AFL round - defaults to all
  rounds. If set to `TRUE`, returns all available rounds in the given
  season range.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache for faster repeated
  loads. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing chains data.

## See also

[`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md),
[`load_xg()`](https://peteowen1.github.io/torp/reference/load_xg.md),
[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_chains(2021:2022)
})
#> Downloading 2 files in parallel...
#> Warning: Round filtering requested but no round column found in data. Returning
#> unfiltered. Available columns: "displayOrder", "description", "periodSeconds",
#> "playerId", "teamId", "disposal", "shotAtGoal", "behindInfo", "x", and "y"
#> # A tibble: 847,299 × 59
#>    displayOrder description    periodSeconds playerId teamId disposal shotAtGoal
#>           <int> <chr>                  <int> <chr>    <chr>  <chr>    <lgl>     
#>  1            1 Centre Bounce              0 NA       NA     NA       NA        
#>  2            2 Ground Kick               10 CD_I260… CD_T1… ineffec… NA        
#>  3            3 Ball Up Call              15 NA       NA     NA       NA        
#>  4            4 Free For                  33 CD_I290… CD_T1… NA       NA        
#>  5            5 Handball                  36 CD_I290… CD_T1… effecti… NA        
#>  6            6 Handball Rece…            38 CD_I992… CD_T1… NA       NA        
#>  7            7 Kick                      39 CD_I992… CD_T1… effecti… NA        
#>  8            8 Kick Into F50             39 CD_I992… CD_T1… NA       NA        
#>  9            9 Kick Inside 5…            40 CD_I250… CD_T1… NA       NA        
#> 10           10 Mark On Lead              42 CD_I250… CD_T1… NA       NA        
#> # ℹ 847,289 more rows
#> # ℹ 52 more variables: behindInfo <chr>, x <int>, y <int>, finalState <chr>,
#> #   initialState <chr>, period <int>, chain_number <int>, matchId <chr>,
#> #   venueWidth <int>, venueLength <int>, homeTeamDirectionQtr1 <chr>,
#> #   status <chr>, utcStartTime <chr>, homeTeamId <chr>, awayTeamId <chr>,
#> #   competitionId <chr>, roundNumber <int>, roundId <chr>,
#> #   venueLocalStartTime <lgl>, lastUpdated <chr>, providerMatchId <chr>, …
# }
```
