# Load AFL Match Results Data

Loads AFL match results and scores from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_results(
  seasons = get_afl_season(),
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- seasons:

  A numeric vector of 4-digit years associated with given AFL seasons -
  defaults to latest season. If set to `TRUE`, returns all available
  data since 2021.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache for faster repeated
  loads. Default is FALSE.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

A data frame containing AFL match results and final scores.

## See also

[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md),
[`load_predictions()`](https://peteowen1.github.io/torp/reference/load_predictions.md),
[`load_teams()`](https://peteowen1.github.io/torp/reference/load_teams.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_results(2021:2022)
})
#> Downloading 2 files in parallel...
#> # A tibble: 414 × 77
#>    k2kSponsored match.name        match.date          match.status match.matchId
#>    <lgl>        <chr>             <dttm>              <chr>        <chr>        
#>  1 FALSE        Richmond Vs Carl… 2021-03-18 08:25:00 CONCLUDED    CD_M20210140…
#>  2 FALSE        Collingwood Vs W… 2021-03-19 08:50:00 CONCLUDED    CD_M20210140…
#>  3 FALSE        Melbourne Vs Fre… 2021-03-20 02:45:00 CONCLUDED    CD_M20210140…
#>  4 FALSE        Adelaide Crows V… 2021-03-20 05:35:00 CONCLUDED    CD_M20210140…
#>  5 FALSE        Essendon Vs Hawt… 2021-03-20 08:25:00 CONCLUDED    CD_M20210140…
#>  6 FALSE        Brisbane Lions V… 2021-03-20 08:45:00 CONCLUDED    CD_M20210140…
#>  7 FALSE        North Melbourne … 2021-03-21 02:10:00 CONCLUDED    CD_M20210140…
#>  8 FALSE        GWS GIANTS Vs St… 2021-03-21 04:20:00 CONCLUDED    CD_M20210140…
#>  9 FALSE        West Coast Eagle… 2021-03-21 07:10:00 CONCLUDED    CD_M20210140…
#> 10 FALSE        Carlton Vs Colli… 2021-03-25 08:20:00 CONCLUDED    CD_M20210140…
#> # ℹ 404 more rows
#> # ℹ 72 more variables: match.venue <chr>, match.utcStartTime <chr>,
#> #   match.homeTeamId <chr>, match.awayTeamId <chr>, match.round <chr>,
#> #   match.venueLocalStartTime <chr>, match.abbr <chr>,
#> #   match.twitterHashTag <chr>, match.homeTeam.name <chr>,
#> #   match.homeTeam.timeZone <lgl>, match.homeTeam.teamId <chr>,
#> #   match.homeTeam.abbr <chr>, match.homeTeam.nickname <chr>, …
# }
```
