# Load AFL Player Details Data

Loads AFL player biographical and details data from the [torpdata
repository](https://github.com/peteowen1/torpdata)

## Usage

``` r
load_player_details(
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

A data frame containing AFL player biographical details including names,
ages, and team affiliations.

## See also

[`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md),
[`calculate_torp_ratings()`](https://peteowen1.github.io/torp/reference/calculate_torp_ratings.md),
[`player_game_ratings()`](https://peteowen1.github.io/torp/reference/player_game_ratings.md)

## Examples

``` r
# \donttest{
try({ # prevents cran errors
  load_player_details(2021:2022)
})
#> Downloading 2 files in parallel...
#> # A tibble: 1,615 × 20
#>    firstName surname       id team       season jumperNumber position providerId
#>    <chr>     <chr>      <int> <chr>       <int>        <int> <chr>    <chr>     
#>  1 Tom       Doedee       813 Adelaide …   2021           39 KEY_DEF… CD_I10009…
#>  2 Lachlan   Murphy      1723 Adelaide …   2021            4 MEDIUM_… CD_I10009…
#>  3 Mitchell  Hinge       1292 Adelaide …   2021           20 MEDIUM_… CD_I10048…
#>  4 Jordon    Butts       1824 Adelaide …   2021           41 KEY_DEF… CD_I10049…
#>  5 Elliott   Himmelberg  1357 Adelaide …   2021           34 KEY_FOR… CD_I10051…
#>  6 Lachlan   Sholl       1761 Adelaide …   2021           38 MIDFIEL… CD_I10061…
#>  7 Chayce    Jones       1782 Adelaide …   2021            1 MEDIUM_… CD_I10062…
#>  8 Will      Hamill      1781 Adelaide …   2021           17 MEDIUM_… CD_I10081…
#>  9 Ned       McHenry     1767 Adelaide …   2021           25 MEDIUM_… CD_I10081…
#> 10 Nick      Murray      2474 Adelaide …   2021           28 KEY_DEF… CD_I10082…
#> # ℹ 1,605 more rows
#> # ℹ 12 more variables: dateOfBirth <chr>, draftYear <chr>, heightInCm <int>,
#> #   weightInKg <int>, recruitedFrom <chr>, debutYear <chr>, draftType <chr>,
#> #   draftPosition <chr>, data_accessed <date>, player_name <chr>, age <dbl>,
#> #   row_id <chr>
# }
```
