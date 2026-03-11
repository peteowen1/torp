# Clear Data Cache

Clears all cached data from memory (fixtures, results, teams, player
stats, player details, and AFL API lookups).

## Usage

``` r
clear_data_cache(verbose = FALSE)
```

## Arguments

- verbose:

  Logical. If TRUE, prints cache clearing information.

## Value

Invisible NULL

## Examples

``` r
# \donttest{
clear_data_cache()
clear_data_cache(verbose = TRUE)
#> No data cache entries to clear
# }
```
