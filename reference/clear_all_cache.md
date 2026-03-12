# Clear All Caches

Clears all torp caches: in-memory data, in-memory models, and on-disk
parquet files. This is the nuclear option — use when you want a
completely fresh start (e.g. after a package update that changes column
schemas).

## Usage

``` r
clear_all_cache(verbose = FALSE)
```

## Arguments

- verbose:

  Logical. If TRUE, prints cache clearing information.

## Value

Invisible NULL

## Examples

``` r
# \donttest{
clear_all_cache()
clear_all_cache(verbose = TRUE)
#> No data cache entries to clear
#> No model cache entries to clear
#> No cache files to clear
#> All torp caches cleared (data, model, disk)
# }
```
