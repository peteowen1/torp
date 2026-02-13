# Get Disk Cache Information

Returns information about the current disk cache state.

## Usage

``` r
get_disk_cache_info()
```

## Value

A data frame with cache information including file names, sizes, and
ages

## Examples

``` r
# \donttest{
# Check disk cache status
get_disk_cache_info()
#> [1] file     size_mb  age_days created 
#> <0 rows> (or 0-length row.names)
# }
```
