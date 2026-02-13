# Get Cache Information

Returns information about the current fixture cache state.

## Usage

``` r
get_cache_info()
```

## Value

A data frame with cache information including keys, timestamps, and data
sizes

## Examples

``` r
# \donttest{
# Check current cache status
get_cache_info()
#> [1] cache_key   timestamp   age_seconds data_rows  
#> <0 rows> (or 0-length row.names)
# }
```
