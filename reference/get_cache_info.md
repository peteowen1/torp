# Get Cache Information

Returns information about all data cached in memory (fixtures, results,
teams, player stats, player details, and AFL API lookups).

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
get_cache_info()
#> [1] cache_key   timestamp   age_seconds data_rows  
#> <0 rows> (or 0-length row.names)
# }
```
