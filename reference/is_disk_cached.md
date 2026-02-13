# Check if URL is Cached on Disk

Check if URL is Cached on Disk

## Usage

``` r
is_disk_cached(url, max_age_days = 7)
```

## Arguments

- url:

  Character URL to check

- max_age_days:

  Maximum age in days for cache to be considered valid. Default is 7
  days. Set to NULL for no expiration.

## Value

Logical indicating if valid cache exists
