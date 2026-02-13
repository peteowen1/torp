# Load parquet file from a remote connection with disk caching

Load parquet file from a remote connection with disk caching

## Usage

``` r
parquet_from_url_cached(url, use_cache = TRUE, max_age_days = 7)
```

## Arguments

- url:

  A character URL

- use_cache:

  Logical. If TRUE, use disk cache.

- max_age_days:

  Maximum age for cached files in days.

## Value

A data frame
