# Load parquet file from a remote connection with local-first loading

Always checks local `torpdata/data/` first (with smart staleness), then
optionally checks the disk cache, then downloads. Downloaded data is
auto-saved to local storage for next time.

## Usage

``` r
parquet_from_url_cached(
  url,
  use_cache = TRUE,
  max_age_days = 7,
  columns = NULL
)
```

## Arguments

- url:

  A character URL

- use_cache:

  Logical. If TRUE, also check/write the `~/.torp/cache/` disk cache.

- max_age_days:

  Maximum age for disk cache files in days.

- columns:

  Optional character vector of column names to select.

## Value

A data frame
