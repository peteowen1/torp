# Download multiple parquet files in parallel using curl

Always checks local `torpdata/data/` first for each URL (with smart
staleness), then optionally checks disk cache, then downloads missing
files in parallel. Local files are batch-read via
[`arrow::open_dataset()`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
for speed. Downloaded data is auto-saved to local storage.

## Usage

``` r
parquet_from_urls_parallel(
  urls,
  use_cache = FALSE,
  max_age_days = 7,
  columns = NULL
)
```

## Arguments

- urls:

  Character vector of URLs

- use_cache:

  Logical. If TRUE, also check/write the `~/.torp/cache/` disk cache.

- max_age_days:

  Maximum disk cache age in days.

- columns:

  Optional character vector of column names to select.

## Value

A data.table with all files combined
