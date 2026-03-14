# Load parquet files from remote URLs

This function is intended for internal use and may be unexported in a
future release.

## Usage

``` r
load_from_url(
  url,
  ...,
  seasons = TRUE,
  rounds = TRUE,
  use_disk_cache = FALSE,
  columns = NULL
)
```

## Arguments

- url:

  A vector of URLs to load into memory. If more than one URL provided,
  will row-bind them.

- ...:

  Additional arguments (currently unused).

- seasons:

  A numeric vector of years that will be used to filter the dataframe's
  `season` column. If `TRUE` (default), does not filter.

- rounds:

  A numeric vector of rounds that will be used to filter the dataframe's
  `round` column. If `TRUE` (default), does not filter.

- use_disk_cache:

  Logical. If TRUE, uses persistent disk cache for faster repeated
  loads.

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns. Filter columns (season, round, round_number, week)
  are auto-included when filtering is active.

## Value

A tibble
