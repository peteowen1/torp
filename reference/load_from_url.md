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
  peteowen1 = FALSE,
  use_disk_cache = TRUE
)
```

## Arguments

- url:

  A vector of URLs to load into memory. If more than one URL provided,
  will row-bind them.

- ...:

  Named arguments that will be added as attributes to the data, e.g.
  `peteowen1_type` = "pbp"

- seasons:

  A numeric vector of years that will be used to filter the dataframe's
  `season` column. If `TRUE` (default), does not filter.

- rounds:

  A numeric vector of rounds that will be used to filter the dataframe's
  `round` column. If `TRUE` (default), does not filter.

- peteowen1:

  TRUE to add peteowen1_data classing and attributes.

- use_disk_cache:

  Logical. If TRUE (default), uses persistent disk cache for faster
  repeated loads.

## Value

A tibble, possibly of type `peteowen1_data`
