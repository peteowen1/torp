# Generate URLs for data download

Generate URLs for data download

## Usage

``` r
generate_urls(
  data_type,
  file_prefix,
  seasons,
  rounds = NULL,
  prefer_aggregated = NULL
)
```

## Arguments

- data_type:

  Type of data (e.g., "chain-data", "pbp-data")

- file_prefix:

  Prefix for the file name

- seasons:

  A numeric vector of seasons

- rounds:

  A numeric vector of rounds (optional)

- prefer_aggregated:

  Logical. If TRUE, prefer aggregated seasonal files when loading all
  rounds for a season. Default is TRUE (aggregated files are available
  on torpdata). Set via option `torp.use_aggregated_files`.

## Value

A character vector of URLs
