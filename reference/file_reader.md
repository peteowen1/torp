# Read a Parquet File from a GitHub Release via Piggyback

Downloads and reads a `.parquet` file from a GitHub release using the
`piggyback` package.

## Usage

``` r
file_reader(file_name, release_tag)
```

## Arguments

- file_name:

  The base name of the file (without `.parquet` extension).

- release_tag:

  The GitHub release tag the file is associated with.

## Value

A data frame read from the downloaded `.parquet` file.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- file_reader("latest_data", "v1.0.0")
} # }
```
