# Read a Parquet File from Local Storage

Read a Parquet File from Local Storage

## Usage

``` r
read_local_parquet(url, columns = NULL)
```

## Arguments

- url:

  Character URL used as the key to find the local file

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

Data frame if local file exists, NULL otherwise
