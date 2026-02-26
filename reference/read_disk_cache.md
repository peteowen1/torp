# Read Data from Disk Cache

Read Data from Disk Cache

## Usage

``` r
read_disk_cache(url, columns = NULL)
```

## Arguments

- url:

  Character URL to read cached data for

- columns:

  Optional character vector of column names to read. If NULL (default),
  reads all columns.

## Value

Data frame if cache exists, NULL otherwise
