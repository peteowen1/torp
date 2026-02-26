# Get Local Data Directory

Returns the path to the local torpdata/data/ directory for storing
parquet files locally. Checks the `torp.local_data_dir` option first,
then auto-detects based on common workspace layouts.

## Usage

``` r
get_local_data_dir()
```

## Value

Character path to local data directory, or NULL if not found

## Examples

``` r
if (FALSE) { # \dontrun{
get_local_data_dir()

# Set explicitly
set_local_data_dir("path/to/torpdata/data")
get_local_data_dir()
} # }
```
