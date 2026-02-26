# Save Data Frame Locally in torpdata/data/

Saves a data frame as a parquet file in the local torpdata/data/
directory. This is called automatically by
[`save_to_release()`](https://peteowen1.github.io/torp/reference/save_to_release.md)
but can also be used directly.

## Usage

``` r
save_locally(df, file_name)
```

## Arguments

- df:

  A data frame to save

- file_name:

  A string for the file name (without extension)

## Value

Invisible logical indicating success

## Examples

``` r
# \donttest{
my_df <- data.frame(x = 1:3)
save_locally(my_df, "test_data")
#> Warning: Local data directory not found. Use `set_local_data_dir()` to configure.
# }
```
