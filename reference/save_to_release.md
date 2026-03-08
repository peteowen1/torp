# Save a Data Frame to a GitHub Release via Piggyback

This function is intended for internal use and may be unexported in a
future release. Saves a data frame as a `.parquet` file and uploads it
to a GitHub release using the `piggyback` package.

## Usage

``` r
save_to_release(df, file_name, release_tag, also_csv = FALSE)
```

## Arguments

- df:

  A data frame to save.

- file_name:

  A string for the file name (without extension).

- release_tag:

  The GitHub release tag to associate with the uploaded file.

- also_csv:

  Logical. If TRUE, also upload a `.csv` copy alongside parquet.

## Value

No return value. Used for side effects (file upload).

## Examples

``` r
if (FALSE) { # \dontrun{
my_df <- data.frame(x = 1:3)
save_to_release(my_df, "my_data", "v1.0.0")
save_to_release(my_df, "my_data", "v1.0.0", also_csv = TRUE)
} # }
```
