# Set Local Data Directory

Sets the path to the local torpdata/data/ directory for the current
session. Creates the directory if it doesn't exist.

## Usage

``` r
set_local_data_dir(path)
```

## Arguments

- path:

  Character path to the local data directory

## Value

Invisible normalized path

## Examples

``` r
if (FALSE) { # \dontrun{
set_local_data_dir("path/to/torpdata/data")
} # }
```
