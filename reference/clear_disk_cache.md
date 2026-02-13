# Clear Disk Cache

Removes cached files from the disk cache directory.

## Usage

``` r
clear_disk_cache(pattern = NULL, older_than_days = NULL, verbose = FALSE)
```

## Arguments

- pattern:

  Optional regex pattern to match specific cache files. If NULL
  (default), clears all cached files.

- older_than_days:

  Only clear files older than this many days. If NULL (default), clears
  regardless of age.

- verbose:

  Logical. If TRUE, prints information about cleared files.

## Value

Invisible count of files removed

## Examples

``` r
# \donttest{
# Clear all disk cache
clear_disk_cache()

# Clear only chains data cache
clear_disk_cache(pattern = "chains")

# Clear files older than 30 days
clear_disk_cache(older_than_days = 30)

# Clear with verbose output
clear_disk_cache(verbose = TRUE)
#> No cache files to clear
# }
```
