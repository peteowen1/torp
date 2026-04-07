# Set Disk Cache Options

This function is intended for internal use and may be unexported in a
future release. Configure disk cache behavior for the current session.

## Usage

``` r
set_disk_cache_options(
  enabled = TRUE,
  max_age_days = DISK_CACHE_DEFAULT_AGE_DAYS
)
```

## Arguments

- enabled:

  Logical. Enable or disable disk caching.

- max_age_days:

  Numeric. Maximum age in days for cached files. Files older than this
  will be re-downloaded.

## Value

Invisible list of current settings

## Examples

``` r
# \donttest{
# Disable disk caching
set_disk_cache_options(enabled = FALSE)

# Set cache to expire after 30 days
set_disk_cache_options(max_age_days = 30)
# }
```
