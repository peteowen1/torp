# Check if a File Exists Locally and is Fresh

Check if a File Exists Locally and is Fresh

## Usage

``` r
is_locally_stored(url, max_age_days = NULL)
```

## Arguments

- url:

  Character URL to check for a local copy

- max_age_days:

  Maximum age in days before the local file is considered stale. If
  NULL, no staleness check is performed.

## Value

Logical
