# Determine Max Age for a Local File Based on URL

Historical seasons (before current year) never expire. Current season
files expire after 1 day to pick up mid-season updates.

## Usage

``` r
local_max_age_for_url(url)
```

## Arguments

- url:

  Character URL of the remote parquet file

## Value

Numeric max age in days, or NULL for no expiration
