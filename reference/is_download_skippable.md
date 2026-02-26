# Check if a Download Should Be Skipped (Negative Cache)

When a URL previously produced an invalid file (too small / read error),
a `.skip` marker is written next to the expected local path. This avoids
re-downloading known-bad files every call.

## Usage

``` r
is_download_skippable(url)
```

## Arguments

- url:

  Character URL to check

## Value

Logical — TRUE if a fresh `.skip` marker exists
