# Load data from AFL API with caching

Generalised helper that wraps any `get_afl_*()` function with per-season
fetching, in-memory caching, and column selection. Used by
[`load_fixtures()`](https://peteowen1.github.io/torp/reference/load_fixtures.md),
[`load_results()`](https://peteowen1.github.io/torp/reference/load_results.md),
[`load_teams()`](https://peteowen1.github.io/torp/reference/load_teams.md),
[`load_player_stats()`](https://peteowen1.github.io/torp/reference/load_player_stats.md),
and
[`load_player_details()`](https://peteowen1.github.io/torp/reference/load_player_details.md).

## Usage

``` r
.load_with_cache(
  cache_prefix,
  seasons,
  fetch_fn,
  use_cache = TRUE,
  cache_ttl = 3600,
  verbose = FALSE,
  columns = NULL,
  fetch_all_fn = NULL,
  use_disk_cache = FALSE,
  refresh = FALSE
)
```

## Arguments

- cache_prefix:

  Character. Cache key prefix (e.g. "fixtures", "results").

- seasons:

  Numeric vector of seasons.

- fetch_fn:

  Function that takes a single season year and returns a
  data.frame/tibble.

- use_cache:

  Logical. Whether to use in-memory caching.

- cache_ttl:

  Numeric. Cache time-to-live in seconds.

- verbose:

  Logical. Print cache hit/miss info.

- columns:

  Optional character vector of column names to select.

- fetch_all_fn:

  Optional function that takes a vector of seasons and returns all data
  in one batch. When provided and `length(seasons) > 1`, used instead of
  per-season `lapply(seasons, fetch_fn)` for efficiency (e.g. one big
  [`curl::multi_download()`](https://jeroen.r-universe.dev/curl/reference/multi_download.html)
  instead of N sequential batches).

## Value

A tibble.
