# Get data from cache (no TTL check)

Returns cached data regardless of age. Use for session-scoped lookups
(e.g. comp season IDs) where staleness is acceptable. For TTL-aware
retrieval, use
[`get_from_cache_ttl()`](https://peteowen1.github.io/torp/reference/get_from_cache_ttl.md)
or the full
[`.load_with_cache()`](https://peteowen1.github.io/torp/reference/dot-load_with_cache.md)
pipeline.

## Usage

``` r
get_from_cache(cache_key)
```

## Arguments

- cache_key:

  Character cache key

## Value

Cached data or NULL if not found
