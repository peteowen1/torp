# Get data from cache with TTL enforcement

Returns cached data only if it exists and is younger than `cache_ttl`
seconds. Expired entries are removed from the cache.

## Usage

``` r
get_from_cache_ttl(cache_key, cache_ttl = 3600)
```

## Arguments

- cache_key:

  Character cache key

- cache_ttl:

  Maximum age in seconds (default 3600 = 1 hour)

## Value

Cached data or NULL if not found or expired
