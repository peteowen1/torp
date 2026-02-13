# Clear Fixture Cache

Clears the cached fixture data from memory.

## Usage

``` r
clear_fixture_cache(verbose = FALSE)
```

## Arguments

- verbose:

  Logical. If TRUE, prints cache clearing information.

## Value

Invisible NULL

## Examples

``` r
# \donttest{
# Clear all cached fixtures
clear_fixture_cache()

# Clear with verbose output
clear_fixture_cache(verbose = TRUE)
#> No fixture cache entries to clear
# }
```
