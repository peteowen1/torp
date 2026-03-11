# Batch-fetch JSON from CFS API endpoints

Downloads multiple CFS API JSON responses in parallel using in-memory
[`curl::curl_fetch_multi()`](https://jeroen.r-universe.dev/curl/reference/curl_fetch.html),
then parses each with a supplied parser. No disk I/O — callers handle
caching at the season level.

## Usage

``` r
.fetch_cfs_batch(ids, url_template, token, parse_fn, label = "data")
```

## Arguments

- ids:

  Character vector of endpoint suffixes (e.g. match IDs).

- url_template:

  Character. [`sprintf()`](https://rdrr.io/r/base/sprintf.html)-style
  template with one `%s` placeholder.

- token:

  Character. CFS API auth token.

- parse_fn:

  Function taking `(parsed_json, id)` and returning a data.frame.

- label:

  Character. Label for progress messages (e.g. "roster", "stats").

## Value

A tibble of all parsed results row-bound together.
