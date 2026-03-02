# Get filenames available in a GitHub release

Queries the GitHub API via piggyback for the list of assets attached to
a release tag. Results are cached per tag for the session so repeated
calls (e.g. across multiple `load_*()` calls) don't hit the network.

## Usage

``` r
get_release_assets(release_tag)
```

## Arguments

- release_tag:

  Character. The release tag name (e.g. "predictions-data").

## Value

Character vector of filenames, or NULL if the query fails.
