# Get Local File Path for a URL

Maps a GitHub release URL to a local file path in torpdata/data/.

## Usage

``` r
get_local_path(url)
```

## Arguments

- url:

  Character URL of the remote parquet file

## Value

Character path to the local file, or NULL if local dir not configured
