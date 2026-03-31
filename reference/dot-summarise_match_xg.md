# Summarise shot-level PBP into match-level xG stats

Summarise shot-level PBP into match-level xG stats

## Usage

``` r
.summarise_match_xg(pbp, quarter = 1:4)
```

## Arguments

- pbp:

  Play-by-play data with xscore and shot_row columns

- quarter:

  Numeric vector of quarters to include

## Value

Tibble with one row per match
