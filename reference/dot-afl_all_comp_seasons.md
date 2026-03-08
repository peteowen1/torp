# Fetch all AFL comp seasons (cached)

Uses the public AFL API to look up competition season IDs. The AFL
Premiership competition code is "AFL" (or "CD_AFLPrem").

## Usage

``` r
.afl_all_comp_seasons()
```

## Value

A data.frame with id and year columns, or NULL on failure
