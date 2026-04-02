# Maximum distance (metres) after sign-flipping for a row to be considered a sign-flip error. If negating a row's coordinates puts it within this distance of the predecessor, the coordinates are likely in the wrong frame. Set to 70m to cover the longest realistic kick distances (~65m displacement). Safe because the 100m jump threshold already filters out all legitimate plays.

Maximum distance (metres) after sign-flipping for a row to be considered
a sign-flip error. If negating a row's coordinates puts it within this
distance of the predecessor, the coordinates are likely in the wrong
frame. Set to 70m to cover the longest realistic kick distances (~65m
displacement). Safe because the 100m jump threshold already filters out
all legitimate plays.

## Usage

``` r
COORD_FLIP_TOLERANCE
```

## Format

An object of class `numeric` of length 1.
