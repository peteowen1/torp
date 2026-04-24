# Reduced injury noise SD when known injuries are excluded (points) When specific injured players are removed from team ratings, the remaining per-round noise can be smaller since a major source of variation is gone. Set to match `SIM_INJURY_SD` because scraped injury lists only capture officially-listed absences — form slumps, minor niggles, and game-day late-outs still contribute meaningful week-to-week jitter.

Reduced injury noise SD when known injuries are excluded (points) When
specific injured players are removed from team ratings, the remaining
per-round noise can be smaller since a major source of variation is
gone. Set to match `SIM_INJURY_SD` because scraped injury lists only
capture officially-listed absences — form slumps, minor niggles, and
game-day late-outs still contribute meaningful week-to-week jitter.

## Usage

``` r
SIM_INJURY_SD_KNOWN
```

## Format

An object of class `numeric` of length 1.
