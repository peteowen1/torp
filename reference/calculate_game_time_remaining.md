# Calculate game time remaining in an AFL match (playing seconds)

Uses estimated playing time (excluding stoppages) rather than raw clock
time.

## Usage

``` r
calculate_game_time_remaining(period, game_time_elapsed)
```

## Arguments

- period:

  Numeric vector of period numbers (1-4)

- game_time_elapsed:

  Numeric vector of estimated playing seconds elapsed in the current
  quarter

## Value

Numeric vector of estimated playing seconds remaining in the match
