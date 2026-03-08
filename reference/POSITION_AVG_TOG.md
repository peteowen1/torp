# Average time-on-ground fraction by field position (position.x from load_teams()) Computed from historical data (2021-2025). Used to estimate per-player TOG when lineups are announced but games haven't started. EMERG/SUB are currently filtered upstream but kept here for future use. Unknown positions fall back to 0.75 with a warning. Run data-raw/debug/compute_position_tog.R to regenerate from current data.

Average time-on-ground fraction by field position (position.x from
load_teams()) Computed from historical data (2021-2025). Used to
estimate per-player TOG when lineups are announced but games haven't
started. EMERG/SUB are currently filtered upstream but kept here for
future use. Unknown positions fall back to 0.75 with a warning. Run
data-raw/debug/compute_position_tog.R to regenerate from current data.

## Usage

``` r
POSITION_AVG_TOG
```

## Format

An object of class `numeric` of length 21.
