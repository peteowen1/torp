# Simulate individual scoring events within a quarter

Decomposes a quarter score into goals and behinds, then generates
time-ordered scoring events. Uses the constraint: 6\*goals + behinds =
total.

## Usage

``` r
.simulate_quarter_events(qtr_score, conv_rate, quarter, sim_id, team_label)
```

## Arguments

- qtr_score:

  Integer. Total score for this team in this quarter.

- conv_rate:

  Numeric. Goal conversion rate (goals / total shots).

- quarter:

  Integer. Quarter number (1-4).

- sim_id:

  Integer. Simulation ID.

- team_label:

  Character. "home" or "away".

## Value

data.table of scoring events, or NULL if qtr_score == 0.
