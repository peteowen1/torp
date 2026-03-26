# TORP Movers: Biggest Round-Over-Round Rating Changes

Compares TORP ratings between two rounds and shows which players moved
the most. Uses pre-computed ratings from torpdata releases for speed.

## Usage

``` r
torp_movers(
  season_val = get_afl_season(),
  round_val = get_afl_week(),
  prev_round = round_val - 1L,
  top_n = 10,
  metric = "torp"
)
```

## Arguments

- season_val:

  Season year. Default is current season.

- round_val:

  The "current" round. Default is the latest round.

- prev_round:

  The "previous" round to compare against. Default is `round_val - 1`.

- top_n:

  Number of biggest movers to show in each direction. Default 10.

- metric:

  Column to compare. Default `"torp"`. Can also be `"epr"`, `"psr"`,
  `"recv_epr"`, etc.

## Value

A data.table with columns: `player_name`, `team`, `prev` (previous round
value), `curr` (current round value), `change`, `direction` ("up" or
"down").
