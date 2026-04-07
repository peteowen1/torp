# Compute Contest Credit from Aerial Contests

Joins aerial contest data back to PBP to get the kicker's `delta_epv`,
then splits credit three ways: kicker, target, and defender. When an
opponent is involved (spoil, intercept mark), each gets 1/3 of the EPV
at stake. The target and defender receive credit from their own team's
perspective (positive if they won, negative if they lost).

## Usage

``` r
compute_contest_credit(chains, pbp_data, contest_share = 1/3)
```

## Arguments

- chains:

  Raw chains data (from
  [`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md)).

- pbp_data:

  Clean PBP data (from
  [`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md))
  containing `delta_epv` values.

- contest_share:

  Fraction of `delta_epv` attributed to each contest participant.
  Default `1/3`.

## Value

A data.table with columns: `player_id`, `match_id`, `contest_epv`
(positive for winners, negative for losers), `aerial_target_wins`,
`aerial_target_losses`, `aerial_def_wins`, `aerial_def_losses`.

## Details

Only applies to contests with a 3rd player from the opposing team. When
the target takes the mark themselves (no opponent), the standard 50/50
kicker/receiver split is unchanged.
