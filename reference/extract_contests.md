# Extract Head-to-Head Contests from Chains Data

Identifies contest pairs from raw chains data by examining adjacent rows
at the same field coordinates. The raw chains data contains contest
descriptions (Spoil, Contest Target, Tackle, etc.) that are stripped
during PBP cleaning.

## Usage

``` r
extract_contests(chains = NULL, type = "all", seasons = TRUE, rounds = TRUE)
```

## Arguments

- chains:

  A data.frame or data.table of chains data (from
  [`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md)).
  If NULL, loads chains data using `seasons` and `rounds`.

- type:

  Contest type to extract: `"aerial"`, `"ground_ball"`, or `"all"`
  (default).

- seasons:

  Seasons to load if `chains` is NULL. Passed to
  [`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md).

- rounds:

  Rounds to load if `chains` is NULL. Passed to
  [`load_chains()`](https://peteowen1.github.io/torp/reference/load_chains.md).

## Value

A data.table with one row per contest, containing columns: `match_id`,
`season`, `round_number`, `player1_id`, `team1_id`, `player1_desc`,
`player2_id`, `team2_id`, `player2_desc`, `contest_type` (`"aerial"` or
`"ground_ball"`), `outcome` (`"mark"`, `"intercept_mark"`, `"spoil"`, or
`NA` for ground ball), `winner` (`"player1"` or `"player2"`), `x`, `y`,
`period`, `period_seconds`.

## Details

The primary contest signal is **matching x,y coordinates** on
consecutive rows — both players were at the same spot contesting.

Aerial contest outcomes (Contest Target / Kick Inside 50 Result at same
x,y):

- mark:

  A player from the same team as the Contest Target marks it. The
  kicking team won the aerial contest (winner = player1).

- intercept_mark:

  A player from the opposing team marks it (Contested Mark, Uncontested
  Mark, Mark On Lead). The defending team won (winner = player2).

- spoil:

  A player from the opposing team spoils it. The defending team won
  (winner = player2).

Ground ball contests are consecutive ground ball actions (Hard Ball Get,
Loose Ball Get, crumbs) at the same x,y from opposing teams.

## Examples

``` r
if (FALSE) { # \dontrun{
try({
  contests <- extract_contests(seasons = 2024, rounds = 1:5, type = "aerial")
  head(contests)
})
} # }
```
