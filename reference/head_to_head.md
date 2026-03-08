# Head-to-Head Contest History Between Two Players

Retrieves the contest history between two specific players across their
career matchups. Uses fuzzy name matching via
[`resolve_player()`](https://peteowen1.github.io/torp/reference/resolve_player.md).

## Usage

``` r
head_to_head(
  player1,
  player2,
  type = "all",
  seasons = TRUE,
  rounds = TRUE,
  chains = NULL
)
```

## Arguments

- player1:

  Character string of the first player's name (partial match OK).

- player2:

  Character string of the second player's name (partial match OK).

- type:

  Contest type: `"aerial"`, `"ground_ball"`, or `"all"` (default).

- seasons:

  Seasons to search. Numeric vector or `TRUE` for all.

- rounds:

  Rounds to include. Numeric vector or `TRUE` for all.

- chains:

  Optional pre-loaded chains data to avoid re-downloading.

## Value

An S3 object of class `torp_head_to_head` with elements:

- player1:

  List with `player_id`, `player_name`, `team`.

- player2:

  List with `player_id`, `player_name`, `team`.

- contests:

  data.table of all matchups between the two players.

- summary:

  data.table with win/loss record per contest type.

## Examples

``` r
if (FALSE) { # \dontrun{
try({
  h2h <- head_to_head("Harris Andrews", "Charlie Curnow", type = "aerial")
  print(h2h)
})
} # }
```
