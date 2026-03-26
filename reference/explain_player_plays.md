# Show a Player's Biggest Plays from PBP

Pulls play-by-play rows for a player and computes the EPV credit each
play generated (using the same formula as
[`create_player_game_data()`](https://peteowen1.github.io/torp/reference/create_player_game_data.md)).
Shows the surrounding context (1 row before, 1 after) for each top play
so you can see how the passage unfolded.

## Usage

``` r
explain_player_plays(
  player,
  season = get_afl_season(),
  match_id = NULL,
  top_n = 5,
  context = 1,
  pbp_data = NULL
)
```

## Arguments

- player:

  Character player name (partial match OK) or player_id string.

- season:

  Season year(s) to search. Default is current season.

- match_id:

  Optional match ID to filter to a single game.

- top_n:

  Number of top plays to show per role. Default 5.

- context:

  Number of rows before/after each play to show. Default 1.

- pbp_data:

  Optional pre-loaded PBP data. If NULL, loads automatically.

## Value

Invisibly returns a list with `as_receiver` and `as_disposer`
data.tables of the biggest plays.
