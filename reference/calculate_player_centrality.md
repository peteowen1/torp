# Calculate Player Centrality

Builds a player interaction network from match data and computes
centrality centrality scores. Players who face diverse, high-quality
opponents get higher centrality. Players in isolated pools (e.g., few
games, always same opponents) get lower centrality.

## Usage

``` r
calculate_player_centrality(player_matches, min_matches = 5L, damping = 0.85)
```

## Arguments

- player_matches:

  Data frame with columns:

  - `player_id` or `playerId`: Player identifier

  - `team`: Player's team

  - `match_id` or `matchId`: Match identifier

  - `time_on_ground` (optional): Playing time as weight

- min_matches:

  Integer. Minimum matches for inclusion. Default 5.

- damping:

  Numeric. centrality damping factor (0-1). Default 0.85.

## Value

Data frame with player_id, centrality (0-1 normalized),
unique_opponents, matches_played, component_id, component_size

## Examples

``` r
if (FALSE) { # \dontrun{
# From player_stats data
ps <- load_player_stats(2024)
centrality <- calculate_player_centrality(ps)
} # }
```
