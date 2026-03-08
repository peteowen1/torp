# Create Win Probability Credit Data

Splits Win Probability Added (WPA) between disposers and receivers for
each play, then aggregates per player per game. Also tracks each
player's highest impact play by absolute WPA.

## Usage

``` r
create_wp_credit(pbp_data = NULL, disp_share = WP_CREDIT_DISP_SHARE)
```

## Arguments

- pbp_data:

  Play-by-play data with WPA already computed (via
  [`add_wp_vars()`](https://peteowen1.github.io/torp/reference/add_wp_vars.md)).
  Must contain columns: `wpa`, `player_id`, `player_name`,
  `lead_player_id`, `pos_team`, `display_order`, `match_id`, `team`,
  `season`, `round_number`, `utc_start_time`. If NULL, loads and
  prepares data via
  [`load_pbp()`](https://peteowen1.github.io/torp/reference/load_pbp.md).

- disp_share:

  Fraction of WPA credited to the disposer (0-1). Default is
  `WP_CREDIT_DISP_SHARE` (0.5). The receiver gets `1 - disp_share`.

## Value

A data.table with one row per player per match, containing:

- player_id, player_name, match_id, team, season, round, utc_start_time:

  Identifiers

- wp_credit:

  Total WPA credit (disposal + reception)

- wp_disp_credit:

  WPA credit from disposals only

- wp_recv_credit:

  WPA credit from receptions only

- n_disposals:

  Number of disposals

- n_receptions:

  Number of receptions

- max_play_wpa:

  WPA of highest-impact play (by absolute value)

- max_play_display_order:

  display_order of that play (joinable to PBP)

- max_play_role:

  "disposer" or "receiver"
