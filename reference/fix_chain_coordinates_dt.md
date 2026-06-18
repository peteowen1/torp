# Fix chain coordinate jumps (Pass 1.6)

Converts coordinates to pitch-relative (home-team) space, fixes
unreasonable jumps around throw-ins and ambiguous possession events,
then converts back to possession-team perspective.

## Usage

``` r
fix_chain_coordinates_dt(dt)
```

## Arguments

- dt:

  A data.table to modify

## Value

Invisible NULL (modifies dt by reference)

## Details

Root cause of the historical sign-flip cascade (issue \#92): coordinates
are recorded in the attacking frame of the chain's possessing team
(`matchChains[i].teamId`), not the acting player's team
(`stats[j].teamId`). Step A now orients to the pitch using
`coord_team_id` (the captured chain-level team), so opponent-actor rows
and the trailing event of the previous chain land in a consistent frame
and no longer present as ~90m jumps that the steps C-F neighbour cascade
would mis-flip. The C-F steps are retained as a defensive net for
residual API noise but should rarely fire on correctly-oriented data.

Backward compatibility: parquets scraped before `chain_team_id` was
captured fall back to `team_id_mdl` for orientation, reproducing the
prior (buggy) behaviour exactly so historical releases and the models
trained on them are unchanged.
`R/clean_features.R::add_shot_geometry_variables` still folds bad
`goal_x` to near-goal distance as a display band-aid for that legacy
data. Retraining EP/WP/shot models on freshly-corrected chains remains a
separate follow-up.
