# Compute finals home advantage based on venue familiarity

AFL finals venue rules: the higher-seeded team hosts in their home
state. Victorian teams play at the MCG, where familiarity varies by
team. Interstate teams host at their home ground (standard home
advantage). The Grand Final is always at the MCG regardless of who is
playing.

## Usage

``` r
finals_home_advantage(home, away, fam_lookup, gf = FALSE)

gf_home_advantage(home, away, fam_lookup)
```

## Arguments

- home:

  Character. Home (higher-seeded) team name.

- away:

  Character. Away team name.

- fam_lookup:

  Named numeric vector of MCG familiarity per team, or NULL (falls back
  to standard home advantage).

- gf:

  Logical. TRUE for the Grand Final (always MCG).

## Value

Numeric points advantage for the home team.
