# Standardise AFL Team Names

Maps team name variants (abbreviations, nicknames, Indigenous round
names) to canonical team names using
[AFL_TEAM_ALIASES](https://peteowen1.github.io/torp/reference/AFL_TEAM_ALIASES.md).
Drop-in replacement for external team name standardisation packages.

## Usage

``` r
torp_replace_teams(team)
```

## Arguments

- team:

  Character vector of team names

## Value

Character vector with standardised names. Unknown values pass through
unchanged.

## Examples

``` r
torp_replace_teams("Adelaide Crows")
#> [1] "Adelaide Crows"
torp_replace_teams(c("GWS Giants", "Narrm", "WB"))
#> [1] "GWS Giants"       "Melbourne Demons" "Western Bulldogs"
```
