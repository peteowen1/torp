# Standardise AFL Team Names

Maps team name variants (abbreviations, nicknames, Indigenous round
names) to canonical team names using
[AFL_TEAM_ALIASES](https://peteowen1.github.io/torp/reference/AFL_TEAM_ALIASES.md).
Drop-in replacement for
[`fitzRoy::replace_teams()`](https://jimmyday12.github.io/fitzRoy/reference/replace_teams.html).

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
#> [1] "Adelaide"
torp_replace_teams(c("GWS Giants", "Narrm", "WB"))
#> [1] "GWS"              "Melbourne"        "Western Bulldogs"
```
