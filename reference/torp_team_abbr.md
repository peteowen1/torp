# Get AFL Team Abbreviation

Converts any team name variant to its canonical AFL API abbreviation.

## Usage

``` r
torp_team_abbr(team)
```

## Arguments

- team:

  Character vector of team names (any recognised variant)

## Value

Character vector of abbreviations (e.g. "ADEL", "BL", "WB")

## Examples

``` r
torp_team_abbr("Adelaide Crows")
#> [1] "ADEL"
torp_team_abbr(c("Narrm", "Western Bulldogs", "CARL"))
#> [1] "MELB" "WB"   "CARL"
```
