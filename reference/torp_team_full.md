# Get AFL Team Full Name

Converts any team name variant to its canonical full name.

## Usage

``` r
torp_team_full(team)
```

## Arguments

- team:

  Character vector of team names (any recognised variant)

## Value

Character vector of full names (e.g. "Adelaide Crows", "GWS Giants")

## Examples

``` r
torp_team_full("Adelaide")
#> [1] "Adelaide Crows"
torp_team_full(c("WB", "Narrm", "Cats"))
#> [1] "Western Bulldogs" "Melbourne Demons" "Geelong Cats"    
```
