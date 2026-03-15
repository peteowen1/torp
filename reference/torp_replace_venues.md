# Standardise AFL Venue Names

Maps venue name variants (sponsor names, old names) to canonical stable
names. Standardises AFL venue names to canonical forms.

## Usage

``` r
torp_replace_venues(venue)
```

## Arguments

- venue:

  Character vector of venue names

## Value

Character vector with standardised names

## Examples

``` r
torp_replace_venues("Marvel Stadium")
#> [1] "Docklands"
torp_replace_venues(c("MCG", "Optus Stadium"))
#> [1] "M.C.G."        "Perth Stadium"
```
