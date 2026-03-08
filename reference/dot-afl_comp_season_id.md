# Resolve AFL API compSeasonId for a season

The public AFL API (`aflapi.afl.com.au`) uses numeric comp season IDs,
not the `CD_S{year}014` format used by the CFS endpoint. This function
resolves the numeric ID via a cached HTTP lookup (at most once per
session).

## Usage

``` r
.afl_comp_season_id(season)
```

## Arguments

- season:

  Numeric year

## Value

Numeric compSeasonId, or NULL if not found
