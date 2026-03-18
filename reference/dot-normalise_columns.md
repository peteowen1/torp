# Normalise column names using a mapping

Renames columns in a data.table by reference using a named character
vector where names are variant (old) names and values are canonical
(new) names. Only renames when the variant exists and the canonical does
not.

## Usage

``` r
.normalise_columns(dt, col_map)
```

## Arguments

- dt:

  A data.frame, tibble, or data.table to modify (renames by reference
  via
  [`data.table::setnames`](https://rdrr.io/pkg/data.table/man/setattr.html)).

- col_map:

  Named character vector: variant_name -\> canonical_name.

## Value

Invisible NULL (modifies dt by reference).
