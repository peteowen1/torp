# Bulk-convert remaining non-snake_case column names

Applies a generic camelCase/dot.notation to snake_case conversion on any
columns that are not already snake_case. Run AFTER explicit column maps
to catch API columns not covered by manual mappings.

## Usage

``` r
.bulk_snake_case(dt, verbose = TRUE, label = NULL)
```

## Arguments

- dt:

  A data.frame, tibble, or data.table to modify.

- verbose:

  Logical. If TRUE, emits a message listing renames.

- label:

  Optional label for the log message.

## Value

Invisible NULL (modifies names by reference for data.table, or in place
for data.frame).
