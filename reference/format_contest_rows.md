# Format contest rows into standard output schema

Columns are already normalized to snake_case by extract_contests().

## Usage

``` r
format_contest_rows(rows, contest_type, outcome, winner)
```

## Arguments

- rows:

  data.table of matched contest rows (snake_case columns)

- contest_type:

  Character label for the contest type

- outcome:

  Character label for the contest outcome

- winner:

  Which player won ("player1" or "player2")

## Value

data.table in standard contest schema
