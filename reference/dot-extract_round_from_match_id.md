# Extract round number from an AFL match ID

Parses `CD_M{year:4}{comp:3}{round:2}{game:2+}` format via regex.
Assumes 4-digit year + 3-digit comp ID (matches all known AFL formats).

## Usage

``` r
.extract_round_from_match_id(match_id)
```

## Arguments

- match_id:

  Character match ID.

## Value

Integer round number, or NA if format does not match.
