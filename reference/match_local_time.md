# Convert UTC match time to local venue time

Parses a UTC start time string and converts it to local time based on
the venue timezone from fixture data.

## Usage

``` r
match_local_time(utc_start_time, venue_timezone = NULL)
```

## Arguments

- utc_start_time:

  Character string in ISO 8601 format (e.g.
  `"2026-03-05T08:30:00.000+0000"`).

- venue_timezone:

  Olson timezone string (e.g. `"Australia/Sydney"`). If NULL or NA,
  defaults to `"Australia/Melbourne"`.

## Value

A formatted local time string (e.g. `"2026-03-05 19:30 AEDT"`), or NA if
the input cannot be parsed.

## Examples

``` r
if (FALSE) { # \dontrun{
match_local_time("2026-03-05T08:30:00.000+0000", "Australia/Sydney")
match_local_time("2026-03-05T08:30:00.000+0000", "Australia/Perth")
} # }
```
