# Normalize Player Names

Internal function. Converts input character strings into a standardized
format by:

- Converting accented characters to ASCII (e.g., "José" → "Jose")

- Lowercasing all text

- Removing non-alphabetic characters (keeping spaces)

- Collapsing multiple spaces into a single space

Useful for preparing player names (or similar text fields) before
joining tables where names may not match exactly due to case,
punctuation, or diacritics.

## Usage

``` r
norm_name(x)
```

## Arguments

- x:

  A character vector of names (or other strings).

## Value

A character vector with normalized names.

## See also

[`stringi::stri_trans_general()`](https://rdrr.io/pkg/stringi/man/stri_trans_general.html),
[`stringr::str_to_lower()`](https://stringr.tidyverse.org/reference/case.html)

## Examples

``` r
if (FALSE) { # \dontrun{
norm_name(c("Cam Zurhaar", "Cameron   Zurhaar", "  Jose Lopez "))
} # }
```
