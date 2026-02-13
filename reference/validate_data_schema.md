# Validate Data Schema

This function is intended for internal use and may be unexported in a
future release. Validates that a dataframe conforms to the expected
schema

## Usage

``` r
validate_data_schema(data, schema_name, strict = TRUE)
```

## Arguments

- data:

  Dataframe to validate

- schema_name:

  Name of the schema to validate against

- strict:

  Logical, whether to fail on schema violations (default: TRUE)

## Value

List containing validation results
