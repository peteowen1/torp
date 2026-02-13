# Setup TORP Logging Configuration

This function is intended for internal use and may be unexported in a
future release. Configures logging for the TORP package with appropriate
levels and outputs

## Usage

``` r
setup_torp_logging(level = "INFO", log_file = NULL, console_output = FALSE)
```

## Arguments

- level:

  Logging level ("DEBUG", "INFO", "WARN", "ERROR")

- log_file:

  Path to log file (optional)

- console_output:

  Logical, whether to output to console
