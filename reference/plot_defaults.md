# Default visual constants for torp plots

Returns a named list of recurring plot constants (line weights, point
sizes, reference colours) used across the `plot_*.R` files. Centralising
these here means changing torp's visual language is a one-file edit
instead of a grep-and-replace across every plot function.

## Usage

``` r
plot_defaults()
```

## Value

A named list with the following slots:

- `gridline_lwd`:

  Subtle reference line weight (e.g. zero hline)

- `primary_lwd`:

  Standard data line weight (e.g. main trace)

- `emphasis_lwd`:

  Heavier weight for axis-zero / important refs

- `heavy_lwd`:

  Thickest weight for callout lines

- `point_sm`:

  Small point/jitter size

- `point_md`:

  Default geom_point size

- `point_lg`:

  Large point size for shot/highlighted markers

- `grey_dark`:

  Primary text/line grey (`grey20`)

- `grey_mid`:

  De-emphasised text grey (`grey30`)

- `grey_axis`:

  Axis line grey (`grey40`)

- `grey_ref`:

  Soft reference grey (`grey50`)

## Details

Use as `td <- plot_defaults(); td$gridline_lwd` or
`with(plot_defaults(), { ... })`. Existing plot functions still hardcode
these values inline; new plots and incremental migrations should
reference this helper instead.
