# Recalculate Leontief inverse after IO changes

Recalculates the Leontief inverse matrix using the current full IO table
and sector output.

## Usage

``` r
recalculate_leontief(full_io_table, sector_idx, output_total)
```

## Arguments

- full_io_table:

  Numeric matrix. Complete IO table.

- sector_idx:

  Integer vector. Row/column indices of sectors.

- output_total:

  Numeric vector. Total output per sector.

## Value

Numeric matrix. New Leontief inverse.

## Examples

``` r
if (FALSE) { # \dontrun{
L <- recalculate_leontief(full_io_table, sector_idx, output_baseline)
} # }
```
