# Get intensity for a sector and period

Returns the implementation intensity for a given sector in a specific
projection period. If no intensity table is provided, returns 1 (100

## Usage

``` r
get_intensity(intensity_table, sector, period_start, period_end)
```

## Arguments

- intensity_table:

  Data frame with columns \`Sector_Code\`, \`Start_Year\`, \`End_Year\`,
  and \`Intensity\`. Can be \`NULL\`.

- sector:

  Numeric. Sector code.

- period_start:

  Numeric. Start year of the period.

- period_end:

  Numeric. End year of the period.

## Value

Numeric. Intensity value (0–1).

## Examples

``` r
if (FALSE) { # \dontrun{
inten <- get_intensity(intensity_table, 1, 2024, 2029)
} # }
```
