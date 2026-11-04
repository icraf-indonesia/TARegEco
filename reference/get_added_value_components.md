# Extract Added Value components from Input-Output table

Identifies and calculates added value components (import, compensation,
surplus, taxes) from the added value matrix. Also computes total input
and GDP per sector.

## Usage

``` r
get_added_value_components(intermediate_consumption_m, added_value_m)
```

## Arguments

- intermediate_consumption_m:

  Numeric matrix of intermediate consumption (Sector x Sector).

- added_value_m:

  Numeric matrix of added value (Component x Sector).

## Value

A list containing:

- total_input:

  Total input per sector

- intermediate_input:

  Intermediate input per sector

- import_input:

  Import input per sector

- compensation:

  Compensation of employees per sector

- surplus:

  Gross operating surplus per sector

- other_tax:

  Other taxes less subsidies on production

- tax_product:

  Taxes less subsidies on products

- primary_input:

  Primary input (compensation + surplus + other taxes)

- gdp_val:

  GDP per sector (same as primary input)

and row indices for each component.

## Details

GDP = Compensation of employees + Gross operating surplus + Other taxes
less subsidies on production. Total input = intermediate input +
import + taxes on products + primary input.

## Examples

``` r
if (FALSE) { # \dontrun{
comp <- get_added_value_components(ic, av)
} # }
```
