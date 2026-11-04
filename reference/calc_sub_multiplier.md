# Surplus Usaha Bruto (SUB) multiplier

Computes the gross operating surplus multiplier using sectoral output as
the denominator, consistent with standard Input-Output multiplier
formulas.

## Usage

``` r
calc_sub_multiplier(
  Leontief,
  total_output,
  added_value_m,
  intermediate_consumption_m,
  sector,
  top_n = 20
)
```

## Arguments

- Leontief:

  Numeric Leontief inverse matrix.

- total_output:

  Numeric vector of total output per sector.

- added_value_m:

  Numeric matrix of added value.

- intermediate_consumption_m:

  Numeric matrix of intermediate consumption.

- sector:

  A data frame or tibble with at least two columns: sector name and
  category.

- top_n:

  Integer. Number of top sectors to include in the plot.

## Value

A list with elements: \`coefficient\`, \`total_output\`, \`surplus\`,
\`table\`, \`top\`, \`plot\`, \`compensation\`.

## Details

The surplus coefficient is calculated as: \$\$sub\\coeff_i =
\frac{S_i}{X_i}\$\$ where \\S_i\\ is gross operating surplus and \\X_i\\
is total output.

## References

Miller, R.E. & Blair, P.D. (2009). \*Input-Output Analysis: Foundations
and Extensions\*, Chapter 6.
