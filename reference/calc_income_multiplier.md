# Income multiplier

Income multiplier = \`colSums(diag(income_coeff) Measures total labour
income generated economy-wide per unit increase in final demand of a
sector.

## Usage

``` r
calc_income_multiplier(Leontief, total_output, added_value, sector, top_n = 20)
```

## Arguments

- Leontief:

  Numeric Leontief inverse matrix.

- total_output:

  Numeric vector of total output per sector.

- added_value:

  Numeric matrix of added value.

- sector:

  A data frame or tibble with at least two columns: sector name and
  category.

- top_n:

  Integer. Number of top sectors to include in the plot.

## Value

A list with elements: \`income\`, \`income_coeff\`,
\`multiplier_matrix\`, \`table\`, \`top\`, \`plot\`.
