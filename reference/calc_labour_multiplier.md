# Labour multiplier

Labour multiplier = \`colSums(diag(labour_coeff) Measures total
employment generated economy-wide per unit increase in final demand of a
sector.

## Usage

``` r
calc_labour_multiplier(Leontief, labour, total_output, sector, top_n = 20)
```

## Arguments

- Leontief:

  Numeric Leontief inverse matrix.

- labour:

  Numeric vector of labour per sector.

- total_output:

  Numeric vector of total output per sector.

- sector:

  A data frame or tibble with at least two columns: sector name and
  category.

- top_n:

  Integer. Number of top sectors to include in the plot.

## Value

A list with elements: \`labour\`, \`labour_coeff\`,
\`multiplier_matrix\`, \`table\`, \`top\`, \`plot\`.
