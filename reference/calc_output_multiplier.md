# Output multiplier

Output multiplier = column sums of Leontief inverse. Measures total
output generated in the economy per unit increase in final demand of a
sector.

## Usage

``` r
calc_output_multiplier(Leontief, sector, top_n = 20)
```

## Arguments

- Leontief:

  Numeric Leontief inverse matrix.

- sector:

  A data frame or tibble with at least two columns: sector name and
  category.

- top_n:

  Integer. Number of top sectors to include in the plot.

## Value

A list with elements: \`table\`, \`top\`, \`plot\`.
