# Create wide comparison table

Converts long projection data into a wide table where each scenario
becomes a column. This format is suitable for display in reports and
dashboards.

## Usage

``` r
create_comparison_table(data, value_col)
```

## Arguments

- data:

  A data frame with at least the columns \`YEAR\`, \`SCENARIO\`, and the
  column specified in \`value_col\`.

- value_col:

  Character. Name of the indicator column to be spread across scenarios
  (e.g., \`"GDP"\`, \`"LABOUR"\`, \`"INCOME"\`, \`"SURPLUS"\`,
  \`"ITFL"\`, \`"ITBL"\`).

## Value

A tibble with columns:

- YEAR:

  Projection year

- ...:

  One column for each scenario, containing the selected value

## Examples

``` r
if (FALSE) { # \dontrun{
gdp_wide <- create_comparison_table(gdp_comparison, "GDP")
} # }
```
