# Plot comparison of scenarios

Creates a line plot comparing a chosen indicator across scenarios over
the projection years. If only one scenario is provided, a single line is
drawn.

## Usage

``` r
plot_comparison(data, value_col, title, y_label)
```

## Arguments

- data:

  A data frame with columns \`YEAR\`, \`SCENARIO\`, and the indicator
  column specified in \`value_col\`.

- value_col:

  Character. Name of the indicator column to plot.

- title:

  Character. Plot title.

- y_label:

  Character. Label for the y-axis.

## Value

A \`ggplot\` object.
