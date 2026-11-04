# Generate top-N multiplier bar chart

Horizontal bar chart of the N sectors with the highest value of one
multiplier, with the value printed at the end of each bar.

## Usage

``` r
generate_multiplier_bar_chart(
  multiplier_table,
  column,
  title,
  x_label = "Multiplier",
  top_n = 15,
  bar_colour = "#FFA61A",
  digits = 4
)
```

## Arguments

- multiplier_table:

  A tibble produced by \[combine_multipliers()\].

- column:

  Character. Column to plot, e.g. \`"Out.multiplier"\`.

- title:

  Character. Plot title.

- x_label:

  Character. Axis label for the multiplier value.

- top_n:

  Integer. Number of sectors to show (default 15).

- bar_colour:

  Character. Fill colour.

- digits:

  Integer. Rounding for the printed value.

## Value

A \`ggplot\` object.
