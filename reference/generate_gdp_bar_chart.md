# Generate GDP bar chart

Creates a horizontal bar chart for the top sectors by GDP.

## Usage

``` r
generate_gdp_bar_chart(gdp_sector, sector_info, top_n = 10)
```

## Arguments

- gdp_sector:

  A tibble produced by \[calculate_gdp()\].

- sector_info:

  A tibble with sector names and categories.

- top_n:

  Integer. Number of top sectors to display.

## Value

A list with elements \`plot\` (ggplot) and \`data\` (tibble).
