# Generate GDP donut chart

Creates an interactive donut chart showing the share of top sectors in
total GDP.

## Usage

``` r
generate_gdp_donut_chart(gdp_sector, sector_info, top_n = 10)
```

## Arguments

- gdp_sector:

  A tibble produced by \[calculate_gdp()\].

- sector_info:

  A tibble with sector names and categories.

- top_n:

  Integer. Number of top sectors to display.

## Value

A list with elements \`plot\` (plotly) and \`data\` (tibble).
