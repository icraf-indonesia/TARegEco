# Generate linkage bubble plot by sector category

Scatter plot of ITBL (Indeks Daya Penyebaran) against ITFL (Indeks
Derajat Kepekaan), with bubble size showing combined linkage strength
and colour showing sector category. Quadrant reference lines are drawn
at 1.

## Usage

``` r
generate_prs_plot(linkages_table, show_labels = TRUE, base_size = 12)
```

## Arguments

- linkages_table:

  A tibble produced by \[create_linkages_table()\].

- show_labels:

  Logical. If \`TRUE\`, sector IDs are displayed.

- base_size:

  Numeric. Base font size for ggplot.

## Value

A \`ggplot\` object.

## Details

Use \[generate_quadrant_plot()\] for the companion view coloured by BPS
quadrant instead of category.
