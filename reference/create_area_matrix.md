# Create area matrix from long projection data

Converts a long data frame of land cover areas into a matrix where rows
are land cover IDs and columns are projection years.

## Usage

``` r
create_area_matrix(projection_long)
```

## Arguments

- projection_long:

  A data frame with columns \`ID\`, \`YEAR\`, and \`AREA\`. Usually
  created using \`tidyr::pivot_longer()\`.

## Value

A numeric matrix with:

- rows:

  Land cover IDs

- columns:

  Projection years (as character)

## Examples

``` r
if (FALSE) { # \dontrun{
area_matrix <- create_area_matrix(projection_long)
} # }
```
