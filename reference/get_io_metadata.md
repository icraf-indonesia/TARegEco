# Extract metadata from Input-Output components

For each component (sector, intermediate_consumption, final_demand,
added_value, labour, land_proportion, landuse_table, growth_rate), this
function returns a tidy tibble with descriptive metadata: - class,
dimensions, missing values, min/max, names, hardcoded descriptions, and
user‑provided units.

## Usage

``` r
get_io_metadata(
  sector,
  intermediate_consumption,
  final_demand,
  added_value,
  labour,
  land_proportion,
  landuse_table,
  growth_rate,
  sector_units = NA_character_,
  ic_units = NA_character_,
  fd_units = NA_character_,
  av_units = NA_character_,
  labour_units = NA_character_,
  lp_units = NA_character_,
  lu_units = NA_character_,
  gr_units = NA_character_
)
```

## Arguments

- sector:

  A data frame/tibble with sector names in first column.

- intermediate_consumption:

  Numeric matrix.

- final_demand:

  Numeric matrix.

- added_value:

  Numeric matrix.

- labour:

  Vector, 1-column matrix, or 1-column data frame.

- land_proportion:

  Numeric matrix.

- landuse_table:

  Data frame/tibble with "LC" and "area".

- growth_rate:

  Numeric vector.

- sector_units:

  Character string for sector units (default NA).

- ic_units:

  Character string for intermediate consumption units (default NA).

- fd_units:

  Character string for final demand units (default NA).

- av_units:

  Character string for added value units (default NA).

- labour_units:

  Character string for labour units (default NA).

- lp_units:

  Character string for land proportion units (default NA).

- lu_units:

  Character string for landuse table units (default NA).

- gr_units:

  Character string for growth rate units (default NA).

## Value

A list with two elements:

- metadata:

  A tibble with one row per component and columns for key attributes
  including Description and Units.

- details:

  A list containing more detailed metadata for each component (e.g.,
  names, per‑column summaries).
