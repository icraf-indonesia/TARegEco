# Validate Input-Output data with comprehensive exploded report

## Usage

``` r
validate_io_data(
  sector,
  intermediate_consumption,
  final_demand,
  added_value,
  labour,
  land_proportion,
  landuse_table = NULL,
  growth_rate = NULL,
  tolerance_io = 0.01,
  stop_on_fail = TRUE
)
```

## Arguments

- sector:

  A data frame or tibble with at least two columns: sector name and
  category.

- intermediate_consumption:

  A numeric matrix of intermediate consumption (Sector x Sector).

- final_demand:

  A numeric matrix of final demand (Sector x Final Demand component).

- added_value:

  A numeric matrix of added value (Component x Sector).

- labour:

  A vector, one-column matrix, or one-column data frame/tibble
  containing labour per sector.

- land_proportion:

  A numeric matrix of land proportion (Sector x Land Cover Class).

- landuse_table:

  Optional data frame or tibble with at least columns "LC" (land cover
  name) and "area" (hectares). Number of rows must equal the number of
  columns of \`land_proportion\`. If \`NULL\` (default), the land use
  checks are reported as \`SKIP\` and not run.

- growth_rate:

  Optional numeric vector of length \`n_sec\` containing sectoral growth
  rates (in decimal, e.g., 0.02 for 2 (default), the growth rate checks
  are reported as \`SKIP\` and not run.

- tolerance_io:

  Numeric; maximum allowed absolute relative difference for the IO
  identity. Default 0.01 (1

  stop_on_failLogical; if \`TRUE\` (default), stops on any \`FAIL\`.

A tibble with validation results (invisibly if all pass). Checks
dimensions, sector names, data types, missing values, non‑negativity
(for IC and labour only), land proportions, land use table consistency,
growth rates, and the fundamental IO identity (warning only). Prints a
detailed validation report; stops only on critical failures.
