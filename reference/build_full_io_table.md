# Build full Input-Output table and code registry

Combines Intermediate Consumption, Final Demand, and Added Value into a
single complete matrix, along with a Code Registry that maps each code
to label, type, and row/column position.

## Usage

``` r
build_full_io_table(
  sector,
  intermediate_consumption_m,
  final_demand_m,
  final_demand_component,
  added_value_m,
  added_value_lookup
)
```

## Arguments

- sector:

  A data frame or tibble with at least one column of sector names.

- intermediate_consumption_m:

  Numeric matrix of intermediate consumption.

- final_demand_m:

  Numeric matrix of final demand.

- final_demand_component:

  Data frame or tibble with two columns: code and label for final demand
  components.

- added_value_m:

  Numeric matrix of added value.

- added_value_lookup:

  Data frame or tibble with two columns: code and label for added value
  components.

## Value

A list with elements:

- full_io_table:

  Complete IO matrix (rownames/colnames = labels)

- code_registry:

  Tibble with code mapping (Code column kept as-is)

- n_sector:

  Number of sectors

## Details

The returned \`full_io_table\` uses \*\*labels\*\* (sector names, FD
names, VA names) as row/column names, so it is human-readable. The
\`code_registry\` retains the original numeric codes for internal
mapping.

## Examples

``` r
if (FALSE) { # \dontrun{
full <- build_full_io_table(sector, ic, fd, fd_comp, av, av_lookup)
} # }
```
