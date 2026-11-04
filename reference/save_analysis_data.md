# Save final descriptive analysis data to RDS or RData

Saves a single combined object containing all inputs and results of the
descriptive Input-Output analysis. This file is intended to be loaded by
the projection module or other downstream modules.

## Usage

``` r
save_analysis_data(
  sector,
  intermediate_consumption_m,
  final_demand_m,
  added_value_m,
  labour,
  land_proportion,
  growth_rate,
  landuse_table,
  gdp_baseline,
  linkage_result,
  OM,
  IM,
  LM,
  SUBM,
  full_io_table,
  code_registry,
  output_dir = "inst/extdata/output",
  combined_file = "descriptive_analysis_module.rds",
  format = c("rds", "rdata")
)
```

## Arguments

- sector:

  Tibble of sectors.

- intermediate_consumption_m:

  Numeric matrix of intermediate consumption.

- final_demand_m:

  Numeric matrix of final demand.

- added_value_m:

  Numeric matrix of added value.

- labour:

  Labour vector or one-column matrix/tibble.

- land_proportion:

  Numeric matrix of land proportions.

- growth_rate:

  Tibble or data frame of growth rates.

- landuse_table:

  Tibble with columns \`id\`, \`lc\`, \`area\`.

- gdp_baseline:

  Tibble returned by \[calculate_gdp()\].

- linkage_result:

  List returned by \[compute_linkage()\].

- OM:

  Output from \[calc_output_multiplier()\].

- IM:

  Output from \[calc_income_multiplier()\].

- LM:

  Output from \[calc_labour_multiplier()\].

- SUBM:

  Output from \[calc_sub_multiplier()\].

- full_io_table:

  Numeric matrix from \[build_full_io_table()\].

- code_registry:

  Tibble from \[build_full_io_table()\].

- output_dir:

  Character. Directory to save the RDS file. Default is
  \`"inst/extdata/output"\`.

- combined_file:

  Character. Filename for the combined object. Default is
  \`"descriptive_analysis_module.rds"\`.

- format:

  Character. Either \`"rds"\` (default) or \`"rdata"\`.

## Value

Invisibly returns the output directory.

## Examples

``` r
if (FALSE) { # \dontrun{
save_analysis_data(
  sector                   = sector,
  intermediate_consumption_m = intermediate_consumption.m,
  final_demand_m           = final_demand.m,
  added_value_m            = added_value.m,
  labour                   = labour,
  land_proportion          = land_proportion,
  growth_rate              = growth_rate,
  landuse_table            = landuse_table,
  gdp_baseline             = gdp_baseline,
  linkage_result           = linkage_result,
  OM = OM, IM = IM, LM = LM, SUBM = SUBM,
  full_io_table            = full_io_table,
  code_registry            = code_registry,
  output_dir               = "inst/extdata/output"
)
} # }
```
