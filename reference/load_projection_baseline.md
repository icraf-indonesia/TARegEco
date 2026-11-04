# Load projection baseline data from Excel or RDS

Loads descriptive analysis results required to run the projection
module. The input can be either an Excel workbook
(\`IO_Analysis_Output.xlsx\`) or an RDS file produced by the descriptive
analysis module.

## Usage

``` r
load_projection_baseline(path, projection_area_file_bau)
```

## Arguments

- path:

  Character. Path to the descriptive analysis file. Supported extensions
  are \`.xlsx\`, \`.xls\`, and \`.rds\`.

- projection_area_file_bau:

  Character. Path to the BAU land projection file (Excel/CSV) containing
  columns \`ID\`, \`LC\`, and one column per projection year.

## Value

A list containing:

- gdp_baseline:

  Tibble of baseline GDP and related coefficients

- leontief_matrix:

  Leontief inverse matrix

- full_io_table:

  Complete IO matrix

- code_registry:

  Code registry tibble

- sector_idx:

  Index of sector rows in the IO table

- income_multiplier:

  Income multiplier table

- labour_multiplier:

  Labour multiplier table

- SUB_multiplier:

  Gross operating surplus multiplier table

- output_multiplier:

  Output multiplier table

- linkage_table:

  Linkage table

- land_prop:

  Land proportion matrix

- P_default:

  Default land proportion matrix

- growth_rate:

  Baseline growth rate data

- area_matrix_bau:

  BAU area matrix

- projection_area_bau:

  Raw BAU land projection table

## Examples

``` r
if (FALSE) { # \dontrun{
baseline <- load_projection_baseline(
  path = "output/descriptive_analysis_module.rds",
  projection_area_file_bau = "data/luc_projection_bau.xlsx"
)
} # }
```
