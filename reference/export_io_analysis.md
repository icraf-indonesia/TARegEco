# Export descriptive analysis outputs to Excel

Writes all main results of the descriptive Input-Output analysis into a
single Excel workbook. The workbook contains the following sheets:

## Usage

``` r
export_io_analysis(
  sector,
  gdp_baseline,
  linkage_result,
  growth_rate,
  labour,
  OM,
  IM,
  LM,
  SUBM,
  land_proportion,
  landuse_table,
  linkage_table,
  full_io_table,
  code_registry,
  balance_detail,
  output_path = "data/IO_Analysis_Output.xlsx"
)
```

## Arguments

- gdp_baseline:

  Tibble returned by \[calculate_gdp()\].

- linkage_result:

  List returned by \[compute_linkage()\].

- growth_rate:

  Tibble or data frame of sectoral growth rates.

- OM:

  Output from \[calc_output_multiplier()\].

- IM:

  Output from \[calc_income_multiplier()\].

- LM:

  Output from \[calc_labour_multiplier()\].

- SUBM:

  Output from \[calc_sub_multiplier()\].

- land_proportion:

  Numeric matrix of land proportions.

- linkage_table:

  Tibble of linkage results (from \[create_linkages_table()\]).

- full_io_table:

  Numeric matrix from \[build_full_io_table()\].

- code_registry:

  Tibble from \[build_full_io_table()\].

- balance_detail:

  Tibble from \[check_io_balance()\].

- output_path:

  Character. Path to the output Excel file. Default is
  \`"data/IO_Analysis_Output.xlsx"\`.

## Value

Invisibly returns the full path to the saved workbook.

## Details

\- \`GDP\`: GDP per sector table - \`Leontief_Matrix\`: Leontief inverse
matrix - \`Growth_Rate\`: sectoral growth rates - \`Multiplier_Output\`,
\`Multiplier_Income\`, \`Multiplier_Labour\`, \`Multiplier_SUB\`:
multiplier tables - \`Land_Proportion\`: land proportion matrix -
\`Linkage\`: linkage table (DBL, DFL, TBL, TFL, ITBL, ITFL) -
\`Full_IO_Table\`: complete IO matrix (sectors + final demand + added
value) - \`Code_Registry\`: code registry for modules that need
cell-level changes - \`Balance_Detail\`: per-sector balance check table

## Examples

``` r
if (FALSE) { # \dontrun{
export_io_analysis(
  gdp_baseline = gdp_baseline,
  linkage_result = linkage_result,
  growth_rate = growth_rate,
  OM = OM, IM = IM, LM = LM, SUBM = SUBM,
  land_proportion = land_proportion,
  linkage_table = linkages_table,
  full_io_table = full_io_table,
  code_registry = code_registry,
  balance_detail = balance_result$balance_detail,
  output_path = "data/IO_Analysis_Output.xlsx"
)
} # }
```
