# Export Input-Output Descriptive Analysis Results to Excel

Generates a multi-tabbed Excel workbook (\`.xlsx\`) containing both the
baseline analysis output (descriptive statistics, multipliers, linkages,
and balance details) and protected raw input tables for data integrity
tracking.

## Usage

``` r
export_io_analysis(
  sector,
  profile,
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
  output_path = "data/Hasil_Analisis_Modul_Deskriptif.xlsx"
)
```

## Arguments

- sector:

  Data frame or NULL. Economic sector definitions/categories.

- profile:

  List or NULL. Metadata containing location, base year, currency, and
  IO source.

- gdp_baseline:

  Data frame. Baseline GDP and sectoral economic indicators.

- linkage_result:

  List. Contains Leontief inverse matrix and linkage indices.

- growth_rate:

  Numeric vector or data frame. Baseline Final Demand growth rates.

- labour:

  Numeric vector, list, or data frame. Sectoral labor data.

- OM:

  List. Output Multiplier calculation results and table.

- IM:

  List. Income Multiplier calculation results and table.

- LM:

  List. Labor Multiplier calculation results and table.

- SUBM:

  List. Gross Operating Surplus (SUB) Multiplier results and details.

- land_proportion:

  Matrix or data frame. Baseline land proportion matrix (P).

- landuse_table:

  Data frame or NULL. Historical/baseline land cover matrix.

- linkage_table:

  Data frame. Summarized Backward and Forward Linkage table.

- full_io_table:

  Matrix or data frame. Complete baseline Input-Output table.

- code_registry:

  Data frame. Registry of sector codes and classifications.

- balance_detail:

  Data frame or NULL. Detailed sector-by-sector IO balance checks.

- output_path:

  Character. The destination file path for the exported Excel file.
  Defaults to \`"data/Hasil_Analisis_Modul_Deskriptif.xlsx"\`.

## Value

Invisibly returns the \`output_path\` string upon successful file
creation.

## Details

The exported workbook uses distinct tab colors for clarity:

- **Blue Tab**: Readme / File Guide.

- **Green Tabs**: Editable output sheets (Descriptive analysis,
  multipliers, linkages).

- **Red Tabs**: Read-only protected input sheets (Raw IO table, land
  use, parameters).
