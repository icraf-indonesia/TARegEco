# Run Projection Scenario (Supply-Constrained Mixed Input-Output Model)

Accepts any free combination of BAU scenario, Assumption 1, Assumption
2, and Assumption 3. All scenario parameters are optional. The function
will automatically enable the appropriate logic based on which
parameters are not NULL.

## Usage

``` r
run_projection_scenario(
  gdp_baseline,
  income_multiplier,
  labour_multiplier,
  SUB_multiplier,
  output_multiplier,
  linkage_table,
  leontief_matrix,
  full_io_table,
  sector_idx,
  P_baseline,
  area_matrix_bau,
  growth_rate,
  constrained = NULL,
  unconstrained = NULL,
  P_scenario = NULL,
  area_matrix_scenario = NULL,
  lpc_scenario = NULL,
  lpc_target_year = NULL,
  io_changes = NULL,
  growth_rate_scenario = NULL,
  pop_projection = NULL
)
```

## Arguments

- gdp_baseline:

  Data frame. Baseline GDP table.

- income_multiplier, labour_multiplier, SUB_multiplier,
  output_multiplier, linkage_table:

  Data frame. Baseline multiplier & linkage tables.

- leontief_matrix:

  Numeric matrix. Baseline Leontief inverse (unused).

- full_io_table:

  Numeric matrix. Complete Z matrix.

- sector_idx:

  Integer vector. Sector indices in the full IO table.

- P_baseline:

  Numeric matrix. Baseline land proportion \[n_sector x n_cover\].

- area_matrix_bau:

  Numeric matrix. BAU land area \[n_cover x n_year\].

- growth_rate:

  Numeric vector. Baseline final demand growth rate.

- constrained, unconstrained:

  Integer vector. Co/nc indicator flags.

- P_scenario:

  Numeric matrix or NULL. Scenario land proportion (Assumption 1).

- area_matrix_scenario:

  Numeric matrix or NULL. Scenario land area (Assumption 1).

- lpc_scenario:

  Data frame or NULL. New LPC targets (Assumption 2).

- lpc_target_year:

  Integer or NULL. LPC target year (Assumption 2). Default: last year.

- io_changes:

  Data frame or NULL. Scenario ΔZ & ΔX (Assumption 3).

- growth_rate_scenario:

  Numeric vector or NULL. Growth rate override.

- pop_projection:

  Data frame atau NULL. Proyeksi penduduk dengan kolom \`Tahun\` dan
  \`Populasi\`. Jika NULL, kolom Pendapatan/PDRB per Kapita akan
  bernilai NA. (Opsional)

## Value

Tibble of projection results per (year x sector) with columns: YEAR,
SECTOR, LAND_REQUIREMENT, LPC, LPC_GROWTH_RATE, FD, OUTPUT, GDP, INCOME,
LABOUR, SURPLUS, ITBL, ITFL (end/active condition values). Diagnostic
attributes, including \`linkage_compare\`: a data frame per sector
containing SECTOR, ITBL_BEFORE, ITBL_AFTER, DELTA_ITBL, ITFL_BEFORE,
ITFL_AFTER, DELTA_ITFL. Without Assumption 3, BEFORE values equal AFTER
values.

## Details

Supported combinations:

1.  BAU only (area_bau, lpc=NULL, io=NULL)

2.  BAU + A1 (area_sc, lpc=NULL, io=NULL)

3.  BAU + A1 + A2 (area_sc, lpc=..., io=NULL)

4.  BAU + A1 + A3 (area_sc, lpc=NULL, io=...)

5.  BAU + A2 (area_bau, lpc=..., io=NULL)

6.  BAU + A2 + A3 (area_bau, lpc=..., io=...)

7.  BAU + A3 (area_bau, lpc=NULL, io=...)
