# Run projection (Supply-Constrained Mixed Input-Output Model)

Simulates regional economic indicators over time using a
supply-constrained mixed input-output model. Land-constrained (co)
sectors are driven by land availability × land productivity coefficient
(LPC), while non-land (nc) sectors are solved via Leontief inverse.

## Usage

``` r
run_projection_bau(
  gdp_baseline,
  income_multiplier,
  labour_multiplier,
  SUB_multiplier,
  output_multiplier,
  linkage_table,
  leontief_matrix,
  full_io_table,
  sector_idx,
  P,
  area_matrix,
  growth_rate,
  lpc_scenario = NULL,
  io_changes = NULL,
  intensity_table = NULL,
  constrained = NULL,
  unconstrained = NULL,
  pop_projection = NULL
)
```

## Arguments

- gdp_baseline:

  Data frame. Baseline GDP table from descriptive analysis.

- income_multiplier:

  Data frame. Income multiplier table (kolom Labour_Compensation).

- labour_multiplier:

  Data frame. Labour multiplier table (kolom Labour).

- SUB_multiplier:

  Data frame. SUB multiplier table (kolom Surplus).

- output_multiplier:

  Data frame. Output multiplier table (unused in this model).

- linkage_table:

  Data frame. Linkage table with ITFL and ITBL.

- leontief_matrix:

  Numeric matrix. Baseline Leontief inverse (unused here).

- full_io_table:

  Numeric matrix. Complete IO table (Z matrix).

- sector_idx:

  Integer vector. Sector indices in full IO table.

- P:

  Numeric matrix. Land proportion matrix \[n_sektor × n_tutupan\].

- area_matrix:

  Numeric matrix. Land area matrix \[n_tutupan × n_tahun\].

- growth_rate:

  Numeric vector. Growth rate permintaan akhir per sektor.

- lpc_scenario:

  Data frame or \`NULL\`. LPC scenario inputs.

- io_changes:

  Data frame or \`NULL\`. IO change scenarios.

- intensity_table:

  Data frame or \`NULL\`. Intensity scenarios.

- constrained:

  Integer vector. Indikator sektor co (1 = constrained).

- unconstrained:

  Integer vector. Indikator sektor nc (1 = unconstrained).

## Value

A data frame with projected indicators for all sectors and years.
