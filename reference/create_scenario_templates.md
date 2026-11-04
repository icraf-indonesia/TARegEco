# Create scenario template Excel file

Generates an Excel workbook containing blank templates for all five
scenario input assumptions used in the projection module.

## Usage

``` r
create_scenario_templates(
  gdp_baseline,
  code_registry,
  initial_year,
  end_year,
  time_step,
  output_path = "Scenario_Input_Templates.xlsx"
)
```

## Arguments

- gdp_baseline:

  Data frame. Baseline GDP table from descriptive analysis. It must
  contain \`SECTOR\` and \`CATEGORY\` columns.

- code_registry:

  Data frame. Code registry from \[build_full_io_table()\].

- initial_year:

  Numeric. First year of projection.

- end_year:

  Numeric. Last year of projection.

- time_step:

  Numeric. Time step in years.

- output_path:

  Character. Path to the output \`.xlsx\` file.

## Value

Invisibly returns the output path.

## Details

The workbook contains the following sheets: - \`README\`: instructions
for filling the template - \`1_Luasan_Lahan\`: land area (ha) per land
cover per projection year - \`2_Distribusi_Lahan\`: land distribution
proportions (optional) - \`3_Growth_Rate\`: sectoral growth rates
(optional) - \`4_Produktivitas_LPC\`: LPC productivity scenario
(optional) - \`5_Perubahan_IO\`: IO table changes (optional) -
\`Code_Registry_Ref\`: sector code reference - \`6_Intensitas\`:
implementation intensity (optional)
