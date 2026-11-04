# Calculate GDP per sector with land productivity and land requirement coefficients

Computes GDP, output, land requirement, Land Requirement Coefficient
(LRC), and Land Productivity Coefficient (LPC) for each sector.

## Usage

``` r
calculate_gdp(
  added_value_m,
  intermediate_consumption_m,
  final_demand_m,
  sector,
  land_proportion,
  landuse_table,
  leontief
)
```

## Arguments

- added_value_m:

  Numeric matrix of added value.

- intermediate_consumption_m:

  Numeric matrix of intermediate consumption.

- final_demand_m:

  Numeric matrix of final demand.

- sector:

  A data frame or tibble with at least two columns: sector name and
  category.

- land_proportion:

  Numeric matrix of land proportions (Sector x Land Cover).

- landuse_table:

  Data frame or tibble with an \`area\` column representing land area in
  hectares.

- leontief:

  Numeric Leontief inverse matrix.

## Value

A tibble with columns: SECTOR, CATEGORY, GDP, OUTPUT, Land_Requirement,
LRC, LPC, GDP_PROP, FD, Prop_FD, Total_OUTPUT.

## Details

Land Requirement Coefficient (LRC) and Land Productivity Coefficient
(LPC) follow Hubacek & Sun (2001):

\$\$LRC_i = \frac{Land_i}{X_i}\$\$ \$\$LPC_i = \frac{1}{LRC_i} =
\frac{X_i}{Land_i}\$\$

where \\X_i\\ is total sectoral output and \\Land_i\\ is total land
requirement of sector i.

## References

Hubacek, K., & Sun, L. (2001). A scenario analysis of China's land use
and land cover change: incorporating biophysical information into
input-output modeling. Structural Change and Economic Dynamics, 12(4),
367–397.
