# Calculate LPC growth rate

Calculates the Land Productivity Coefficient (LPC) growth rate for a
given sector and projection period, based on crop type.

## Usage

``` r
calculate_lpc_growth_rate(lpc_scenario, sector, period_idx, periods_df)
```

## Arguments

- lpc_scenario:

  Data frame containing LPC scenario inputs with columns:
  \`Sector_Code\`, \`Start_Year\`, \`End_Year\`, \`Increment_Rate\`,
  \`Area_Implemented\`, and \`Crop_Type\`.

- sector:

  Numeric. Sector code to be evaluated.

- period_idx:

  Numeric. Index of the current projection period.

- periods_df:

  Data frame containing \`Start_Year\` and \`End_Year\`.

## Value

Numeric. LPC growth rate.

## Details

For annual crops, the growth rate is: \$\$growth = IncrementRate \times
AreaImplemented\$\$

For perennial crops, the growth rate is: \$\$growth =
AreaImplemented\_{now} \times \sum\_{k=1}^{now} IncrementRate_k\$\$

## Examples

``` r
if (FALSE) { # \dontrun{
gr <- calculate_lpc_growth_rate(lpc_scenario, 1, 1, periods_df)
} # }
```
