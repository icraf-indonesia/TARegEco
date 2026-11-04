# Check Input-Output balance

Computes total input and total output per sector, the difference, and
balance status based on a given tolerance.

## Usage

``` r
check_io_balance(
  intermediate_consumption_m,
  final_demand_m,
  added_value_m,
  sector_names = NULL,
  tolerance_percent = 1,
  tolerance_value = 1
)
```

## Arguments

- intermediate_consumption_m:

  Numeric matrix of intermediate consumption.

- final_demand_m:

  Numeric matrix of final demand.

- added_value_m:

  Numeric matrix of added value.

- sector_names:

  Character vector of sector names. Optional; if \`NULL\`, column names
  of \`intermediate_consumption_m\` are used.

- tolerance_percent:

  Numeric. Threshold for balance check in percent (default = 1).

- tolerance_value:

  Numeric. Not used; kept for backward compatibility.

## Value

A list with aggregate values (\`status\`, \`total_input\`,
\`total_output\`, \`gap\`, \`gap_percent\`) and per-sector details
(\`balance_detail\`, \`total_input_per_sector\`,
\`total_output_per_sector\`, \`gap_per_sector\`,
\`gap_percent_per_sector\`).

## Examples

``` r
if (FALSE) { # \dontrun{
bal <- check_io_balance(ic, fd, av, sector_names = sector$name)
} # }
```
