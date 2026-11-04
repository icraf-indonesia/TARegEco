# Apply IO table changes for a specific period

Modifies cells in the full IO table according to user-defined scenario
changes for the given projection period.

## Usage

``` r
apply_io_changes(full_io_table, io_changes, period_start, period_end)
```

## Arguments

- full_io_table:

  Numeric matrix. Complete Input-Output table.

- io_changes:

  Data frame with columns \`Start_Year\`, \`End_Year\`, \`Row_Code\`,
  \`Col_Code\`, and \`Change_Pct\`. Can be \`NULL\` if no changes.

- period_start:

  Numeric. Start year of the period.

- period_end:

  Numeric. End year of the period.

## Value

Numeric matrix. Updated full IO table.

## Examples

``` r
if (FALSE) { # \dontrun{
new_io <- apply_io_changes(full_io_table, io_changes, 2024, 2029)
} # }
```
