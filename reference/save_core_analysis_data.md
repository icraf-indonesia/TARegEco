# Save core analysis results to RDS

Saves the results of the core Input-Output analysis into an RDS file so
that visualisation can be performed without recalculating.

## Usage

``` r
save_core_analysis_data(
  core_analysis_results,
  output_dir = "inst/extdata/output"
)
```

## Arguments

- core_analysis_results:

  List containing core analysis outputs.

- output_dir:

  Character. Directory to save the RDS file. Default is
  \`"inst/extdata/output"\`.

## Value

Invisibly returns the output directory.
