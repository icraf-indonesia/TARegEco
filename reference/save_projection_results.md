# Save projection scenario results to RDS

Saves the result of a single projection scenario (BAU or intervention)
into an RDS file. The file name is derived from the scenario label after
sanitising invalid characters.

## Usage

``` r
save_projection_results(
  scenario_label,
  result,
  area_matrix,
  inputs,
  output_dir = "inst/extdata/output"
)
```

## Arguments

- scenario_label:

  Character. Name of the scenario, e.g., \`"BAU"\` or
  \`"Skenario_Pertanian_Maju"\`.

- result:

  Data frame. Projection result returned by \[run_projection()\].

- area_matrix:

  Numeric matrix. Area matrix used in the projection.

- inputs:

  List. Input parameters used in the projection.

- output_dir:

  Character. Directory to save the RDS file. Default is
  \`"inst/extdata/output"\`.

## Value

Invisibly returns the full path to the saved RDS file.
