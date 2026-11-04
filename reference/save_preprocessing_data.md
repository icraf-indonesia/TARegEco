# Save preprocessing data to RDS

Saves the input and validation results into an RDS file so that the
preprocessing stage can be reloaded before running the core analysis.

## Usage

``` r
save_preprocessing_data(io_preprocessing, output_dir = "inst/extdata/output")
```

## Arguments

- io_preprocessing:

  List containing preprocessed input data.

- output_dir:

  Character. Directory to save the RDS file. Default is
  \`"inst/extdata/output"\`.

## Value

Invisibly returns the output directory.
