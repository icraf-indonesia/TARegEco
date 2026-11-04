# Create projection period table

Creates a data frame containing projection periods based on the initial
year, final year, and time step.

## Usage

``` r
create_period_table(initial_year, end_year, time_step)
```

## Arguments

- initial_year:

  Numeric. First year of the projection.

- end_year:

  Numeric. Last year of the projection.

- time_step:

  Numeric. Interval between projection periods.

## Value

A data frame with columns:

- Period:

  Character label, e.g., "2024-2029"

- Start_Year:

  Numeric start year

- End_Year:

  Numeric end year

## Examples

``` r
if (FALSE) { # \dontrun{
create_period_table(2024, 2049, 5)
} # }
```
