# Find row alias in matrix or data frame rownames

Searches for the first row name that matches one of the provided
aliases, tolerating differences in capitalization and leading/trailing
spaces.

## Usage

``` r
find_row_alias(data, aliases)
```

## Arguments

- data:

  A matrix or data frame with row names.

- aliases:

  Character vector. Candidate row names.

## Value

The first matching row name, or \`NULL\` if none is found.
