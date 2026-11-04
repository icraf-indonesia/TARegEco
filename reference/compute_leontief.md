# Compute technical coefficient matrix and Leontief inverse

Computes the technical coefficient matrix (A) and the Leontief inverse
\\(I - A)^{-1}\\ using validated added value components rather than raw
column sums.

## Usage

``` r
compute_leontief(intermediate_consumption_m, added_value_m)
```

## Arguments

- intermediate_consumption_m:

  Numeric matrix of intermediate consumption (Sector x Sector).

- added_value_m:

  Numeric matrix of added value (Component x Sector).

## Value

A list with elements:

- A:

  Technical coefficient matrix

- Leontief:

  Leontief inverse matrix

- inverse_total_input:

  Vector of inverse total input per sector

## Details

Total input is computed using \[get_added_value_components()\], which
identifies only the relevant primary-input rows (compensation, surplus,
taxes, imports) instead of summing all rows of the added value matrix.

## References

Miller, R.E. & Blair, P.D. (2009). \*Input-Output Analysis: Foundations
and Extensions\*. Eurostat (2008). \*Manual of Supply, Use and
Input-Output Tables\*.

## Examples

``` r
if (FALSE) { # \dontrun{
leontief_result <- compute_leontief(ic, av)
} # }
```
