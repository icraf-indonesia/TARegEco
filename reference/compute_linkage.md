# Compute linkage indices and Leontief inverse

Computes technical coefficient matrix (A), Leontief inverse, and
backward/forward linkage indices (direct, total, and index).

## Usage

``` r
compute_linkage(intermediate_consumption_m, added_value_m)
```

## Arguments

- intermediate_consumption_m:

  Numeric matrix of intermediate consumption.

- added_value_m:

  Numeric matrix of added value.

## Value

A list with elements: \`A\`, \`Leontief\`, \`DFL\`, \`TFL\`, \`ITFL\`,
\`DBL\`, \`TBL\`, \`ITBL\`, \`DIF_TFL_DFL\`, \`DIF_TBL_DBL\`, and
\`inverse_total_input\`.

## Details

This function calls \[compute_leontief()\] to obtain A and Leontief,
then computes direct and total backward/forward linkage.

## References

Miller, R.E. & Blair, P.D. (2009). \*Input-Output Analysis: Foundations
and Extensions\*. Rasmussen, P.N. (1956). \*Studies in Inter-sectoral
Relations\*.

## Examples

``` r
if (FALSE) { # \dontrun{
linkage <- compute_linkage(ic, av)
} # }
```
