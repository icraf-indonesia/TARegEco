# Select key sectors (sektor unggulan)

Selects sectors with \`ITBL \> 1\` \*\*and\*\* \`ITFL \> 1\`, following
BPS practice: "Suatu industri dikategorikan sebagai Industri Unggulan
jika memiliki IBL dan IFL lebih dari 1 (satu)."

## Usage

``` r
select_primary_sectors(linkages_table)
```

## Arguments

- linkages_table:

  A tibble produced by \[create_linkages_table()\].

## Value

A tibble containing key sectors.

## Details

The threshold of 1 belongs to the \*\*normalised\*\* indices ITBL and
ITFL (BPS: Indeks Daya Penyebaran and Indeks Derajat Kepekaan), which
are constructed so their mean is exactly 1. The direct linkages DBL and
DFL must not be tested against 1: \`DBL = colSums(A)\` is the
intermediate input share of total input, so it is strictly below 1
whenever value added is non-negative, which makes the test
unsatisfiable.

## References

BPS, \*Publikasi Tabel Input-Output 2016\* (provincial series).
Rasmussen, P.N. (1956). \*Studies in Inter-sectoral Relations\*.
