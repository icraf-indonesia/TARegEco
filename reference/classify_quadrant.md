# Classify sectors into quadrants

Classifies sectors into quadrants I-IV from the normalised linkage
indices ITBL (BPS: Indeks Daya Penyebaran) and ITFL (Indeks Derajat
Kepekaan), following the quadrant scheme published by BPS.

## Usage

``` r
classify_quadrant(linkages_table)
```

## Arguments

- linkages_table:

  A tibble produced by \[create_linkages_table()\].

## Value

A tibble with an additional \`Quadrant\` column.

## Details

BPS quadrant scheme:

- Kuadran I (ITBL \> 1, ITFL \> 1): industri unggulan

- Kuadran II (ITBL \> 1, ITFL \< 1): potensi unggul

- Kuadran III (ITBL \< 1, ITFL \> 1): kurang diunggulkan

- Kuadran IV (ITBL \< 1, ITFL \< 1): tertinggal

## References

BPS Aceh, \*Publikasi Tabel I-O 2016\*, "Pemetaan Industri Menurut
Kuadran".
