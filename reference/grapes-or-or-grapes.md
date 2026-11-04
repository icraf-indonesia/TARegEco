# Null-coalescing operator

Mengembalikan \`b\` (RHS) jika \`a\` (LHS) adalah \`NULL\`, vektor
kosong, atau \`NA\`. Jika \`a\` memiliki nilai valid, \`a\`
dikembalikan.

## Usage

``` r
a %||% b
```

## Arguments

- a:

  Nilai kiri.

- b:

  Nilai pengganti (fallback) jika \`a\` kosong/NA.

## Value

\`a\` jika valid, selainnya \`b\`.

## Examples

``` r
NULL %||% "default"           # "default"
#> [1] "default"
"Rp Juta" %||% "fallback"     # "Rp Juta"
#> [1] "Rp Juta"
NA %||% "default"             # "default"
#> [1] "default"
character(0) %||% "default"   # "default"
#> [1] "default"
```
