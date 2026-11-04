# Interpolate Population Vector to Match Projection Years

Menginterpolasi (dan ekstrapolasi linear) vektor populasi agar sesuai
dengan rentang tahun proyeksi. Jika tahun berada di luar rentang data,
ekstrapolasi linear menggunakan 2 titik terdekat.

## Usage

``` r
interpolate_population(pop_df, target_years)
```

## Arguments

- pop_df:

  Tibble hasil \`load_population_projection()\`.

- target_years:

  Numeric vector. Tahun proyeksi yang diinginkan.

## Value

Numeric vector sepanjang \`length(target_years)\` dengan nilai populasi
hasil interpolasi.
