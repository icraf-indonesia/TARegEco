# Load Population Projection from Excel or CSV

Membaca file proyeksi penduduk dengan 2 kolom wajib: \`Tahun\` (atau
\`YEAR\`) dan \`Populasi\` (atau \`POPULATION\`). Nama kolom dideteksi
secara fleksibel (case-insensitive).

## Usage

``` r
load_population_projection(path, sheet = 1)
```

## Arguments

- path:

  Character. Path ke file \`.xlsx\`, \`.xls\`, atau \`.csv\`.

- sheet:

  Character atau Integer. Nama/indeks sheet untuk file Excel. Default:
  sheet pertama.

## Value

Tibble dengan kolom \`Tahun\` (integer) dan \`Populasi\` (numeric),
terurut menaik berdasarkan Tahun. Mengembalikan \`NULL\` jika file tidak
valid atau kolom wajib tidak ditemukan.
