# Proses Input Asumsi 3 dan Bangun A_list / Y_list per Tahun

Mendukung dua format input: - \*\*persen_per_tahun\*\*: kolom tahun
(2025, 2026, ...) berisi persentase realokasi dari Y_0 sektor asal. -
\*\*nominal_dz\*\*: kolom \`PENAMBAHAN TRANSAKSI ANTARA / ΔZ\` berisi
nominal.

## Usage

``` r
process_sc_ras_system_dynamic(
  df_input,
  Z_base,
  X_base,
  Labour_base,
  Wage_base,
  sektor_names,
  years,
  max_share = 0.9
)
```

## Arguments

- df_input:

  Data frame. Isi file A3.

- Z_base:

  Numeric matrix. Matriks transaksi antara baseline.

- X_base:

  Numeric vector. Output baseline.

- Labour_base:

  Numeric vector. Labour nominal baseline.

- Wage_base:

  Numeric vector. Wage nominal baseline.

- sektor_names:

  Character vector. Nama sektor.

- years:

  Numeric vector. Tahun proyeksi.

- max_share:

  Numeric. Batas maksimal porsi Y_0 yang dialihkan (default 0.90).

## Value

List dengan \`A_list\`, \`Y_list\`, \`ITBL_list\`, \`ITFL_list\`,
\`Delta_Z_list\`, \`Delta_Y_list\`, \`A_base\`, \`L_base\`, \`Y_base\`,
\`v_c_new\`, \`w_c_new\`, \`l_c_new\`, \`backward_linkage\`,
\`forward_linkage\`, \`output_multiplier\`, \`mode\`,
\`ras_iterations\`, \`ras_diff\`.
