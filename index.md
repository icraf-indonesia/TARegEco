# TARegEco

**TARegEco** adalah paket R untuk **Trade-off Analysis Regional
Economy**.

Paket ini terdiri dari dua modul utama:

1.  **Analisis Deskriptif** — menggambarkan struktur ekonomi wilayah
    pada tahun dasar menggunakan Tabel Input-Output (IO), data lahan,
    tenaga kerja, dan nilai tambah.
2.  **Proyeksi** — memproyeksikan ekonomi regional ke masa depan
    berdasarkan skenario intervensi, seperti perubahan penggunaan lahan,
    laju pertumbuhan, peningkatan produktivitas, perubahan matriks IO,
    dan intensitas implementasi.

Hasil akhir digunakan untuk mendukung perencanaan wilayah dan analisis
kebijakan berbasis bukti.

## Daftar Isi

- [Struktur Repositori](#struktur-repositori)
- [Alur Kerja Umum](#alur-kerja-umum)
- [Modul 1: Analisis Deskriptif](#modul-1-analisis-deskriptif)
  - [Spesifikasi Matriks dan Tabel
    Input](#spesifikasi-matriks-dan-tabel-input)
  - [Fungsi Utama](#fungsi-utama)
- [Modul 2: Proyeksi](#modul-2-proyeksi)
  - [Skenario Intervensi](#skenario-intervensi)
  - [Mekanisme Proyeksi](#mekanisme-proyeksi)
- [Cara Menjalankan](#cara-menjalankan)
- [Keluaran](#keluaran)
- [Referensi](#referensi)

------------------------------------------------------------------------

## Struktur Repositori

``` text
TARegEco/
├── R/
│   ├── read_input.R
│   ├── validate.R
│   ├── io_components.R
│   ├── balance.R
│   ├── linkage.R
│   ├── gdp.R
│   ├── multiplier.R
│   ├── plot.R
│   ├── export.R
│   ├── template.R
│   └── ...
├── inst/
│   ├── extdata/
│   │   ├── template/          # template input kosong
│   │   └── example_data/      # data contoh untuk uji coba
│   └── reports/               # laporan Rmd yang siap dirender
├── vignettes/
│   ├── panduan-input-data.Rmd
│   ├── panduan-analisis-deskriptif.Rmd
│   └── glosarium.Rmd
├── tests/
│   └── testthat/
├── descriptive_analysis.Rmd
├── projection_analysis.Rmd
├── DESCRIPTION
├── NAMESPACE
├── README.md
└── TARegEco.Rproj
```

------------------------------------------------------------------------

## Ringkasan Alur Kerja

- **Analisis Deskriptif**: Membaca data dasar dan menghasilkan:
  - Struktur ekonomi wilayah
  - Matriks Leontief
  - Keterkaitan antar sektor
  - Multiplier
- **Proyeksi**: Memanfaatkan output deskriptif untuk menjalankan
  proyeksi:
  - Proyeksi BAU (*Business As Usual*) tanpa intervensi
  - Proyeksi skenario berdasarkan asumsi yang ditentukan pengguna
  - Perbandingan antar skenario dalam tabel dan grafik

------------------------------------------------------------------------

## Modul 1: Analisis Deskriptif

Modul ini menghasilkan potret ekonomi wilayah tahun dasar. Hasilnya
menjadi *baseline* untuk modul Proyeksi.

### Spesifikasi Matriks dan Tabel Input

1.  **Tabel Intermediate Consumption** (`intermediate_consumption.m`)
    - **Dimensi**: Sektor × Sektor ($`n \times n`$)
    - **Baris (**$`i`$): Sektor penyedia input (penjual)
    - **Kolom (**$`j`$): Sektor pengguna input (pembeli)
    - \*\*Nilai sel \[\*\*$`i, j`$\]: Nilai moneter input dari sektor
      $`i`$ yang digunakan oleh sektor $`j`$.
2.  **Tabel Final Demand** (`final_demand.m`)
    - **Dimensi**: Sektor × Komponen Final Demand ($`n \times k`$)
    - **Baris (**$`i`$): Sektor penyedia barang/jasa
    - **Kolom (**$`k`$): Komponen permintaan akhir (Konsumsi Rumah
      Tangga, Pemerintah, Investasi, Ekspor, dll.)
    - \*\*Nilai sel \[\*\*$`i, k`$\]: Penjualan output sektor $`i`$
      untuk komponen permintaan akhir $`k`$.
3.  **Tabel Added Value** (`added_value.m`)
    - **Dimensi**: Komponen Nilai Tambah × Sektor ($`m \times n`$)
    - **Baris (**$`m`$): Komponen input primer (Kompensasi Tenaga Kerja,
      Surplus Usaha, Pajak, Impor, dll.)
    - **Kolom (**$`j`$): Sektor pengguna input primer
    - \*\*Nilai sel \[\*\*$`m, j`$\]: Pembayaran oleh sektor $`j`$
      kepada faktor produksi atau pemerintah.
4.  **Tabel Land Proportion** (`land_proportion`)
    - **Dimensi**: Sektor × Kelas Tutupan Lahan ($`n \times L`$)
    - **Baris (**$`i`$): Sektor ekonomi
    - **Kolom (**$`L`$): Jenis tutupan lahan
    - \*\*Nilai sel \[\*\*$`i, L`$\]: Proporsi (0–1) luas lahan kelas
      $`L`$ yang dialokasikan ke sektor $`i`$. Total per kolom = 1.
5.  **Tabel Labour** (`labour`)
    - **Dimensi**: Sektor × 1
    - **Nilai**: Jumlah tenaga kerja (orang) per sektor.

### Fungsi Utama

| Fungsi | Deskripsi |
|:---|:---|
| [`read_input_file()`](https://icraf-indonesia.github.io/TARegEco/reference/read_input_file.md) | Membaca file Excel/CSV menjadi tibble/matrix |
| [`validate_io_data()`](https://icraf-indonesia.github.io/TARegEco/reference/validate_io_data.md) | Memvalidasi dimensi, nama sektor, tipe data, dan NA |
| [`get_added_value_components()`](https://icraf-indonesia.github.io/TARegEco/reference/get_added_value_components.md) | Menghitung komponen nilai tambah dan PDRB |
| [`check_io_balance()`](https://icraf-indonesia.github.io/TARegEco/reference/check_io_balance.md) | Memeriksa keseimbangan input-output per sektor |
| [`compute_leontief()`](https://icraf-indonesia.github.io/TARegEco/reference/compute_leontief.md) | Menghitung matriks koefisien teknis dan Leontief inverse |
| [`compute_linkage()`](https://icraf-indonesia.github.io/TARegEco/reference/compute_linkage.md) | Menghitung linkage backward dan forward |
| [`calculate_gdp()`](https://icraf-indonesia.github.io/TARegEco/reference/calculate_gdp.md) | Menghitung PDRB, output, kebutuhan lahan, LPC |
| [`calc_output_multiplier()`](https://icraf-indonesia.github.io/TARegEco/reference/calc_output_multiplier.md) | Menghitung output multiplier |
| [`calc_income_multiplier()`](https://icraf-indonesia.github.io/TARegEco/reference/calc_income_multiplier.md) | Menghitung income multiplier |
| [`calc_labour_multiplier()`](https://icraf-indonesia.github.io/TARegEco/reference/calc_labour_multiplier.md) | Menghitung labour multiplier |
| [`calc_sub_multiplier()`](https://icraf-indonesia.github.io/TARegEco/reference/calc_sub_multiplier.md) | Menghitung surplus usaha bruto multiplier |
| [`combine_multipliers()`](https://icraf-indonesia.github.io/TARegEco/reference/combine_multipliers.md) | Menggabungkan tabel multiplier |
| [`export_io_analysis()`](https://icraf-indonesia.github.io/TARegEco/reference/export_io_analysis.md) | Menyimpan hasil ke Excel |
| [`save_analysis_data()`](https://icraf-indonesia.github.io/TARegEco/reference/save_analysis_data.md) | Menyimpan semua output ke `.rds`/`.RData` |

> **Dokumentasi Lengkap:** Tersedia di folder `R/` dengan format
> Roxygen2 dan dapat diakses melalui
> [`help(package = "TARegEco")`](https://icraf-indonesia.github.io/TARegEco/reference).

------------------------------------------------------------------------

## Modul 2: Proyeksi

Modul ini memproyeksikan ekonomi regional ke periode mendatang (misal
2024–2049) dengan langkah waktu 5 tahun.

### Skenario Intervensi

Pengguna dapat menyusun skenario dengan lima jenis input opsional:

| No | Input | Deskripsi |
|:--:|:---|:---|
| 1 | Perubahan penggunaan lahan | Mengubah distribusi proporsi lahan atau luas lahan per kelas |
| 2 | Growth rate | Menentukan laju pertumbuhan tahunan per sektor |
| 3 | Peningkatan produktivitas (LPC) | Meningkatkan efisiensi lahan (Rp/Ha) |
| 4 | Perubahan matriks Leontief | Mengubah sel tertentu pada Tabel IO |
| 5 | Intensitas implementasi | Menentukan seberapa besar skenario diterapkan (0–100%) |

*Setiap input dapat dikombinasikan sesuai kebutuhan. Jika suatu file
tidak disediakan, sistem otomatis menggunakan nilai baseline.*

### Mekanisme Proyeksi

1.  Luas lahan per skenario dikonversi menjadi *land requirement*.
2.  $`\text{Land requirement} \times \text{proporsi FD} \times \text{LPC} \times \text{growth factor} \rightarrow \text{Final Demand}`$.
3.  $`\text{Final Demand} \times \text{Leontief Inverse} \rightarrow \text{Output ekonomi}`$.
4.  Output ekonomi dikalikan koefisien untuk menghasilkan:
    - PDRB
    - Serapan tenaga kerja
    - Pendapatan tenaga kerja
    - Surplus usaha bruto
    - Keterkaitan antar sektor lahan

*Hasil akhir berupa tabel komparasi dan grafik multi-skenario yang
membandingkan BAU dengan skenario intervensi.*

------------------------------------------------------------------------

## Cara Menjalankan

### Prasyarat

Instal paket R yang diperlukan:

``` r

install.packages(c(
  "DT", "ggplot2", "plotly", "terra", "bslib", "htmltools",
  "openxlsx", "readxl", "dplyr", "tidyr", "stringr", "tibble"
))
```

### Menjalankan Analisis Deskriptif

Buka `descriptive_analysis.Rmd` di RStudio, sesuaikan *path input*, lalu
klik **Knit** atau jalankan:

``` r

rmarkdown::render("descriptive_analysis.Rmd")
```

### Menjalankan Proyeksi

Buka `projection_analysis.Rmd`, sesuaikan parameter `mode` dan
`n_scenarios`, lalu render:

``` r

# Mode BAU
rmarkdown::render("projection_analysis.Rmd", params = list(mode = "BAU", n_scenarios = 2))

# Mode Scenario
rmarkdown::render("projection_analysis.Rmd", params = list(mode = "Scenario", n_scenarios = 2))
```

## Keluaran

### Analisis Deskriptif

- `data/IO_Analysis_Output.xlsx` — Workbook berisi GDP, Leontief,
  multiplier, linkage, full IO table, dan code registry.

- `data/io_descriptive.rds` — Objek `io` siap digunakan oleh modul
  Proyeksi.

### Proyeksi

- **HTML Hasil Proyeksi** — Berisi tabel dan grafik PDRB, tenaga kerja,
  pendapatan, surplus, dan linkage per skenario.

- `data/BAU_projection.RData` — Variabel hasil proyeksi BAU.

- **Template skenario dan folder** `data/scenario/scenario*` — Input
  skenario yang telah diisi pengguna.

## Referensi

- Leontief, W. (1986). *Input-Output Economics*. Oxford University
  Press.

- Miller, R.E. & Blair, P.D. (2009). *Input-Output Analysis: Foundations
  and Extensions*. Cambridge University Press.
