# TARegEco <a href="https://github.com/icraf-indonesia/TARegEco"><img src="man/figures/logo.png" align="right" height="138"/></a>

**TARegEco** adalah paket R untuk **Trade-off Analysis Regional Economy**.

Paket ini terdiri dari dua modul utama:

1.  **Analisis Deskriptif** — menggambarkan struktur ekonomi wilayah pada tahun dasar menggunakan Tabel Input-Output (IO), data lahan, tenaga kerja, dan nilai tambah.
2.  **Proyeksi** — memproyeksikan ekonomi regional ke masa depan berdasarkan skenario intervensi, seperti perubahan penggunaan lahan, laju pertumbuhan, peningkatan produktivitas, perubahan matriks IO, dan intensitas implementasi.

Hasil akhir digunakan untuk mendukung perencanaan wilayah dan analisis kebijakan berbasis bukti.

<!-- badges: start -->

<!-- badges: end -->

## Daftar Isi

- [Struktur Repositori](#struktur-repositori)
- [Alur Kerja Umum](#alur-kerja-umum)
- [Modul 1: Analisis Deskriptif](#modul-1-analisis-deskriptif)
  - [Spesifikasi Matriks dan Tabel Input](#spesifikasi-matriks-dan-tabel-input)
  - [Fungsi Utama](#fungsi-utama)
- [Modul 2: Proyeksi](#modul-2-proyeksi)
  - [Skenario Intervensi](#skenario-intervensi)
  - [Mekanisme Proyeksi](#mekanisme-proyeksi)
- [Cara Menjalankan](#cara-menjalankan)
- [Keluaran](#keluaran)
- [Referensi](#referensi)

------------------------------------------------------------------------

## Struktur Repositori {#struktur-repositori}

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
- **Proyeksi**: Memanfaatkan output deskriptif untuk menjalankan proyeksi:
  - Proyeksi BAU (*Business As Usual*) tanpa intervensi
  - Proyeksi skenario berdasarkan asumsi yang ditentukan pengguna
  - Perbandingan antar skenario dalam tabel dan grafik

------------------------------------------------------------------------

## Modul 1: Analisis Deskriptif {#modul-1-analisis-deskriptif}

Modul ini menghasilkan potret ekonomi wilayah tahun dasar. Hasilnya menjadi *baseline* untuk modul Proyeksi.

### Spesifikasi Matriks dan Tabel Input {#spesifikasi-matriks-dan-tabel-input}

1.  **Tabel Intermediate Consumption** (`intermediate_consumption.m`)
    - **Dimensi**: Sektor × Sektor ($n \times n$)
    - **Baris (**$i$): Sektor penyedia input (penjual)
    - **Kolom (**$j$): Sektor pengguna input (pembeli)
    - **Nilai sel [**$i, j$]: Nilai moneter input dari sektor $i$ yang digunakan oleh sektor $j$.
2.  **Tabel Final Demand** (`final_demand.m`)
    - **Dimensi**: Sektor × Komponen Final Demand ($n \times k$)
    - **Baris (**$i$): Sektor penyedia barang/jasa
    - **Kolom (**$k$): Komponen permintaan akhir (Konsumsi Rumah Tangga, Pemerintah, Investasi, Ekspor, dll.)
    - **Nilai sel [**$i, k$]: Penjualan output sektor $i$ untuk komponen permintaan akhir $k$.
3.  **Tabel Added Value** (`added_value.m`)
    - **Dimensi**: Komponen Nilai Tambah × Sektor ($m \times n$)
    - **Baris (**$m$): Komponen input primer (Kompensasi Tenaga Kerja, Surplus Usaha, Pajak, Impor, dll.)
    - **Kolom (**$j$): Sektor pengguna input primer
    - **Nilai sel [**$m, j$]: Pembayaran oleh sektor $j$ kepada faktor produksi atau pemerintah.
4.  **Tabel Land Proportion** (`land_proportion`)
    - **Dimensi**: Sektor × Kelas Tutupan Lahan ($n \times L$)
    - **Baris (**$i$): Sektor ekonomi
    - **Kolom (**$L$): Jenis tutupan lahan
    - **Nilai sel [**$i, L$]: Proporsi (0–1) luas lahan kelas $L$ yang dialokasikan ke sektor $i$. Total per kolom = 1.
5.  **Tabel Labour** (`labour`)
    - **Dimensi**: Sektor × 1
    - **Nilai**: Jumlah tenaga kerja (orang) per sektor.

### Fungsi Utama {#fungsi-utama}

| Fungsi | Deskripsi |
|:-----------------------------------|:-----------------------------------|
| `read_input_file()` | Membaca file Excel/CSV menjadi tibble/matrix |
| `validate_io_data()` | Memvalidasi dimensi, nama sektor, tipe data, dan NA |
| `get_added_value_components()` | Menghitung komponen nilai tambah dan PDRB |
| `check_io_balance()` | Memeriksa keseimbangan input-output per sektor |
| `compute_leontief()` | Menghitung matriks koefisien teknis dan Leontief inverse |
| `compute_linkage()` | Menghitung linkage backward dan forward |
| `calculate_gdp()` | Menghitung PDRB, output, kebutuhan lahan, LPC |
| `calc_output_multiplier()` | Menghitung output multiplier |
| `calc_income_multiplier()` | Menghitung income multiplier |
| `calc_labour_multiplier()` | Menghitung labour multiplier |
| `calc_sub_multiplier()` | Menghitung surplus usaha bruto multiplier |
| `combine_multipliers()` | Menggabungkan tabel multiplier |
| `export_io_analysis()` | Menyimpan hasil ke Excel |
| `save_analysis_data()` | Menyimpan semua output ke `.rds`/`.RData` |

> **Dokumentasi Lengkap:** Tersedia di folder `R/` dengan format Roxygen2 dan dapat diakses melalui `help(package = "TARegEco")`.

------------------------------------------------------------------------

## Modul 2: Proyeksi {#modul-2-proyeksi}

Modul ini memproyeksikan ekonomi regional ke periode mendatang (misal 2024–2049) dengan langkah waktu 5 tahun.

### Skenario Intervensi {#skenario-intervensi}

Pengguna dapat menyusun skenario dengan lima jenis input opsional:

| No | Input | Deskripsi |
|:--------------------------:|:---------------------|:---------------------|
| 1 | Perubahan penggunaan lahan | Mengubah distribusi proporsi lahan atau luas lahan per kelas |
| 2 | Growth rate | Menentukan laju pertumbuhan tahunan per sektor |
| 3 | Peningkatan produktivitas (LPC) | Meningkatkan efisiensi lahan (Rp/Ha) |
| 4 | Perubahan matriks Leontief | Mengubah sel tertentu pada Tabel IO |
| 5 | Intensitas implementasi | Menentukan seberapa besar skenario diterapkan (0–100%) |

*Setiap input dapat dikombinasikan sesuai kebutuhan. Jika suatu file tidak disediakan, sistem otomatis menggunakan nilai baseline.*

### Mekanisme Proyeksi {#mekanisme-proyeksi}

1.  Luas lahan per skenario dikonversi menjadi *land requirement*.
2.  $\text{Land requirement} \times \text{proporsi FD} \times \text{LPC} \times \text{growth factor} \rightarrow \text{Final Demand}$.
3.  $\text{Final Demand} \times \text{Leontief Inverse} \rightarrow \text{Output ekonomi}$.
4.  Output ekonomi dikalikan koefisien untuk menghasilkan:
    - PDRB
    - Serapan tenaga kerja
    - Pendapatan tenaga kerja
    - Surplus usaha bruto
    - Keterkaitan antar sektor lahan

*Hasil akhir berupa tabel komparasi dan grafik multi-skenario yang membandingkan BAU dengan skenario intervensi.*

------------------------------------------------------------------------

## Cara Menjalankan {#cara-menjalankan}

### Prasyarat

Instal paket R yang diperlukan:

``` r
install.packages(c(
  "DT", "ggplot2", "plotly", "terra", "bslib", "htmltools",
  "openxlsx", "readxl", "dplyr", "tidyr", "stringr", "tibble"
))
```

### Menjalankan Analisis Deskriptif

Buka `descriptive_analysis.Rmd` di RStudio, sesuaikan *path input*, lalu klik **Knit** atau jalankan:

``` r
rmarkdown::render("descriptive_analysis.Rmd")
```

### Menjalankan Proyeksi

Buka `projection_analysis.Rmd`, sesuaikan parameter `mode` dan `n_scenarios`, lalu render:

``` r
# Mode BAU
rmarkdown::render("projection_analysis.Rmd", params = list(mode = "BAU", n_scenarios = 2))

# Mode Scenario
rmarkdown::render("projection_analysis.Rmd", params = list(mode = "Scenario", n_scenarios = 2))
```

## Keluaran {#keluaran}

### Analisis Deskriptif

- `data/IO_Analysis_Output.xlsx` — Workbook berisi GDP, Leontief, multiplier, linkage, full IO table, dan code registry.

- `data/io_descriptive.rds` — Objek `io` siap digunakan oleh modul Proyeksi.

### Proyeksi

- **HTML Hasil Proyeksi** — Berisi tabel dan grafik PDRB, tenaga kerja, pendapatan, surplus, dan linkage per skenario.

- `data/BAU_projection.RData` — Variabel hasil proyeksi BAU.

- **Template skenario dan folder** `data/scenario/scenario*` — Input skenario yang telah diisi pengguna.

## Referensi

- Leontief, W. (1986). *Input-Output Economics*. Oxford University Press.

- Miller, R.E. & Blair, P.D. (2009). *Input-Output Analysis: Foundations and Extensions*. Cambridge University Press.
