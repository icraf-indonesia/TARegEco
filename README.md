# TARegEco <a href="https://github.com/icraf-indonesia/TARegEco"><img src="man/figures/logo.png" align="right" height="138" /></a>

Modul Analisis Deskriptif dari **Trade-off Analysis (TA) Regional Economy** digunakan untuk menghasilkan potret ekonomi wilayah pada tahun dasar berdasarkan Tabel Input-Output (IO), data lahan, dan pertumbuhan. Keluaran modul ini berupa tabel dan grafik PDRB, tenaga kerja, keterkaitan, serta multiplier, yang menjadi dasar untuk modul Proyeksi.

## Daftar Isi

- [Diagram Alur Kerja](#diagram-alur-kerja)
- [Spesifikasi Matriks dan Tabel Input](#spesifikasi-matriks-dan-tabel-input)
- [Fungsi Utama dan Dokumentasi](#fungsi-utama-dan-dokumentasi)
- [Cara Menjalankan](#cara-menjalankan)
- [Keluaran](#keluaran)
- [Referensi](#referensi)

------------------------------------------------------------------------

## Alur Kerja {#diagram-alur-kerja}

1.  **Input File Excel** – Membaca data dari `Input_IO_*.xlsx`, `laju_pertumbuhan.xlsx`, raster tutupan lahan, dan lookup table.

2.  **Pembentukan Matriks** – Data diurai menjadi matriks: `intermediate_consumption`, `final_demand`, `added_value`, `labour`, `land_proportion`.

3.  **Pengecekan Keseimbangan** – Total Input vs Total Output. Selisih dalam batas toleransi → SEIMBANG.

4.  **Koefisien Teknis & Leontief** – A = IC × diag(1/total_input); L = (I-A)⁻¹.

5.  **Linkage** – Backward & Forward Linkage (langsung, total, indeks).

6.  **GDP per Sektor** – Nilai tambah bruto, output, kebutuhan lahan, LPC, FD.

7.  **Multiplier** – Output, Income, Labour, Surplus Usaha Bruto.

8.  **Output** – Excel `IO_Analysis_Output.xlsx` dan `.RData` untuk modul Proyeksi.

## Spesifikasi Matriks dan Tabel Input {#spesifikasi-matriks-dan-tabel-input}

### 1. Tabel Intermediate Consumption (`intermediate_consumption.m`)

- **Dimensi:** Sektor × Sektor (n × n)

- **Baris (i):** Sektor penyedia input (penjual)

- **Kolom (j):** Sektor pengguna input (pembeli)

- **Nilai sel `[i, j]`:** Nilai moneter input dari sektor \*i\* yang digunakan oleh sektor \*j\*.

### 2. Tabel Final Demand (`final_demand.m`)

- **Dimensi:** Sektor × Komponen Final Demand (n × k)

- **Baris (i):** Sektor penyedia barang/jasa

- **Kolom (k):** Komponen permintaan akhir (Konsumsi RT, Pemerintah, Investasi, Ekspor, dll.)

- **Nilai sel `[i, k]`:** Penjualan output sektor \*i\* untuk komponen permintaan akhir \*k\*.

### 3. Tabel Added Value (`added_value.m`)

- **Dimensi:** Komponen Nilai Tambah × Sektor (m × n)

- **Baris (m):** Komponen input primer (Kompensasi Tenaga Kerja, Surplus Usaha, Pajak, Impor, dll.)

- **Kolom (j):** Sektor pengguna input primer

- **Nilai sel `[m, j]`:** Pembayaran oleh sektor \*j\* kepada faktor produksi/pemerintah.

### 4. Tabel Land Proportion (`land_proportion`)

- **Dimensi:** Sektor × Kelas Tutupan Lahan (n × L)

- **Baris (i):** Sektor ekonomi

- **Kolom (L):** Jenis tutupan lahan (land cover class)

- **Nilai sel `[i, L]`:** Proporsi (0–1) dari luas lahan kelas *L* yang dialokasikan ke sektor \*i\*. **Total proporsi per kolom (kelas lahan) = 1.**

### 5. Tabel Labour (`labour`)

- **Dimensi:** Sektor × 1

- **Nilai:** Jumlah tenaga kerja (orang) per sektor.

## Fungsi Utama dan Dokumentasi {#fungsi-utama-dan-dokumentasi}

### `get_added_value_components(intermediate_consumption_m, added_value_m)`

Mengidentifikasi baris-baris komponen nilai tambah dan menghitung PDRB.

- **Parameter:**

  - `intermediate_consumption_m`: matriks konsumsi antara (Sektor x Sektor)

  - `added_value_m`: matriks nilai tambah (Komponen x Sektor)

- **Proses:** Mencocokkan nama baris (alias) dengan komponen Impor, Kompensasi, Surplus, Pajak. Menghitung `gdp = compensation + surplus + other_tax`.

- **Keluaran:** `list` berisi `total_input`, `compensation`, `surplus`, `gdp_val`, dll.

- **Rumus utama:** PDRB = Kompensasi + Surplus + Pajak Lainnya.

### `check_io_balance(intermediate_consumption_m, final_demand_m, added_value_m, tolerance_percent = 1)`

Memeriksa keseimbangan total input dan output.

- **Parameter:** matriks IC, FD, AV; batas toleransi (%) default 1.

- **Proses:** `Total Output = rowSums(IC) + rowSums(FD)`, `Total Input` dari `get_added_value_components`. Selisih % dihitung terhadap total output.

- **Keluaran:** `list` dengan `status`, `total_input`, `total_output`, `gap`, `gap_percent`.

### `compute_linkage(intermediate_consumption_m, added_value_m)`

Menghitung matriks koefisien teknis (A), matriks Leontief, dan indeks keterkaitan.

- **Parameter:** matriks IC dan AV.

- **Proses:**

  - `A = IC %*% diag(1 / total_input)`

  - `L = solve(I - A)`

  - DBL = colSums(A), TBL = colSums(L), ITBL = (TBL × n) / sum(TBL)

  - DFL = rowSums(A), TFL = rowSums(L), ITFL = (TFL × n) / sum(TFL)

- **Keluaran:** `list` berisi `A`, `Leontief`, `DFL`, `TFL`, `ITFL`, `DBL`, `TBL`, `ITBL`.

- **Referensi:** Rasmussen (1956); Miller & Blair (2009).

### `build_full_io_table(sector, intermediate_consumption_m, final_demand_m, final_demand_component, added_value_m, added_value_lookup)`

Menggabungkan semua blok tabel IO ke dalam satu matriks lengkap dan membuat Code Registry.

- **Parameter:** semua tabel penyusun dan data lookup komponen.

- **Keluaran:** `list` dengan `full_io_table`, `code_registry`, `n_sector`.

### `calculate_gdp(added_value_m, intermediate_consumption_m, final_demand_m, sector, land_proportion, landuse, leontief)`

Menghitung PDRB, output, Land Requirement, LPC, dan total output model.

- **Parameter:** semua tabel, data sektor, path raster lahan, matriks Leontief.

- **Proses:**

  - Luas lahan dari raster, `land_requirement = land_proportion %*% diag(luas)`.

  - GDP dari `get_added_value_components`.

  - `LPC = OUTPUT / Land_Requirement` (Rp/Ha).

  - `Total_OUTPUT = Leontief %*% FD`.

- **Keluaran:** `data.frame` dengan kolom lengkap.

### `calc_output_multiplier(Leontief, sector, top_n = 20)`

Multiplier output = `colSums(Leontief)`.

### `calc_income_multiplier(Leontief, total_output, added_value, sector, top_n = 20)`

Multiplier pendapatan tenaga kerja = `colSums(Leontief %*% diag(income_coeff))`, dengan `income_coeff = Kompensasi / Total Output`.

### `calc_labour_multiplier(Leontief, labour, total_output, sector, top_n = 20)`

Multiplier tenaga kerja = `colSums(Leontief %*% diag(labour_coeff))`, dengan `labour_coeff = Tenaga Kerja / Total Output`.

### `calc_sub_multiplier(Leontief, intermediate_consumption_m, added_value_m, sector, top_n = 20)`

Multiplier surplus usaha bruto = `colSums(Leontief %*% diag(sub_coeff))`, dengan `sub_coeff = Surplus / Total Input`.

### `combine_multipliers(OM, IM, LM, SUBM)`

Menggabungkan keempat tabel multiplier menjadi satu `data.frame`.

## Cara Menjalankan {#cara-menjalankan}

1.  Buka file `descriptive_analysis.Rmd` di RStudio.

2.  Sesuaikan path input di blok `## INPUT FILES`.

3.  Pastikan semua paket terinstal:

    `install.packages(c("DT","ggplot2","plotly","terra","bslib","htmltools","openxlsx","readxl","dplyr","tidyr","stringr","tibble"))`

4.  Klik **Knit** atau jalankan:

    `Rscript -e "rmarkdown::render('descriptive_analysis.Rmd')"`

## Keluaran {#keluaran}

1.  **`data/IO_Analysis_Output.xlsx`** – Workbook berisi sheet: GDP, Leontief_Matrix, Growth_Rate, multiplier (Output, Income, Labour, SUB), Land_Proportion, Linkage, Full_IO_Table, Code_Registry.

2.  **`data/Descriptive_Analysis_Environment.RData`** – Seluruh variabel lingkungan untuk digunakan oleh modul Proyeksi (load dengan `load("data/Descriptive_Analysis_Environment.RData")`).
