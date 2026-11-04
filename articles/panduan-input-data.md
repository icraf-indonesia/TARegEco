# Panduan Penyiapan Data Input untuk TA Regional Economy

## Pendahuluan

Modul Analisis Deskriptif TA Regional Economy membutuhkan tiga file
input utama:

1.  **File Input-Output** (`Input_IO_*.xlsx`) — berisi tabel sektor,
    konsumsi antara, permintaan akhir, nilai tambah, tenaga kerja, dan
    proporsi lahan.
2.  **File Laju Pertumbuhan** (`laju_pertumbuhan_*.xlsx`) — berisi laju
    pertumbuhan tahunan per sektor.
3.  **File Lookup Tutupan Lahan** (`landcover_lookuptable_*.xlsx`) —
    berisi ID, nama kelas lahan, dan luas (hektar).

## Struktur File Input-Output

File ini harus memiliki 6 sheet dengan nama persis sebagai berikut:

### Sheet `sector`

| Kolom        | Deskripsi                     | Tipe            |
|--------------|-------------------------------|-----------------|
| `Sector`     | Nama sektor ekonomi           | Teks            |
| `Category`   | Kategori sektor               | Teks            |
| `Land_based` | Apakah sektor berbasis lahan? | Boolean/Logical |

### Sheet `intermediate_consumption`

- Matriks numerik persegi (n x n)
- Baris = sektor penjual, kolom = sektor pembeli
- Nilai sel `[i, j]` = nilai input dari sektor i yang digunakan sektor j

### Sheet `final_demand`

- Dua kolom pertama: `Code` (kode unik, numerik) dan `Label` (deskripsi
  komponen)
- Kolom berikutnya: nilai permintaan akhir per sektor (jumlah kolom =
  jumlah sektor)

### Sheet `added_value`

- Dua kolom pertama: `Code` (kode unik, numerik) dan `Label` (deskripsi
  komponen)
- Kolom berikutnya: nilai tambah per sektor (jumlah kolom = jumlah
  sektor)
- Wajib ada baris dengan label mengandung `Kompensasi Tenaga Kerja`,
  `Surplus Usaha Bruto`, `Pajak Dikurang Subsidi Lainnya Atas Produksi`,
  `Pajak Dikurang Subsidi Atas Produk`, dan salah satu
  `Total Konsumsi Antara Impor` atau `Input Antara (Impor)`

### Sheet `labour`

- Satu kolom numerik berisi jumlah tenaga kerja per sektor (urutan
  sesuai `sector`)

### Sheet `land_proportion`

- Matriks proporsi lahan (Sektor x Kelas Lahan)
- Baris = sektor, kolom = kelas tutupan lahan
- Total setiap kolom harus 1 (atau 100%)

## Struktur File Laju Pertumbuhan

| Kolom         | Deskripsi                                            |
|---------------|------------------------------------------------------|
| `Sector`      | Nama sektor (harus sama dengan sheet `sector`)       |
| `Growth_Rate` | Laju pertumbuhan tahunan dalam desimal (misal 0.035) |

## Struktur File Lookup Tutupan Lahan

| Kolom  | Deskripsi                                           |
|--------|-----------------------------------------------------|
| `id`   | ID kelas lahan (harus sesuai raster atau referensi) |
| `lc`   | Nama kelas lahan                                    |
| `area` | Luas dalam hektar                                   |

## Template

Template kosong tersedia di `inst/extdata/template/template_io.xlsx`.
Contoh data tersedia di `inst/extdata/example_data/`.

## Alias Nama Komponen

Karena penamaan komponen dapat berbeda antar sumber data, fungsi
[`find_row_alias()`](https://icraf-indonesia.github.io/TARegEco/reference/find_row_alias.md)
mengenali beberapa alias:

- `Kompensasi Tenaga Kerja` = `Upah dan Gaji`
- `Surplus Usaha Bruto` = `Keuntungan`
- `Total Konsumsi Antara Impor` = `Input Antara (Impor)`
- `Pajak Dikurang Subsidi Atas Produk` =
  `Pajak Neto Subsidi Atas Produk`

Alias lengkap dapat dilihat di glosarium.
