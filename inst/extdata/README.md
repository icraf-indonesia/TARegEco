# Data Template dan Contoh

Folder ini berisi file-file pendukung untuk mempersiapkan data Input-Output
yang akan digunakan oleh modul TARegEco.

## Struktur

- `template/template_io.xlsx` — template kosong dengan sheet yang sesuai.
- `example_data/` — contoh data kecil (5 sektor) yang siap digunakan.

## Cara Menggunakan Template

1. Buka `template_io.xlsx`.
2. Isi setiap sheet sesuai panduan di `vignettes`.
3. Simpan dengan nama `Input_IO_<wilayah>.xlsx`.
4. Siapkan `laju_pertumbuhan_<wilayah>.xlsx` dan
   `landcover_lookuptable_<wilayah>.xlsx`.

## Cara Menggunakan Data Contoh

1. Salin folder `example_data` ke direktori kerja.
2. Sesuaikan path di `.Rmd` atau aplikasi Shiny.
3. Jalankan modul.

Detail lebih lanjut lihat `vignettes/panduan-input-data.Rmd`.