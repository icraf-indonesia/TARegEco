# Builds a minimal added value matrix with the primary-input row labels that
# get_added_value_components() looks up. Each argument is one value per sector.
make_added_value <- function(tax_product, import, compensation, surplus, other_tax) {
  av <- rbind(tax_product, import, compensation, surplus, other_tax)
  rownames(av) <- c(
    "Pajak Dikurang Subsidi Atas Produk",
    "Total Konsumsi Antara Impor",
    "Kompensasi Tenaga Kerja",
    "Surplus Usaha Bruto",
    "Pajak Dikurang Subsidi Lainnya Atas Produksi"
  )
  colnames(av) <- NULL
  av
}
