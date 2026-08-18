test_that("multiplier fungsi menghasilkan panjang yang benar", {
  L <- matrix(c(1.2, 0.3, 0.4, 1.1), 2, 2)
  sector <- tibble::tibble(
    Nama_Sektor = c("A", "B"),
    Kategori = c("X", "Y")
  )
  
  OM <- calc_output_multiplier(L, sector)
  expect_equal(nrow(OM$table), 2)
  expect_true(all(c("SECTOR", "CATEGORY", "Out.multiplier") %in% names(OM$table)))
})