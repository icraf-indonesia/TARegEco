test_that("compute_leontief menghasilkan matriks yang benar", {
  ic <- matrix(c(20, 30, 40, 10), 2, 2)
  av <- make_added_value(
    tax_product  = c(5, 5),
    import       = c(10, 10),
    compensation = c(20, 15),
    surplus      = c(12, 17),
    other_tax    = c(3, 3)
  )

  res <- compute_leontief(ic, av)

  expect_true(is.matrix(res$A))
  expect_true(is.matrix(res$Leontief))
  expect_equal(nrow(res$A), 2)
  expect_equal(ncol(res$Leontief), 2)

  # A = Z / total input, total input = colSums(ic) + colSums(av) = 100 per sector
  expect_equal(res$A, ic / 100, ignore_attr = TRUE)
})

test_that("get_added_value_components melaporkan baris yang hilang dengan jelas", {
  ic <- matrix(c(20, 30, 40, 10), 2, 2)

  # Matriks tanpa dimnames tidak dapat dipetakan ke komponen input primer
  av_no_names <- matrix(c(50, 40, 20, 30), 2, 2)
  expect_error(
    get_added_value_components(ic, av_no_names),
    "were not found"
  )

  # Satu baris wajib hilang (Surplus Usaha Bruto)
  av <- make_added_value(
    tax_product  = c(5, 5),
    import       = c(10, 10),
    compensation = c(20, 15),
    surplus      = c(12, 17),
    other_tax    = c(3, 3)
  )
  av_missing <- av[rownames(av) != "Surplus Usaha Bruto", , drop = FALSE]
  expect_error(
    get_added_value_components(ic, av_missing),
    "Surplus Usaha Bruto"
  )
})

test_that("find_row_alias mengembalikan NULL bila tidak ada yang cocok", {
  m <- matrix(1:4, 2, 2, dimnames = list(c("Kompensasi Tenaga Kerja", "Lainnya"), NULL))

  expect_equal(find_row_alias(m, "Kompensasi Tenaga Kerja"), "Kompensasi Tenaga Kerja")
  expect_null(find_row_alias(m, "Tidak Ada"))
  expect_null(find_row_alias(matrix(1:4, 2, 2), "Kompensasi Tenaga Kerja"))
})
