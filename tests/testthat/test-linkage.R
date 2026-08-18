test_that("compute_leontief menghasilkan matriks yang benar", {
  ic <- matrix(c(20, 30, 40, 10), 2, 2)
  av <- matrix(c(50, 40, 20, 30), 2, 2)
  
  res <- compute_leontief(ic, av)
  
  expect_true(is.matrix(res$A))
  expect_true(is.matrix(res$Leontief))
  expect_equal(nrow(res$A), 2)
  expect_equal(ncol(res$Leontief), 2)
})