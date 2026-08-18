test_that("read_input_file membaca file CSV dengan benar", {
  # Buat file CSV sementara
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:3, b = letters[1:3]), tmp, row.names = FALSE)
  
  result <- read_input_file(tmp, header = TRUE)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("a", "b"))
})