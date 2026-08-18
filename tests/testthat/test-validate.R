test_that("validate_io_data mendeteksi ketidakcocokan nama sektor", {
  sector <- tibble::tibble(
    Nama_Sektor = c("Padi", "Jagung"),
    Kategori = c("Pertanian", "Pertanian")
  )
  
  ic <- matrix(1:4, 2, 2,
               dimnames = list(c("Padi", "Jagung"), c("Padi", "Jagung"))
  )
  fd <- matrix(1:4, 2, 2,
               dimnames = list(c("Padi", "Jagung"), c("Ekspor", "Konsumsi"))
  )
  av <- matrix(1:4, 2, 2,
               dimnames = list(c("Kompensasi Tenaga Kerja", "Surplus Usaha Bruto"),
                               c("Padi", "Jagung"))
  )
  labour <- c(100, 200)
  land_prop <- matrix(c(0.6, 0.4, 0.3, 0.7), 2, 2,
                      dimnames = list(c("Padi", "Jagung"), NULL)
  )
  
  # Lolos validasi
  expect_invisible(
    validate_io_data(sector, ic, fd, av, labour, land_prop)
  )
  
  # Nama sektor tidak cocok
  ic_bad <- ic
  colnames(ic_bad)[1] <- "Ubi"
  expect_error(
    validate_io_data(sector, ic_bad, fd, av, labour, land_prop),
    "Dimension names on col do not match sector list"
  )
})