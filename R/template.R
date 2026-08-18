#' Create Input-Output template Excel file
#'
#' Generates an Excel template with blank sheets that match the expected
#' structure of the Input-Output model. This template helps users prepare
#' their own data correctly.
#'
#' @param output_path Character. Path to the output `.xlsx` file.
#'
#' @return Invisibly returns the output path.
#'
#' @details
#' The template contains the following sheets:
#' - `sector`: 3 columns (Sector, Category, Land-based)
#' - `intermediate_consumption`: blank matrix (users fill with numeric values)
#' - `final_demand`: first two columns for component code and label,
#'   followed by empty columns for each sector
#' - `added_value`: first two columns for component code and label,
#'   followed by empty columns for each sector
#' - `labour`: one numeric column
#' - `land_proportion`: blank matrix where column sums should be 1
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @examples
#' \dontrun{
#' create_io_template("inst/extdata/template/template_io.xlsx")
#' }
#'
#' @export
create_io_template <- function(output_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to create Excel template.")
  }
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "sector")
  openxlsx::writeData(wb, "sector", data.frame(
    Sector = character(),
    Category = character(),
    Land_based = logical()
  ))
  
  openxlsx::addWorksheet(wb, "intermediate_consumption")
  openxlsx::writeData(wb, "intermediate_consumption", matrix(NA, 5, 5),
                      colNames = FALSE, rowNames = FALSE)
  
  openxlsx::addWorksheet(wb, "final_demand")
  openxlsx::writeData(wb, "final_demand", data.frame(
    Code = integer(),
    Label = character(),
    S1 = numeric(), S2 = numeric(), S3 = numeric(), S4 = numeric(), S5 = numeric()
  ), colNames = TRUE, rowNames = FALSE)
  
  openxlsx::addWorksheet(wb, "added_value")
  openxlsx::writeData(wb, "added_value", data.frame(
    Code = integer(),
    Label = character(),
    S1 = numeric(), S2 = numeric(), S3 = numeric(), S4 = numeric(), S5 = numeric()
  ), colNames = TRUE, rowNames = FALSE)
  
  openxlsx::addWorksheet(wb, "labour")
  openxlsx::writeData(wb, "labour", data.frame(Labour = numeric()))
  
  openxlsx::addWorksheet(wb, "land_proportion")
  openxlsx::writeData(wb, "land_proportion", matrix(NA, 5, 3),
                      colNames = FALSE, rowNames = FALSE)
  
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
  
  invisible(output_path)
}

#' Create example dataset for Input-Output analysis
#'
#' Generates a small, self-contained example dataset that can be used to test
#' the descriptive analysis module.
#'
#' @param output_dir Character. Directory where example files should be saved.
#'
#' @return Invisibly returns the output directory.
#'
#' @details
#' The example dataset consists of:
#' - `Input_IO_example.xlsx`: sector, intermediate_consumption,
#'   final_demand, added_value, labour, land_proportion
#' - `laju_pertumbuhan_example.xlsx`: growth rate table
#' - `landcover_lookuptable_example.xlsx`: land cover lookup table with
#'   columns `id`, `lc`, `area`
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @examples
#' \dontrun{
#' create_example_data("inst/extdata/example_data")
#' }
#'
#' @export
create_example_data <- function(output_dir) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # --- Sectors ---
  sector <- tibble::tibble(
    Sector = c("Padi", "Jagung", "Industri Makanan", "Perdagangan", "Jasa"),
    Category = c("Pertanian", "Pertanian", "Industri", "Perdagangan", "Jasa"),
    Land_based = c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  
  n <- nrow(sector)
  
  # --- Intermediate consumption (random but plausible) ---
  set.seed(123)
  ic <- matrix(round(runif(n*n, 5, 30), 1), nrow = n, ncol = n)
  diag(ic) <- round(runif(n, 2, 8), 1)
  
  # --- Final demand components ---
  fd_comp <- data.frame(
    Code = c(301, 302, 303),
    Label = c("Konsumsi Rumah Tangga", "Pengeluaran Pemerintah", "Ekspor")
  )
  fd_mat <- matrix(round(runif(n * nrow(fd_comp), 10, 50), 1),
                   nrow = n, ncol = nrow(fd_comp))
  
  # --- Added value components ---
  av_comp <- data.frame(
    Code = c(200, 201, 202, 203, 195),
    Label = c("Total Konsumsi Antara Impor", "Kompensasi Tenaga Kerja",
              "Surplus Usaha Bruto", "Pajak Dikurang Subsidi Lainnya Atas Produksi",
              "Pajak Dikurang Subsidi Atas Produk")
  )
  av_mat <- matrix(round(runif(nrow(av_comp) * n, 3, 15), 1),
                   nrow = nrow(av_comp), ncol = n)
  
  # --- Labour ---
  labour <- round(runif(n, 500, 2000))
  
  # --- Land proportion (5 sectors x 3 land classes) ---
  land_prop_raw <- matrix(round(runif(n * 3, 0.1, 0.7), 2), nrow = n, ncol = 3)
  land_prop <- sweep(land_prop_raw, 2, colSums(land_prop_raw), "/")
  
  # --- Save Input_IO_example.xlsx ---
  io_path <- file.path(output_dir, "Input_IO_example.xlsx")
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "sector")
  openxlsx::writeData(wb, "sector", sector)
  
  openxlsx::addWorksheet(wb, "intermediate_consumption")
  openxlsx::writeData(wb, "intermediate_consumption", ic, colNames = FALSE, rowNames = FALSE)
  
  openxlsx::addWorksheet(wb, "final_demand")
  fd_df <- cbind(fd_comp, as.data.frame(t(fd_mat)))
  names(fd_df)[-c(1,2)] <- paste0("S", 1:n)
  openxlsx::writeData(wb, "final_demand", fd_df)
  
  openxlsx::addWorksheet(wb, "added_value")
  av_df <- cbind(av_comp, as.data.frame(t(av_mat)))
  names(av_df)[-c(1,2)] <- paste0("S", 1:n)
  openxlsx::writeData(wb, "added_value", av_df)
  
  openxlsx::addWorksheet(wb, "labour")
  openxlsx::writeData(wb, "labour", data.frame(Labour = labour))
  
  openxlsx::addWorksheet(wb, "land_proportion")
  openxlsx::writeData(wb, "land_proportion", land_prop, colNames = FALSE, rowNames = FALSE)
  
  openxlsx::saveWorkbook(wb, file = io_path, overwrite = TRUE)
  
  # --- Growth rate example ---
  growth_df <- tibble::tibble(
    Sector = sector$Sector,
    Growth_Rate = round(runif(n, 0.02, 0.05), 4)
  )
  openxlsx::write.xlsx(growth_df,
                       file = file.path(output_dir, "laju_pertumbuhan_example.xlsx"),
                       overwrite = TRUE)
  
  # --- Land cover lookup table example ---
  landcover_df <- tibble::tibble(
    id = 1:3,
    lc = c("Sawah", "Kebun", "Hutan"),
    area = c(15000, 8500, 22000)  # hectare
  )
  openxlsx::write.xlsx(landcover_df,
                       file = file.path(output_dir, "landcover_lookuptable_example.xlsx"),
                       overwrite = TRUE)
  
  invisible(output_dir)
}