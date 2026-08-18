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