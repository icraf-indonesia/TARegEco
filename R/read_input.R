#' Read input file (Excel or CSV) into a tibble or matrix
#'
#' Reads an input file from Excel (`.xlsx`, `.xls`) or CSV (`.csv`, `.txt`),
#' with optional conversion to a numeric matrix.
#'
#' @param path Character. Path to the input file.
#' @param sheet Character or `NULL`. Sheet name for Excel files. If `NULL`,
#'   the first sheet is used.
#' @param header Logical. If `TRUE`, the first row is treated as column names.
#' @param as_matrix Logical. If `TRUE`, the data is returned as a matrix.
#'
#' @return A `tibble` if `as_matrix = FALSE`, or a `matrix` if `TRUE`.
#'
#' @details
#' The function automatically detects the file extension. For Excel files,
#' the `readxl` package is used. For CSV/TXT files, `utils::read.csv` is used.
#'
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom tibble as_tibble
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' sector <- read_input_file("data/io.xlsx", sheet = "sector", header = TRUE)
#' ic <- read_input_file("data/io.xlsx", sheet = "intermediate_consumption",
#'                        as_matrix = TRUE)
#' }
#'
#' @export
read_input_file <- function(path, sheet = NULL, header = FALSE, as_matrix = FALSE) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required for Excel files.")
    }
    data <- readxl::read_excel(path, sheet = sheet, col_names = header)
  } else if (ext %in% c("csv", "txt")) {
    data <- utils::read.csv(path, header = header)
  } else {
    stop("Unsupported file format. Please upload CSV or Excel.")
  }
  
  data <- tibble::as_tibble(data)
  
  if (as_matrix) {
    return(as.matrix(data))
  }
  
  return(data)
}