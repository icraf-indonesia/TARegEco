#' Find row alias in matrix or data frame rownames
#'
#' Searches for the first row name that matches one of the provided aliases,
#' tolerating differences in capitalization and leading/trailing spaces.
#'
#' @param data A matrix or data frame with row names.
#' @param aliases Character vector. Candidate row names.
#'
#' @return The first matching row name, or `NULL` if none is found.
#'
#' @keywords internal
find_row_alias <- function(data, aliases) {
  available_rows <- trimws(tolower(rownames(data)))
  aliases_clean  <- trimws(tolower(aliases))
  idx <- match(aliases_clean, available_rows)
  found <- aliases[which(!is.na(idx))[1]]
  if (length(found) > 0) return(found[1])
  return(NULL)
}

#' Validate Input-Output data
#'
#' Checks dimension, sector names, data types, and missing values (NA)
#' across all matrices/tables that compose the Input-Output model.
#'
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param intermediate_consumption A numeric matrix of intermediate
#'   consumption (Sector x Sector).
#' @param final_demand A numeric matrix of final demand
#'   (Sector x Final Demand component).
#' @param added_value A numeric matrix of added value
#'   (Component x Sector).
#' @param labour A vector, one-column matrix, or one-column data frame/tibble
#'   containing labour per sector.
#' @param land_proportion A numeric matrix of land proportion
#'   (Sector x Land Cover Class).
#'
#' @return Returns `TRUE` invisibly if all inputs pass validation.
#'   Otherwise, stops with a clear error message.
#'
#' @details
#' This function is called after reading all input files and before any
#' computation begins. Sector order is aligned automatically based on the
#' sector names from `sector`.
#'
#' @examples
#' \dontrun{
#' validate_io_data(sector, ic, fd, av, labour, land_prop)
#' }
#'
#' @export
validate_io_data <- function(sector,
                             intermediate_consumption,
                             final_demand,
                             added_value,
                             labour,
                             land_proportion) {
  
  if (!is.data.frame(sector) || ncol(sector) < 2)
    stop("`sector` must be a data frame or tibble with at least 2 columns (name, category).")
  sector_names <- as.character(sector[[1]])
  n_sec <- length(sector_names)
  
  are_auto_names <- function(nms) {
    if (is.null(nms)) return(TRUE)
    default_pattern <- grepl("^(?:\\.\\.[0-9]+|\\.\\.\\.[0-9]+|[A-Za-z]+\\.?[0-9]+)$", nms)
    if (all(default_pattern)) return(TRUE)
    suppressWarnings({
      nums <- as.numeric(nms)
      if (!anyNA(nums)) return(TRUE)
    })
    FALSE
  }
  
  fix_dimnames <- function(mat, expected_names, dim = c("row", "col")) {
    dim <- match.arg(dim)
    current_names <- if (dim == "row") rownames(mat) else colnames(mat)
    
    if (is.null(current_names) || are_auto_names(current_names)) {
      if (dim == "row") rownames(mat) <- expected_names
      else colnames(mat) <- expected_names
    } else {
      if (!identical(sort(current_names), sort(expected_names)))
        stop(sprintf("Dimension names on %s do not match sector list. Current names: %s",
                     dim, paste(current_names, collapse = ", ")))
      if (dim == "row") mat <- mat[expected_names, , drop = FALSE]
      else mat <- mat[, expected_names, drop = FALSE]
    }
    mat
  }
  
  check_matrix <- function(mat, nrow_expected, ncol_expected, mat_name) {
    if (!is.matrix(mat))
      stop(sprintf("'%s' must be a matrix.", mat_name))
    if (nrow(mat) != nrow_expected)
      stop(sprintf("Number of rows in '%s' (%d) does not match number of sectors (%d).",
                   mat_name, nrow(mat), nrow_expected))
    if (ncol(mat) != ncol_expected)
      stop(sprintf("Number of columns in '%s' (%d) does not match expected (%d).",
                   mat_name, ncol(mat), ncol_expected))
    if (any(is.na(mat)))
      stop(sprintf("'%s' contains NA values. Please check the input.", mat_name))
    if (!is.numeric(mat))
      stop(sprintf("'%s' must be numeric.", mat_name))
  }
  
  intermediate_consumption <- fix_dimnames(intermediate_consumption, sector_names, "row")
  intermediate_consumption <- fix_dimnames(intermediate_consumption, sector_names, "col")
  check_matrix(intermediate_consumption, n_sec, n_sec, "intermediate_consumption")
  
  n_fd <- ncol(final_demand)
  if (n_fd < 1) stop("'final_demand' must have at least one column.")
  final_demand <- fix_dimnames(final_demand, sector_names, "row")
  check_matrix(final_demand, n_sec, n_fd, "final_demand")
  
  n_av <- nrow(added_value)
  if (n_av < 1) stop("'added_value' must have at least one row.")
  added_value <- fix_dimnames(added_value, sector_names, "col")
  check_matrix(added_value, n_av, n_sec, "added_value")
  
  labour_vec <- NULL
  if (is.matrix(labour)) {
    if (ncol(labour) != 1) stop("'labour' must be a one-column matrix or numeric vector.")
    labour_vec <- as.numeric(labour)
  } else if (is.data.frame(labour)) {
    if (ncol(labour) != 1) stop("'labour' must be a one-column data frame or tibble.")
    labour_vec <- as.numeric(labour[[1]])
  } else {
    labour_vec <- as.numeric(unlist(labour))
  }
  if (length(labour_vec) != n_sec)
    stop(sprintf("Length of labour vector (%d) does not match number of sectors (%d).",
                 length(labour_vec), n_sec))
  if (any(is.na(labour_vec)))
    stop("Labour data contains NA values.")
  
  land_proportion <- fix_dimnames(land_proportion, sector_names, "row")
  check_matrix(land_proportion, n_sec, ncol(land_proportion), "land_proportion")
  col_sums <- colSums(land_proportion)
  if (any(abs(col_sums - 1) > 1e-6))
    stop("Column sums of 'land_proportion' must be 1.0. Problem columns: ",
         paste(which(abs(col_sums - 1) > 1e-6), collapse = ", "))
  
  message("Input validation passed: ", n_sec, " sectors confirmed consistent.")
  invisible(TRUE)
}