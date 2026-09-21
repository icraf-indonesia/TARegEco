#' Compute technical coefficient matrix and Leontief inverse
#'
#' Computes the technical coefficient matrix (A) and the Leontief inverse
#' \eqn{(I - A)^{-1}} using validated added value components rather than
#' raw column sums.
#'
#' @param intermediate_consumption_m Numeric matrix of intermediate
#'   consumption (Sector x Sector).
#' @param added_value_m Numeric matrix of added value (Component x Sector).
#'
#' @return A list with elements:
#'   \item{A}{Technical coefficient matrix}
#'   \item{Leontief}{Leontief inverse matrix}
#'   \item{inverse_total_input}{Vector of inverse total input per sector}
#'
#' @details
#' Total input is computed using [get_added_value_components()], which
#' identifies only the relevant primary-input rows (compensation, surplus,
#' taxes, imports) instead of summing all rows of the added value matrix.
#'
#' @references
#' Miller, R.E. & Blair, P.D. (2009). *Input-Output Analysis: Foundations and
#' Extensions*. Eurostat (2008). *Manual of Supply, Use and Input-Output Tables*.
#'
#' @examples
#' \dontrun{
#' leontief_result <- compute_leontief(ic, av)
#' }
#'
#' @export
compute_leontief <- function(intermediate_consumption_m, added_value_m) {
  
  if (!is.matrix(intermediate_consumption_m)) stop("intermediate_consumption_m must be a matrix.")
  if (!is.matrix(added_value_m)) stop("added_value_m must be a matrix.")
  
  # Total input from validated added value components
  comp <- get_added_value_components(intermediate_consumption_m, added_value_m)
  total_input <- comp$total_input
  
  inverse_total_input <- 1 / total_input
  inverse_total_input[is.infinite(inverse_total_input)] <- 0
  
  t_input_inverse <- diag(inverse_total_input)
  A <- intermediate_consumption_m %*% t_input_inverse
  I <- diag(ncol(intermediate_consumption_m))
  Leontief <- solve(I - A)
  
  list(
    A = A,
    Leontief = Leontief,
    inverse_total_input = inverse_total_input
  )
}

#' Compute linkage indices and Leontief inverse
#'
#' Computes technical coefficient matrix (A), Leontief inverse, and
#' backward/forward linkage indices (direct, total, and index).
#'
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param added_value_m Numeric matrix of added value.
#'
#' @return A list with elements: `A`, `Leontief`, `DFL`, `TFL`, `ITFL`,
#'   `DBL`, `TBL`, `ITBL`, `DIF_TFL_DFL`, `DIF_TBL_DBL`, and
#'   `inverse_total_input`.
#'
#' @details
#' This function calls [compute_leontief()] to obtain A and Leontief,
#' then computes direct and total backward/forward linkage.
#'
#' @references
#' Miller, R.E. & Blair, P.D. (2009). *Input-Output Analysis: Foundations and
#' Extensions*. Rasmussen, P.N. (1956). *Studies in Inter-sectoral Relations*.
#'
#' @examples
#' \dontrun{
#' linkage <- compute_linkage(ic, av)
#' }
#'
#' @export
compute_linkage <- function(intermediate_consumption_m, added_value_m) {
  leontief_result <- compute_leontief(intermediate_consumption_m, added_value_m)
  
  A <- leontief_result$A
  Leontief <- leontief_result$Leontief
  inverse_total_input <- leontief_result$inverse_total_input
  
  dim_n <- ncol(A)
  
  dfl_value <- rowSums(A)
  tfl_value <- rowSums(Leontief)
  itfl_value <- (tfl_value * dim_n) / sum(tfl_value)
  dbl_value <- colSums(A)
  tbl_value <- colSums(Leontief)
  itbl_value <- (tbl_value * dim_n) / sum(tbl_value)
  
  list(
    A = A,
    Leontief = Leontief,
    DFL = dfl_value,
    TFL = tfl_value,
    ITFL = itfl_value,
    DIF_TFL_DFL = abs(tfl_value - dfl_value),
    DBL = dbl_value,
    TBL = tbl_value,
    ITBL = itbl_value,
    DIF_TBL_DBL = abs(tbl_value - dbl_value),
    inverse_total_input = inverse_total_input
  )
}

#' Create linkage table
#'
#' Creates a tibble containing direct, total, and index linkage values for
#' each sector.
#'
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param linkage_result A list returned by [compute_linkage()].
#'
#' @return A tibble with columns: ID, SECTOR, CATEGORY, DFL, TFL, ITFL,
#'   DBL, TBL, ITBL, |TFL-DFL|, |TBL-DBL|.
#'
#' @export
create_linkages_table <- function(sector, linkage_result) {
  tibble::tibble(
    ID = seq_len(nrow(sector)),
    SECTOR = sector[[1]],
    CATEGORY = sector[[2]],
    DFL = linkage_result$DFL,
    TFL = linkage_result$TFL,
    ITFL = linkage_result$ITFL,
    DBL = linkage_result$DBL,
    TBL = linkage_result$TBL,
    ITBL = linkage_result$ITBL,
    `|TFL-DFL|` = linkage_result$DIF_TFL_DFL,
    `|TBL-DBL|` = linkage_result$DIF_TBL_DBL
  )
}

#' Select key sectors (sektor unggulan)
#'
#' Selects sectors with `ITBL > 1` **and** `ITFL > 1`, following BPS practice:
#' "Suatu industri dikategorikan sebagai Industri Unggulan jika memiliki IBL
#' dan IFL lebih dari 1 (satu)."
#'
#' @param linkages_table A tibble produced by [create_linkages_table()].
#'
#' @return A tibble containing key sectors.
#'
#' @details
#' The threshold of 1 belongs to the **normalised** indices ITBL and ITFL
#' (BPS: Indeks Daya Penyebaran and Indeks Derajat Kepekaan), which are
#' constructed so their mean is exactly 1. The direct linkages DBL and DFL
#' must not be tested against 1: `DBL = colSums(A)` is the intermediate input
#' share of total input, so it is strictly below 1 whenever value added is
#' non-negative, which makes the test unsatisfiable.
#'
#' @references
#' BPS, *Publikasi Tabel Input-Output 2016* (provincial series).
#' Rasmussen, P.N. (1956). *Studies in Inter-sectoral Relations*.
#'
#' @export
select_primary_sectors <- function(linkages_table) {
  primary_sector <- subset(linkages_table, ITBL > 1 & ITFL > 1)
  rownames(primary_sector) <- NULL
  primary_sector
}

#' Classify sectors into quadrants
#'
#' Classifies sectors into quadrants I-IV from the normalised linkage indices
#' ITBL (BPS: Indeks Daya Penyebaran) and ITFL (Indeks Derajat Kepekaan),
#' following the quadrant scheme published by BPS.
#'
#' @param linkages_table A tibble produced by [create_linkages_table()].
#'
#' @return A tibble with an additional `Quadrant` column.
#'
#' @details
#' BPS quadrant scheme:
#' \itemize{
#'   \item Kuadran I   (ITBL > 1, ITFL > 1): industri unggulan
#'   \item Kuadran II  (ITBL > 1, ITFL < 1): potensi unggul
#'   \item Kuadran III (ITBL < 1, ITFL > 1): kurang diunggulkan
#'   \item Kuadran IV  (ITBL < 1, ITFL < 1): tertinggal
#' }
#'
#' @references
#' BPS Aceh, *Publikasi Tabel I-O 2016*, "Pemetaan Industri Menurut Kuadran".
#'
#' @export
classify_quadrant <- function(linkages_table) {
  linkages_table$Quadrant <- dplyr::case_when(
    linkages_table$ITBL > 1 & linkages_table$ITFL > 1 ~ "Kuadran I",
    linkages_table$ITBL > 1 & linkages_table$ITFL < 1 ~ "Kuadran II",
    linkages_table$ITBL < 1 & linkages_table$ITFL > 1 ~ "Kuadran III",
    linkages_table$ITBL < 1 & linkages_table$ITFL < 1 ~ "Kuadran IV",
    TRUE ~ "Undefined"
  )
  linkages_table
}