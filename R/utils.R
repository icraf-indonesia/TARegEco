#' Create projection period table
#'
#' Creates a data frame containing projection periods based on the initial
#' year, final year, and time step.
#'
#' @param initial_year Numeric. First year of the projection.
#' @param end_year Numeric. Last year of the projection.
#' @param time_step Numeric. Interval between projection periods.
#'
#' @return A data frame with columns:
#'   \item{Period}{Character label, e.g., "2024-2029"}
#'   \item{Start_Year}{Numeric start year}
#'   \item{End_Year}{Numeric end year}
#'
#' @examples
#' \dontrun{
#' create_period_table(2024, 2049, 5)
#' }
#'
#' @export
create_period_table <- function(initial_year, end_year, time_step) {
  start_year <- seq(initial_year, end_year - time_step, by = time_step)
  end_period <- start_year + time_step
  
  data.frame(
    Period = paste0(start_year, "-", end_period),
    Start_Year = start_year,
    End_Year = end_period,
    stringsAsFactors = FALSE
  )
}

#' Create area matrix from long projection data
#'
#' Converts a long data frame of land cover areas into a matrix where rows are
#' land cover IDs and columns are projection years.
#'
#' @param projection_long A data frame with columns `ID`, `YEAR`, and `AREA`.
#'   Usually created using `tidyr::pivot_longer()`.
#'
#' @return A numeric matrix with:
#'   \item{rows}{Land cover IDs}
#'   \item{columns}{Projection years (as character)}
#'
#' @examples
#' \dontrun{
#' area_matrix <- create_area_matrix(projection_long)
#' }
#'
#' @export
create_area_matrix <- function(projection_long) {
  years <- sort(unique(projection_long$YEAR))
  ids <- sort(unique(projection_long$ID))
  
  area_matrix <- matrix(
    0,
    nrow = length(ids),
    ncol = length(years),
    dimnames = list(ids, years)
  )
  
  for (i in seq_len(nrow(projection_long))) {
    area_matrix[
      as.character(projection_long$ID[i]),
      as.character(projection_long$YEAR[i])
    ] <- projection_long$AREA[i]
  }
  
  area_matrix
}