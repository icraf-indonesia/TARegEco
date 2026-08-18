#' Check Input-Output balance
#'
#' Computes total input and total output per sector, the difference, and
#' balance status based on a given tolerance.
#'
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param added_value_m Numeric matrix of added value.
#' @param sector_names Character vector of sector names. Optional; if `NULL`,
#'   column names of `intermediate_consumption_m` are used.
#' @param tolerance_percent Numeric. Threshold for balance check in percent
#'   (default = 1).
#' @param tolerance_value Numeric. Not used; kept for backward compatibility.
#'
#' @return A list with aggregate values (`status`, `total_input`, `total_output`,
#'   `gap`, `gap_percent`) and per-sector details (`balance_detail`,
#'   `total_input_per_sector`, `total_output_per_sector`, `gap_per_sector`,
#'   `gap_percent_per_sector`).
#'
#' @examples
#' \dontrun{
#' bal <- check_io_balance(ic, fd, av, sector_names = sector$name)
#' }
#'
#' @export
check_io_balance <- function(intermediate_consumption_m,
                             final_demand_m,
                             added_value_m,
                             sector_names = NULL,
                             tolerance_percent = 1,
                             tolerance_value = 1) {
  if (!is.matrix(intermediate_consumption_m)) stop("intermediate_consumption_m must be a matrix")
  if (!is.matrix(final_demand_m)) stop("final_demand_m must be a matrix")
  if (!is.matrix(added_value_m)) stop("added_value_m must be a matrix")
  
  total_output <- rowSums(intermediate_consumption_m, na.rm = TRUE) +
    rowSums(final_demand_m, na.rm = TRUE)
  component <- get_added_value_components(intermediate_consumption_m, added_value_m)
  total_input <- component$total_input
  
  gap_per_sector <- total_input - total_output
  gap_percent_per_sector <- ifelse(total_output == 0, 0,
                                   abs(gap_per_sector / total_output) * 100)
  sector_status <- ifelse(gap_percent_per_sector <= tolerance_percent,
                          "BALANCED", "NOT BALANCED")
  
  if (is.null(sector_names)) {
    sector_names <- colnames(intermediate_consumption_m)
    if (is.null(sector_names))
      sector_names <- paste0("Sector ", seq_len(ncol(intermediate_consumption_m)))
  }
  
  balance_detail <- tibble::tibble(
    Sector = sector_names,
    Total_Input = total_input,
    Total_Output = total_output,
    Difference = gap_per_sector,
    Difference_Percent = gap_percent_per_sector,
    Status = sector_status
  )
  
  overall_input <- sum(total_input, na.rm = TRUE)
  overall_output <- sum(total_output, na.rm = TRUE)
  overall_gap <- overall_input - overall_output
  overall_gap_percent <- ifelse(overall_output == 0, 0,
                                round(abs(overall_gap / overall_output) * 100, 2))
  status <- if (overall_gap_percent <= tolerance_percent) "BALANCED" else "NOT BALANCED"
  
  list(status = status,
       total_input = overall_input,
       total_output = overall_output,
       gap = overall_gap,
       gap_percent = overall_gap_percent,
       tolerance_value = tolerance_value,
       tolerance_percent = tolerance_percent,
       balance_detail = balance_detail,
       total_input_per_sector = total_input,
       total_output_per_sector = total_output,
       gap_per_sector = gap_per_sector,
       gap_percent_per_sector = gap_percent_per_sector)
}