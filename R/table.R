#' Create wide comparison table
#'
#' Converts long projection data into a wide table where each scenario becomes
#' a column. This format is suitable for display in reports and dashboards.
#'
#' @param data A data frame with at least the columns `YEAR`, `SCENARIO`, and
#'   the column specified in `value_col`.
#' @param value_col Character. Name of the indicator column to be spread across
#'   scenarios (e.g., `"GDP"`, `"LABOUR"`, `"INCOME"`, `"SURPLUS"`, `"ITFL"`,
#'   `"ITBL"`).
#'
#' @return A tibble with columns:
#'   \item{YEAR}{Projection year}
#'   \item{...}{One column for each scenario, containing the selected value}
#'
#' @examples
#' \dontrun{
#' gdp_wide <- create_comparison_table(gdp_comparison, "GDP")
#' }
#'
#' @export
create_comparison_table <- function(data, value_col) {
  data |>
    dplyr::select(YEAR, SCENARIO, dplyr::all_of(value_col)) |>
    tidyr::pivot_wider(
      names_from = SCENARIO,
      values_from = dplyr::all_of(value_col)
    ) |>
    dplyr::arrange(YEAR)
}