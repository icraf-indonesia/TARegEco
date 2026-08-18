#' Export descriptive analysis outputs to Excel
#'
#' Writes all main results of the descriptive Input-Output analysis into a
#' single Excel workbook. The workbook contains the following sheets:
#'
#' - `GDP`: GDP per sector table
#' - `Leontief_Matrix`: Leontief inverse matrix
#' - `Growth_Rate`: sectoral growth rates
#' - `Multiplier_Output`, `Multiplier_Income`, `Multiplier_Labour`,
#'   `Multiplier_SUB`: multiplier tables
#' - `Land_Proportion`: land proportion matrix
#' - `Linkage`: linkage table (DBL, DFL, TBL, TFL, ITBL, ITFL)
#' - `Full_IO_Table`: complete IO matrix (sectors + final demand + added value)
#' - `Code_Registry`: code registry for modules that need cell-level changes
#' - `Balance_Detail`: per-sector balance check table
#'
#' @param gdp_baseline Tibble returned by [calculate_gdp()].
#' @param linkage_result List returned by [compute_linkage()].
#' @param growth_rate Tibble or data frame of sectoral growth rates.
#' @param OM Output from [calc_output_multiplier()].
#' @param IM Output from [calc_income_multiplier()].
#' @param LM Output from [calc_labour_multiplier()].
#' @param SUBM Output from [calc_sub_multiplier()].
#' @param land_proportion Numeric matrix of land proportions.
#' @param linkage_table Tibble of linkage results (from [create_linkages_table()]).
#' @param full_io_table Numeric matrix from [build_full_io_table()].
#' @param code_registry Tibble from [build_full_io_table()].
#' @param balance_detail Tibble from [check_io_balance()].
#' @param output_path Character. Path to the output Excel file.
#'   Default is `"data/IO_Analysis_Output.xlsx"`.
#'
#' @return Invisibly returns the full path to the saved workbook.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook setColWidths
#'
#' @examples
#' \dontrun{
#' export_io_analysis(
#'   gdp_baseline = gdp_baseline,
#'   linkage_result = linkage_result,
#'   growth_rate = growth_rate,
#'   OM = OM, IM = IM, LM = LM, SUBM = SUBM,
#'   land_proportion = land_proportion,
#'   linkage_table = linkages_table,
#'   full_io_table = full_io_table,
#'   code_registry = code_registry,
#'   balance_detail = balance_result$balance_detail,
#'   output_path = "data/IO_Analysis_Output.xlsx"
#' )
#' }
#'
#' @export
export_io_analysis <- function(gdp_baseline,
                               linkage_result,
                               growth_rate,
                               OM, IM, LM, SUBM,
                               land_proportion,
                               linkage_table,
                               full_io_table,
                               code_registry,
                               balance_detail,
                               output_path = "data/IO_Analysis_Output.xlsx") {
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to export Excel files.")
  }
  
  # Ensure output directory exists
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  
  wb <- openxlsx::createWorkbook()
  
  # GDP
  openxlsx::addWorksheet(wb, "GDP")
  openxlsx::writeData(wb, sheet = "GDP", x = gdp_baseline)
  
  # Leontief matrix
  openxlsx::addWorksheet(wb, "Leontief_Matrix")
  openxlsx::writeData(wb, sheet = "Leontief_Matrix",
                      x = as.data.frame(linkage_result$Leontief),
                      rowNames = FALSE, colNames = FALSE)
  
  # Growth rate
  openxlsx::addWorksheet(wb, "Growth_Rate")
  openxlsx::writeData(wb, sheet = "Growth_Rate", x = as.data.frame(growth_rate))
  
  # Multiplier tables
  openxlsx::addWorksheet(wb, "Multiplier_Output")
  openxlsx::writeData(wb, sheet = "Multiplier_Output", x = OM$table)
  
  openxlsx::addWorksheet(wb, "Multiplier_Income")
  openxlsx::writeData(wb, sheet = "Multiplier_Income", x = IM$table)
  
  labour_table <- LM$table |>
    dplyr::mutate(
      Labour_Compensation = SUBM$compensation,
      Labour = as.numeric(unlist(labour))
    )
  
  openxlsx::addWorksheet(wb, "Multiplier_Labour")
  openxlsx::writeData(wb, sheet = "Multiplier_Labour", x = labour_table)
  
  sub_table <- SUBM$table |>
    dplyr::mutate(Surplus = SUBM$surplus)
  
  openxlsx::addWorksheet(wb, "Multiplier_SUB")
  openxlsx::writeData(wb, sheet = "Multiplier_SUB", x = sub_table)
  
  # Land proportion
  openxlsx::addWorksheet(wb, "Land_Proportion")
  openxlsx::writeData(wb, sheet = "Land_Proportion",
                      x = land_proportion,
                      rowNames = TRUE, colNames = TRUE)
  
  # Linkage
  openxlsx::addWorksheet(wb, "Linkage")
  openxlsx::writeData(wb, sheet = "Linkage", x = linkage_table)
  
  # Full IO table & code registry
  openxlsx::addWorksheet(wb, "Full_IO_Table")
  openxlsx::writeData(wb, sheet = "Full_IO_Table",
                      x = full_io_table,
                      rowNames = TRUE, colNames = TRUE)
  
  openxlsx::addWorksheet(wb, "Code_Registry")
  openxlsx::writeData(wb, sheet = "Code_Registry", x = code_registry)
  
  # Balance detail
  if (!missing(balance_detail) && !is.null(balance_detail)) {
    openxlsx::addWorksheet(wb, "Balance_Detail")
    openxlsx::writeData(wb, sheet = "Balance_Detail", x = balance_detail)
  }
  
  # Auto width
  for (s in names(wb)) {
    openxlsx::setColWidths(wb, sheet = s, cols = 1:100, widths = "auto")
  }
  
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
  invisible(output_path)
}

#' Save descriptive analysis results to RData / RDS
#'
#' Saves the main objects of the descriptive Input-Output analysis into a
#' single `.rds` file (or optionally `.RData`) for use by other modules
#' such as the projection module.
#'
#' @param sector Tibble of sectors.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param added_value_m Numeric matrix of added value.
#' @param labour Labour vector or one-column matrix/tibble.
#' @param land_proportion Numeric matrix of land proportions.
#' @param growth_rate Tibble or data frame of growth rates.
#' @param landuse_table Tibble with columns `id`, `lc`, `area`.
#' @param gdp_baseline Tibble returned by [calculate_gdp()].
#' @param linkage_result List returned by [compute_linkage()].
#' @param OM Output from [calc_output_multiplier()].
#' @param IM Output from [calc_income_multiplier()].
#' @param LM Output from [calc_labour_multiplier()].
#' @param SUBM Output from [calc_sub_multiplier()].
#' @param full_io_table Numeric matrix from [build_full_io_table()].
#' @param code_registry Tibble from [build_full_io_table()].
#' @param output_path Character. Path to the output file.
#'   Default is `"data/io_descriptive.rds"`.
#' @param format Character. Either `"rds"` (default) or `"rdata"`.
#'
#' @return Invisibly returns the full path to the saved file.
#'
#' @examples
#' \dontrun{
#' save_analysis_data(
#'   sector = sector,
#'   intermediate_consumption_m = intermediate_consumption.m,
#'   final_demand_m = final_demand.m,
#'   added_value_m = added_value.m,
#'   labour = labour,
#'   land_proportion = land_proportion,
#'   growth_rate = growth_rate,
#'   landuse_table = landuse_table,
#'   gdp_baseline = gdp_baseline,
#'   linkage_result = linkage_result,
#'   OM = OM, IM = IM, LM = LM, SUBM = SUBM,
#'   full_io_table = full_io_table,
#'   code_registry = code_registry,
#'   output_path = "data/io_descriptive.rds"
#' )
#' }
#'
#' @export
save_analysis_data <- function(sector,
                               intermediate_consumption_m,
                               final_demand_m,
                               added_value_m,
                               labour,
                               land_proportion,
                               growth_rate,
                               landuse_table,
                               gdp_baseline,
                               linkage_result,
                               OM, IM, LM, SUBM,
                               full_io_table,
                               code_registry,
                               output_path = "data/io_descriptive.rds",
                               format = c("rds", "rdata")) {
  
  format <- match.arg(format)
  
  io <- list(
    sector                  = sector,
    intermediate_consumption= intermediate_consumption_m,
    final_demand            = final_demand_m,
    added_value             = added_value_m,
    labour                  = labour,
    land_proportion         = land_proportion,
    growth_rate             = growth_rate,
    landuse_table           = landuse_table,
    gdp_baseline            = gdp_baseline,
    linkage                 = linkage_result,
    multipliers = list(
      output  = OM$table,
      income  = IM$table,
      labour  = LM$table,
      sub     = SUBM$table
    ),
    full_io_table           = full_io_table,
    code_registry           = code_registry
  )
  
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  
  if (format == "rds") {
    saveRDS(io, file = output_path)
  } else {
    save(io, file = output_path)
  }
  
  invisible(output_path)
}