#' Calculate GDP per sector
#'
#' Computes GDP, output, land requirement, LPC, final demand, and total
#' output from the Leontief model for each sector.
#'
#' @param added_value_m Numeric matrix of added value.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param land_proportion Numeric matrix of land proportion (Sector x LC).
#' @param landuse_table Data frame or tibble with an `area` column
#'   representing land area in hectares.
#' @param leontief Numeric Leontief inverse matrix.
#'
#' @return A tibble with columns: SECTOR, CATEGORY, GDP, OUTPUT, GDP_PROP,
#'   Land_Requirement, LPC, FD, Prop_FD, Total_OUTPUT.
#'
#' @examples
#' \dontrun{
#' gdp <- calculate_gdp(av, ic, fd, sector, land_prop, landuse_table, L)
#' }
#'
#' @export
calculate_gdp <- function(added_value_m,
                          intermediate_consumption_m,
                          final_demand_m,
                          sector,
                          land_proportion,
                          landuse_table,
                          leontief) {
  
  landuse_area <- as.numeric(landuse_table$area)
  landuse_area_diag <- diag(landuse_area)
  land_requirement <- land_proportion %*% landuse_area_diag
  
  component <- get_added_value_components(intermediate_consumption_m, added_value_m)
  gdp_val <- component$gdp_val
  output_val <- rowSums(final_demand_m) + rowSums(intermediate_consumption_m)
  
  GDP <- tibble::tibble(
    SECTOR = sector[[1]],
    CATEGORY = sector[[2]],
    GDP = as.numeric(gdp_val),
    OUTPUT = as.numeric(output_val)
  )
  
  GDP$GDP_PROP         <- ifelse(GDP$OUTPUT == 0, 0, GDP$GDP / GDP$OUTPUT)
  GDP$Land_Requirement <- rowSums(land_requirement)
  GDP$LPC              <- GDP$OUTPUT / GDP$Land_Requirement
  GDP$FD               <- rowSums(final_demand_m)
  GDP$Prop_FD          <- GDP$FD / GDP$OUTPUT
  GDP$Total_OUTPUT     <- leontief %*% GDP$FD
  
  GDP
}