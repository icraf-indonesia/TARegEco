#' Calculate GDP per sector with land productivity and land requirement coefficients
#'
#' Computes GDP, output, land requirement, Land Requirement Coefficient (LRC),
#' and Land Productivity Coefficient (LPC) for each sector.
#'
#' @param added_value_m Numeric matrix of added value.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param land_proportion Numeric matrix of land proportions (Sector x Land Cover).
#' @param landuse_table Data frame or tibble with an `area` column
#'   representing land area in hectares.
#' @param leontief Numeric Leontief inverse matrix.
#'
#' @return A tibble with columns: SECTOR, CATEGORY, GDP, OUTPUT,
#'   Land_Requirement, LRC, LPC, GDP_PROP, FD, Prop_FD, Total_OUTPUT.
#'
#' @details
#' Land Requirement Coefficient (LRC) and Land Productivity Coefficient (LPC)
#' follow Hubacek & Sun (2001):
#'
#' \deqn{LRC_i = \frac{Land_i}{X_i}}
#' \deqn{LPC_i = \frac{1}{LRC_i} = \frac{X_i}{Land_i}}
#'
#' where \eqn{X_i} is total sectoral output and \eqn{Land_i} is total land
#' requirement of sector i.
#'
#' @references
#' Hubacek, K., & Sun, L. (2001). A scenario analysis of China's land use and
#' land cover change: incorporating biophysical information into input-output
#' modeling. Structural Change and Economic Dynamics, 12(4), 367–397.
#'
#' @export
calculate_gdp <- function(added_value_m,
                          intermediate_consumption_m,
                          final_demand_m,
                          sector,
                          land_proportion,
                          landuse_table,
                          leontief) {
  
  # Land requirement matrix
  landuse_area <- as.numeric(landuse_table$area)
  landuse_area_diag <- diag(landuse_area)
  land_requirement <- land_proportion %*% landuse_area_diag
  
  # Total output per sector
  output_val <- rowSums(final_demand_m) + rowSums(intermediate_consumption_m)
  
  # Added value / GDP
  component <- get_added_value_components(intermediate_consumption_m, added_value_m)
  gdp_val <- component$gdp_val
  
  # Land Requirement Coefficient (ha per unit of output)
  land_req_total <- rowSums(land_requirement)
  LRC <- ifelse(output_val == 0, 0, land_req_total / output_val)
  
  # Land Productivity Coefficient (output per ha)
  LPC <- ifelse(LRC == 0, 0, 1 / LRC)
  
  GDP <- tibble::tibble(
    SECTOR = sector[[1]],
    CATEGORY = sector[[2]],
    GDP = as.numeric(gdp_val),
    OUTPUT = as.numeric(output_val),
    Land_Requirement = land_req_total,
    LRC = LRC,
    LPC = LPC
  )
  
  GDP$GDP_PROP <- ifelse(GDP$OUTPUT == 0, 0, GDP$GDP / GDP$OUTPUT)
  GDP$FD <- rowSums(final_demand_m)
  GDP$Prop_FD <- ifelse(GDP$OUTPUT == 0, 0, GDP$FD / GDP$OUTPUT)
  GDP$Total_OUTPUT <- leontief %*% GDP$FD
  
  GDP
}