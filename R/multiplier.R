#' Output multiplier
#'
#' Output multiplier = column sums of Leontief inverse. Measures total output
#' generated in the economy per unit increase in final demand of a sector.
#'
#' @param Leontief Numeric Leontief inverse matrix.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param top_n Integer. Number of top sectors to include in the plot.
#'
#' @return A list with elements: `table`, `top`, `plot`.
#'
#' @export
calc_output_multiplier <- function(Leontief, sector, top_n = 20) {
  sector_name <- sector[[1]]
  sector_category <- sector[[2]]
  Out.multiplier <- colSums(Leontief)
  Out.multiplier <- tibble::tibble(
    SECTOR = sector_name,
    CATEGORY = sector_category,
    Out.multiplier = Out.multiplier
  )
  order_Out.multiplier <- Out.multiplier[order(-Out.multiplier$Out.multiplier), ]
  OMPL_graph <- ggplot(order_Out.multiplier,
                       aes(x = SECTOR, y = Out.multiplier, fill = CATEGORY)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip() +
    xlab("Sectors") +
    ylab("Output multiplier")
  list(table = Out.multiplier, top = order_Out.multiplier, plot = OMPL_graph)
}

#' Income multiplier
#'
#' Income multiplier = `colSums(Leontief %*% diag(income_coeff))`.
#' Measures total labour income generated economy-wide per unit increase
#' in final demand of a sector.
#'
#' @param Leontief Numeric Leontief inverse matrix.
#' @param total_output Numeric vector of total output per sector.
#' @param added_value Numeric matrix of added value.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param top_n Integer. Number of top sectors to include in the plot.
#'
#' @return A list with elements: `income`, `income_coeff`,
#'   `multiplier_matrix`, `table`, `top`, `plot`.
#'
#' @export
calc_income_multiplier <- function(Leontief, total_output, added_value, sector, top_n = 20) {
  sector_name <- sector[[1]]
  sector_category <- sector[[2]]
  if (!"Kompensasi Tenaga Kerja" %in% rownames(added_value))
    stop("Row 'Kompensasi Tenaga Kerja' not found in added_value matrix.")
  
  income_value <- as.numeric(added_value["Kompensasi Tenaga Kerja", ])
  total_output <- as.numeric(total_output)
  income_coeff <- ifelse(total_output == 0, 0, income_value / total_output)
  income_multiplier_matrix <- Leontief %*% diag(income_coeff)
  income_multiplier <- colSums(income_multiplier_matrix, na.rm = TRUE)
  
  Inc.multiplier <- tibble::tibble(
    SECTOR = sector_name,
    CATEGORY = sector_category,
    Inc.multiplier = income_multiplier
  )
  order_Inc.multiplier <- head(Inc.multiplier[order(-Inc.multiplier$Inc.multiplier), ], top_n)
  IMPL_graph <- ggplot(order_Inc.multiplier,
                       aes(x = reorder(SECTOR, Inc.multiplier),
                           y = Inc.multiplier, fill = CATEGORY)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip() +
    xlab("Sector") +
    ylab("Income Multiplier") +
    theme_minimal()
  
  list(income = income_value,
       income_coeff = income_coeff,
       multiplier_matrix = income_multiplier_matrix,
       table = Inc.multiplier,
       top = order_Inc.multiplier,
       plot = IMPL_graph)
}

#' Labour multiplier
#'
#' Labour multiplier = `colSums(Leontief %*% diag(labour_coeff))`.
#' Measures total employment generated economy-wide per unit increase
#' in final demand of a sector.
#'
#' @param Leontief Numeric Leontief inverse matrix.
#' @param labour Numeric vector of labour per sector.
#' @param total_output Numeric vector of total output per sector.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param top_n Integer. Number of top sectors to include in the plot.
#'
#' @return A list with elements: `labour`, `labour_coeff`,
#'   `multiplier_matrix`, `table`, `top`, `plot`.
#'
#' @export
calc_labour_multiplier <- function(Leontief, labour, total_output, sector, top_n = 20) {
  sector_name <- sector[[1]]
  sector_category <- sector[[2]]
  labour <- as.numeric(unlist(labour))
  total_output <- as.numeric(total_output)
  labour_coeff <- ifelse(total_output == 0, 0, labour / total_output)
  labour_multiplier_matrix <- Leontief %*% diag(labour_coeff)
  labour_multiplier <- colSums(labour_multiplier_matrix, na.rm = TRUE)
  
  Lab.multiplier <- tibble::tibble(
    SECTOR = sector_name,
    CATEGORY = sector_category,
    Lab.multiplier = labour_multiplier
  )
  order_Lab.multiplier <- head(Lab.multiplier[order(-Lab.multiplier$Lab.multiplier), ], top_n)
  LMPL_graph <- ggplot(order_Lab.multiplier,
                       aes(x = reorder(SECTOR, Lab.multiplier),
                           y = Lab.multiplier, fill = CATEGORY)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip() +
    xlab("Sector") +
    ylab("Labour Multiplier") +
    theme_minimal()
  
  list(labour = labour,
       labour_coeff = labour_coeff,
       multiplier_matrix = labour_multiplier_matrix,
       table = Lab.multiplier,
       top = order_Lab.multiplier,
       plot = LMPL_graph)
}

#' Surplus Usaha Bruto (SUB) multiplier
#'
#' SUB multiplier = `colSums(Leontief %*% diag(sub_coeff))`.
#' Measures total gross operating surplus generated economy-wide per unit
#' increase in final demand of a sector.
#'
#' @param Leontief Numeric Leontief inverse matrix.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param added_value_m Numeric matrix of added value.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param top_n Integer. Number of top sectors to include in the plot.
#'
#' @return A list with elements: `coefficient`, `total_input`, `surplus`,
#'   `table`, `top`, `plot`, `compensation`.
#'
#' @export
calc_sub_multiplier <- function(Leontief, intermediate_consumption_m,
                                added_value_m, sector, top_n = 20) {
  sector_name <- sector[[1]]
  sector_category <- sector[[2]]
  comp <- get_added_value_components(intermediate_consumption_m, added_value_m)
  total_input <- comp$total_input
  surplus <- comp$surplus
  compensation <- comp$compensation
  
  sub_coeff <- ifelse(total_input == 0, 0, surplus / total_input)
  sub_multiplier_matrix <- Leontief %*% diag(sub_coeff)
  sub_multiplier <- colSums(sub_multiplier_matrix, na.rm = TRUE)
  
  SUB.multiplier <- tibble::tibble(
    SECTOR = sector_name,
    CATEGORY = sector_category,
    SUB.multiplier = sub_multiplier
  )
  top_SUB <- head(SUB.multiplier[order(-SUB.multiplier$SUB.multiplier), ], top_n)
  SUB_graph <- ggplot(top_SUB,
                      aes(x = reorder(SECTOR, SUB.multiplier),
                          y = SUB.multiplier, fill = CATEGORY)) +
    geom_col(colour = "black") +
    coord_flip() +
    xlab("Sector") +
    ylab("SUB Multiplier") +
    theme_minimal()
  
  list(coefficient = sub_coeff,
       total_input = total_input,
       surplus = surplus,
       table = SUB.multiplier,
       top = top_SUB,
       plot = SUB_graph,
       compensation = compensation)
}

#' Combine multiplier tables
#'
#' Combines output, income, labour, and SUB multiplier tables into a single
#' tibble.
#'
#' @param OM Output from [calc_output_multiplier()].
#' @param IM Output from [calc_income_multiplier()].
#' @param LM Output from [calc_labour_multiplier()].
#' @param SUBM Output from [calc_sub_multiplier()].
#'
#' @return A tibble with columns: SECTOR, CATEGORY, Out.multiplier,
#'   Inc.multiplier, Lab.multiplier, SUB.multiplier.
#'
#' @export
combine_multipliers <- function(OM, IM, LM, SUBM) {
  multiplier <- OM$table |>
    dplyr::left_join(IM$table[, c("SECTOR", "Inc.multiplier")], by = "SECTOR") |>
    dplyr::left_join(LM$table[, c("SECTOR", "Lab.multiplier")], by = "SECTOR") |>
    dplyr::left_join(SUBM$table[, c("SECTOR", "SUB.multiplier")], by = "SECTOR")
  multiplier$Out.multiplier <- round(multiplier$Out.multiplier, 3)
  multiplier
}