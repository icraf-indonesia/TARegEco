#' Extract metadata from Input-Output components
#'
#' For each component (sector, intermediate_consumption, final_demand,
#' added_value, labour, land_proportion, landuse_table, growth_rate),
#' this function returns a tidy tibble with descriptive metadata:
#' - class, dimensions, missing values, min/max, names, hardcoded descriptions,
#'   and user‑provided units.
#'
#' @param sector A data frame/tibble with sector names in first column.
#' @param intermediate_consumption Numeric matrix.
#' @param final_demand Numeric matrix.
#' @param added_value Numeric matrix.
#' @param labour Vector, 1-column matrix, or 1-column data frame.
#' @param land_proportion Numeric matrix.
#' @param landuse_table Data frame/tibble with "LC" and "area".
#' @param growth_rate Numeric vector.
#' @param sector_units Character string for sector units (default NA).
#' @param ic_units Character string for intermediate consumption units (default NA).
#' @param fd_units Character string for final demand units (default NA).
#' @param av_units Character string for added value units (default NA).
#' @param labour_units Character string for labour units (default NA).
#' @param lp_units Character string for land proportion units (default NA).
#' @param lu_units Character string for landuse table units (default NA).
#' @param gr_units Character string for growth rate units (default NA).
#'
#' @return A list with two elements:
#'   \item{metadata}{A tibble with one row per component and columns for key attributes including Description and Units.}
#'   \item{details}{A list containing more detailed metadata for each component (e.g., names, per‑column summaries).}
#' @export
get_io_metadata <- function(sector,
                            intermediate_consumption,
                            final_demand,
                            added_value,
                            labour,
                            land_proportion,
                            landuse_table,
                            growth_rate,
                            sector_units = NA_character_,
                            ic_units = NA_character_,
                            fd_units = NA_character_,
                            av_units = NA_character_,
                            labour_units = NA_character_,
                            lp_units = NA_character_,
                            lu_units = NA_character_,
                            gr_units = NA_character_) {
  
  # Helper
  safe_attr <- function(obj, attr, default = NA) {
    tryCatch({
      val <- attr(obj, attr)
      if (is.null(val)) default else val
    }, error = function(e) default)
  }

  name_summary <- function(nms, max_show = 5) {
    if (is.null(nms) || length(nms) == 0) return("No names")
    n <- length(nms)
    if (n <= max_show) {
      paste(nms, collapse = ", ")
    } else {
      paste(paste(head(nms, max_show), collapse = ", "), "... and", n - max_show, "more")
    }
  }
  details <- list()
  
  # 1. Sector
  sector_meta <- list(
    class = class(sector)[1],
    nrow = if (is.data.frame(sector)) nrow(sector) else NA,
    ncol = if (is.data.frame(sector)) ncol(sector) else NA,
    sector_names = if (is.data.frame(sector) && ncol(sector) >= 1) name_summary(as.character(sector[[1]])) else NA,
    n_sectors = if (is.data.frame(sector) && ncol(sector) >= 1) length(unique(sector[[1]])) else NA,
    description = "Master sector list with categories and land-use flags",
    units = sector_units
  )
  details$sector <- sector_meta
  
  # 2. Intermediate consumption
  ic_meta <- list(
    class = class(intermediate_consumption)[1],
    is_matrix = is.matrix(intermediate_consumption),
    dim = if (is.matrix(intermediate_consumption)) dim(intermediate_consumption) else NA,
    rownames = if (is.matrix(intermediate_consumption)) name_summary(rownames(intermediate_consumption)) else NA,
    colnames = if (is.matrix(intermediate_consumption)) name_summary(colnames(intermediate_consumption)) else NA,
    n_na = if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption)) sum(is.na(intermediate_consumption)) else NA,
    min = if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption)) min(intermediate_consumption, na.rm = TRUE) else NA,
    max = if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption)) max(intermediate_consumption, na.rm = TRUE) else NA,
    mean = if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption)) mean(intermediate_consumption, na.rm = TRUE) else NA,
    description = "Intermediate consumption matrix (Sector × Sector)",
    units = ic_units
  )
  details$intermediate_consumption <- ic_meta
  
  # 3. Final demand
  fd_meta <- list(
    class = class(final_demand)[1],
    is_matrix = is.matrix(final_demand),
    dim = if (is.matrix(final_demand)) dim(final_demand) else NA,
    rownames = if (is.matrix(final_demand)) name_summary(rownames(final_demand)) else NA,
    colnames = if (is.matrix(final_demand)) name_summary(colnames(final_demand)) else NA,
    n_na = if (is.matrix(final_demand) && is.numeric(final_demand)) sum(is.na(final_demand)) else NA,
    min = if (is.matrix(final_demand) && is.numeric(final_demand)) min(final_demand, na.rm = TRUE) else NA,
    max = if (is.matrix(final_demand) && is.numeric(final_demand)) max(final_demand, na.rm = TRUE) else NA,
    mean = if (is.matrix(final_demand) && is.numeric(final_demand)) mean(final_demand, na.rm = TRUE) else NA,
    description = "Final demand matrix (Sector × Final Demand Components)",
    units = fd_units
  )
  details$final_demand <- fd_meta
  
  # 4. Added value
  av_meta <- list(
    class = class(added_value)[1],
    is_matrix = is.matrix(added_value),
    dim = if (is.matrix(added_value)) dim(added_value) else NA,
    rownames = if (is.matrix(added_value)) name_summary(rownames(added_value)) else NA,
    colnames = if (is.matrix(added_value)) name_summary(colnames(added_value)) else NA,
    n_na = if (is.matrix(added_value) && is.numeric(added_value)) sum(is.na(added_value)) else NA,
    min = if (is.matrix(added_value) && is.numeric(added_value)) min(added_value, na.rm = TRUE) else NA,
    max = if (is.matrix(added_value) && is.numeric(added_value)) max(added_value, na.rm = TRUE) else NA,
    mean = if (is.matrix(added_value) && is.numeric(added_value)) mean(added_value, na.rm = TRUE) else NA,
    description = "Value added matrix (Components × Sector)",
    units = av_units
  )
  details$added_value <- av_meta
  
  # 5. Labour
  labour_vec <- NULL
  if (is.matrix(labour) && ncol(labour) == 1) {
    labour_vec <- as.numeric(labour[, 1])
  } else if (is.data.frame(labour) && ncol(labour) == 1) {
    labour_vec <- as.numeric(labour[[1]])
  } else if (is.vector(labour)) {
    labour_vec <- as.numeric(labour)
  }
  labour_meta <- list(
    class = class(labour)[1],
    length = length(labour),
    is_numeric = is.numeric(labour_vec),
    n_na = if (!is.null(labour_vec)) sum(is.na(labour_vec)) else NA,
    min = if (!is.null(labour_vec)) min(labour_vec, na.rm = TRUE) else NA,
    max = if (!is.null(labour_vec)) max(labour_vec, na.rm = TRUE) else NA,
    mean = if (!is.null(labour_vec)) mean(labour_vec, na.rm = TRUE) else NA,
    sum = if (!is.null(labour_vec)) sum(labour_vec, na.rm = TRUE) else NA,
    description = "Labour employment per sector",
    units = labour_units
  )
  details$labour <- labour_meta
  
  # 6. Land proportion
  lp_meta <- list(
    class = class(land_proportion)[1],
    is_matrix = is.matrix(land_proportion),
    dim = if (is.matrix(land_proportion)) dim(land_proportion) else NA,
    rownames = if (is.matrix(land_proportion)) name_summary(rownames(land_proportion)) else NA,
    colnames = if (is.matrix(land_proportion)) name_summary(colnames(land_proportion)) else NA,
    n_na = if (is.matrix(land_proportion) && is.numeric(land_proportion)) sum(is.na(land_proportion)) else NA,
    min = if (is.matrix(land_proportion) && is.numeric(land_proportion)) min(land_proportion, na.rm = TRUE) else NA,
    max = if (is.matrix(land_proportion) && is.numeric(land_proportion)) max(land_proportion, na.rm = TRUE) else NA,
    mean = if (is.matrix(land_proportion) && is.numeric(land_proportion)) mean(land_proportion, na.rm = TRUE) else NA,
    description = "Land proportion matrix (Sector × Land Cover Class)",
    units = lp_units
  )
  details$land_proportion <- lp_meta
  
  # 7. Landuse table
  if (is.data.frame(landuse_table)) {
    landuse_meta <- list(
      class = class(landuse_table)[1],
      nrow = nrow(landuse_table),
      ncol = ncol(landuse_table),
      has_LC = "LC" %in% colnames(landuse_table),
      has_area = "area" %in% colnames(landuse_table),
      LC_summary = if ("LC" %in% colnames(landuse_table)) name_summary(as.character(landuse_table$LC)) else NA,
      area_min = if ("area" %in% colnames(landuse_table) && is.numeric(landuse_table$area)) min(landuse_table$area, na.rm = TRUE) else NA,
      area_max = if ("area" %in% colnames(landuse_table) && is.numeric(landuse_table$area)) max(landuse_table$area, na.rm = TRUE) else NA,
      area_sum = if ("area" %in% colnames(landuse_table) && is.numeric(landuse_table$area)) sum(landuse_table$area, na.rm = TRUE) else NA,
      n_na_area = if ("area" %in% colnames(landuse_table)) sum(is.na(landuse_table$area)) else NA,
      description = "Land use table (Land Cover Class × Area)",
      units = lu_units
    )
  } else {
    landuse_meta <- list(
      class = class(landuse_table)[1],
      error = "Not a data frame",
      description = "Land use table (Land Cover Class × Area)",
      units = lu_units
    )
  }
  details$landuse_table <- landuse_meta
  
  # 8. Growth rate
  growth_meta <- list(
    class = class(growth_rate)[1],
    length = length(growth_rate),
    is_numeric = is.numeric(growth_rate),
    n_na = if (is.numeric(growth_rate)) sum(is.na(growth_rate)) else NA,
    min = if (is.numeric(growth_rate)) min(growth_rate, na.rm = TRUE) else NA,
    max = if (is.numeric(growth_rate)) max(growth_rate, na.rm = TRUE) else NA,
    mean = if (is.numeric(growth_rate)) mean(growth_rate, na.rm = TRUE) else NA,
    sum = if (is.numeric(growth_rate)) sum(growth_rate, na.rm = TRUE) else NA,
    description = "Sectoral growth rates (annual, decimal)",
    units = gr_units
  )
  details$growth_rate <- growth_meta
  
  # Build summary table
  metadata_tbl <- tibble::tibble(
    Component = names(details),
    Class = sapply(details, function(x) if ("class" %in% names(x)) x$class else NA),
    Description = sapply(details, function(x) if ("description" %in% names(x)) x$description else NA),
    Units = sapply(details, function(x) if ("units" %in% names(x)) x$units else NA),
    Rows = sapply(details, function(x) {
      if ("dim" %in% names(x) && length(x$dim) == 2) x$dim[1]
      else if ("nrow" %in% names(x)) x$nrow
      else if ("length" %in% names(x)) x$length
      else NA
    }),
    Cols = sapply(details, function(x) {
      if ("dim" %in% names(x) && length(x$dim) == 2) x$dim[2]
      else if ("ncol" %in% names(x)) x$ncol
      else NA
    }),
    N_NA = sapply(details, function(x) if ("n_na" %in% names(x)) x$n_na else NA),
    Min = sapply(details, function(x) if ("min" %in% names(x)) x$min else NA),
    Max = sapply(details, function(x) if ("max" %in% names(x)) x$max else NA),
    Mean = sapply(details, function(x) if ("mean" %in% names(x)) x$mean else NA),
    Names_Summary = sapply(details, function(x) {
      if ("sector_names" %in% names(x)) x$sector_names
      else if ("rownames" %in% names(x)) x$rownames
      else if ("colnames" %in% names(x)) x$colnames
      else if ("LC_summary" %in% names(x)) x$LC_summary
      else NA
    })
  )

  list(
    metadata = metadata_tbl,
    details = details
  )
}
