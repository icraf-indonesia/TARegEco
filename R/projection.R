#' Calculate LPC growth rate
#'
#' Calculates the Land Productivity Coefficient (LPC) growth rate for a given
#' sector and projection period, based on crop type.
#'
#' @param lpc_scenario Data frame containing LPC scenario inputs with columns:
#'   `Sector_Code`, `Start_Year`, `End_Year`, `Increment_Rate`,
#'   `Area_Implemented`, and `Crop_Type`.
#' @param sector Numeric. Sector code to be evaluated.
#' @param period_idx Numeric. Index of the current projection period.
#' @param periods_df Data frame containing `Start_Year` and `End_Year`.
#'
#' @return Numeric. LPC growth rate.
#'
#' @details
#' For annual crops, the growth rate is:
#' \deqn{growth = IncrementRate \times AreaImplemented}
#'
#' For perennial crops, the growth rate is:
#' \deqn{growth = AreaImplemented_{now} \times \sum_{k=1}^{now} IncrementRate_k}
#'
#' @examples
#' \dontrun{
#' gr <- calculate_lpc_growth_rate(lpc_scenario, 1, 1, periods_df)
#' }
#'
#' @export
calculate_lpc_growth_rate <- function(lpc_scenario, sector, period_idx, periods_df) {
  if (is.null(lpc_scenario)) return(0)
  
  sub <- lpc_scenario[
    lpc_scenario$Sector_Code == sector &
      lpc_scenario$Start_Year <= periods_df$End_Year[period_idx],
  ]
  
  if (nrow(sub) == 0) return(0)
  
  # Urutkan agar periode berurutan
  sub <- sub[order(sub$Start_Year), ]
  
  crop_type <- sub$Crop_Type[1]
  
  if (crop_type == "annual") {
    row_now <- sub[sub$Start_Year == periods_df$Start_Year[period_idx], ]
    if (nrow(row_now) == 0) return(0)
    return(row_now$Increment_Rate * row_now$Area_Implemented)
  } else if (crop_type == "perennial") {
    area_now <- sub$Area_Implemented[nrow(sub)]
    total_increment <- sum(sub$Increment_Rate)
    return(area_now * total_increment)
  } else {
    stop("Crop_Type tidak dikenal: ", crop_type)
  }
}

#' Apply IO table changes for a specific period
#'
#' Modifies cells in the full IO table according to user-defined scenario
#' changes for the given projection period.
#'
#' @param full_io_table Numeric matrix. Complete Input-Output table.
#' @param io_changes Data frame with columns `Start_Year`, `End_Year`,
#'   `Row_Code`, `Col_Code`, and `Change_Pct`. Can be `NULL` if no changes.
#' @param period_start Numeric. Start year of the period.
#' @param period_end Numeric. End year of the period.
#'
#' @return Numeric matrix. Updated full IO table.
#'
#' @examples
#' \dontrun{
#' new_io <- apply_io_changes(full_io_table, io_changes, 2024, 2029)
#' }
#'
#' @export
apply_io_changes <- function(full_io_table, io_changes, period_start, period_end) {
  if (is.null(io_changes)) return(full_io_table)
  
  changes_period <- io_changes[
    io_changes$Start_Year == period_start &
      io_changes$End_Year == period_end,
  ]
  
  if (nrow(changes_period) == 0) return(full_io_table)
  
  for (i in seq_len(nrow(changes_period))) {
    r <- changes_period$Row_Code[i]
    c <- changes_period$Col_Code[i]
    pct <- changes_period$Change_Pct[i]
    full_io_table[r, c] <- full_io_table[r, c] * (1 + pct)
  }
  
  return(full_io_table)
}

#' Recalculate Leontief inverse after IO changes
#'
#' Recalculates the Leontief inverse matrix using the current full IO table
#' and sector output.
#'
#' @param full_io_table Numeric matrix. Complete IO table.
#' @param sector_idx Integer vector. Row/column indices of sectors.
#' @param output_total Numeric vector. Total output per sector.
#'
#' @return Numeric matrix. New Leontief inverse.
#'
#' @examples
#' \dontrun{
#' L <- recalculate_leontief(full_io_table, sector_idx, output_baseline)
#' }
#'
#' @export
recalculate_leontief <- function(full_io_table, sector_idx, output_total) {
  intermediate <- full_io_table[sector_idx, sector_idx, drop = FALSE]
  A <- sweep(intermediate, 2, output_total, "/")
  A[!is.finite(A)] <- 0
  I <- diag(nrow(A))
  L <- solve(I - A)
  return(L)
}

#' Get intensity for a sector and period
#'
#' Returns the implementation intensity for a given sector in a specific
#' projection period. If no intensity table is provided, returns 1 (100%).
#'
#' @param intensity_table Data frame with columns `Sector_Code`,
#'   `Start_Year`, `End_Year`, and `Intensity`. Can be `NULL`.
#' @param sector Numeric. Sector code.
#' @param period_start Numeric. Start year of the period.
#' @param period_end Numeric. End year of the period.
#'
#' @return Numeric. Intensity value (0–1).
#'
#' @examples
#' \dontrun{
#' inten <- get_intensity(intensity_table, 1, 2024, 2029)
#' }
#'
#' @export
get_intensity <- function(intensity_table, sector, period_start, period_end) {
  if (is.null(intensity_table)) return(1.0)
  
  row <- intensity_table[
    intensity_table$Sector_Code == sector &
      intensity_table$Start_Year == period_start &
      intensity_table$End_Year == period_end,
  ]
  
  if (nrow(row) == 0) return(1.0)
  return(row$Intensity[1])
}

#' Run projection
#'
#' Main projection function that simulates regional economic indicators over
#' time based on Input-Output data and scenario inputs.
#'
#' @param gdp_baseline Data frame. Baseline GDP table from descriptive analysis.
#' @param income_multiplier Data frame. Income multiplier table.
#' @param labour_multiplier Data frame. Labour multiplier table.
#' @param SUB_multiplier Data frame. Gross operating surplus multiplier table.
#' @param output_multiplier Data frame. Output multiplier table.
#' @param linkage_table Data frame. Linkage table with ITFL and ITBL.
#' @param leontief_matrix Numeric matrix. Baseline Leontief inverse.
#' @param full_io_table Numeric matrix. Complete IO table.
#' @param sector_idx Integer vector. Sector indices in full IO table.
#' @param P Numeric matrix. Land proportion matrix.
#' @param area_matrix Numeric matrix. Land area matrix (IDs x years).
#' @param growth_rate Data frame. Growth rate data.
#' @param lpc_scenario Data frame or `NULL`. LPC scenario inputs.
#' @param io_changes Data frame or `NULL`. IO change scenarios.
#' @param intensity_table Data frame or `NULL`. Intensity scenarios.
#'
#' @return A data frame with projected indicators for all sectors and years.
#'
#' @export
run_projection <- function(
    gdp_baseline,
    income_multiplier,
    labour_multiplier,
    SUB_multiplier,
    output_multiplier,
    linkage_table,
    leontief_matrix,
    full_io_table,
    sector_idx,
    P,
    area_matrix,
    growth_rate,
    lpc_scenario = NULL,
    io_changes = NULL,
    intensity_table = NULL
) {
  
  years <- as.numeric(colnames(area_matrix))
  years <- sort(years)
  
  if (length(years) > 1) {
    time_step <- years[2] - years[1]
  }
  
  periods_df <- create_period_table(
    initial_year = years[1] - time_step,
    end_year = years[length(years)],
    time_step = time_step
  )
  
  current_prop_fd   <- as.numeric(gdp_baseline$Prop_FD)
  current_lpc       <- as.numeric(gdp_baseline$LPC)
  output_baseline   <- as.numeric(gdp_baseline$OUTPUT)
  gdp_prop          <- as.numeric(gdp_baseline$GDP_PROP)
  income_coeff      <- as.numeric(income_multiplier$Inc.multiplier)
  labour_coeff      <- as.numeric(labour_multiplier$Lab.multiplier)
  labour_compensation <- as.numeric(labour_multiplier$Labour_Compensation)
  labour_baseline   <- as.numeric(labour_multiplier$Labour)
  sub_coeff         <- as.numeric(SUB_multiplier$SUB.multiplier)
  surplus_baseline  <- as.numeric(SUB_multiplier$Surplus)
  output_coeff      <- as.numeric(output_multiplier$Out.multiplier)
  itfl_coeff        <- linkage_table$ITFL
  itbl_coeff        <- linkage_table$ITBL
  
  if (!is.null(growth_rate) && length(growth_rate) > 0) {
    r <- as.numeric(growth_rate)
    if (length(r) != nrow(gdp_baseline)) {
      warning("Panjang growth_rate tidak sesuai jumlah sektor. Menggunakan default 3%.")
      r <- rep(0.03, nrow(gdp_baseline))
    }
  } else {
    r <- rep(0.03, nrow(gdp_baseline))
  }
  
  current_labour <- labour_baseline
  
  prop_labour <- output_baseline / labour_baseline
  prop_labour[!is.finite(prop_labour)] <- 0
  
  growth_factor <- (1 + r)^time_step
  
  previous_output <- output_baseline
  current_leontief <- leontief_matrix
  current_io_table <- full_io_table
  
  results <- list()
  
  for (yr_idx in seq_along(years)) {
    year <- years[yr_idx]
    
    period_row <- periods_df[periods_df$End_Year == year, ]
    if (nrow(period_row) == 0) next
    p_start <- period_row$Start_Year
    p_end   <- period_row$End_Year
    
    current_io_table <- apply_io_changes(current_io_table, io_changes, p_start, p_end)
    current_leontief <- recalculate_leontief(current_io_table, sector_idx, output_baseline)
    
    area_t <- area_matrix[, as.character(year), drop = FALSE]
    
    if (ncol(P) == length(area_t)) {
      land_req_t <- as.numeric(P %*% area_t)
    } else if (nrow(P) == length(area_t)) {
      land_req_t <- as.numeric(t(P) %*% area_t)
    } else {
      stop("Dimensi P tidak cocok dengan area_t")
    }
    land_req_t[land_req_t <= 0] <- NA
    
    lpc_gr_vec <- rep(0, length(current_lpc))
    
    if (!is.null(lpc_scenario)) {
      for (s in seq_along(current_lpc)) {
        sector_code <- gdp_baseline$Sector_Code[s]
        lpc_gr <- calculate_lpc_growth_rate(lpc_scenario, sector_code, yr_idx, periods_df)
        lpc_gr_vec[s] <- lpc_gr
        current_lpc[s] <- current_lpc[s] + (current_lpc[s] * lpc_gr)
      }
    }
    current_lpc[!is.finite(current_lpc)] <- 0
    lpc_gr_vec[!is.finite(lpc_gr_vec)] <- 0
    
    fd_t <- land_req_t * current_prop_fd * current_lpc * growth_factor
    fd_t[is.na(fd_t)] <- 0
    
    intensity_vec <- sapply(seq_along(fd_t), function(s) {
      get_intensity(intensity_table, gdp_baseline$Sector_Code[s], p_start, p_end)
    })
    
    fd_baseline <- as.numeric(gdp_baseline$FD)
    fd_effective <- fd_baseline + intensity_vec * (fd_t - fd_baseline)
    fd_effective[!is.finite(fd_effective)] <- 0
    fd_effective[fd_effective < 0] <- 0
    
    output_t <- as.numeric(current_leontief %*% matrix(fd_effective))
    
    gdp_t <- output_t * gdp_prop
    
    projected_compensation <- income_coeff * output_baseline
    prop_compensation <- labour_compensation / projected_compensation
    prop_compensation[!is.finite(prop_compensation)] <- 0
    income_t <- output_t * income_coeff * prop_compensation
    
    add <- (output_t / prop_labour) * labour_coeff
    add[!is.finite(add)] <- 0
    labour_t <- current_labour + add
    
    projected_SUB <- sub_coeff * output_baseline
    prop_SUB <- surplus_baseline / projected_SUB
    prop_SUB[!is.finite(prop_SUB)] <- 0
    surplus_t <- output_t * sub_coeff * prop_SUB
    
    output_ratio <- output_t / previous_output
    output_ratio[!is.finite(output_ratio)] <- 1
    output_multiplier_t <- output_ratio * output_coeff
    
    itfl_t <- itfl_coeff * (land_req_t / gdp_baseline$Land_Requirement)
    itbl_t <- itbl_coeff * (land_req_t / gdp_baseline$Land_Requirement)
    itfl_t[!is.finite(itfl_t)] <- 0
    itbl_t[!is.finite(itbl_t)] <- 0
    
    prop_fd_t <- fd_t / output_t
    prop_fd_t[!is.finite(prop_fd_t)] <- 0
    
    lpc_t <- output_t / land_req_t
    lpc_t[!is.finite(lpc_t)] <- 0
    
    current_prop_fd <- prop_fd_t
    current_lpc <- lpc_t
    previous_output <- output_t
    current_labour <- labour_t
    
    results[[as.character(year)]] <- tibble::tibble(
      YEAR = year,
      SECTOR = gdp_baseline$SECTOR,
      CATEGORY = gdp_baseline$CATEGORY,
      LAND_REQUIREMENT = land_req_t,
      LPC_GROWTH_RATE  = lpc_gr_vec,
      LPC = lpc_t,
      LPC_NEW = lpc_t + (lpc_t * lpc_gr_vec),
      FD = fd_t,
      FD_EFFECTIVE = fd_effective,
      INTENSITY = intensity_vec,
      PROP_FD = prop_fd_t,
      OUTPUT = output_t,
      GDP = gdp_t,
      INCOME = income_t,
      LABOUR = labour_t,
      SURPLUS = surplus_t,
      OUTPUT_MULTIPLIER = output_multiplier_t,
      ITFL = itfl_t,
      ITBL = itbl_t
    )
  }
  
  final_result <- do.call(rbind, results)
  final_result <- final_result |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ ifelse(!is.finite(.x), 0, .x)
      )
    )
  
  return(final_result)
}