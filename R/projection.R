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

#' Run projection (Supply-Constrained Mixed Input-Output Model)
#'
#' Simulates regional economic indicators over time using a supply-constrained
#' mixed input-output model. Land-constrained (co) sectors are driven by
#' land availability × land productivity coefficient (LPC), while
#' non-land (nc) sectors are solved via Leontief inverse.
#'
#' @param gdp_baseline Data frame. Baseline GDP table from descriptive analysis.
#' @param income_multiplier Data frame. Income multiplier table (kolom Labour_Compensation).
#' @param labour_multiplier Data frame. Labour multiplier table (kolom Labour).
#' @param SUB_multiplier Data frame. SUB multiplier table (kolom Surplus).
#' @param output_multiplier Data frame. Output multiplier table (unused in this model).
#' @param linkage_table Data frame. Linkage table with ITFL and ITBL.
#' @param leontief_matrix Numeric matrix. Baseline Leontief inverse (unused here).
#' @param full_io_table Numeric matrix. Complete IO table (Z matrix).
#' @param sector_idx Integer vector. Sector indices in full IO table.
#' @param P Numeric matrix. Land proportion matrix [n_sektor × n_tutupan].
#' @param area_matrix Numeric matrix. Land area matrix [n_tutupan × n_tahun].
#' @param growth_rate Numeric vector. Growth rate permintaan akhir per sektor.
#' @param lpc_scenario Data frame or `NULL`. LPC scenario inputs.
#' @param io_changes Data frame or `NULL`. IO change scenarios.
#' @param intensity_table Data frame or `NULL`. Intensity scenarios.
#' @param constrained Integer vector. Indikator sektor co (1 = constrained).
#' @param unconstrained Integer vector. Indikator sektor nc (1 = unconstrained).
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
    lpc_scenario    = NULL,
    io_changes      = NULL,
    intensity_table = NULL,
    constrained     = NULL,
    unconstrained   = NULL
) {
  
  # ============================================================================
  # 1. PERSIAPAN DIMENSI & VEKTOR BASELINE
  # ============================================================================
  
  years <- as.numeric(colnames(area_matrix))
  years <- sort(years)
  t_max <- length(years)
  
  if (length(years) > 1) {
    time_step <- years[2] - years[1]
  } else {
    time_step <- 1
  }
  
  initial_year <- if (!is.null(getOption("initial_year"))) {
    getOption("initial_year")
  } else {
    years[1] - time_step
  }
  
  # Ekstrak vektor dari gdp_baseline
  sektor_names <- as.character(unlist(gdp_baseline$SECTOR))
  Y_0          <- as.numeric(unlist(gdp_baseline$FD));          names(Y_0)    <- sektor_names
  x_0          <- as.numeric(unlist(gdp_baseline$OUTPUT));      names(x_0)    <- sektor_names
  v_0          <- as.numeric(unlist(gdp_baseline$GDP));         names(v_0)    <- sektor_names
  Land_0       <- as.numeric(unlist(gdp_baseline$Land_Requirement))
  LPC_0        <- as.numeric(unlist(gdp_baseline$LPC))
  
  # Z matrix (intermediate consumption)
  Z <- as.matrix(full_io_table)
  storage.mode(Z) <- "numeric"
  rownames(Z) <- colnames(Z) <- sektor_names
  
  # Filter sektor dummy (X_0 = 0)
  idx_valid    <- which(x_0 > 0)
  Z            <- Z[idx_valid, idx_valid, drop = FALSE]
  x_0          <- x_0[idx_valid]
  Y_0          <- Y_0[idx_valid]
  v_0          <- v_0[idx_valid]
  Land_0       <- Land_0[idx_valid]
  LPC_0        <- LPC_0[idx_valid]
  sektor_names <- sektor_names[idx_valid]
  n            <- length(idx_valid)
  
  # Koefisien input primer (proporsi terhadap output baseline)
  v_c <- as.numeric(unlist(gdp_baseline$GDP_PROP))[idx_valid]
  
  w_c <- as.numeric(unlist(labour_multiplier$Labour_Compensation))[idx_valid] / x_0
  l_c <- as.numeric(unlist(labour_multiplier$Labour))[idx_valid] / x_0
  sub_c <- as.numeric(unlist(SUB_multiplier$Surplus))[idx_valid] / x_0
  
  # Growth rate per sektor
  growth_rate <- as.numeric(unlist(growth_rate))
  if (length(growth_rate) != nrow(gdp_baseline)) {
    warning("Panjang growth_rate tidak sesuai. Gunakan default 0.")
    growth_rate <- rep(0, nrow(gdp_baseline))
  }
  growth_rate <- growth_rate[idx_valid]
  
  # ============================================================================
  # 2. IDENTIFIKASI SEKTOR CO & NC
  # ============================================================================
  
  # Kalau constrained/unconstrained tidak diberikan, deteksi dari LPC > 0
  if (is.null(constrained) || is.null(unconstrained)) {
    constrained   <- as.integer(LPC_0 > 0 & Land_0 > 0)
    unconstrained <- as.integer(!(LPC_0 > 0 & Land_0 > 0))
  }
  
  co_idx <- which(idx_valid %in% which(constrained == 1))
  no_idx <- which(idx_valid %in% which(unconstrained == 1))
  
  # Reklasifikasi: sektor co dengan L=0 atau LPC=0 → pindah ke nc
  mask_bermasalah <- (Land_0[co_idx] <= 0) | (LPC_0[co_idx] <= 0)
  
  if (any(mask_bermasalah)) {
    co_idx_pindah <- co_idx[mask_bermasalah]
    co_idx        <- co_idx[!mask_bermasalah]
    no_idx        <- sort(c(no_idx, co_idx_pindah))
  }
  
  idx_co <- co_idx
  idx_nc <- no_idx
  p_0_co <- as.numeric(LPC_0[idx_co])
  m      <- length(idx_co)
  
  # ============================================================================
  # 3. FORMULASI MATRIKS LEONTIEF & SUB-PARTISI
  # ============================================================================
  
  A_full <- sweep(Z, 2, x_0, "/")
  M_full <- diag(n) - A_full
  B_full <- solve(M_full)
  
  # Indeks keterkaitan (ITBL & ITFL)
  col_sum_B   <- colSums(B_full)
  row_sum_B   <- rowSums(B_full)
  total_sum_B <- sum(B_full)
  
  ITBL_vector <- (n * col_sum_B) / total_sum_B
  ITFL_vector <- (n * row_sum_B) / total_sum_B
  
  # Sub-partisi M
  M_nc_co     <- M_full[idx_nc, idx_co, drop = FALSE]
  M_nc_nc     <- M_full[idx_nc, idx_nc, drop = FALSE]
  M_nc_nc_inv <- solve(M_nc_nc)
  
  # P matrix untuk sektor co: [n_co × n_tutupan]
  P_sub <- as.matrix(P[idx_co, , drop = FALSE])
  storage.mode(P_sub) <- "numeric"
  
  # ============================================================================
  # 4. ITERASI PROYEKSI MULTI-INDIKATOR
  # ============================================================================
  
  x_out             <- matrix(0, nrow = t_max, ncol = n)
  PDRB_sektoral     <- matrix(0, nrow = t_max, ncol = n)
  Inc_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  Lab_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  SUB_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  L_co_matrix       <- matrix(0, nrow = t_max, ncol = m)
  LPC_growth_matrix <- matrix(0, nrow = t_max, ncol = m)
  
  # LPC dinamis untuk skenario
  current_lpc_co <- p_0_co   # salinan, akan diupdate tiap tahun
  
  for (k in seq_len(t_max)) {
    yr             <- years[k]
    selisih_tahun  <- yr - initial_year
    
    # --- A. Permintaan akhir sektor nc (eksogen) ---
    Y_nc_t <- Y_0[idx_nc] * ((1 + growth_rate[idx_nc]) ^ selisih_tahun)
    
    # --- B. Update LPC per sektor co (jika ada lpc_scenario) ---
    lpc_gr_vec <- rep(0, m)
    if (!is.null(lpc_scenario)) {
      for (j in seq_along(idx_co)) {
        sector_code <- gdp_baseline$Sector_Code[idx_co[j]]
        lpc_gr <- calculate_lpc_growth_rate(
          lpc_scenario, sector_code, k,
          data.frame(Start_Year = yr, End_Year = yr)
        )
        lpc_gr_vec[j] <- lpc_gr
        current_lpc_co[j] <- current_lpc_co[j] * (1 + lpc_gr)
      }
    }
    current_lpc_co[!is.finite(current_lpc_co)] <- 0
    
    # --- C. Output sektor co dibatasi lahan ---
    luas_tutupan_t <- as.numeric(unlist(area_matrix[, k]))   # [n_tutupan]
    L_co_t         <- as.vector(P_sub %*% luas_tutupan_t)    # [n_co]
    x_co_t         <- matrix(L_co_t * current_lpc_co, ncol = 1)   # [n_co × 1]
    
    # --- D. Mixed IO solve untuk sektor nc ---
    rhs    <- Y_nc_t - (M_nc_co %*% x_co_t)
    x_nc_t <- M_nc_nc_inv %*% rhs
    
    # --- E. Rekonstruksi output total ---
    x_t             <- numeric(n)
    x_t[idx_co]     <- as.vector(x_co_t)
    x_t[idx_nc]     <- as.vector(x_nc_t)
    
    # --- F. Multi-indikator ---
    PDRB_sektoral[k, ] <- v_c * x_t
    Inc_sektoral[k, ]  <- w_c * x_t
    Lab_sektoral[k, ]  <- l_c * x_t
    SUB_sektoral[k, ]  <- sub_c * x_t
    x_out[k, ]         <- x_t
    L_co_matrix[k, ]   <- L_co_t
    LPC_growth_matrix[k, ] <- lpc_gr_vec
  }
  
  # ============================================================================
  # 5. REKAPITULASI HASIL DALAM FORMAT TIBBLE (PER TAHUN × SEKTOR)
  # ============================================================================
  
  # Bangun hasil per (tahun × sektor)
  results_list <- list()
  
  for (k in seq_len(t_max)) {
    yr <- years[k]
    
    # Vektor per sektor (panjang n)
    land_req_t      <- numeric(n)
    land_req_t[idx_co] <- L_co_matrix[k, ]
    land_req_t[idx_nc] <- 0   # sektor nc tidak pakai lahan
    
    lpc_growth_t      <- numeric(n)
    lpc_growth_t[idx_co] <- LPC_growth_matrix[k, ]
    
    # LPC per sektor (hanya co yang > 0)
    lpc_t      <- numeric(n)
    lpc_t[idx_co] <- current_lpc_co  # setelah update
    
    results_list[[as.character(yr)]] <- tibble::tibble(
      YEAR             = yr,
      SECTOR           = sektor_names,
      LAND_REQUIREMENT = land_req_t,
      LPC              = lpc_t,
      LPC_GROWTH_RATE  = lpc_growth_t,
      FD               = Y_0,                     # permintaan akhir baseline
      OUTPUT           = x_out[k, ],
      GDP              = PDRB_sektoral[k, ],
      INCOME           = Inc_sektoral[k, ],
      LABOUR           = Lab_sektoral[k, ],
      SURPLUS          = SUB_sektoral[k, ],
      ITBL             = ITBL_vector,
      ITFL             = ITFL_vector
    )
  }
  
  final_result <- do.call(rbind, results_list)
  final_result <- final_result |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ ifelse(!is.finite(.x), 0, .x)
      )
    )
  
  return(final_result)
}