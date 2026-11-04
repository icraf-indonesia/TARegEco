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

#' Indeks keterkaitan total (Rasmussen) dari matriks invers Leontief
#'
#' ITBL_j = n * sum_i(L_ij) / sum(L)   (backward, per kolom)
#' ITFL_i = n * sum_j(L_ij) / sum(L)   (forward,  per baris)
#'
#' Dipakai untuk kondisi "sebelum" (invers Leontief baseline) dan "sesudah"
#' (L_new hasil RAS) supaya definisinya identik.
#'
#' @param L Matriks invers Leontief [n x n].
#' @param sector_names Vektor nama sektor (opsional).
#' @return List berisi `itbl` dan `itfl` (vektor numerik panjang n).
#' @keywords internal
#' @noRd
compute_linkage_index <- function(L, sector_names = NULL) {
  L     <- as.matrix(L)
  n     <- nrow(L)
  total <- sum(L)
  
  itbl <- as.numeric((n * colSums(L)) / total)
  itfl <- as.numeric((n * rowSums(L)) / total)
  
  if (!is.null(sector_names)) {
    names(itbl) <- sector_names
    names(itfl) <- sector_names
  }
  
  list(itbl = itbl, itfl = itfl)
}

#' Diagnostik io_changes terhadap baseline (hanya peringatan/pesan)
#'
#' Mengecek dua hal yang membuat Asumsi 3 gagal senyap:
#' (1) nama sektor asal/tujuan tidak cocok dengan baseline, dan
#' (2) sel [From, To] di Z baseline bernilai 0. RAS hanya menskalakan sel yang
#' sudah ada, jadi hubungan baru tidak terbentuk lewat penambahan transaksi.
#' Hanya berjalan jika kolom `SEKTOR ASAL (FROM SECTOR)` dan
#' `SEKTOR TUJUAN (TO SECTOR)` ada.
#'
#' @keywords internal
#' @noRd
check_io_changes <- function(io_changes, sektor_names, Z) {
  from_col <- "SEKTOR ASAL (FROM SECTOR)"
  to_col   <- "SEKTOR TUJUAN (TO SECTOR)"
  if (!all(c(from_col, to_col) %in% names(io_changes))) return(invisible(NULL))
  
  from_nm <- as.character(io_changes[[from_col]])
  to_nm   <- as.character(io_changes[[to_col]])
  keep    <- !is.na(from_nm) & !is.na(to_nm)
  from_nm <- from_nm[keep]
  to_nm   <- to_nm[keep]
  
  unknown <- unique(c(setdiff(from_nm, sektor_names), setdiff(to_nm, sektor_names)))
  if (length(unknown) > 0) {
    warning(
      "Nama sektor pada io_changes tidak ditemukan di baseline (baris dilewati): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  
  i  <- match(from_nm, sektor_names)
  j  <- match(to_nm,   sektor_names)
  ok <- !is.na(i) & !is.na(j)
  
  if (any(ok)) {
    n_zero <- sum(Z[cbind(i[ok], j[ok])] == 0, na.rm = TRUE)
    if (n_zero > 0) {
      message(sprintf(
        "  ! %d dari %d pasangan From-To bernilai 0 di Z baseline. RAS tidak membentuk hubungan baru dari sel nol.",
        n_zero, sum(ok)
      ))
    }
  }
  
  invisible(NULL)
}

#' Run Projection Scenario (Supply-Constrained Mixed Input-Output Model)
#'
#' Accepts any free combination of BAU scenario, Assumption 1, Assumption 2,
#' and Assumption 3. All scenario parameters are optional. The function will
#' automatically enable the appropriate logic based on which parameters are
#' not NULL.
#'
#' Supported combinations:
#' \enumerate{
#'   \item BAU only                              (area_bau, lpc=NULL,  io=NULL)
#'   \item BAU + A1                              (area_sc,  lpc=NULL,  io=NULL)
#'   \item BAU + A1 + A2                         (area_sc,  lpc=...,  io=NULL)
#'   \item BAU + A1 + A3                         (area_sc,  lpc=NULL,  io=...)
#'   \item BAU + A2                              (area_bau, lpc=...,  io=NULL)
#'   \item BAU + A2 + A3                         (area_bau, lpc=...,  io=...)
#'   \item BAU + A3                              (area_bau, lpc=NULL,  io=...)
#' }
#'
#' @param gdp_baseline Data frame. Baseline GDP table.
#' @param income_multiplier,labour_multiplier,SUB_multiplier,output_multiplier,linkage_table
#'   Data frame. Baseline multiplier & linkage tables.
#' @param leontief_matrix Numeric matrix. Baseline Leontief inverse (unused).
#' @param full_io_table Numeric matrix. Complete Z matrix.
#' @param sector_idx Integer vector. Sector indices in the full IO table.
#' @param P_baseline Numeric matrix. Baseline land proportion [n_sector x n_cover].
#' @param area_matrix_bau Numeric matrix. BAU land area [n_cover x n_year].
#' @param growth_rate Numeric vector. Baseline final demand growth rate.
#' @param constrained,unconstrained Integer vector. Co/nc indicator flags.
#'
#' @param P_scenario Numeric matrix or NULL. Scenario land proportion (Assumption 1).
#' @param area_matrix_scenario Numeric matrix or NULL. Scenario land area (Assumption 1).
#' @param lpc_scenario Data frame or NULL. New LPC targets (Assumption 2).
#' @param lpc_target_year Integer or NULL. LPC target year (Assumption 2). Default: last year.
#' @param io_changes Data frame or NULL. Scenario ΔZ & ΔX (Assumption 3).
#' @param growth_rate_scenario Numeric vector or NULL. Growth rate override.
#' @param pop_projection Data frame atau NULL. Proyeksi penduduk dengan kolom
#'   `Tahun` dan `Populasi`. Jika NULL, kolom Pendapatan/PDRB per Kapita akan
#'   bernilai NA. (Opsional)
#'
#' @return Tibble of projection results per (year x sector) with columns:
#'   YEAR, SECTOR, LAND_REQUIREMENT, LPC, LPC_GROWTH_RATE, FD, OUTPUT,
#'   GDP, INCOME, LABOUR, SURPLUS, ITBL, ITFL (end/active condition values).
#'   Diagnostic attributes, including `linkage_compare`: a data frame per sector
#'   containing SECTOR, ITBL_BEFORE, ITBL_AFTER, DELTA_ITBL, ITFL_BEFORE,
#'   ITFL_AFTER, DELTA_ITFL. Without Assumption 3, BEFORE values equal AFTER values.
#'
#' @export
run_projection_scenario <- function(
    # --- Required baseline ---
  gdp_baseline,
  income_multiplier,
  labour_multiplier,
  SUB_multiplier,
  output_multiplier,
  linkage_table,
  leontief_matrix,
  full_io_table,
  sector_idx,
  P_baseline,
  area_matrix_bau,
  growth_rate,
  constrained     = NULL,
  unconstrained   = NULL,
  
  # --- Assumption 1: Land Use Change ---
  P_scenario           = NULL,
  area_matrix_scenario = NULL,
  
  # --- Assumption 2: Productivity Improvement ---
  lpc_scenario    = NULL,
  lpc_target_year = NULL,
  
  # --- Assumption 3: Downstreaming (Hilirisasi) ---
  io_changes = NULL,
  
  # --- Optional override ---
  growth_rate_scenario = NULL,
  
  # --- Population Projection ---
  pop_projection = NULL
) {
  
  # ==========================================================================
  # 0. ACTIVE SCENARIO LOG (for debugging & Shiny)
  # ==========================================================================
  use_a1 <- !is.null(area_matrix_scenario)
  use_a2 <- !is.null(lpc_scenario) && nrow(lpc_scenario) > 0
  use_a3 <- !is.null(io_changes)   && nrow(io_changes)   > 0
  
  active_assumptions <- c(
    if (use_a1) "A1",
    if (use_a2) "A2",
    if (use_a3) "A3"
  )
  if (length(active_assumptions) == 0) active_assumptions <- "BAU"
  
  message(sprintf("[run_projection] Active assumptions: %s",
                  paste(active_assumptions, collapse = " + ")))
  
  # ==========================================================================
  # 1. BASELINE DIMENSION & VECTOR PREPARATION
  # ==========================================================================
  
  # ---- Area matrix: use scenario if available, fallback to BAU ----
  area_matrix <- if (use_a1) area_matrix_scenario else area_matrix_bau
  
  years <- sort(as.numeric(colnames(area_matrix)))
  t_max <- length(years)
  time_step <- if (length(years) > 1) years[2] - years[1] else 1
  
  initial_year <- if (!is.null(getOption("initial_year"))) {
    getOption("initial_year")
  } else {
    years[1] - time_step
  }
  
  # ---- Interpolasi populasi ke rentang tahun proyeksi ----
  pop_vector <- NULL
  has_population <- !is.null(pop_projection) && 
    is.data.frame(pop_projection) && 
    nrow(pop_projection) > 0
  
  if (has_population) {
    pop_vector <- tryCatch(
      interpolate_population(pop_projection, years),
      error = function(e) {
        warning("[run_projection] Gagal interpolasi populasi: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(pop_vector) && all(pop_vector > 0)) {
      message(sprintf(
        "[run_projection] Population projection loaded: %d years, %.0f → %.0f",
        t_max, pop_vector[1], pop_vector[t_max]
      ))
    } else {
      has_population <- FALSE
      pop_vector <- NULL
    }
  } else {
    message("[run_projection] No population projection provided; per-capita indicators will be NA.")
  }
  
  # ---- P matrix: use scenario if available, fallback to baseline ----
  P <- if (!is.null(P_scenario)) P_scenario else P_baseline
  
  # ---- Vector extraction ----
  sektor_names <- as.character(unlist(gdp_baseline$SECTOR))
  Y_0    <- as.numeric(unlist(gdp_baseline$FD));      names(Y_0)    <- sektor_names
  x_0    <- as.numeric(unlist(gdp_baseline$OUTPUT));  names(x_0)    <- sektor_names
  v_0    <- as.numeric(unlist(gdp_baseline$GDP));     names(v_0)    <- sektor_names
  Land_0 <- as.numeric(unlist(gdp_baseline$Land_Requirement))
  LPC_0  <- as.numeric(unlist(gdp_baseline$LPC))
  
  # ---- Z matrix ----
  Z <- as.matrix(full_io_table); storage.mode(Z) <- "numeric"
  rownames(Z) <- colnames(Z) <- sektor_names
  
  # ---- Filter dummy sectors ----
  idx_valid    <- which(x_0 > 0)
  Z            <- Z[idx_valid, idx_valid, drop = FALSE]
  x_0          <- x_0[idx_valid]
  Y_0          <- Y_0[idx_valid]
  v_0          <- v_0[idx_valid]
  Land_0       <- Land_0[idx_valid]
  LPC_0        <- LPC_0[idx_valid]
  sektor_names <- sektor_names[idx_valid]
  n            <- length(idx_valid)
  
  # ---- Primary input coefficients ----
  v_c   <- as.numeric(unlist(gdp_baseline$GDP_PROP))[idx_valid]
  w_c   <- as.numeric(unlist(labour_multiplier$Labour_Compensation))[idx_valid] / x_0
  l_c   <- as.numeric(unlist(labour_multiplier$Labour))[idx_valid] / x_0
  sub_c <- as.numeric(unlist(SUB_multiplier$Surplus))[idx_valid] / x_0
  
  # Nominal vectors for RAS
  labour_nominal <- as.numeric(unlist(labour_multiplier$Labour))[idx_valid]
  wage_nominal   <- as.numeric(unlist(labour_multiplier$Labour_Compensation))[idx_valid]
  
  # ---- Growth rate: override if provided ----
  gr_vec <- if (!is.null(growth_rate_scenario)) growth_rate_scenario else growth_rate
  gr_vec <- as.numeric(unlist(gr_vec))
  if (length(gr_vec) != nrow(gdp_baseline)) {
    warning("growth_rate length mismatch, falling back to 0.")
    gr_vec <- rep(0, nrow(gdp_baseline))
  }
  gr_vec <- gr_vec[idx_valid]
  gr_vec[is.na(gr_vec)] <- 0
  
  # ==========================================================================
  # 2. IDENTIFY CO / NC SECTORS
  # ==========================================================================
  if (is.null(constrained) || is.null(unconstrained)) {
    constrained   <- as.integer(LPC_0 > 0 & Land_0 > 0)
    unconstrained <- as.integer(!(LPC_0 > 0 & Land_0 > 0))
  }
  
  co_idx <- which(idx_valid %in% which(constrained == 1))
  no_idx <- which(idx_valid %in% which(unconstrained == 1))
  
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
  
  # ==========================================================================
  # 3. BASELINE LEONTIEF MATRIX & "BEFORE" LINKAGE FORMULATION
  # ==========================================================================
  A_full <- sweep(Z, 2, x_0, "/")
  M_full <- diag(n) - A_full
  B_full <- solve(M_full)
  
  # "Before" linkage: from baseline Leontief inverse (A = Z / x_0, same basis
  # as the RAS starting point, so the before-vs-after comparison is apples-to-apples)
  link_before <- compute_linkage_index(B_full, sektor_names)
  ITBL_vector <- link_before$itbl
  ITFL_vector <- link_before$itfl
  
  # ==========================================================================
  # 4. ASSUMPTION 3: IO SYSTEM OVERRIDE VIA RAS
  # ==========================================================================
  sc_io_system <- NULL
  
  if (use_a3) {
    message("[run_projection] Running RAS balancing (Assumption 3)...")
    
    # Diagnostics: sector name mismatches / From-To sectors with zero values
    check_io_changes(io_changes, sektor_names, Z)
    
    sc_io_system <- process_sc_ras_system(
      df_input     = io_changes,
      Z_base       = Z,
      X_base       = x_0,
      Labour_base  = labour_nominal,
      Wage_base    = wage_nominal,
      sektor_names = sektor_names
    )
    
    message(sprintf("  ✓ RAS iterations=%d, max diff=%.3e",
                    sc_io_system$ras_iterations, sc_io_system$ras_diff))
    
    # ==========================================================
    # GUARD: Cek apakah RAS gagal konvergen / menghasilkan NaN
    # ==========================================================
    is_ras_failed <- is.na(sc_io_system$ras_diff) || 
      !is.finite(sc_io_system$ras_diff) || 
      sc_io_system$ras_diff > 1e3
    
    if (is_ras_failed) {
      warning(
        "\n[RAS GAGAL] Target Output (ΔX) atau Transaksi (ΔZ) di file Asumsi 3 terlalu besar ",
        "dibandingkan baseline (max diff = ", sc_io_system$ras_diff, "). ",
        "Mengabaikan Asumsi 3 dan menggunakan tabel I-O baseline."
      )
      
      # Fallback: Jangan pakai hasil RAS, kembalikan ke baseline
      use_a3 <- FALSE
      active_assumptions <- setdiff(active_assumptions, "A3")
      
      L_new <- NULL
      link_after <- link_before
      
      A_full      <- sweep(Z, 2, x_0, "/")
      M_full      <- diag(n) - A_full
      B_full      <- solve(M_full)
      # v_c, w_c, l_c, Y_0, x_0, ITBL, ITFL dibiarkan menggunakan nilai baseline
      
    } else {
      # Jika RAS berhasil, lanjutkan seperti biasa
      L_new <- sc_io_system$L_new
      if (is.null(L_new)) L_new <- solve(diag(n) - sc_io_system$A_new)
      
      link_after <- compute_linkage_index(L_new, sektor_names)
      
      A_full      <- sc_io_system$A_new
      M_full      <- diag(n) - A_full
      B_full      <- L_new
      v_c         <- sc_io_system$v_c_new
      w_c         <- sc_io_system$w_c_new
      l_c         <- sc_io_system$l_c_new
      Y_0         <- sc_io_system$Y_new
      x_0         <- sc_io_system$X_new
      ITBL_vector <- link_after$itbl
      ITFL_vector <- link_after$itfl
      
      d_itbl <- max(abs(link_after$itbl - link_before$itbl), na.rm = TRUE)
      d_itfl <- max(abs(link_after$itfl - link_before$itfl), na.rm = TRUE)
      message(sprintf("  ✓ max|dITBL|=%.4g, max|dITFL|=%.4g", d_itbl, d_itfl))
    }
    
    if (max(d_itbl, d_itfl) < 1e-8) {
      warning(
        "Assumption 3 is active but ITBL/ITFL did not change. Check: ",
        "(1) sector names in io_changes exactly match baseline sectors, ",
        "(2) the delta Z column contains values > 0.",
        call. = FALSE
      )
    }
  }
  
  # ==========================================================================
  # 5. SUB-PARTITION (AFTER A3 OVERRIDE)
  # ==========================================================================
  M_nc_co     <- M_full[idx_nc, idx_co, drop = FALSE]
  M_nc_nc     <- M_full[idx_nc, idx_nc, drop = FALSE]
  M_nc_nc_inv <- solve(M_nc_nc)
  
  P_sub <- as.matrix(P[idx_co, , drop = FALSE])
  storage.mode(P_sub) <- "numeric"
  
  # ==========================================================================
  # 6. ASSUMPTION 2: PREPARE LPC TARGET & TARGET YEAR
  # ==========================================================================
  p_0_co_target <- p_0_co
  target_year   <- if (!is.null(lpc_target_year)) lpc_target_year else max(years)
  
  if (use_a2) {
    message("[run_projection] Preparing LPC target (Assumption 2)...")
    
    # Flexible LPC column name detection
    lpc_col <- NULL
    candidates <- c(
      "LPC SKENARIO 2 (Rp Juta/Ha)",
      "LPC_S2", "LPC", "LPC_Scenario", "LPC Scenario"
    )
    for (cand in candidates) {
      if (cand %in% colnames(lpc_scenario)) {
        lpc_col <- lpc_scenario[[cand]]
        break
      }
    }
    
    if (is.null(lpc_col)) {
      warning("LPC column not found in lpc_scenario. Assumption 2 ignored.")
      use_a2 <- FALSE
      active_assumptions <- setdiff(active_assumptions, "A2")
    } else {
      lpc_new <- as.numeric(lpc_col)
      
      # --- Handle length mismatches ---
      if (length(lpc_new) == nrow(gdp_baseline)) {
        lpc_new <- lpc_new[idx_valid]
      } else if (length(lpc_new) == n) {
        # already matching, ok
      } else {
        warning(sprintf(
          "Scenario LPC vector length (%d) != number of baseline sectors (%d) or valid sectors (%d).",
          length(lpc_new), nrow(gdp_baseline), n
        ))
        use_a2 <- FALSE
      }
      
      if (use_a2) {
        lpc_new[is.na(lpc_new)] <- 0
        
        # Fallback: if zero, use baseline (do not collapse)
        zero_mask <- lpc_new == 0
        lpc_new[zero_mask] <- LPC_0[zero_mask]
        
        p_0_co_target <- as.numeric(lpc_new[idx_co])
        
        message(sprintf("  ✓ LPC target prepared for %d co sectors (interpolation %d → %d)",
                        length(p_0_co_target), initial_year, target_year))
      }
    }
  }
  
  # ==========================================================================
  # 7. PROJECTION ITERATION
  # ==========================================================================
  x_out             <- matrix(0, nrow = t_max, ncol = n)
  PDRB_sektoral     <- matrix(0, nrow = t_max, ncol = n)
  Inc_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  Lab_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  SUB_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  L_co_matrix       <- matrix(0, nrow = t_max, ncol = m)
  LPC_growth_matrix <- matrix(0, nrow = t_max, ncol = m)
  LPC_t_matrix      <- matrix(0, nrow = t_max, ncol = m)
  
  for (k in seq_len(t_max)) {
    yr            <- years[k]
    selisih_tahun <- yr - initial_year
    
    # --- A. Final demand for nc sectors ---
    Y_nc_t <- Y_0[idx_nc] * ((1 + gr_vec[idx_nc]) ^ selisih_tahun)
    
    # --- B. Dynamic LPC for A2, static otherwise ---
    if (use_a2 && yr > initial_year) {
      if (yr >= target_year) {
        p_0_co_t <- p_0_co_target
      } else {
        prop     <- (yr - initial_year) / (target_year - initial_year)
        p_0_co_t <- p_0_co + prop * (p_0_co_target - p_0_co)
      }
      lpc_gr_vec <- (p_0_co_t / p_0_co) - 1
    } else {
      p_0_co_t   <- p_0_co
      lpc_gr_vec <- rep(0, m)
    }
    
    # --- C. Output of co sectors constrained by land ---
    luas_tutupan_t <- as.numeric(unlist(area_matrix[, k]))
    L_co_t         <- as.vector(P_sub %*% luas_tutupan_t)
    x_co_t         <- matrix(L_co_t * p_0_co_t, ncol = 1)
    
    # --- D. Mixed IO solve ---
    rhs    <- Y_nc_t - (M_nc_co %*% x_co_t)
    x_nc_t <- M_nc_nc_inv %*% rhs
    
    # --- E. Output reconstruction ---
    x_t         <- numeric(n)
    x_t[idx_co] <- as.vector(x_co_t)
    x_t[idx_nc] <- as.vector(x_nc_t)
    
    # --- F. Multi-indicator ---
    PDRB_sektoral[k, ]     <- v_c * x_t
    Inc_sektoral[k, ]      <- w_c * x_t
    Lab_sektoral[k, ]      <- l_c * x_t
    SUB_sektoral[k, ]      <- sub_c * x_t
    x_out[k, ]             <- x_t
    L_co_matrix[k, ]       <- L_co_t
    LPC_growth_matrix[k, ] <- lpc_gr_vec
    LPC_t_matrix[k, ]      <- p_0_co_t
  }
  
  # ==========================================================================
  # 8. RESULT RECAPITULATION
  # ==========================================================================
  results_list <- list()
  
  for (k in seq_len(t_max)) {
    yr <- years[k]
    
    land_req_t         <- numeric(n)
    land_req_t[idx_co] <- L_co_matrix[k, ]
    
    lpc_growth_t         <- numeric(n)
    lpc_growth_t[idx_co] <- LPC_growth_matrix[k, ]
    
    lpc_t         <- numeric(n)
    lpc_t[idx_co] <- LPC_t_matrix[k, ]
    
    # ---- Total makroekonomi per tahun ----
    total_pdrb   <- sum(PDRB_sektoral[k, ], na.rm = TRUE)
    total_inc_tk <- sum(Inc_sektoral[k, ],  na.rm = TRUE)
    
    # ---- Indikator per kapita ----
    pop_k <- if (has_population) pop_vector[k] else NA_real_
    
    pdrb_per_kapita <- if (!is.na(pop_k) && pop_k > 0) total_pdrb   / pop_k else NA_real_
    inc_per_kapita  <- if (!is.na(pop_k) && pop_k > 0) total_inc_tk / pop_k else NA_real_
    
    results_list[[as.character(yr)]] <- tibble::tibble(
      YEAR                           = yr,
      SECTOR                         = sektor_names,
      LAND_REQUIREMENT               = land_req_t,
      LPC                            = lpc_t,
      LPC_GROWTH_RATE                = lpc_growth_t,
      FD                             = Y_0,
      OUTPUT                         = x_out[k, ],
      GDP                            = PDRB_sektoral[k, ],
      INCOME                         = Inc_sektoral[k, ],
      LABOUR                         = Lab_sektoral[k, ],
      SURPLUS                        = SUB_sektoral[k, ],
      ITBL                           = as.numeric(ITBL_vector),
      ITFL                           = as.numeric(ITFL_vector),
      # ---- per-kapita untuk sektor terkait ----
      POPULATION                     = pop_k,
      GDP_PER_CAPITA                 = pdrb_per_kapita,
      INCOME_PER_CAPITA              = inc_per_kapita
    )
  }
  
  final_result <- do.call(rbind, results_list) |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ ifelse(!is.finite(.x), 0, .x))
    )
  
  # ==========================================================================
  # 9. LINKAGE COMPARISON BEFORE vs AFTER (ITBL & ITFL)
  # ==========================================================================
  linkage_compare <- data.frame(
    SECTOR      = sektor_names,
    ITBL_BEFORE = as.numeric(link_before$itbl),
    ITBL_AFTER  = as.numeric(ITBL_vector),
    ITFL_BEFORE = as.numeric(link_before$itfl),
    ITFL_AFTER  = as.numeric(ITFL_vector),
    stringsAsFactors = FALSE
  )
  linkage_compare$DELTA_ITBL <- linkage_compare$ITBL_AFTER - linkage_compare$ITBL_BEFORE
  linkage_compare$DELTA_ITFL <- linkage_compare$ITFL_AFTER - linkage_compare$ITFL_BEFORE
  linkage_compare <- linkage_compare[, c(
    "SECTOR",
    "ITBL_BEFORE", "ITBL_AFTER", "DELTA_ITBL",
    "ITFL_BEFORE", "ITFL_AFTER", "DELTA_ITFL"
  )]
  
  # ==========================================================================
  # 10. METADATA (for Shiny / debugging)
  # ==========================================================================
  attr(final_result, "active_assumptions") <- active_assumptions
  attr(final_result, "use_a1") <- use_a1
  attr(final_result, "use_a2") <- use_a2
  attr(final_result, "use_a3") <- use_a3
  attr(final_result, "ras_iterations") <- if (!is.null(sc_io_system)) sc_io_system$ras_iterations else NA
  attr(final_result, "ras_diff")       <- if (!is.null(sc_io_system)) sc_io_system$ras_diff       else NA
  attr(final_result, "linkage_compare")  <- linkage_compare
  # Preserved so that older code using these attributes continues to work
  attr(final_result, "backward_linkage") <- as.numeric(ITBL_vector)
  attr(final_result, "forward_linkage")  <- as.numeric(ITFL_vector)
  attr(final_result, "sector_names")     <- sektor_names
  attr(final_result, "years")            <- years
  attr(final_result, "has_population")   <- has_population
  attr(final_result, "pop_vector")       <- pop_vector
  
  return(final_result)
}