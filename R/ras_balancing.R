#' @title RAS Biproportional Matrix Balancing
#' @keywords internal
#' @noRd
ras_balance_io <- function(A_0, x_target, u_target, v_target,
                           tol = 1e-6, max_iter = 1000) {
  n    <- length(x_target)
  A_k  <- A_0
  iter <- 0
  diff <- Inf
  
  while (diff > tol && iter < max_iter) {
    iter <- iter + 1
    
    Z_k <- sweep(A_k, 2, x_target, "*")
    
    u_k <- rowSums(Z_k)
    r   <- ifelse(u_k == 0, 1, u_target / u_k)
    r[!is.finite(r)] <- 1
    A_k <- sweep(A_k, 1, r, "*")
    
    Z_k <- sweep(A_k, 2, x_target, "*")
    v_k <- colSums(Z_k)
    s   <- ifelse(v_k == 0, 1, v_target / v_k)
    s[!is.finite(s)] <- 1
    A_k <- sweep(A_k, 2, s, "*")
    
    Z_k  <- sweep(A_k, 2, x_target, "*")
    diff <- max(abs(c(rowSums(Z_k) - u_target,
                      colSums(Z_k) - v_target)), na.rm = TRUE)
  }
  
  list(
    A_balanced = A_k,
    Z_balanced = Z_k,
    L_balanced = solve(diag(n) - A_k),
    iterations = iter,
    max_diff   = diff
  )
}


#' @title Proses Re-alokasi Final Demand ke Intermediate Transaction (Asumsi 3)
#' @keywords internal
#' @noRd
process_sc_ras_system <- function(df_input, Z_base, X_base, Labour_base, Wage_base, sektor_names, years) {
  Z_base <- as.matrix(Z_base)
  storage.mode(Z_base) <- "numeric"
  
  n <- length(sektor_names)
  stopifnot(nrow(Z_base) == n, ncol(Z_base) == n)
  
  X_base <- as.numeric(unlist(X_base))
  Labour_base <- as.numeric(unlist(Labour_base))
  Wage_base <- as.numeric(unlist(Wage_base))
  
  # Baseline Final Demand (Y_0) = Output - Total Intermediate Demand
  Y_0 <- X_base - rowSums(Z_base)
  X_safe <- ifelse(X_base == 0, 1, X_base)
  
  # Penampung struktur matriks dinamis per tahun
  Z_list <- list()
  A_list <- list()
  L_list <- list()
  Y_list <- list()
  ITBL_list <- list()
  ITFL_list <- list()
  
  if (!is.null(df_input) && nrow(df_input) > 0) {
    names_df <- trimws(names(df_input))
    col_from <- names_df[grep("Sektor Asal|From Sector|FROM SECTOR|Sektor_Asal", names_df, ignore.case = TRUE)][1]
    col_to   <- names_df[grep("Sektor Tujuan|To Sector|TO SECTOR|Sektor_Tujuan", names_df, ignore.case = TRUE)][1]
    
    if (is.na(col_from) || is.na(col_to)) {
      stop("File Asumsi 3 harus memiliki kolom 'Sektor Asal' dan 'Sektor Tujuan'.")
    }
    
    # Kolom tahun yang sesuai periode proyeksi
    year_cols <- intersect(as.character(years), names_df)
    if (length(year_cols) == 0) {
      stop(
        "File Asumsi 3 tidak punya kolom tahun yang cocok dengan periode proyeksi (",
        paste(years, collapse = ", "), "). Kolom yang ditemukan: ",
        paste(names_df, collapse = " | "),
        call. = FALSE
      )
    }
    
    # Iterasi re-alokasi per tahun k
    for (yr in year_cols) {
      yr_str <- as.character(yr)
      Z_yr <- Z_base
      Y_yr <- Y_0
      
      for (i in seq_len(nrow(df_input))) {
        from_name <- as.character(df_input[[col_from]][i])
        to_name   <- as.character(df_input[[col_to]][i])
        
        if (is.na(from_name) || is.na(to_name) || trimws(from_name) == "" || trimws(to_name) == "") next
        
        idx_from <- which(tolower(trimws(sektor_names)) == tolower(trimws(from_name)))
        idx_to   <- which(tolower(trimws(sektor_names)) == tolower(trimws(to_name)))
        
        if (length(idx_from) == 0 || length(idx_to) == 0) next
        
        # Parse persentase realokasi (bisa 0.05 atau "5%")
        pct_val <- df_input[[yr_str]][i]
        if (is.character(pct_val)) {
          pct_val <- as.numeric(gsub("%", "", pct_val))
          if (!is.na(pct_val) && pct_val > 1) pct_val <- pct_val / 100
        } else if (is.numeric(pct_val)) {
          if (!is.na(pct_val) && pct_val > 1) pct_val <- pct_val / 100
        }
        
        if (is.na(pct_val) || pct_val <= 0) next
        
        # Capping maksimal 90% (0.90)
        if (pct_val > 0.90) pct_val <- 0.90
        
        # Nominal yang dialihkan dari FD Sektor Asal ke Transaksi Antara Z[Sektor Asal, Sektor Tujuan]
        nominal_alih <- Y_0[idx_from] * pct_val
        Y_yr[idx_from] <- Y_yr[idx_from] - nominal_alih
        Z_yr[idx_from, idx_to] <- Z_yr[idx_from, idx_to] + nominal_alih
      }
      
      # Hitung Koefisien Teknis A_yr dan Leontief Inverse L_yr Dinamis Tahun yr
      A_yr <- sweep(Z_yr, 2, X_safe, "/")
      A_yr[!is.finite(A_yr)] <- 0
      
      I <- diag(n)
      L_yr <- tryCatch(
        solve(I - A_yr),
        error = function(e) {
          warning("[Asumsi 3] Matriks (I - A) singular pada tahun ", yr_str, ", gunakan pseudo-inverse.")
          MASS::ginv(I - A_yr)
        }
      )
      
      sum_L <- sum(L_yr)
      itbl_yr <- if (sum_L > 0) (n * colSums(L_yr)) / sum_L else rep(0, n)
      itfl_yr <- if (sum_L > 0) (n * rowSums(L_yr)) / sum_L else rep(0, n)
      
      Z_list[[yr_str]]    <- Z_yr
      A_list[[yr_str]]    <- A_yr
      L_list[[yr_str]]    <- L_yr
      Y_list[[yr_str]]    <- Y_yr
      ITBL_list[[yr_str]] <- itbl_yr
      ITFL_list[[yr_str]] <- itfl_yr
    }
  }
  
  # Matriks baseline default
  A_base <- sweep(Z_base, 2, X_safe, "/")
  A_base[!is.finite(A_base)] <- 0
  L_base <- solve(diag(n) - A_base)
  sum_L_base <- sum(L_base)
  
  list(
    Z_list            = Z_list,
    A_list            = A_list,
    L_list            = L_list,
    Y_list            = Y_list,
    ITBL_list         = ITBL_list,
    ITFL_list         = ITFL_list,
    A_base            = A_base,
    L_base            = L_base,
    Y_base            = Y_0,
    v_c_new           = (X_base - colSums(Z_base)) / X_safe,
    w_c_new           = Wage_base / X_safe,
    l_c_new           = Labour_base / X_safe,
    output_multiplier = colSums(L_base),
    backward_linkage  = (n * colSums(L_base)) / sum_L_base,
    forward_linkage   = (n * rowSums(L_base)) / sum_L_base,
    ras_iterations    = 0,
    ras_diff          = 0
  )
}