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


#' @title Proses Restrukturisasi IO dengan RAS Balancing (Asumsi 3)
#' @keywords internal
#' @noRd
process_sc_ras_system <- function(df_input, Z_base, X_base, Labour_base, Wage_base,
                                  sektor_names) {
  Z_base <- as.matrix(Z_base); storage.mode(Z_base) <- "numeric"
  n      <- length(sektor_names)
  
  stopifnot(nrow(Z_base) == n, ncol(Z_base) == n)
  
  X_base      <- as.numeric(unlist(X_base))
  Labour_base <- as.numeric(unlist(Labour_base))
  Wage_base   <- as.numeric(unlist(Wage_base))
  
  # Filter ΔZ > 0
  if (!is.null(df_input) && nrow(df_input) > 0) {
    df_input <- df_input |>
      dplyr::filter(
        !is.na(`SEKTOR ASAL (FROM SECTOR)`),
        `PENAMBAHAN TRANSAKSI ANTARA / ΔZ (Rp Juta)` > 0
      )
  } else {
    df_input <- data.frame()
  }
  
  Z_init <- Z_base
  X_new  <- X_base
  
  # Injeksi ΔZ dan ΔX
  if (nrow(df_input) > 0) {
    for (i in seq_len(nrow(df_input))) {
      from_name    <- df_input$`SEKTOR ASAL (FROM SECTOR)`[i]
      to_name      <- df_input$`SEKTOR TUJUAN (TO SECTOR)`[i]
      delta_z      <- as.numeric(df_input$`PENAMBAHAN TRANSAKSI ANTARA / ΔZ (Rp Juta)`[i])
      delta_x_from <- as.numeric(df_input$`PERUBAHAN OUTPUT ASAL ΔX (Rp Juta)`[i])
      delta_x_to   <- as.numeric(df_input$`PERUBAHAN OUTPUT TUJUAN ΔX (Rp Juta)`[i])
      
      idx_from <- which(sektor_names == from_name)
      idx_to   <- which(sektor_names == to_name)
      
      if (length(idx_from) > 0 && length(idx_to) > 0) {
        Z_init[idx_from, idx_to] <- Z_init[idx_from, idx_to] + delta_z
        if (!is.na(delta_x_from)) X_new[idx_from] <- X_new[idx_from] + delta_x_from
        if (!is.na(delta_x_to))   X_new[idx_to]   <- X_new[idx_to]   + delta_x_to
      }
    }
  }
  
  u_target <- rowSums(Z_init)
  v_target <- colSums(Z_init)
  
  X_safe <- ifelse(X_base == 0, 1, X_base)
  A_0    <- sweep(Z_base, 2, X_safe, "/")
  
  ras_res <- ras_balance_io(A_0, X_new, u_target, v_target)
  
  Y_new  <- X_new - rowSums(ras_res$Z_balanced)
  VA_new <- X_new - colSums(ras_res$Z_balanced)
  
  X_safe_new <- ifelse(X_new == 0, 1, X_new)
  
  L_new   <- ras_res$L_balanced
  sum_L   <- sum(L_new)
  
  list(
    Z_new             = ras_res$Z_balanced,
    A_new             = ras_res$A_balanced,
    L_new             = L_new,
    X_new             = X_new,
    Y_new             = Y_new,
    v_c_new           = VA_new / X_safe_new,
    w_c_new           = Wage_base / X_safe_new,
    l_c_new           = Labour_base / X_safe_new,
    output_multiplier = colSums(L_new),
    backward_linkage  = (n * colSums(L_new)) / sum_L,
    forward_linkage   = (n * rowSums(L_new)) / sum_L,
    ras_iterations    = ras_res$iterations,
    ras_diff          = ras_res$max_diff
  )
}