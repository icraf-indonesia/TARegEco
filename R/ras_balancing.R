#' RAS Balancing (Biproportional Matrix Balancing)
#'
#' Menyesuaikan matriks koefisien teknis A agar row sums dan col sums dari Z
#' cocok dengan target u dan v.
#'
#' @param A_0 Numeric matrix. Koefisien teknis awal.
#' @param x_target Numeric vector. Target output total.
#' @param u_target Numeric vector. Target row sums Z.
#' @param v_target Numeric vector. Target col sums Z.
#' @param tol Numeric. Toleransi konvergensi.
#' @param max_iter Integer. Maksimum iterasi.
#'
#' @return List dengan `A_balanced`, `Z_balanced`, `L_balanced`, `iterations`, `max_diff`.
#' @keywords internal
#' @noRd
ras_balance_io <- function(A_0, x_target, u_target, v_target, tol = 1e-6, max_iter = 1000) {
  n <- length(x_target); A_k <- A_0; iter <- 0; diff <- Inf
  while (diff > tol && iter < max_iter) {
    iter <- iter + 1
    Z_k <- sweep(A_k, 2, x_target, "*")
    u_k <- rowSums(Z_k); r <- ifelse(u_k == 0, 1, u_target / u_k); r[!is.finite(r)] <- 1
    A_k <- sweep(A_k, 1, r, "*")
    Z_k <- sweep(A_k, 2, x_target, "*")
    v_k <- colSums(Z_k); s <- ifelse(v_k == 0, 1, v_target / v_k); s[!is.finite(s)] <- 1
    A_k <- sweep(A_k, 2, s, "*")
    Z_k <- sweep(A_k, 2, x_target, "*")
    diff <- max(abs(c(rowSums(Z_k) - u_target, colSums(Z_k) - v_target)), na.rm = TRUE)
  }
  L_k <- solve(diag(n) - A_k)
  list(A_balanced = A_k, Z_balanced = Z_k, L_balanced = L_k,
       iterations = iter, max_diff = diff)
}

#' Proses Input Asumsi 3 dan Bangun A_list / Y_list per Tahun
#'
#' Mendukung dua format input:
#' - **persen_per_tahun**: kolom tahun (2025, 2026, ...) berisi persentase
#'   realokasi dari Y_0 sektor asal.
#' - **nominal_dz**: kolom `PENAMBAHAN TRANSAKSI ANTARA / ΔZ` berisi nominal.
#'
#' @param df_input Data frame. Isi file A3.
#' @param Z_base Numeric matrix. Matriks transaksi antara baseline.
#' @param X_base Numeric vector. Output baseline.
#' @param Labour_base Numeric vector. Labour nominal baseline.
#' @param Wage_base Numeric vector. Wage nominal baseline.
#' @param sektor_names Character vector. Nama sektor.
#' @param years Numeric vector. Tahun proyeksi.
#' @param max_share Numeric. Batas maksimal porsi Y_0 yang dialihkan (default 0.90).
#'
#' @return List dengan `A_list`, `Y_list`, `ITBL_list`, `ITFL_list`,
#'   `Delta_Z_list`, `Delta_Y_list`, `A_base`, `L_base`, `Y_base`,
#'   `v_c_new`, `w_c_new`, `l_c_new`, `backward_linkage`, `forward_linkage`,
#'   `output_multiplier`, `mode`, `ras_iterations`, `ras_diff`.
#' @export
process_sc_ras_system_dynamic <- function(df_input, Z_base, X_base, Labour_base, Wage_base,
                                          sektor_names, years, max_share = 0.90) {
  Z_base <- as.matrix(Z_base); storage.mode(Z_base) <- "numeric"
  n <- length(sektor_names)
  X_base      <- as.numeric(unlist(X_base))
  Labour_base <- as.numeric(unlist(Labour_base))
  Wage_base   <- as.numeric(unlist(Wage_base))
  X_safe      <- ifelse(X_base == 0, 1, X_base)
  Y_0         <- X_base - rowSums(Z_base)
  
  if (is.null(df_input) || nrow(df_input) == 0) {
    stop("[A3] io_changes kosong atau NULL.", call. = FALSE)
  }
  
  # ---- Deteksi kolom ----
  names_df <- trimws(names(df_input))
  find_col <- function(pattern) {
    hit <- names_df[grep(pattern, names_df, ignore.case = TRUE)]
    if (length(hit) == 0) NA_character_ else hit[1]
  }
  col_from <- find_col("Sektor Asal|From Sector")
  col_to   <- find_col("Sektor Tujuan|To Sector")
  if (is.na(col_from) || is.na(col_to)) {
    stop("[A3] io_changes harus punya kolom 'Sektor Asal' dan 'Sektor Tujuan'. ",
         "Kolom ditemukan: ", paste(names_df, collapse = " | "), call. = FALSE)
  }
  
  # ---- Deteksi mode ----
  year_cols <- intersect(as.character(years), names_df)
  col_dz    <- find_col("PENAMBAHAN TRANSAKSI ANTARA|DELTA_?Z|ΔZ|Delta Z")
  
  mode <- if (length(year_cols) > 0) {
    "persen_per_tahun"
  } else if (!is.na(col_dz)) {
    "nominal_dz"
  } else {
    stop("[A3] io_changes tidak punya kolom tahun (", paste(range(years), collapse = "-"),
         ") maupun kolom ΔZ. Kolom ditemukan: ", paste(names_df, collapse = " | "),
         call. = FALSE)
  }
  
  # ---- Pemetaan nama sektor ----
  idx_of <- function(nm) {
    nm <- as.character(nm)
    if (is.na(nm) || !nzchar(trimws(nm))) return(NA_integer_)
    hit <- which(tolower(trimws(sektor_names)) == tolower(trimws(nm)))
    if (length(hit) == 0) NA_integer_ else hit[1]
  }
  from_raw <- as.character(df_input[[col_from]])
  to_raw   <- as.character(df_input[[col_to]])
  idx_from <- vapply(from_raw, idx_of, integer(1), USE.NAMES = FALSE)
  idx_to   <- vapply(to_raw,   idx_of, integer(1), USE.NAMES = FALSE)
  
  filled <- !is.na(from_raw) & !is.na(to_raw) &
    nzchar(trimws(from_raw)) & nzchar(trimws(to_raw))
  valid  <- filled & !is.na(idx_from) & !is.na(idx_to)
  
  if (any(filled & !valid)) {
    bad <- which(filled & !valid)
    warning("[A3] Nama sektor tidak cocok dengan baseline, baris dilewati: ",
            paste0(from_raw[bad], " -> ", to_raw[bad], collapse = "; "),
            call. = FALSE)
  }
  if (!any(valid)) {
    stop("[A3] Tidak ada baris dengan sektor asal & tujuan yang cocok dengan baseline.",
         call. = FALSE)
  }
  
  # ---- Builder delta per tahun ----
  n_capped <- 0L
  build_year <- function(amounts) {
    Z_delta <- matrix(0, nrow = n, ncol = n)
    Y_delta <- numeric(n)
    used    <- numeric(n)
    for (i in which(valid)) {
      a <- amounts[i]
      if (is.na(a) || a <= 0) next
      f <- idx_from[i]; j <- idx_to[i]
      room <- max(0, max_share * Y_0[f] - used[f])
      if (a > room) { a <- room; n_capped <<- n_capped + 1L }
      if (a <= 0) next
      used[f]       <- used[f] + a
      Y_delta[f]    <- Y_delta[f] - a
      Y_delta[j]    <- Y_delta[j] + a
      Z_delta[f, j] <- Z_delta[f, j] + a
    }
    list(Z = Z_delta, Y = Y_delta)
  }
  
  # ---- Isi Delta_Z_list / Delta_Y_list ----
  Delta_Z_list <- list(); Delta_Y_list <- list()
  
  if (mode == "persen_per_tahun") {
    parse_pct <- function(x) {
      if (is.character(x)) x <- suppressWarnings(as.numeric(gsub("%", "", x)))
      x <- suppressWarnings(as.numeric(x))
      ifelse(!is.na(x) & x > 1, x / 100, x)
    }
    for (yr in year_cols) {
      pct     <- parse_pct(df_input[[yr]])
      amounts <- ifelse(valid, Y_0[ifelse(is.na(idx_from), 1L, idx_from)] * pct, NA_real_)
      res     <- build_year(amounts)
      if (any(res$Y != 0)) {
        Delta_Z_list[[yr]] <- res$Z
        Delta_Y_list[[yr]] <- res$Y
      }
    }
  } else {
    dz <- suppressWarnings(as.numeric(df_input[[col_dz]]))
    for (yr in years) {
      amounts <- ifelse(valid, dz, NA_real_)
      res     <- build_year(amounts)
      if (any(res$Y != 0)) {
        Delta_Z_list[[as.character(yr)]] <- res$Z
        Delta_Y_list[[as.character(yr)]] <- res$Y
      }
    }
  }
  
  if (length(Delta_Y_list) == 0) {
    stop("[A3] Tidak ada tahun dengan delta. Cek nilai persen > 0 di kolom tahun.",
         call. = FALSE)
  }
  
  # ---- Turunkan A_list / Y_list / ITBL / ITFL per tahun ----
  A_list <- list(); Y_list <- list(); L_list <- list()
  ITBL_list <- list(); ITFL_list <- list()
  
  for (yr_str in names(Delta_Z_list)) {
    Z_yr <- Z_base + Delta_Z_list[[yr_str]]
    Y_yr <- Y_0    + Delta_Y_list[[yr_str]]
    
    A_yr <- sweep(Z_yr, 2, X_safe, "/")
    A_yr[!is.finite(A_yr)] <- 0
    
    L_yr <- tryCatch(
      solve(diag(n) - A_yr),
      error = function(e) MASS::ginv(diag(n) - A_yr)
    )
    
    link_yr <- compute_linkage_index(L_yr, sektor_names)
    
    A_list[[yr_str]]    <- A_yr
    Y_list[[yr_str]]    <- Y_yr
    L_list[[yr_str]]    <- L_yr
    ITBL_list[[yr_str]] <- link_yr$itbl
    ITFL_list[[yr_str]] <- link_yr$itfl
  }
  
  # ---- Baseline default ----
  A_base <- sweep(Z_base, 2, X_safe, "/")
  A_base[!is.finite(A_base)] <- 0
  L_base <- solve(diag(n) - A_base)
  link_base <- compute_linkage_index(L_base, sektor_names)
  
  # ---- Nilai statis (tahun terakhir) ----
  last_yr <- names(Delta_Z_list)[length(Delta_Z_list)]
  A_last  <- A_list[[last_yr]]
  Y_last  <- Y_list[[last_yr]]
  Z_last  <- Z_base + Delta_Z_list[[last_yr]]
  X_last  <- X_base
  VA_last <- X_last - colSums(Z_last)
  X_safe_last <- ifelse(X_last == 0, 1, X_last)
  
  message(sprintf("[A3] mode=%s | tahun berisi delta=%d | capping=%d | max|ΔY|=%.3e",
                  mode, length(Delta_Y_list), n_capped,
                  max(abs(unlist(Delta_Y_list)))))
  
  list(
    # per tahun
    Z_list = Delta_Z_list, A_list = A_list, Y_list = Y_list, L_list = L_list,
    ITBL_list = ITBL_list, ITFL_list = ITFL_list,
    Delta_Z_list = Delta_Z_list, Delta_Y_list = Delta_Y_list,
    # baseline
    A_base = A_base, L_base = L_base, Y_base = Y_0,
    ITBL_base = link_base$itbl, ITFL_base = link_base$itfl,
    # statis (tahun terakhir) untuk kompatibilitas
    Z_new = Z_last, A_new = A_last, L_new = L_list[[last_yr]],
    X_new = X_last, Y_new = Y_last, VA_new = VA_last,
    v_c_new = VA_last / X_safe_last,
    w_c_new = Wage_base / X_safe_last,
    l_c_new = Labour_base / X_safe_last,
    output_multiplier = colSums(L_list[[last_yr]]),
    backward_linkage  = ITBL_list[[last_yr]],
    forward_linkage   = ITFL_list[[last_yr]],
    mode = mode,
    ras_iterations = NA_integer_,
    ras_diff = NA_real_
  )
}