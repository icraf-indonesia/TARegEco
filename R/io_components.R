#' Extract Added Value components from Input-Output table
#'
#' Identifies and calculates added value components (import, compensation,
#' surplus, taxes) from the added value matrix. Also computes total input
#' and GDP per sector.
#'
#' @param intermediate_consumption_m Numeric matrix of intermediate
#'   consumption (Sector x Sector).
#' @param added_value_m Numeric matrix of added value (Component x Sector).
#'
#' @return A list containing:
#'   \item{total_input}{Total input per sector}
#'   \item{intermediate_input}{Intermediate input per sector}
#'   \item{import_input}{Import input per sector}
#'   \item{compensation}{Compensation of employees per sector}
#'   \item{surplus}{Gross operating surplus per sector}
#'   \item{other_tax}{Other taxes less subsidies on production}
#'   \item{tax_product}{Taxes less subsidies on products}
#'   \item{primary_input}{Primary input (compensation + surplus + other taxes)}
#'   \item{gdp_val}{GDP per sector (same as primary input)}
#'   and row indices for each component.
#'
#' @details
#' GDP = Compensation of employees + Gross operating surplus + Other taxes
#' less subsidies on production. Total input = intermediate input + import +
#' taxes on products + primary input.
#'
#' @examples
#' \dontrun{
#' comp <- get_added_value_components(ic, av)
#' }
#'
#' @export
get_added_value_components <- function(intermediate_consumption_m, added_value_m) {
  row_mapping <- list(
    import_input   = c("Total Konsumsi Antara Impor", "Input Antara (Impor)"),
    compensation   = c("Kompensasi Tenaga Kerja"),
    surplus        = c("Surplus Usaha Bruto"),
    other_tax      = c("Pajak Dikurang Subsidi Lainnya Atas Produksi", "Pajak Neto Subsidi Atas Produksi"),
    tax_product    = c("Pajak Dikurang Subsidi Atas Produk", "Pajak Neto Subsidi Atas Produk")
  )
  
  import_row       <- find_row_alias(added_value_m, row_mapping$import_input)
  compensation_row <- find_row_alias(added_value_m, row_mapping$compensation)
  surplus_row      <- find_row_alias(added_value_m, row_mapping$surplus)
  other_tax_row    <- find_row_alias(added_value_m, row_mapping$other_tax)
  tax_product_row  <- find_row_alias(added_value_m, row_mapping$tax_product)
  
  missing_rows <- c()
  if (is.null(import_row))       missing_rows <- c(missing_rows, "200 - Total Konsumsi Antara Impor / Input Antara (Impor)")
  if (is.null(compensation_row)) missing_rows <- c(missing_rows, "201 - Kompensasi Tenaga Kerja")
  if (is.null(surplus_row))      missing_rows <- c(missing_rows, "202 - Surplus Usaha Bruto")
  if (is.null(other_tax_row))    missing_rows <- c(missing_rows, "203 - Pajak Dikurang Subsidi Lainnya Atas Produksi / Pajak Neto Subsidi Atas Produksi")
  if (is.null(tax_product_row))  missing_rows <- c(missing_rows, "195 - Pajak Dikurang Subsidi Atas Produk")
  if (length(missing_rows) > 0) {
    stop(paste("The following components were not found:\n", paste(missing_rows, collapse = "\n")),
         call. = FALSE)
  }
  
  intermediate_input <- colSums(intermediate_consumption_m, na.rm = TRUE)
  import_input    <- as.numeric(added_value_m[import_row, ])
  compensation    <- as.numeric(added_value_m[compensation_row, ])
  surplus         <- as.numeric(added_value_m[surplus_row, ])
  other_tax       <- as.numeric(added_value_m[other_tax_row, ])
  tax_product     <- as.numeric(added_value_m[tax_product_row, ])
  
  gdp_val       <- compensation + surplus + other_tax
  primary_input <- gdp_val
  total_input   <- intermediate_input + import_input + tax_product + primary_input
  
  list(
    total_input        = total_input,
    intermediate_input = intermediate_input,
    import_input       = import_input,
    compensation       = compensation,
    surplus            = surplus,
    other_tax          = other_tax,
    tax_product        = tax_product,
    primary_input      = primary_input,
    gdp_val            = gdp_val,
    import_row         = import_row,
    compensation_row   = compensation_row,
    surplus_row        = surplus_row,
    other_tax_row      = other_tax_row,
    tax_product_row    = tax_product_row
  )
}

#' Build full Input-Output table and code registry
#'
#' Combines Intermediate Consumption, Final Demand, and Added Value into a
#' single complete matrix, along with a Code Registry that maps each code to
#' label, type, and row/column position.
#'
#' The returned `full_io_table` uses **labels** (sector names, FD names, VA names)
#' as row/column names, so it is human-readable. The `code_registry` retains the
#' original numeric codes for internal mapping.
#'
#' @param sector A data frame or tibble with at least one column of sector names.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param final_demand_component Data frame or tibble with two columns:
#'   code and label for final demand components.
#' @param added_value_m Numeric matrix of added value.
#' @param added_value_lookup Data frame or tibble with two columns:
#'   code and label for added value components.
#'
#' @return A list with elements:
#'   \item{full_io_table}{Complete IO matrix (rownames/colnames = labels)}
#'   \item{code_registry}{Tibble with code mapping (Code column kept as-is)}
#'   \item{n_sector}{Number of sectors}
#'
#' @examples
#' \dontrun{
#' full <- build_full_io_table(sector, ic, fd, fd_comp, av, av_lookup)
#' }
#'
#' @export
build_full_io_table <- function(sector, intermediate_consumption_m, final_demand_m,
                                final_demand_component, added_value_m, added_value_lookup) {
  n_sector     <- nrow(sector)
  sector_codes <- as.character(seq_len(n_sector))
  sector_labels <- as.character(sector[[1]])
  
  fd_codes  <- as.character(final_demand_component[[1]])
  fd_labels <- as.character(final_demand_component[[2]])
  va_codes  <- as.character(added_value_lookup[[1]])
  va_labels <- as.character(added_value_lookup[[2]])
  
  # ---- Validasi ----
  if (anyNA(fd_codes) || any(trimws(fd_codes) == ""))
    stop("There are empty/NA codes in the first column of 'final_demand'.", call. = FALSE)
  if (anyNA(va_codes) || any(trimws(va_codes) == ""))
    stop("There are empty/NA codes in the first column of 'added_value'.", call. = FALSE)
  if (any(fd_codes %in% sector_codes))
    stop("Final Demand codes conflict with sector codes (1..N).")
  if (any(va_codes %in% sector_codes))
    stop("Added Value codes conflict with sector codes (1..N).")
  if (any(fd_codes %in% va_codes))
    stop("Final Demand codes conflict with Added Value codes.")
  
  if (anyNA(sector_labels) || any(trimws(sector_labels) == ""))
    stop("There are empty/NA sector labels.", call. = FALSE)
  if (anyNA(fd_labels) || any(trimws(fd_labels) == ""))
    stop("There are empty/NA labels in 'final_demand'.", call. = FALSE)
  if (anyNA(va_labels) || any(trimws(va_labels) == ""))
    stop("There are empty/NA labels in 'added_value'.", call. = FALSE)
  
  # ---- Cek duplikat label (rownames/colnames harus unik) ----
  all_row_labels <- c(sector_labels, va_labels)
  all_col_labels <- c(sector_labels, fd_labels)
  if (anyDuplicated(all_row_labels))
    stop("Duplicate labels detected in rownames (sector + added value). ",
         "Pastikan nama sektor dan komponen added value unik.", call. = FALSE)
  if (anyDuplicated(all_col_labels))
    stop("Duplicate labels detected in colnames (sector + final demand). ",
         "Pastikan nama sektor dan komponen final demand unik.", call. = FALSE)
  
  # ---- Bangun matriks dengan LABEL sebagai dimnames ----
  row_codes <- c(sector_codes, va_codes)   # untuk internal mapping (registry)
  col_codes <- c(sector_codes, fd_codes)
  row_labels <- c(sector_labels, va_labels)
  col_labels <- c(sector_labels, fd_labels)
  
  n_row <- length(row_codes)
  n_col <- length(col_codes)
  
  full_io <- matrix(
    NA_real_,
    nrow = n_row, ncol = n_col,
    dimnames = list(row_labels, col_labels)   # <-- LABEL, bukan kode
  )
  full_io[seq_len(n_sector), seq_len(n_sector)]                   <- intermediate_consumption_m
  full_io[seq_len(n_sector), (n_sector + 1):n_col]                <- final_demand_m
  full_io[(n_sector + 1):n_row, seq_len(n_sector)]                <- added_value_m
  
  # ---- Code Registry (Code tetap kode asli, Label untuk referensi) ----
  code_registry <- dplyr::bind_rows(
    tibble::tibble(
      Code      = sector_codes,
      Label     = sector_labels,
      Type      = "Sector",
      Axis      = "Both",
      Row_Index = seq_len(n_sector),
      Col_Index = seq_len(n_sector)
    ),
    tibble::tibble(
      Code      = fd_codes,
      Label     = fd_labels,
      Type      = "Final_Demand",
      Axis      = "Column",
      Row_Index = NA_integer_,
      Col_Index = (n_sector + 1):n_col
    ),
    tibble::tibble(
      Code      = va_codes,
      Label     = va_labels,
      Type      = "Added_Value",
      Axis      = "Row",
      Row_Index = (n_sector + 1):n_row,
      Col_Index = NA_integer_
    )
  )
  
  list(
    full_io_table = full_io,
    code_registry = code_registry,
    n_sector      = n_sector
  )
}

#' Extract Final Demand Sectors by KBLI Code Range
#'
#' Retrieves a subset of Final Demand sectors from the `code_registry` table 
#' within the baseline object based on a 3-digit KBLI code range and (optionally) 
#' the sector category.
#'
#' @param baseline List. A baseline object containing `code_registry` as a data frame.
#' @param kbli_range Integer vector. The targeted 3-digit KBLI code range. Defaults to `301:305`.
#'
#' @return A data frame with `Kode` (Code) and `Nama` (Name) columns containing the 
#'   filtered Final Demand sectors, ordered numerically by code. Returns `NULL` if 
#'   the registry is missing, columns cannot be mapped, or no matching rows are found.
#'
#' @keywords internal
#' @noRd
get_final_demand_kbli <- function(baseline, kbli_range = 301:305) {
  cr <- baseline$code_registry
  if (is.null(cr) || !is.data.frame(cr) || nrow(cr) == 0) {
    warning("[get_final_demand_kbli] code_registry kosong.")
    return(NULL)
  }
  
  names_cr <- trimws(names(cr))
  
  # --- Deteksi kolom dengan fallback eksplisit ---
  # Kolom kode: prioritas "Code" (sesuai struktur Anda)
  col_code <- names_cr[grep("^Code$|^Kode$|^KBLI$|^Kode_KBLI$",
                            names_cr, ignore.case = TRUE)][1]
  # Kolom nama: prioritas "Label" (sesuai struktur Anda)
  col_name <- names_cr[grep("^Label$|^Nama$|^Name$|^Description$|^Sektor$|^Deskripsi$",
                            names_cr, ignore.case = TRUE)][1]
  # Kolom kategori: "Type" (sesuai struktur Anda)
  col_type <- names_cr[grep("^Type$|^Kategori$|^Category$|^Tipe$",
                            names_cr, ignore.case = TRUE)][1]
  
  # Validasi: kolom kode & nama wajib ada
  if (is.na(col_code) || is.na(col_name)) {
    warning("[get_final_demand_kbli] Kolom 'Code' / 'Label' tidak ditemukan. ",
            "Kolom tersedia: ", paste(names_cr, collapse = ", "))
    return(NULL)
  }
  
  # --- Ekstrak kode 3 digit ---
  code_str  <- as.character(cr[[col_code]])
  code_num  <- suppressWarnings(as.numeric(gsub("[^0-9]", "", code_str)))
  code_3dig <- floor(code_num / 10^(nchar(code_num) - 3))
  
  # --- Filter: kode dalam range DAN Type == "Final_Demand" ---
  hit <- code_3dig %in% kbli_range
  
  if (!is.na(col_type)) {
    hit <- hit & grepl("final.?demand", cr[[col_type]], ignore.case = TRUE)
  }
  
  if (sum(hit) == 0) {
    warning("[get_final_demand_kbli] Tidak ada baris dengan kode ",
            paste(range(kbli_range), collapse = "-"),
            " dan Type = 'Final_Demand'.")
    return(NULL)
  }
  
  # --- Bangun output ---
  out <- data.frame(
    Code  = code_str[hit],
    Label = as.character(cr[[col_name]][hit]),
    Type  = if (!is.na(col_type)) as.character(cr[[col_type]][hit]) else "Final_Demand",
    stringsAsFactors = FALSE
  )
  
  # Urutkan berdasarkan kode numerik
  out <- out[order(as.numeric(gsub("[^0-9]", "", out$Code))), ]
  rownames(out) <- NULL
  out
}