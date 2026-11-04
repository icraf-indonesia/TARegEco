#' Read input file (Excel or CSV) into a tibble or matrix
#'
#' Reads an input file from Excel (`.xlsx`, `.xls`) or CSV (`.csv`, `.txt`),
#' with optional conversion to a numeric matrix.
#'
#' @param path Character. Path to the input file.
#' @param sheet Character or `NULL`. Sheet name for Excel files. If `NULL`,
#'   the first sheet is used.
#' @param skip Integer. Number of rows to skip before reading data.
#' @param header Logical. If `TRUE`, the first row after `skip` is used as
#'   column names.
#' @param as_matrix Logical. If `TRUE`, the data is returned as a matrix.
#'
#' @return A `tibble` if `as_matrix = FALSE`, or a `matrix` if `TRUE`.
#'
#' @details
#' The function automatically detects the file extension. For Excel files,
#' the `readxl` package is used. For CSV/TXT files, `utils::read.csv` is used.
#'
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom tibble as_tibble
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' sector <- read_input_file("data/io.xlsx", sheet = "sector", header = TRUE)
#' ic <- read_input_file("data/io.xlsx", sheet = "intermediate_consumption",
#'                        as_matrix = TRUE)
#' }
#'
#' @export
read_input_file <- function(path, sheet = NULL, skip = 0, header = FALSE, as_matrix = FALSE) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required for Excel files.")
    }
    data <- readxl::read_excel(path, sheet = sheet, skip = skip, col_names = header)
  } else if (ext %in% c("csv", "txt")) {
    data <- utils::read.csv(path, skip = skip, header = header)
  } else {
    stop("Unsupported file format. Please upload CSV or Excel.")
  }
  
  data <- tibble::as_tibble(data)
  
  if (as_matrix) {
    return(as.matrix(data))
  }
  
  return(data)
}

#' Load projection baseline data from Excel or RDS
#'
#' Loads descriptive analysis results required to run the projection module.
#' The input can be either an Excel workbook (`IO_Analysis_Output.xlsx`) or
#' an RDS file produced by the descriptive analysis module.
#'
#' @param path Character. Path to the descriptive analysis file. Supported
#'   extensions are `.xlsx`, `.xls`, and `.rds`.
#' @param projection_area_file_bau Character. Path to the BAU land projection
#'   file (Excel/CSV) containing columns `ID`, `LC`, and one column per
#'   projection year.
#'
#' @return A list containing:
#'   \item{gdp_baseline}{Tibble of baseline GDP and related coefficients}
#'   \item{leontief_matrix}{Leontief inverse matrix}
#'   \item{full_io_table}{Complete IO matrix}
#'   \item{code_registry}{Code registry tibble}
#'   \item{sector_idx}{Index of sector rows in the IO table}
#'   \item{income_multiplier}{Income multiplier table}
#'   \item{labour_multiplier}{Labour multiplier table}
#'   \item{SUB_multiplier}{Gross operating surplus multiplier table}
#'   \item{output_multiplier}{Output multiplier table}
#'   \item{linkage_table}{Linkage table}
#'   \item{land_prop}{Land proportion matrix}
#'   \item{P_default}{Default land proportion matrix}
#'   \item{growth_rate}{Baseline growth rate data}
#'   \item{area_matrix_bau}{BAU area matrix}
#'   \item{projection_area_bau}{Raw BAU land projection table}
#'
#' @examples
#' \dontrun{
#' baseline <- load_projection_baseline(
#'   path = "output/descriptive_analysis_module.rds",
#'   projection_area_file_bau = "data/luc_projection_bau.xlsx"
#' )
#' }
#'
#' @export
load_projection_baseline <- function(path, projection_area_file_bau) {
  
  profile <- list(
    area_name = "Wilayah Tidak Diketahui",
    year      = NA_integer_,
    currency  = "Rp Juta",
    io_source = "-"
  )

  if (grepl("\\.rds$", path, ignore.case = TRUE)) {

    # =====================================================
    # RDS BRANCH
    # =====================================================
    io <- readRDS(path)

    gdp_baseline      <- io$gdp_baseline
    leontief_matrix   <- io$linkage$Leontief
    full_io_table     <- io$full_io_table
    code_registry     <- io$code_registry
    sector_idx        <- which(code_registry$Type == "Sector")
    income_multiplier <- io$multipliers$income
    labour_multiplier <- io$multipliers$labour
    SUB_multiplier    <- io$multipliers$sub
    output_multiplier <- io$multipliers$output
    linkage_table     <- io$linkage
    growth_rate       <- io$growth_rate
    
    if (!is.null(io$profile) && is.list(io$profile)) {
      profile <- list(
        area_name = io$profile$area_name %||% profile$area_name,
        year      = io$profile$year      %||% profile$year,
        currency  = io$profile$currency  %||% profile$currency,
        io_source = io$profile$io_source %||% profile$io_source
      )
    } else if (!is.null(io$metadata$profile) && is.list(io$metadata$profile)) {
      profile <- list(
        area_name = io$metadata$profile$area_name %||% profile$area_name,
        year      = io$metadata$profile$year      %||% profile$year,
        currency  = io$metadata$profile$currency  %||% profile$currency,
        io_source = io$metadata$profile$io_source %||% profile$io_source
      )
    }

    # Intermediate consumption: langsung dari RDS, fallback ke full_io_table
    if (!is.null(io$intermediate_consumption)) {
      intermediate_consumption <- io$intermediate_consumption
    } else {
      sector_idx <- which(code_registry$Type == "Sector")
      intermediate_consumption <- full_io_table[sector_idx, sector_idx, drop = FALSE]
    }
    storage.mode(intermediate_consumption) <- "numeric"

    # Land proportion
    land_prop <- io$land_proportion
    P_default <- as.matrix(land_prop)
    storage.mode(P_default) <- "numeric"

    # Sector index: constrained / unconstrained
    if (!is.null(io$constrained) && !is.null(io$unconstrained)) {
      constrained   <- io$constrained
      unconstrained <- io$unconstrained
    } else {
      berbasis_lahan <- tolower(trimws(as.character(io$sector$Berbasis_Lahan)))
      constrained    <- as.numeric(berbasis_lahan %in% c("ya", "yes", "true", "1"))
      unconstrained  <- 1 - constrained
    }

  } else if (grepl("\\.xlsx$|\\.xls$", path, ignore.case = TRUE)) {

    # =====================================================
    # EXCEL BRANCH
    # =====================================================
    sheets <- readxl::excel_sheets(path)
    
    profile_sheet <- intersect(
      c("Profile Data", "Profil Data", "0. Profil Data", "Profile", "Profil"),
      sheets
    )[1]
    
    if (!is.na(profile_sheet) && length(profile_sheet) == 1) {
      profile_raw <- tryCatch(
        readxl::read_excel(path, sheet = profile_sheet, col_names = TRUE),
        error = function(e) NULL
      )
      
      if (!is.null(profile_raw) && nrow(profile_raw) > 0) {
        pnames <- tolower(trimws(as.character(profile_raw[[1]])))
        pvals  <- as.character(profile_raw[[2]])
        
        pick <- function(pattern) {
          idx <- grep(pattern, pnames, ignore.case = TRUE)
          if (length(idx) > 0) trimws(pvals[idx[1]]) else NA_character_
        }
        
        area_name <- pick("lokasi|area")
        year_val  <- pick("tahun|year")
        currency  <- pick("mata uang|currency")
        io_source <- pick("sumber|source")
        
        profile <- list(
          area_name = if (!is.na(area_name)) area_name else profile$area_name,
          year      = if (!is.na(year_val))  as.integer(year_val) else profile$year,
          currency  = if (!is.na(currency))  currency  else profile$currency,
          io_source = if (!is.na(io_source)) io_source else profile$io_source
        )
      }
    }
    
    gdp_sheet_candidates <- c("GDP", "Analisis Deskriptif", "Analisis_Deskriptif",
                              "Gdp", "gdp")
    gdp_sheet <- intersect(gdp_sheet_candidates, sheets)[1]
    
    if (is.na(gdp_sheet) || length(gdp_sheet) == 0) {
      stop("Sheet 'GDP' (atau alias 'Analisis Deskriptif') tidak ditemukan. ",
           "Sheet tersedia: ", paste(sheets, collapse = ", "))
    }
    
    gdp_baseline <- read_input_file(path, sheet = gdp_sheet, header = TRUE)
    if (is.null(gdp_baseline$CATEGORY)) {
      gdp_baseline$CATEGORY <- rep("Unknown", nrow(gdp_baseline))
    }

    leontief_matrix <- as.matrix(
      read_input_file(path, sheet = "Leontief_Matrix", as_matrix = TRUE)
    )

    full_io_table <- as.matrix(
      read_input_file(path, sheet = "Full_IO_Table", as_matrix = TRUE)
    )

    code_registry <- read_input_file(path, sheet = "Code_Registry", header = TRUE)

    income_multiplier <- read_input_file(path, sheet = "Multiplier_Income", header = TRUE)
    labour_multiplier <- read_input_file(path, sheet = "Multiplier_Labour", header = TRUE)
    SUB_multiplier    <- read_input_file(path, sheet = "Multiplier_SUB",    header = TRUE)
    output_multiplier <- read_input_file(path, sheet = "Multiplier_Output", header = TRUE)
    linkage_table     <- read_input_file(path, sheet = "Linkage",           header = TRUE)

    # Land proportion — deteksi fleksibel berbagai format
    land_prop_raw <- read_input_file(path, sheet = "Land_Proportion", header = TRUE)
    col_names     <- colnames(land_prop_raw)
    
    # ---- Deteksi format ----
    # A. Format baru: kolom "V1", "V2", ... (LC codes) → orientasi Sektor × LC
    # B. Format lama: kolom "S1", "S2", ... (Sector codes) → orientasi LC × Sektor (perlu transpose)
    # C. Format label: ada kolom "Sektor"/"ID" + kolom numerik
    
    sector_cols <- grep("^S\\d+$", col_names, value = TRUE)   # S1, S2, ...
    lc_cols     <- grep("^V\\d+$", col_names, value = TRUE)   # V1, V2, ...
    
    if (length(lc_cols) > 0) {
      # ---- Format A: V1, V2, ... (26 LC) — orientasi sudah Sektor × LC ----
      P_default <- as.matrix(land_prop_raw[, lc_cols, drop = FALSE])
      storage.mode(P_default) <- "numeric"
      
    } else if (length(sector_cols) > 0) {
      # ---- Format B: S1, S2, ... (66 sektor) — orientasi LC × Sektor, perlu transpose ----
      P_default <- t(as.matrix(land_prop_raw[, sector_cols, drop = FALSE]))
      storage.mode(P_default) <- "numeric"
      
    } else {
      # ---- Format C: kolom pertama = label, sisanya LC ----
      label_candidates <- c("Sektor", "SECTOR", "Sector", "sector",
                            "ID", "id", "Kode", "KODE", "LC")
      label_col <- intersect(label_candidates, col_names)
      
      numeric_cols <- setdiff(col_names, label_col)
      if (length(numeric_cols) == 0) {
        stop("Land_Proportion sheet tidak memiliki kolom tutupan lahan yang valid.")
      }
      
      P_default <- suppressWarnings(
        as.matrix(land_prop_raw[, numeric_cols, drop = FALSE])
      )
      P_default <- apply(P_default, 2, function(x) as.numeric(as.character(x)))
      if (is.null(dim(P_default))) P_default <- matrix(P_default, ncol = 1)
      P_default[is.na(P_default)] <- 0
      storage.mode(P_default) <- "numeric"
    }

    # Intermediate consumption from Full IO Table
    sector_idx <- which(code_registry$Type == "Sector")
    intermediate_consumption <- full_io_table[sector_idx, sector_idx, drop = FALSE]
    storage.mode(intermediate_consumption) <- "numeric"

    # Growth rate
    growth_rate <- tryCatch(
      read_input_file(path, sheet = "Growth_Rate", header = TRUE),
      error = function(e) NULL
    )

    # Sector index: constrained / unconstrained from sector sheet
    sector_tbl <- read_input_file(path, sheet = "Sector", header = TRUE)
    berbasis_lahan <- tolower(trimws(as.character(sector_tbl[[3]])))
    constrained    <- as.numeric(berbasis_lahan %in% c("ya", "yes", "true", "1"))
    unconstrained  <- 1 - constrained

  } else {
    stop("Unsupported file format. Please provide .xlsx or .rds.")
  }

  # =====================================================
  # AREA MATRIX BAU
  # =====================================================
  sector_idx <- which(code_registry$Type == "Sector")

  projection_area_bau <- read_input_file(projection_area_file_bau, header = TRUE)

  projection_long_bau <- projection_area_bau |>
    tidyr::pivot_longer(
      cols = -c(ID, LC),
      names_to = "YEAR",
      values_to = "AREA"
    ) |>
    dplyr::mutate(YEAR = as.numeric(YEAR))

  area_matrix_bau <- create_area_matrix(projection_long_bau)

  # =====================================================
  # RETURN
  # =====================================================
  list(
    gdp_baseline             = gdp_baseline,
    leontief_matrix          = leontief_matrix,
    full_io_table            = full_io_table,
    code_registry            = code_registry,
    sector_idx               = sector_idx,
    intermediate_consumption = intermediate_consumption,
    income_multiplier        = income_multiplier,
    labour_multiplier        = labour_multiplier,
    SUB_multiplier           = SUB_multiplier,
    output_multiplier        = output_multiplier,
    linkage_table            = linkage_table, 
    land_prop                = P_default, # proporsi lahan dari tahap IO, Data input statis
    P_default                = P_default, # parameter awal untuk proyeksi, Parameter yang bisa di-override
    growth_rate              = growth_rate,
    area_matrix_bau          = area_matrix_bau,
    projection_area_bau      = projection_area_bau,
    constrained              = constrained,
    unconstrained            = unconstrained,
    profile                  = profile
  )
}

#' Load scenario inputs with fallback to baseline
#'
#' @param sc_input Named list berisi path file skenario:
#'   `luc_projection`, `land_distribution`, `growth_rate`, `lpc`,
#'   `io_changes`, `intensity`.
#' @param P_default Matriks proporsi lahan baseline.
#' @param growth_rate Data growth rate baseline.
#'
#' @return List berisi:
#'   \item{luc_projection}{Path ke file proyeksi lahan}
#'   \item{area_matrix_scenario}{Matriks tutupan lahan skenario (opsional)}
#'   \item{P}{Matriks proporsi lahan skenario}
#'   \item{lpc_scenario}{Tibble LPC skenario atau NULL}
#'   \item{io_changes}{Tibble perubahan IO atau NULL}
#'
#' @export
load_scenario_inputs <- function(sc_input, P_default) {
  
  # --- A. Land Distribution (sheet "land_proportion") ------------------------
  # Default: gunakan P_default baseline
  P_sc <- P_default
  
  if (!is.null(sc_input$land_distribution) &&
      file.exists(sc_input$land_distribution)) {
    
    # Cek apakah sheet "land_proportion" ada di file
    sheet_names <- readxl::excel_sheets(sc_input$land_distribution)
    
    if ("land_proportion" %in% sheet_names) {
      # ✓ Sheet ada → baca dan proses
      cat("✓ Sheet 'land_proportion' ditemukan → gunakan proporsi skenario\n")
      
      land_prop_sc <- read_input_file(
        sc_input$land_distribution,
        header = TRUE,
        sheet  = "land_proportion"
      )
      
      # Kolom LC = semua kolom selain ID dan Sektor
      lc_cols <- setdiff(colnames(land_prop_sc), c("ID", "Sektor"))
      
      if (length(lc_cols) == 0) {
        stop("Tidak ditemukan kolom tutupan lahan pada sheet 'land_proportion'.")
      }
      
      # Keluarkan kolom ID & Sektor → matriks numerik
      P_sc <- as.matrix(land_prop_sc[, lc_cols])
      storage.mode(P_sc) <- "numeric"
      
      # Set nama baris = Sektor, kolom = nama tutupan lahan
      if ("Sektor" %in% colnames(land_prop_sc)) {
        rownames(P_sc) <- as.character(land_prop_sc$Sektor)
      }
      
      # Validasi dimensi
      if (ncol(P_sc) != ncol(P_default)) {
        stop("Jumlah kolom tutupan lahan pada skenario (", ncol(P_sc),
             ") tidak sesuai dengan baseline (", ncol(P_default), ").")
      }
      if (nrow(P_sc) != nrow(P_default)) {
        stop("Jumlah baris sektor pada skenario (", nrow(P_sc),
             ") tidak sesuai dengan baseline (", nrow(P_default), ").")
      }
      
      # Samakan nama dengan P_default
      rownames(P_sc) <- rownames(P_default)
      colnames(P_sc) <- colnames(P_default)
      
    } else {
      # ✗ Sheet tidak ada → pakai P_default
      cat("ℹ Sheet 'land_proportion' tidak ditemukan → gunakan P_default baseline\n")
      P_sc <- P_default
    }
    
  } else {
    cat("ℹ File land_distribution tidak tersedia → gunakan P_default baseline\n")
    P_sc <- P_default
  }
  
  # --- B. LUC Projection (sheet "luc_projection") ----------------------------
  area_matrix_sc <- NULL
  if (!is.null(sc_input$luc_projection) &&
      file.exists(sc_input$luc_projection)) {
    
    sheet_names  <- readxl::excel_sheets(sc_input$luc_projection)
    sheet_target <- if ("luc_projection" %in% sheet_names) {
      "luc_projection"
    } else {
      1  # fallback: sheet pertama
    }
    
    df_luc <- read_input_file(
      sc_input$luc_projection,
      header = TRUE,
      sheet  = sheet_target
    )
    
    # Asumsi: kolom pertama = ID, kolom kedua = LC, sisanya tahun
    projection_long <- df_luc |>
      tidyr::pivot_longer(
        cols      = -c(ID, LC),
        names_to  = "YEAR",
        values_to = "AREA"
      ) |>
      dplyr::mutate(YEAR = as.numeric(YEAR))
    
    area_matrix_sc <- create_area_matrix(projection_long)
  }
  
  # --- C. LPC Productivity ---------------------------------------------------
  lpc_sc <- NULL
  
  if (!is.null(sc_input$lpc) && file.exists(sc_input$lpc)) {
    # 1. Panggil read_input_file dengan parameter sheet dan skip
    # Catatan: skip = 11 melompati 11 baris judul/info sehingga Baris 12 menjadi Header Kolom.
    lpc_sc <- read_input_file(
      path = sc_input$lpc,
      sheet = "Skenario_LPC",
      skip = 11, # Sesuaikan dengan template
      header = TRUE
    )
    
    # 2. Ekstraksi & Konversi Kolom secara Fleksibel (Mendukung Template Baru & Lama)
    if ("LPC SKENARIO 2 (Rp Juta/Ha)" %in% colnames(lpc_sc)) {
      # Format Template produktivitas_lpc.xlsx Baru
      lpc_sc$LPC_S2 <- as.numeric(lpc_sc$`LPC SKENARIO 2 (Rp Juta/Ha)`)
      
      if ("TARGET GROWTH FACTOR (%)" %in% colnames(lpc_sc)) {
        lpc_sc$Growth_Factor <- as.numeric(lpc_sc$`TARGET GROWTH FACTOR (%)`)
      }
    } else if ("Increment_Rate" %in% colnames(lpc_sc)) {
      # Format Skrip Lama
      lpc_sc$Increment_Rate <- as.numeric(lpc_sc$Increment_Rate)
      
      if ("Area_Implemented" %in% colnames(lpc_sc)) {
        lpc_sc$Area_Implemented <- as.numeric(lpc_sc$Area_Implemented)
      }
    }
  }
  
  # --- D. IO Changes / Restrukturisasi (sheet "Restrukturisasi") ------------
  io_sc <- NULL
  if (!is.null(sc_input$io_changes) && file.exists(sc_input$io_changes)) {
    sheet_names <- readxl::excel_sheets(sc_input$io_changes)
    
    # Deteksi nama sheet Restrukturisasi (fleksibel "Restrukturisasi" atau "Skenario_4_Restrukturisasi")
    if ("Restrukturisasi" %in% sheet_names) {
      sheet_target <- "Restrukturisasi"
      skip_rows <- 11
    } else if ("Skenario_4_Restrukturisasi" %in% sheet_names) {
      sheet_target <- "Skenario_4_Restrukturisasi"
      skip_rows <- 11
    } else {
      sheet_target <- 1
      skip_rows <- 0
    }
    
    io_sc <- read_input_file(
      path   = sc_input$io_changes,
      sheet  = sheet_target,
      skip   = skip_rows,
      header = TRUE
    )
    
    # Penanganan format kolom lama (jika ada Change_Pct)
    if ("Change_Pct" %in% colnames(io_sc)) {
      io_sc$Change_Pct <- as.numeric(io_sc$Change_Pct) / 100
    }
  }
  
  
  list(
    luc_projection       = sc_input$luc_projection,
    area_matrix_scenario = area_matrix_sc,
    P                    = P_sc,
    lpc_scenario         = lpc_sc,
    io_changes           = io_sc
  )
}

#' Load Population Projection from Excel or CSV
#'
#' Membaca file proyeksi penduduk dengan 2 kolom wajib:
#' `Tahun` (atau `YEAR`) dan `Populasi` (atau `POPULATION`).
#' Nama kolom dideteksi secara fleksibel (case-insensitive).
#'
#' @param path Character. Path ke file `.xlsx`, `.xls`, atau `.csv`.
#' @param sheet Character atau Integer. Nama/indeks sheet untuk file Excel.
#'   Default: sheet pertama.
#'
#' @return Tibble dengan kolom `Tahun` (integer) dan `Populasi` (numeric),
#'   terurut menaik berdasarkan Tahun. Mengembalikan `NULL` jika file tidak
#'   valid atau kolom wajib tidak ditemukan.
#'
#' @export
load_population_projection <- function(path, sheet = 1) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  
  # ---- 1. Baca file sesuai ekstensi ----
  ext <- tolower(tools::file_ext(path))
  
  df <- tryCatch({
    if (ext %in% c("xlsx", "xls")) {
      as.data.frame(readxl::read_excel(path, sheet = sheet))
    } else if (ext == "csv") {
      as.data.frame(readr::read_csv(path, show_col_types = FALSE))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    warning("[load_population_projection] Gagal baca file: ", conditionMessage(e))
    NULL
  })
  
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # ---- 2. Deteksi kolom Tahun dan Populasi secara fleksibel ----
  col_names <- tolower(trimws(colnames(df)))
  
  year_candidates <- c("tahun", "year", "yr", "periode", "period")
  pop_candidates  <- c("populasi", "population", "pop", "jumlah_penduduk",
                       "jumlah penduduk", "penduduk", "total_population")
  
  year_col <- which(col_names %in% year_candidates)
  pop_col  <- which(col_names %in% pop_candidates)
  
  if (length(year_col) == 0 || length(pop_col) == 0) {
    warning(
      "[load_population_projection] Kolom 'Tahun' dan/atau 'Populasi' tidak ditemukan. ",
      "Kolom tersedia: ", paste(colnames(df), collapse = ", ")
    )
    return(NULL)
  }
  
  # Ambil kolom pertama jika ada duplikat
  year_col <- year_col[1]
  pop_col  <- pop_col[1]
  
  # ---- 3. Sanitasi nilai ----
  years <- suppressWarnings(as.integer(df[[year_col]]))
  pops  <- suppressWarnings(as.numeric(df[[pop_col]]))
  
  # Buang baris dengan NA
  valid_idx <- !is.na(years) & !is.na(pops) & pops > 0
  years <- years[valid_idx]
  pops  <- pops[valid_idx]
  
  if (length(years) == 0) {
    warning("[load_population_projection] Tidak ada baris valid setelah sanitasi.")
    return(NULL)
  }
  
  # ---- 4. Urutkan dan kembalikan tibble ----
  ord <- order(years)
  tibble::tibble(
    Tahun    = years[ord],
    Populasi = pops[ord]
  )
}


#' Interpolate Population Vector to Match Projection Years
#'
#' Menginterpolasi (dan ekstrapolasi linear) vektor populasi agar sesuai
#' dengan rentang tahun proyeksi. Jika tahun berada di luar rentang data,
#' ekstrapolasi linear menggunakan 2 titik terdekat.
#'
#' @param pop_df Tibble hasil `load_population_projection()`.
#' @param target_years Numeric vector. Tahun proyeksi yang diinginkan.
#'
#' @return Numeric vector sepanjang `length(target_years)` dengan nilai
#'   populasi hasil interpolasi.
#'
#' @export
interpolate_population <- function(pop_df, target_years) {
  if (is.null(pop_df) || nrow(pop_df) == 0) {
    stop("[interpolate_population] pop_df kosong.")
  }
  
  pop_df <- pop_df[order(pop_df$Tahun), ]
  
  # Jika hanya 1 titik, anggap populasi konstan
  if (nrow(pop_df) == 1) {
    return(rep(pop_df$Populasi[1], length(target_years)))
  }
  
  # Interpolasi linear; ekstrapolasi otomatis dengan rule = 2
  # (batas kiri/kanan = titik terdekat)
  approx(
    x    = pop_df$Tahun,
    y    = pop_df$Populasi,
    xout = target_years,
    method = "linear",
    rule   = 2,
    ties   = mean
  )$y
}