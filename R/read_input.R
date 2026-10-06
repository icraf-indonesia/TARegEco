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

  if (grepl("\\.rds$", path, ignore.case = TRUE)) {

    # =====================================================
    # RDS BRANCH
    # =====================================================
    io <- readRDS(path)

    gdp_baseline      <- io$gdp_baseline
    leontief_matrix   <- io$linkage$Leontief
    full_io_table     <- io$full_io_table
    code_registry     <- io$code_registry
    income_multiplier <- io$multipliers$income
    labour_multiplier <- io$multipliers$labour
    SUB_multiplier    <- io$multipliers$sub
    output_multiplier <- io$multipliers$output
    linkage_table     <- io$linkage
    growth_rate       <- io$growth_rate

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
    gdp_baseline <- read_input_file(path, sheet = "GDP", header = TRUE)
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

    # Land proportion (LC x Sektor) -> transpose ke Sektor x LC
    land_prop_raw <- read_input_file(path, sheet = "Land_Proportion", header = TRUE)
    sector_cols <- grep("^S\\d+", colnames(land_prop_raw), value = TRUE)
    if (length(sector_cols) == 0) {
      stop("Land_Proportion sheet tidak memiliki kolom S1, S2, ...")
    }
    P_default <- t(as.matrix(land_prop_raw[, sector_cols]))
    storage.mode(P_default) <- "numeric"

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
    land_prop                = land_prop,
    P_default                = P_default,
    growth_rate              = growth_rate,
    area_matrix_bau          = area_matrix_bau,
    projection_area_bau      = projection_area_bau,
    constrained              = constrained,
    unconstrained            = unconstrained
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
#'   \item{growth_rate}{Growth rate skenario}
#'   \item{lpc_scenario}{Tibble LPC skenario atau NULL}
#'   \item{io_changes}{Tibble perubahan IO atau NULL}
#'   \item{intensity_table}{Tibble intensitas atau NULL}
#'
#' @export
load_scenario_inputs <- function(sc_input, P_default, growth_rate) {
  
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
  
  # --- C. Growth Rate --------------------------------------------------------
  growth_rate_sc <- growth_rate
  if (!is.null(sc_input$growth_rate) && file.exists(sc_input$growth_rate)) {
    growth_rate_sc <- read_input_file(sc_input$growth_rate, header = TRUE)
  }
  
  # --- D. LPC Productivity ---------------------------------------------------
  lpc_sc <- NULL
  if (!is.null(sc_input$lpc) && file.exists(sc_input$lpc)) {
    lpc_sc <- read_input_file(sc_input$lpc, header = TRUE)
    lpc_sc$Increment_Rate   <- as.numeric(lpc_sc$Increment_Rate)
    lpc_sc$Area_Implemented <- as.numeric(lpc_sc$Area_Implemented)
  }
  
  # --- E. IO Changes ---------------------------------------------------------
  io_sc <- NULL
  if (!is.null(sc_input$io_changes) && file.exists(sc_input$io_changes)) {
    io_sc <- read_input_file(sc_input$io_changes, header = TRUE)
    io_sc$Change_Pct <- as.numeric(io_sc$Change_Pct) / 100
  }
  
  # --- F. Intensity ----------------------------------------------------------
  int_sc <- NULL
  if (!is.null(sc_input$intensity) && file.exists(sc_input$intensity)) {
    int_sc <- read_input_file(sc_input$intensity, header = TRUE)
    int_sc$Intensity <- as.numeric(int_sc$Intensity) / 100
  }
  
  list(
    luc_projection       = sc_input$luc_projection,
    area_matrix_scenario = area_matrix_sc,
    P                    = P_sc,
    growth_rate          = growth_rate_sc,
    lpc_scenario         = lpc_sc,
    io_changes           = io_sc,
    intensity_table      = int_sc
  )
}