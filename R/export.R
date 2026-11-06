#' Save preprocessing data to RDS
#'
#' Saves the input and validation results into an RDS file so that the
#' preprocessing stage can be reloaded before running the core analysis.
#'
#' @param io_preprocessing List containing preprocessed input data.
#' @param output_dir Character. Directory to save the RDS file.
#'   Default is `"inst/extdata/output"`.
#'
#' @return Invisibly returns the output directory.
#' @export
save_preprocessing_data <- function(io_preprocessing,
                                    output_dir = "inst/extdata/output") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(io_preprocessing,
          file = file.path(output_dir, "descriptive_input_preprocessing.rds"))
  invisible(output_dir)
}

#' Save core analysis results to RDS
#'
#' Saves the results of the core Input-Output analysis into an RDS file so
#' that visualisation can be performed without recalculating.
#'
#' @param core_analysis_results List containing core analysis outputs.
#' @param output_dir Character. Directory to save the RDS file.
#'   Default is `"inst/extdata/output"`.
#'
#' @return Invisibly returns the output directory.
#' @export
save_core_analysis_data <- function(core_analysis_results,
                                    output_dir = "inst/extdata/output") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(core_analysis_results,
          file = file.path(output_dir, "descriptive_core_analysis.rds"))
  invisible(output_dir)
}

#' Set Excel Worksheet Tab Color
#'
#' Sets the tab color of a worksheet within an `openxlsx` workbook object by
#' directly manipulating the internal XML structure of the sheet. This is a
#' workaround for `openxlsx`, which does not expose a public function to set
#' the tab color directly.
#'
#' @param wb An `openxlsx` Workbook object (created via
#'   \code{\link[openxlsx]{createWorkbook}}).
#' @param sheet Character. The name of the worksheet whose tab color should be
#'   set. Must match an existing sheet in `wb`.
#' @param color Character. A hex color code (with or without leading `"#"`),
#'   e.g. `"#C00000"` or `"C00000"`.
#'
#' @return The modified `openxlsx` Workbook object (invisibly returned for
#'   piping convenience).
#'
#' @details
#' The function inserts a `<sheetPr><tabColor .../></sheetPr>` node into the
#' sheet's XML metadata. The alpha channel is fixed to `"FF"` (fully opaque).
#' If the given `sheet` name is not found in `wb`, the workbook is returned
#' unchanged.
#'
#' @keywords internal
#' @noRd
set_tab_color <- function(wb, sheet, color) {
  # Hapus tanda '#' jika ada di awal hex color
  hex_color <- gsub("^#", "", color)
  
  # Akses objek internal workbook
  sheet_index <- which(names(wb) == sheet)
  if (length(sheet_index) > 0) {
    # Manipulasi XML internal workbook
    wb$worksheets[[sheet_index]]$sheetPr <- sprintf(
      '<sheetPr><tabColor rgb="FF%s"/></sheetPr>',
      toupper(hex_color)
    )
  }
  wb
}

#' Export Input-Output Descriptive Analysis Results to Excel
#'
#' Generates a multi-tabbed Excel workbook (`.xlsx`) containing both the 
#' baseline analysis output (descriptive statistics, multipliers, linkages, 
#' and balance details) and protected raw input tables for data integrity tracking.
#'
#' @param sector Data frame or NULL. Economic sector definitions/categories.
#' @param profile List or NULL. Metadata containing location, base year, currency, and IO source.
#' @param gdp_baseline Data frame. Baseline GDP and sectoral economic indicators.
#' @param linkage_result List. Contains Leontief inverse matrix and linkage indices.
#' @param growth_rate Numeric vector or data frame. Baseline Final Demand growth rates.
#' @param labour Numeric vector, list, or data frame. Sectoral labor data.
#' @param OM List. Output Multiplier calculation results and table.
#' @param IM List. Income Multiplier calculation results and table.
#' @param LM List. Labor Multiplier calculation results and table.
#' @param SUBM List. Gross Operating Surplus (SUB) Multiplier results and details.
#' @param land_proportion Matrix or data frame. Baseline land proportion matrix (P).
#' @param landuse_table Data frame or NULL. Historical/baseline land cover matrix.
#' @param linkage_table Data frame. Summarized Backward and Forward Linkage table.
#' @param full_io_table Matrix or data frame. Complete baseline Input-Output table.
#' @param code_registry Data frame. Registry of sector codes and classifications.
#' @param balance_detail Data frame or NULL. Detailed sector-by-sector IO balance checks.
#' @param output_path Character. The destination file path for the exported Excel file.
#'   Defaults to `"data/Hasil_Analisis_Modul_Deskriptif.xlsx"`.
#'
#' @return Invisibly returns the `output_path` string upon successful file creation.
#'
#' @details
#' The exported workbook uses distinct tab colors for clarity:
#' \itemize{
#'   \item \strong{Blue Tab}: Readme / File Guide.
#'   \item \strong{Green Tabs}: Editable output sheets (Descriptive analysis, multipliers, linkages).
#'   \item \strong{Red Tabs}: Read-only protected input sheets (Raw IO table, land use, parameters).
#' }
#'
#' @export

export_io_analysis <- function(sector,
                               profile,
                               gdp_baseline,
                               linkage_result,
                               growth_rate,
                               labour,
                               OM, IM, LM, SUBM,
                               land_proportion,
                               landuse_table,
                               linkage_table,
                               full_io_table,
                               code_registry,
                               balance_detail,
                               output_path = "data/Hasil_Analisis_Modul_Deskriptif.xlsx") {
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to export Excel files.")
  }
  
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  
  wb <- openxlsx::createWorkbook()
  
  # Helper internal untuk set warna tab
  set_tab_color <- function(wb, sheet, color) {
    hex_color <- gsub("^#", "", color)
    sheet_index <- which(names(wb) == sheet)
    if (length(sheet_index) > 0) {
      wb$worksheets[[sheet_index]]$sheetPr <- sprintf(
        '<sheetPr><tabColor rgb="FF%s"/></sheetPr>',
        toupper(hex_color)
      )
    }
    wb
  }
  
  # =========================================================================
  # DEFINISI WARNA & STYLE
  # =========================================================================
  tab_input_color  <- "#C00000" 
  tab_output_color <- "#00B050" 
  
  style_input_cells <- openxlsx::createStyle(
    fgFill = "#F8CBAD", 
    border = "TopBottomLeftRight",
    borderColour = "#FFFFFF"
  )
  
  # =========================================================================
  # DAFTAR SHEET
  # =========================================================================
  output_sheets <- c(
    "Analisis Deskriptif", "Leontief_Matrix", "Multiplier_Output",
    "Multiplier_Income", "Multiplier_Labour", "Multiplier_SUB",
    "Linkage", "Balance_Detail"
  )
  
  input_sheets <- c(
    "Sector", "Growth_Rate", "Land_Cover", "Land_Proportion",
    "Full_IO_Table", "Code_Registry"
  )
  
  # -------------------------------------------------------------------------
  # SHEET 1: README
  # -------------------------------------------------------------------------
  openxlsx::addWorksheet(wb, "Readme")
  readme_text <- data.frame(
    Info = c(
      "PETUNJUK PENGGUNAAN FILE",
      "",
      "File ini berisi hasil analisis deskriptif ekonomi regional.",
      "",
      "PEMBEDAAN WARNA TAB:",
      "1. Tab Hijau: Lembar OUTPUT/HASIL ANALISIS (dapat diedit).",
      "2. Tab Merah: Lembar INPUT (data asli yang diunggah, TIDAK DAPAT DIEDIT).",
      "",
      "DAFTAR SHEET:",
      "- Readme: Penjelasan mengenai file ini.",
      "- Analisis Deskriptif: Tabel PDRB dan indikator per sektor.",
      "- Leontief_Matrix: Matriks Leontief inverse.",
      "- Multiplier_*: Tabel pengganda (Output, Income, Labour, SUB).",
      "- Linkage: Indeks keterkaitan antar sektor.",
      "- Balance_Detail: Status keseimbangan Input-Output per sektor.",
      "- Sector: Data sektor ekonomi (Input).",
      "- Growth_Rate: Laju pertumbuhan (Input).",
      "- Land_Cover: Luas tutupan lahan (Input).",
      "- Land_Proportion: Matriks proporsi lahan (Input).",
      "- Full_IO_Table: Tabel Input-Output lengkap (Input).",
      "- Code_Registry: Registrasi kode (Input).",
      "",
      "CATATAN:",
      "Seluruh sheet yang berwarna merah (Input) tidak dapat diedit",
      "untuk menjaga integritas data."
    )
  )
  
  openxlsx::writeData(wb, sheet = "Readme", x = readme_text, colNames = FALSE)
  
  # -------------------------------------------------------------------------
  # SHEET: PROFILE DATA (TAMBAHAN BARU)
  # -------------------------------------------------------------------------
  if (!is.null(profile)) {
    # Ubah list profile menjadi data frame 2 kolom (Parameter & Keterangan)
    profile_df <- data.frame(
      Parameter = c("Lokasi / Area", "Tahun IO", "Mata Uang", "Sumber IO"),
      Keterangan = c(
        as.character(profile$area_name),
        as.numeric(profile$year),
        as.character(profile$currency),
        as.character(profile$io_source)
      ),
      stringsAsFactors = FALSE
    )
    
    openxlsx::addWorksheet(wb, "Profile Data")
    openxlsx::writeData(wb, sheet = "Profile Data", x = profile_df)
    
    # Opsional: Beri style bold pada header (baris 1)
    header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9D9D9")
    openxlsx::addStyle(wb, sheet = "Profile Data", style = header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
  }
  
  # -------------------------------------------------------------------------
  # SHEET OUTPUT
  # -------------------------------------------------------------------------
  # Sheet "Analisis Deskriptif" (sebelumnya "GDP")
  openxlsx::addWorksheet(wb, "Analisis Deskriptif")
  openxlsx::writeData(wb, sheet = "Analisis Deskriptif", x = gdp_baseline)
  
  openxlsx::addWorksheet(wb, "Leontief_Matrix")
  openxlsx::writeData(wb, sheet = "Leontief_Matrix",
                      x = as.data.frame(linkage_result$Leontief),
                      rowNames = FALSE, colNames = FALSE)
  
  openxlsx::addWorksheet(wb, "Multiplier_Output")
  openxlsx::writeData(wb, sheet = "Multiplier_Output", x = OM$table)
  
  openxlsx::addWorksheet(wb, "Multiplier_Income")
  openxlsx::writeData(wb, sheet = "Multiplier_Income", x = IM$table)
  
  labour_table <- LM$table |>
    dplyr::mutate(
      Labour_Compensation = SUBM$compensation,
      Labour = as.numeric(unlist(labour))
    )
  openxlsx::addWorksheet(wb, "Multiplier_Labour")
  openxlsx::writeData(wb, sheet = "Multiplier_Labour", x = labour_table)
  
  sub_table <- SUBM$table |>
    dplyr::mutate(Surplus = SUBM$surplus)
  openxlsx::addWorksheet(wb, "Multiplier_SUB")
  openxlsx::writeData(wb, sheet = "Multiplier_SUB", x = sub_table)
  
  openxlsx::addWorksheet(wb, "Linkage")
  openxlsx::writeData(wb, sheet = "Linkage", x = linkage_table)
  
  if (!missing(balance_detail) && !is.null(balance_detail)) {
    openxlsx::addWorksheet(wb, "Balance_Detail")
    openxlsx::writeData(wb, sheet = "Balance_Detail", x = balance_detail)
  }
  
  # -------------------------------------------------------------------------
  # SHEET INPUT
  # -------------------------------------------------------------------------
  if (!is.null(sector)) {
    openxlsx::addWorksheet(wb, "Sector")
    openxlsx::writeData(wb, sheet = "Sector", x = sector)
  } else {
    sector_fallback <- gdp_baseline[, c("SECTOR", "CATEGORY")]
    openxlsx::addWorksheet(wb, "Sector")
    openxlsx::writeData(wb, sheet = "Sector", x = sector_fallback)
  }
  
  openxlsx::addWorksheet(wb, "Growth_Rate")
  openxlsx::writeData(wb, sheet = "Growth_Rate", x = as.data.frame(growth_rate))
  
  if (!missing(landuse_table) && !is.null(landuse_table)) {
    openxlsx::addWorksheet(wb, "Land_Cover")
    openxlsx::writeData(wb, sheet = "Land_Cover", x = landuse_table)
  }
  
  openxlsx::addWorksheet(wb, "Land_Proportion")
  openxlsx::writeData(wb, sheet = "Land_Proportion",
                      x = land_proportion,
                      rowNames = TRUE, colNames = TRUE)
  
  openxlsx::addWorksheet(wb, "Full_IO_Table")
  openxlsx::writeData(wb, sheet = "Full_IO_Table",
                      x = full_io_table,
                      rowNames = TRUE, colNames = TRUE)
  
  openxlsx::addWorksheet(wb, "Code_Registry")
  openxlsx::writeData(wb, sheet = "Code_Registry", x = code_registry)
  
  # =========================================================================
  # PEWARNAAN & PROTEKSI
  # =========================================================================
  wb <- set_tab_color(wb, sheet = "Readme", color = "#4472C4")
  
  for (s in output_sheets) {
    if (s %in% names(wb)) {
      wb <- set_tab_color(wb, sheet = s, color = tab_output_color)
    }
  }
  
  for (s in input_sheets) {
    if (s %in% names(wb)) {
      wb <- set_tab_color(wb, sheet = s, color = tab_input_color)
    }
  }
  
  # Warnai Sel Input dengan Oranye & Proteksi
  for (s in input_sheets) {
    if (s %in% names(wb)) {
      openxlsx::addStyle(wb, sheet = s, style = style_input_cells,
                         rows = 1:1000, cols = 1:50,
                         gridExpand = TRUE, stack = TRUE)
      
      openxlsx::protectWorksheet(wb, sheet = s, protect = TRUE,
                                 lockFormattingCells = TRUE)
    }
  }
  
  # =========================================================================
  # AUTO WIDTH
  # =========================================================================
  for (s in names(wb)) {
    tryCatch({
      openxlsx::setColWidths(wb, sheet = s, cols = 1:100, widths = "auto")
    }, error = function(e) {})
  }
  
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
  invisible(output_path)
}

#' Save final descriptive analysis data to RDS or RData
#'
#' Saves a single combined object containing all inputs and results of the
#' descriptive Input-Output analysis. This file is intended to be loaded by
#' the projection module or other downstream modules.
#'
#' @param sector Tibble of sectors.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param added_value_m Numeric matrix of added value.
#' @param labour Labour vector or one-column matrix/tibble.
#' @param land_proportion Numeric matrix of land proportions.
#' @param growth_rate Tibble or data frame of growth rates.
#' @param landuse_table Tibble with columns `id`, `lc`, `area`.
#' @param gdp_baseline Tibble returned by [calculate_gdp()].
#' @param linkage_result List returned by [compute_linkage()].
#' @param OM Output from [calc_output_multiplier()].
#' @param IM Output from [calc_income_multiplier()].
#' @param LM Output from [calc_labour_multiplier()].
#' @param SUBM Output from [calc_sub_multiplier()].
#' @param full_io_table Numeric matrix from [build_full_io_table()].
#' @param code_registry Tibble from [build_full_io_table()].
#' @param output_dir Character. Directory to save the RDS file.
#'   Default is `"inst/extdata/output"`.
#' @param combined_file Character. Filename for the combined object.
#'   Default is `"descriptive_analysis_module.rds"`.
#' @param format Character. Either `"rds"` (default) or `"rdata"`.
#'
#' @return Invisibly returns the output directory.
#'
#' @examples
#' \dontrun{
#' save_analysis_data(
#'   sector                   = sector,
#'   intermediate_consumption_m = intermediate_consumption.m,
#'   final_demand_m           = final_demand.m,
#'   added_value_m            = added_value.m,
#'   labour                   = labour,
#'   land_proportion          = land_proportion,
#'   growth_rate              = growth_rate,
#'   landuse_table            = landuse_table,
#'   gdp_baseline             = gdp_baseline,
#'   linkage_result           = linkage_result,
#'   OM = OM, IM = IM, LM = LM, SUBM = SUBM,
#'   full_io_table            = full_io_table,
#'   code_registry            = code_registry,
#'   output_dir               = "inst/extdata/output"
#' )
#' }
#'
#' @export
save_analysis_data <- function(sector,
                               intermediate_consumption_m,
                               final_demand_m,
                               added_value_m,
                               labour,
                               land_proportion,
                               growth_rate,
                               landuse_table,
                               gdp_baseline,
                               linkage_result,
                               OM, IM, LM, SUBM,
                               full_io_table,
                               code_registry,
                               output_dir = "inst/extdata/output",
                               combined_file = "descriptive_analysis_module.rds",
                               format = c("rds", "rdata")) {
  
  format <- match.arg(format)
  
  # -----------------------------------------------
  # Derive sector index
  # -----------------------------------------------
  berbasis_lahan <- tolower(trimws(as.character(sector$Berbasis_Lahan)))
  constrained    <- as.numeric(berbasis_lahan %in% c("ya", "yes", "true", "1"))
  unconstrained  <- 1 - constrained
  
  # -----------------------------------------------
  # Full combined list
  # -----------------------------------------------
  io <- list(
    sector                   = sector,
    constrained              = constrained,
    unconstrained            = unconstrained,
    intermediate_consumption = intermediate_consumption_m,
    final_demand             = final_demand_m,
    final_demand_component   = if (exists("final_demand_component")) final_demand_component else NULL,
    added_value              = added_value_m,
    added_value_lookup       = if (exists("added_value_lookup")) added_value_lookup else NULL,
    labour                   = labour,
    land_proportion          = land_proportion,
    P_default                = if (exists("P_default")) P_default else NULL,
    landuse_table            = landuse_table,
    growth_rate              = growth_rate,
    gdp_baseline             = gdp_baseline,
    linkage                  = linkage_result,
    multipliers = list(
      output = OM$table,
      income = IM$table,
      labour = LM$table  |> 
        dplyr::mutate(
          Labour_Compensation = SUBM$compensation,
          Labour = as.numeric(unlist(labour))
        ),
      sub    = SUBM$table |> 
        dplyr::mutate(Surplus = SUBM$surplus)
    ),
    full_io_table            = full_io_table,
    code_registry            = code_registry
  )
  
  # -----------------------------------------------
  # Create output directory
  # -----------------------------------------------
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # -----------------------------------------------
  # Save combined object
  # -----------------------------------------------
  if (format == "rds") {
    saveRDS(io, file = file.path(output_dir, combined_file))
  } else {
    save(io, file = file.path(output_dir, sub("\\.rds$", ".RData", combined_file)))
  }
  
  invisible(output_dir)
}

#' Save projection scenario results to RDS
#'
#' Saves the result of a single projection scenario (BAU or intervention) into
#' an RDS file. The file name is derived from the scenario label after
#' sanitising invalid characters.
#'
#' @param scenario_label Character. Name of the scenario, e.g., `"BAU"` or
#'   `"Skenario_Pertanian_Maju"`.
#' @param result Data frame. Projection result returned by [run_projection()].
#' @param area_matrix Numeric matrix. Area matrix used in the projection.
#' @param inputs List. Input parameters used in the projection.
#' @param output_dir Character. Directory to save the RDS file.
#'   Default is `"inst/extdata/output"`.
#'
#' @return Invisibly returns the full path to the saved RDS file.
#' @export
save_projection_results <- function(scenario_label,
                                    result,
                                    area_matrix,
                                    inputs,
                                    output_dir = "inst/extdata/output") {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  safe_label <- gsub("[^A-Za-z0-9_]", "_", scenario_label)
  file_name <- paste0("projection_", safe_label, ".rds")
  full_path <- file.path(output_dir, file_name)
  
  output <- list(
    scenario    = scenario_label,
    result      = result,
    area_matrix = area_matrix,
    inputs      = inputs
  )
  
  saveRDS(output, full_path)
  invisible(full_path)
}