#' Create Input-Output template Excel file
#'
#' Generates an Excel template with blank sheets that match the expected
#' structure of the Input-Output model. This template helps users prepare
#' their own data correctly.
#'
#' @param output_path Character. Path to the output `.xlsx` file.
#'
#' @return Invisibly returns the output path.
#'
#' @details
#' The template contains the following sheets:
#' - `sector`: 3 columns (Sector, Category, Land-based)
#' - `intermediate_consumption`: blank matrix (users fill with numeric values)
#' - `final_demand`: first two columns for component code and label,
#'   followed by empty columns for each sector
#' - `added_value`: first two columns for component code and label,
#'   followed by empty columns for each sector
#' - `labour`: one numeric column
#' - `land_proportion`: blank matrix where column sums should be 1
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @examples
#' \dontrun{
#' create_io_template("inst/extdata/template/template_io.xlsx")
#' }
#'
#' @export
create_io_template <- function(output_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to create Excel template.")
  }
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "sector")
  openxlsx::writeData(wb, "sector", data.frame(
    Sector = character(),
    Category = character(),
    Land_based = logical()
  ))
  
  openxlsx::addWorksheet(wb, "intermediate_consumption")
  openxlsx::writeData(wb, "intermediate_consumption", matrix(NA, 5, 5),
                      colNames = FALSE, rowNames = FALSE)
  
  openxlsx::addWorksheet(wb, "final_demand")
  openxlsx::writeData(wb, "final_demand", data.frame(
    Code = integer(),
    Label = character(),
    S1 = numeric(), S2 = numeric(), S3 = numeric(), S4 = numeric(), S5 = numeric()
  ), colNames = TRUE, rowNames = FALSE)
  
  openxlsx::addWorksheet(wb, "added_value")
  openxlsx::writeData(wb, "added_value", data.frame(
    Code = integer(),
    Label = character(),
    S1 = numeric(), S2 = numeric(), S3 = numeric(), S4 = numeric(), S5 = numeric()
  ), colNames = TRUE, rowNames = FALSE)
  
  openxlsx::addWorksheet(wb, "labour")
  openxlsx::writeData(wb, "labour", data.frame(Labour = numeric()))
  
  openxlsx::addWorksheet(wb, "land_proportion")
  openxlsx::writeData(wb, "land_proportion", matrix(NA, 5, 3),
                      colNames = FALSE, rowNames = FALSE)
  
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
  
  invisible(output_path)
}

#' Create scenario template Excel file
#'
#' Generates an Excel workbook containing blank templates for all five
#' scenario input assumptions used in the projection module.
#'
#' @param gdp_baseline Data frame. Baseline GDP table from descriptive analysis.
#'   It must contain `SECTOR` and `CATEGORY` columns.
#' @param code_registry Data frame. Code registry from [build_full_io_table()].
#' @param initial_year Numeric. First year of projection.
#' @param end_year Numeric. Last year of projection.
#' @param time_step Numeric. Time step in years.
#' @param output_path Character. Path to the output `.xlsx` file.
#'
#' @return Invisibly returns the output path.
#'
#' @details
#' The workbook contains the following sheets:
#' - `README`: instructions for filling the template
#' - `1_Luasan_Lahan`: land area (ha) per land cover per projection year
#' - `2_Distribusi_Lahan`: land distribution proportions (optional)
#' - `3_Growth_Rate`: sectoral growth rates (optional)
#' - `4_Produktivitas_LPC`: LPC productivity scenario (optional)
#' - `5_Perubahan_IO`: IO table changes (optional)
#' - `Code_Registry_Ref`: sector code reference
#' - `6_Intensitas`: implementation intensity (optional)
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @export
create_scenario_templates <- function(gdp_baseline,
                                      code_registry,
                                      initial_year,
                                      end_year,
                                      time_step,
                                      output_path = "Scenario_Input_Templates.xlsx") {
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' diperlukan.")
  }
  
  wb <- openxlsx::createWorkbook()
  
  n_sectors <- nrow(gdp_baseline)
  if (n_sectors == 0) stop("gdp_baseline kosong.")
  
  sector_codes <- as.character(1:n_sectors)
  sector_names <- as.character(gdp_baseline$SECTOR)
  
  header_style <- openxlsx::createStyle(
    fontColour = "#FFFFFF", fgFill = "#4472C4", halign = "CENTER",
    textDecoration = "bold", border = "TopBottomLeftRight"
  )
  editable_style <- openxlsx::createStyle(
    fgFill = "#FFF2CC", border = "TopBottomLeftRight"
  )
  info_style <- openxlsx::createStyle(
    fgFill = "#D9D9D9", border = "TopBottomLeftRight"
  )
  title_style <- openxlsx::createStyle(
    fontSize = 12, textDecoration = "bold", fgFill = "#2F5496",
    fontColour = "#FFFFFF"
  )
  
  periods_df <- create_period_table(initial_year, end_year, time_step)
  years_proj <- seq(initial_year + time_step, end_year, by = time_step)
  n_periods <- nrow(periods_df)
  
  # README
  openxlsx::addWorksheet(wb, "README")
  instructions <- data.frame(
    No = c(1,2,3,4,5,6,7,8),
    Keterangan = c(
      "PETUNJUK PENGISIAN TEMPLATE SKENARIO",
      "Kode sektor (Sector_Code) adalah NOMOR BARIS sektor di sheet GDP (1,2,3,...)",
      "1_Luasan_Lahan (WAJIB): ID, LC, dan tahun proyeksi. Nilai dalam hektar.",
      "2_Distribusi_Lahan (Opsional): Proporsi lahan per sektor. Total per kolom sektor = 1.",
      "3_Growth_Rate (Opsional): Sector_Code, Sector, Growth_Rate (desimal).",
      "4_Produktivitas_LPC (Opsional): Crop_Type 'annual' atau 'perennial'.",
      "5_Perubahan_IO (Opsional): Row_Code, Col_Code, Change_Pct (%).",
      "6_Intensitas (Opsional): Intensity 0-100 per sektor per periode."
    )
  )
  openxlsx::writeData(wb, "README", instructions)
  openxlsx::addStyle(wb, "README", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "README", cols = 1, widths = 6)
  openxlsx::setColWidths(wb, "README", cols = 2, widths = 95)
  
  # 1_Luasan_Lahan
  openxlsx::addWorksheet(wb, "1_Luasan_Lahan")
  n_lc_example <- 5
  luasan_list <- list(
    ID = 1:n_lc_example,
    LC = c("Sawah Irigasi","Sawah Tadah Hujan","Kebun Campuran","Hutan","Permukiman")
  )
  for (yr in years_proj) luasan_list[[as.character(yr)]] <- rep(0, n_lc_example)
  luasan_template <- as.data.frame(luasan_list, stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "1_Luasan_Lahan", luasan_template, startRow = 3, startCol = 1)
  openxlsx::addStyle(wb, "1_Luasan_Lahan", header_style, rows = 3, cols = 1:ncol(luasan_template), gridExpand = TRUE)
  openxlsx::addStyle(wb, "1_Luasan_Lahan", info_style, rows = 4:(n_lc_example+3), cols = 1:2, gridExpand = TRUE)
  openxlsx::addStyle(wb, "1_Luasan_Lahan", editable_style, rows = 4:(n_lc_example+3), cols = 3:ncol(luasan_template), gridExpand = TRUE)
  openxlsx::setColWidths(wb, "1_Luasan_Lahan", cols = 1, widths = 8)
  openxlsx::setColWidths(wb, "1_Luasan_Lahan", cols = 2, widths = 24)
  openxlsx::setColWidths(wb, "1_Luasan_Lahan", cols = 3:ncol(luasan_template), widths = 14)
  
  # 2_Distribusi_Lahan
  openxlsx::addWorksheet(wb, "2_Distribusi_Lahan")
  
  n_lc <- n_lc_example
  
  dist_template <- data.frame(
    ID = seq_len(n_sectors),
    Sektor = sector_names,
    matrix(0, nrow = n_sectors, ncol = n_lc)
  )
  
  lc_names <- paste0("LC", seq_len(n_lc))
  colnames(dist_template)[3:(2 + n_lc)] <- lc_names
  
  openxlsx::writeData(wb, "2_Distribusi_Lahan", dist_template,
                      startRow = 4, startCol = 1)
  openxlsx::addStyle(wb, "2_Distribusi_Lahan", header_style,
                     rows = 4, cols = 1:ncol(dist_template), gridExpand = TRUE)
  openxlsx::addStyle(wb, "2_Distribusi_Lahan", info_style,
                     rows = 5:(n_sectors + 4), cols = 1:2, gridExpand = TRUE)
  openxlsx::addStyle(wb, "2_Distribusi_Lahan", editable_style,
                     rows = 5:(n_sectors + 4), cols = 3:ncol(dist_template), gridExpand = TRUE)
  
  openxlsx::setColWidths(wb, "2_Distribusi_Lahan", cols = 1, widths = 6)
  openxlsx::setColWidths(wb, "2_Distribusi_Lahan", cols = 2, widths = 30)
  openxlsx::setColWidths(wb, "2_Distribusi_Lahan", cols = 3:ncol(dist_template), widths = 10)
  
  # 3_Growth_Rate
  openxlsx::addWorksheet(wb, "3_Growth_Rate")
  growth_template <- data.frame(
    Sector_Code = sector_codes,
    Sector = sector_names,
    Growth_Rate = rep(0.03, n_sectors)
  )
  openxlsx::writeData(wb, "3_Growth_Rate", growth_template, startRow = 3, startCol = 1)
  openxlsx::addStyle(wb, "3_Growth_Rate", header_style, rows = 3, cols = 1:3, gridExpand = TRUE)
  openxlsx::addStyle(wb, "3_Growth_Rate", info_style, rows = 4:(n_sectors+3), cols = 1:2, gridExpand = TRUE)
  openxlsx::addStyle(wb, "3_Growth_Rate", editable_style, rows = 4:(n_sectors+3), cols = 3, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "3_Growth_Rate", cols = 1, widths = 14)
  openxlsx::setColWidths(wb, "3_Growth_Rate", cols = 2, widths = 50)
  openxlsx::setColWidths(wb, "3_Growth_Rate", cols = 3, widths = 16)
  
  # 4_Produktivitas_LPC
  openxlsx::addWorksheet(wb, "4_Produktivitas_LPC")
  example_n <- min(3, n_sectors)
  lpc_sector_codes <- sector_codes[1:example_n]
  lpc_sector_names <- sector_names[1:example_n]
  n_row_lpc <- example_n * n_periods
  lpc_template <- data.frame(
    Sector_Code = rep(lpc_sector_codes, each = n_periods),
    Sector = rep(lpc_sector_names, each = n_periods),
    Start_Year = rep(periods_df$Start_Year, example_n),
    End_Year = rep(periods_df$End_Year, example_n),
    Increment_Rate = rep(0.02, n_row_lpc),
    Area_Implemented = rep(50, n_row_lpc),
    Crop_Type = rep("annual", n_row_lpc),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "4_Produktivitas_LPC", lpc_template, startRow = 7, startCol = 1)
  openxlsx::addStyle(wb, "4_Produktivitas_LPC", header_style, rows = 7, cols = 1:7, gridExpand = TRUE)
  openxlsx::addStyle(wb, "4_Produktivitas_LPC", info_style, rows = 8:(n_row_lpc+7), cols = 1:4, gridExpand = TRUE)
  openxlsx::addStyle(wb, "4_Produktivitas_LPC", editable_style, rows = 8:(n_row_lpc+7), cols = 5:7, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "4_Produktivitas_LPC", cols = 1:2, widths = 14)
  openxlsx::setColWidths(wb, "4_Produktivitas_LPC", cols = 3:4, widths = 12)
  openxlsx::setColWidths(wb, "4_Produktivitas_LPC", cols = 5:6, widths = 18)
  openxlsx::setColWidths(wb, "4_Produktivitas_LPC", cols = 7, widths = 14)
  
  # 5_Perubahan_IO
  openxlsx::addWorksheet(wb, "5_Perubahan_IO")
  io_template <- data.frame(
    Start_Year = rep(periods_df$Start_Year[1], 3),
    End_Year = rep(periods_df$End_Year[1], 3),
    Row_Code = c(5, 12, 8),
    Row_Description = rep("(isi deskripsi)", 3),
    Col_Code = c(8, 3, 15),
    Col_Description = rep("(isi deskripsi)", 3),
    Change_Pct = c(10, -5, 20),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "5_Perubahan_IO", io_template, startRow = 3, startCol = 1)
  openxlsx::addStyle(wb, "5_Perubahan_IO", header_style, rows = 3, cols = 1:7, gridExpand = TRUE)
  openxlsx::addStyle(wb, "5_Perubahan_IO", editable_style, rows = 4:6, cols = c(1,2,3,5,7), gridExpand = TRUE)
  openxlsx::addStyle(wb, "5_Perubahan_IO", info_style, rows = 4:6, cols = c(4,6), gridExpand = TRUE)
  openxlsx::setColWidths(wb, "5_Perubahan_IO", cols = 1:2, widths = 12)
  openxlsx::setColWidths(wb, "5_Perubahan_IO", cols = 3, widths = 14)
  openxlsx::setColWidths(wb, "5_Perubahan_IO", cols = 4, widths = 20)
  openxlsx::setColWidths(wb, "5_Perubahan_IO", cols = 5, widths = 14)
  openxlsx::setColWidths(wb, "5_Perubahan_IO", cols = 6, widths = 20)
  openxlsx::setColWidths(wb, "5_Perubahan_IO", cols = 7, widths = 16)
  
  # Code_Registry_Ref
  openxlsx::addWorksheet(wb, "Code_Registry_Ref")
  openxlsx::writeData(wb, "Code_Registry_Ref", code_registry, startRow = 3, startCol = 1)
  openxlsx::addStyle(wb, "Code_Registry_Ref", header_style, rows = 3, cols = 1:ncol(code_registry), gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Code_Registry_Ref", cols = 1:ncol(code_registry), widths = 20)
  
  # 6_Intensitas
  openxlsx::addWorksheet(wb, "6_Intensitas")
  intensity_template <- data.frame(
    Sector_Code = rep(sector_codes[1:min(3, n_sectors)], each = n_periods),
    Sector = rep(sector_names[1:min(3, n_sectors)], each = n_periods),
    Start_Year = rep(periods_df$Start_Year, min(3, n_sectors)),
    End_Year = rep(periods_df$End_Year, min(3, n_sectors)),
    Intensity = rep(100, min(3, n_sectors) * n_periods),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "6_Intensitas", intensity_template, startRow = 4, startCol = 1)
  openxlsx::addStyle(wb, "6_Intensitas", header_style, rows = 4, cols = 1:5, gridExpand = TRUE)
  openxlsx::addStyle(wb, "6_Intensitas", info_style, rows = 5:(nrow(intensity_template)+4), cols = 1:4, gridExpand = TRUE)
  openxlsx::addStyle(wb, "6_Intensitas", editable_style, rows = 5:(nrow(intensity_template)+4), cols = 5, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "6_Intensitas", cols = 1:2, widths = 14)
  openxlsx::setColWidths(wb, "6_Intensitas", cols = 3:4, widths = 12)
  openxlsx::setColWidths(wb, "6_Intensitas", cols = 5, widths = 14)
  
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
  
  invisible(output_path)
}