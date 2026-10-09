# =========================================================
# TA Regional Economy: Projection — FULL APP
# =========================================================
# Integrasi UI mockup + run_projection() dengan dukungan
# kombinasi bebas skenario BAU + Asumsi 1/2/3.
#
# Jalankan : shiny::runApp("app.R")
# Paket    : shiny, shinyjs, bslib, shinyFiles, ggplot2, scales,
#            V8, readxl, openxlsx, dplyr, tidyr, tibble
# =========================================================

library(shiny)
library(shinyjs)
library(bslib)
library(shinyFiles)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(readxl)
library(openxlsx)
library(scales)

# Load all functions from R/
source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(source_files, source))

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

app_css <- "
  .scenario-box {
    border: 1px solid #dee2e6;
    border-radius: 6px;
    padding: 12px 14px;
    margin-top: 10px;
    background-color: #f8f9fa;
  }
  .section-title { font-weight: 600; margin: 6px 0; }
  .note { font-size: 12px; color: #6c757d; }
  .period-preview { font-size: 12px; color: #6c757d; margin: -6px 0 10px 0; }
"

btn_style <- function(bg) {
  sprintf("font-size: 18px; padding: 10px 15px; background-color: %s; color: white;", bg)
}

xlsx_types <- c(".xlsx", ".xls")
rds_types  <- c(".rds")

# ---------------------------------------------------------
# Indicator metadata untuk grafik & rekapitulasi
# ---------------------------------------------------------
indicator_meta <- list(
  PDRB_Total_Juta          = list(label = "PDRB Total (Rp Juta)",              col = "GDP"),
  Serapan_TK_Orang         = list(label = "Serapan Tenaga Kerja (Orang)",      col = "LABOUR"),
  Pendapatan_TK_Juta       = list(label = "Pendapatan Tenaga Kerja (Rp Juta)", col = "INCOME"),
  Surplus_Usaha_Bruto_Juta = list(label = "Surplus Usaha Bruto (Rp Juta)",     col = "SURPLUS"),
  Output_Total_Juta        = list(label = "Output Total (Rp Juta)",            col = "OUTPUT")
)

indicator_choices <- setNames(
  names(indicator_meta),
  vapply(indicator_meta, function(m) m$label, character(1))
)

# ---------------------------------------------------------
# Baca tabel GDP baseline
# - Mode .rds : readRDS() lalu ambil komponen $gdp_baseline
# - Mode excel: read_excel(sheet = "GDP")
# ---------------------------------------------------------
read_baseline_gdp <- function(use_rds, rds_file, xlsx_file) {
  if (use_rds) {
    if (is.null(rds_file)) return(NULL)
    obj <- tryCatch(readRDS(rds_file$datapath), error = function(e) NULL)
    if (is.null(obj)) return(NULL)
    
    # Format 1: obj langsung gdp_baseline
    if (is.data.frame(obj) && "SECTOR" %in% names(obj)) return(as.data.frame(obj))
    
    # Format 2: list dengan $gdp_baseline
    if (is.list(obj) && !is.null(obj$gdp_baseline)) {
      return(as.data.frame(obj$gdp_baseline))
    }
    return(NULL)
  }
  if (is.null(xlsx_file)) return(NULL)
  as.data.frame(readxl::read_excel(xlsx_file$datapath, sheet = "GDP"))
}

# =========================================================
# UI
# =========================================================

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$style(HTML(app_css))
  ),
  
  titlePanel("Regional Economic Projection"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # ---------------------------------------------------
      # 1. BASELINE
      # ---------------------------------------------------
      div(class = "section-title", "1. Baseline dari Analisis Deskriptif"),
      
      checkboxInput(
        "load_rds",
        "Load from Descriptive Analysis .rds (skip Excel reading)",
        value = FALSE
      ),
      
      conditionalPanel(
        condition = "input.load_rds == false",
        fileInput(
          "io_output_file",
          "IO Analysis Output (.xlsx) (wajib)",
          accept = xlsx_types,
          placeholder = "Pilih IO_Analysis_Output.xlsx"
        )
      ),
      
      conditionalPanel(
        condition = "input.load_rds == true",
        fileInput(
          "descriptive_rds_file",
          "Descriptive Analysis .rds (wajib)",
          accept = rds_types,
          placeholder = "Pilih Descriptive_Analysis_Module.rds"
        )
      ),
      
      fileInput(
        "projection_area_file",
        "Proyeksi Luas Lahan BAU (.xlsx) (wajib)",
        accept = xlsx_types,
        placeholder = "Pilih luc_projection.xlsx"
      ),
      tags$hr(),
      
      # ---------------------------------------------------
      # 2. PERIODE PROYEKSI
      # ---------------------------------------------------
      div(class = "section-title", "2. Periode proyeksi"),
      
      fluidRow(
        column(4, numericInput("initial_year", "Tahun awal", value = 2025, min = 1990, step = 1)),
        column(4, numericInput("end_year", "Tahun akhir", value = 2049, min = 1991, step = 1)),
        column(4, numericInput("time_step", "Interval (tahun)", value = 5, min = 1, step = 1))
      ),
      div(class = "period-preview", textOutput("period_preview")),
      
      # ---------------------------------------------------
      # 3. SKENARIO
      # ---------------------------------------------------
      tags$hr(),
      div(class = "section-title", "3. Skenario Intervensi"),
      
      textInput("scenario_name", "Nama Skenario Intervensi",
                placeholder = "mis. Skenario Ekonomi Hijau"),
      p(
        "Satu skenario dapat menggabungkan Asumsi 1, 2, dan 3. Kosongkan file pada asumsi yang tidak dipakai.",
        class = "note"
      ),
      
      downloadButton("download_template", "Unduh Template Skenario"),
      p(
        "Berisi nama sektor dan LPC baseline dari data deskriptif. Muat file baseline terlebih dulu.",
        class = "note",
        style = "margin-top: 6px;"
      ),
      
      tabsetPanel(
        id = "assumption_tabs",
        type = "pills",
        
        tabPanel(
          "A1: Lahan",
          div(
            class = "scenario-box",
            tags$b("Asumsi 1: Perubahan Lahan"),
            p("Simulasi dampak perubahan luas dan distribusi penggunaan lahan.",
              class = "note"),
            fileInput("a1_land_file", "Perubahan Lahan (.xlsx)",
                      accept = xlsx_types, placeholder = "luc_projection.xlsx"),
            div(class = "note", textOutput("a1_sheet_check")),
            # Tombol reset A1
            div(
              style = "margin-top: 8px;",
              actionButton("reset_a1", "Hapus Asumsi 1",
                           icon = icon("trash"),
                           style = "background-color: #dc3545; color: white; font-size: 12px; padding: 4px 10px;")
            )
          )
        ),
        
        tabPanel(
          "A2: Produktivitas",
          div(
            class = "scenario-box",
            tags$b("Asumsi 2: Peningkatan Produktivitas"),
            p("Simulasi peningkatan produktivitas lahan (LPC) menuju target.",
              class = "note"),
            fileInput("a2_lpc_file", "Target Produktivitas LPC (.xlsx)",
                      accept = xlsx_types, placeholder = "produktivitas_lpc.xlsx"),
            # Tombol reset A2
            div(
              style = "margin-top: 8px;",
              actionButton("reset_a2", "Hapus Asumsi 2",
                           icon = icon("trash"),
                           style = "background-color: #dc3545; color: white; font-size: 12px; padding: 4px 10px;")
            )
          )
        ),
        
        tabPanel(
          "A3: Hilirisasi",
          div(
            class = "scenario-box",
            tags$b("Asumsi 3: Hilirisasi"),
            p("Perubahan struktur I-O via RAS balancing.", class = "note"),
            fileInput("a3_io_changes_file", "Perubahan Struktur I-O (.xlsx)",
                      accept = xlsx_types, placeholder = "perubahan_io.xlsx"),
            # Tombol reset A3
            div(
              style = "margin-top: 8px;",
              actionButton("reset_a3", "Hapus Asumsi 3",
                           icon = icon("trash"),
                           style = "background-color: #dc3545; color: white; font-size: 12px; padding: 4px 10px;")
            )
          )
        )
      ),
      
      # ---------------------------------------------------
      # METADATA & OUTPUT
      # ---------------------------------------------------
      tags$hr(),
      tags$b("Metadata (auto-detected)"),
      verbatimTextOutput("metadata_preview"),
      
      tags$hr(),
      
      div(
        style = "display: flex; flex-direction: column; gap: 10px;",
        shinyDirButton("wd", "Select Output Directory", "Please select a directory"),
        textOutput("selected_directory"),
        actionButton("processTAReg2", "Run Analysis", style = btn_style("#4CAF50")),
        hidden(downloadButton("download_results", "Download Results (.xlsx)",
                              style = btn_style("#008CBA"))),
        hidden(actionButton("open_report", "Open Report", style = btn_style("#008CBA"))),
        hidden(actionButton("open_output_folder", "Open Output Folder", style = btn_style("#008CBA"))),
        actionButton("returnButton", "Return to Main Menu", style = btn_style("#FA8072"))
      )
    ),
    
    mainPanel(
      width = 7,
      tabsetPanel(
        
        tabPanel("User Guide", uiOutput("user_guide")),
        
        tabPanel(
          "Scenario Summary",
          br(),
          div(tags$b("Nama skenario: "), textOutput("scenario_name_text", inline = TRUE)),
          h5("Komponen skenario", style = "margin-top: 14px;"),
          tableOutput("scenario_summary"),
          
          hr(),
          h5("Perbandingan Proyeksi"),
          selectInput("chart_indicator", "Indikator",
                      choices = indicator_choices,
                      selected = "PDRB_Total_Juta"),
          plotOutput("projection_plot", height = "420px"),
          h5("Rekapitulasi Proyeksi", style = "margin-top: 14px;"),
          tableOutput("recap_table"),
          
          hr(),
          uiOutput("linkage_section")
        ),
        
        tabPanel(
          "Log",
          br(),
          textOutput("selected_dir"),
          verbatimTextOutput("status_messages"),
          verbatimTextOutput("error_messages")
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  observe({
    toggleState("reset_a1", condition = !is.null(a1_file()))
    toggleState("reset_a2", condition = !is.null(a2_file()))
    toggleState("reset_a3", condition = !is.null(a3_file()))
  })
  
  # ==========================================================================
  # RESET HANDLER PER ASUMSI
  # ==========================================================================
  
  # Simpan state file A1/A2/A3 secara reaktif
  # (fileInput tidak bisa di-set NULL langsung, jadi kita pakai reactiveValues)
  a1_file <- reactiveVal(NULL)
  a2_file <- reactiveVal(NULL)
  a3_file <- reactiveVal(NULL)
  
  # Sync input fileInput ke reactiveVal
  observeEvent(input$a1_land_file,   { a1_file(input$a1_land_file) })
  observeEvent(input$a2_lpc_file,    { a2_file(input$a2_lpc_file) })
  observeEvent(input$a3_io_changes_file, { a3_file(input$a3_io_changes_file) })
  
  # Handler tombol Reset A1
  observeEvent(input$reset_a1, {
    a1_file(NULL)
    # Reset UI fileInput dengan JavaScript
    shinyjs::runjs("
      var input = document.getElementById('a1_land_file');
      if (input) { input.value = ''; }
      var label = input && input.parentElement.querySelector('.form-control');
      if (label) { label.textContent = 'luc_projection.xlsx'; }
    ")
    showNotification("Asumsi 1: Perubahan Lahan dihapus.", type = "warning", duration = 3)
  })
  
  # Handler tombol Reset A2
  observeEvent(input$reset_a2, {
    a2_file(NULL)
    shinyjs::runjs("
      var input = document.getElementById('a2_lpc_file');
      if (input) { input.value = ''; }
      var label = input && input.parentElement.querySelector('.form-control');
      if (label) { label.textContent = 'produktivitas_lpc.xlsx'; }
    ")
    showNotification("Asumsi 2: Peningkatan Produktivitas dihapus.", type = "warning", duration = 3)
  })
  
  # Handler tombol Reset A3
  observeEvent(input$reset_a3, {
    a3_file(NULL)
    shinyjs::runjs("
      var input = document.getElementById('a3_io_changes_file');
      if (input) { input.value = ''; }
      var label = input && input.parentElement.querySelector('.form-control');
      if (label) { label.textContent = 'perubahan_io.xlsx'; }
    ")
    showNotification("Asumsi 3: Hilirisasi dihapus.", type = "warning", duration = 3)
  })
  
  baseline_sheets <- c(
    "GDP", "Leontief_Matrix", "Growth_Rate", "Multiplier_Output",
    "Multiplier_Income", "Multiplier_Labour", "Multiplier_SUB",
    "Land_Proportion", "Linkage", "Full_IO_Table", "Code_Registry"
  )
  
  # ---- Output directory ----
  roots <- c(Home = path.expand("~"))
  shinyDirChoose(input, "wd", roots = roots, session = session)
  
  output_dir <- reactive({
    if (is.null(input$wd) || is.integer(input$wd)) return(NULL)
    parseDirPath(roots, input$wd)
  })
  
  dir_text <- reactive({
    d <- output_dir()
    if (is.null(d) || length(d) == 0) "Folder output belum dipilih." else as.character(d)
  })
  
  output$selected_directory <- renderText(dir_text())
  output$selected_dir       <- renderText(paste("Output directory:", dir_text()))
  
  # ---- Label periode ----
  periods <- reactive({
    validate(
      need(input$time_step >= 1, "Interval minimal 1 tahun."),
      need(
        input$end_year - input$time_step >= input$initial_year,
        "Tahun akhir harus minimal satu interval setelah tahun awal."
      )
    )
    start <- seq(input$initial_year, input$end_year - input$time_step, by = input$time_step)
    paste0(start, "-", start + input$time_step)
  })
  
  output$period_preview <- renderText({
    paste("Periode:", paste(periods(), collapse = ", "))
  })
  
  # ---- Nama skenario ----
  scen_label <- reactive({
    nm <- trimws(input$scenario_name)
    if (!nzchar(nm)) nm <- "Skenario"
    if (identical(toupper(nm), "BAU")) nm <- "BAU (skenario)"
    nm
  })
  
  output$scenario_name_text <- renderText({
    nm <- trimws(input$scenario_name)
    if (nzchar(nm)) nm else "belum diisi"
  })
  
  n_assumptions <- reactive({
    sum(
      !is.null(a1_file()),
      !is.null(a2_file()),
      !is.null(a3_file())
    )
  })
  
  # ---- Baseline: sektor & LPC dari data deskriptif ----
  baseline_sectors <- reactive({
    gdp <- tryCatch(
      read_baseline_gdp(isTRUE(input$load_rds),
                        input$descriptive_rds_file,
                        input$io_output_file),
      error = function(e) NULL
    )
    if (is.null(gdp) || !all(c("SECTOR", "CATEGORY", "LPC") %in% names(gdp))) return(NULL)
    
    data.frame(
      Sector_Code  = seq_len(nrow(gdp)),
      Sector       = as.character(gdp$SECTOR),
      Category     = as.character(gdp$CATEGORY),
      LPC_Baseline = as.numeric(gdp$LPC),
      stringsAsFactors = FALSE
    )
  })
  
  output$metadata_preview <- renderText({
    use_rds <- isTRUE(input$load_rds)
    f <- if (use_rds) input$descriptive_rds_file else input$io_output_file
    if (is.null(f)) return("Belum ada file baseline dipilih.")
    
    sec   <- baseline_sectors()
    lines <- paste0("File   : ", f$name)
    
    if (!use_rds) {
      sheets  <- tryCatch(readxl::excel_sheets(f$datapath), error = function(e) character(0))
      missing <- setdiff(baseline_sheets, sheets)
      lines <- c(
        lines,
        paste0("Sheet  : ",
               if (length(missing) == 0) "lengkap"
               else paste("kurang ->", paste(missing, collapse = ", ")))
      )
    } else {
      lines <- c(lines, "Mode   : .rds (skip Excel)")
    }
    
    lines <- c(lines,
               paste0("Sektor : ",
                      if (is.null(sec)) "belum terbaca" else nrow(sec)))
    paste(lines, collapse = "\n")
  })
  
  observe({
    toggleState("download_template", condition = !is.null(baseline_sectors()))
  })
  
  # ---- Cek sheet file Asumsi 1 ----
  a1_sheets <- reactive({
    f <- a1_file()
    if (is.null(f)) return(NULL)
    tryCatch(readxl::excel_sheets(f$datapath), error = function(e) character(0))
  })
  
  output$a1_sheet_check <- renderText({
    s <- a1_sheets()
    if (is.null(s)) return("")
    paste0(
      "luc_projection: ", if ("luc_projection" %in% s) "ada" else "TIDAK ADA",
      " | land_proportion: ", if ("land_proportion" %in% s) "ada" else "tidak ada (opsional)"
    )
  })
  
  # ---- Template skenario ----
  output$download_template <- downloadHandler(
    filename = function() {
      nm <- gsub("[^A-Za-z0-9_-]+", "_", trimws(input$scenario_name))
      if (!nzchar(nm)) "Template_Skenario.xlsx" else paste0("Template_", nm, ".xlsx")
    },
    content = function(file) {
      sec <- baseline_sectors()
      per <- tryCatch(periods(), error = function(e) character(0))
      wb  <- openxlsx::createWorkbook()
      
      if (is.null(sec)) {
        openxlsx::addWorksheet(wb, "Petunjuk")
        openxlsx::writeData(
          wb, "Petunjuk",
          data.frame(Catatan = "Data deskriptif belum dimuat. Muat file baseline, lalu unduh ulang template.")
        )
      } else {
        tpl <- sec
        tpl$Cycle_Type <- "Annual"
        for (p in per) {
          tpl[[paste("Increment_Rate", p)]]   <- NA_real_
          tpl[[paste("Area_Implemented", p)]] <- NA_real_
        }
        openxlsx::addWorksheet(wb, "lpc")
        openxlsx::writeData(wb, "lpc", tpl)
        
        openxlsx::addWorksheet(wb, "Petunjuk")
        openxlsx::writeData(
          wb, "Petunjuk",
          data.frame(
            Kolom = c("LPC_Baseline", "Cycle_Type", "Increment_Rate", "Area_Implemented"),
            Keterangan = c(
              "Output per hektar tahun dasar, dari analisis deskriptif. Jangan diubah.",
              "Annual (default) atau Perennial untuk komoditas bersiklus panjang.",
              "Peningkatan LPC pada periode tsb. Kosongkan jika sektor tidak diintervensi.",
              "Porsi area yang menerapkan peningkatan pada periode tsb."
            )
          )
        )
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---- Ringkasan komponen skenario ----
  output$scenario_summary <- renderTable({
    file_name <- function(x) if (is.null(x)) "-" else x$name
    assumption_status <- function(x) if (is.null(x)) "Tidak dipakai" else "Siap"
    
    a1_status <- if (is.null(a1_file())) {
      "Tidak dipakai"
    } else if (!("luc_projection" %in% a1_sheets())) {
      "Sheet luc_projection tidak ditemukan"
    } else {
      "Siap"
    }
    
    data.frame(
      Komponen = c(
        "Skenario dasar (BAU)",
        "Asumsi 1: Perubahan Lahan",
        "Asumsi 2: Peningkatan Produktivitas",
        "Asumsi 3: Hilirisasi"
      ),
      File = c(
        file_name(input$projection_area_file),
        file_name(a1_file()),
        file_name(a2_file()),
        file_name(a3_file())
      ),
      Status = c(
        if (is.null(input$projection_area_file)) "Belum diisi (wajib)" else "Siap",
        a1_status,
        assumption_status(a2_file()),
        assumption_status(a3_file())
      ),
      check.names = FALSE
    )
  })
  
  # ==========================================================================
  # CORE: REACTIVE BASELINE & PROYEKSI
  # ==========================================================================
  
  # ---- Reactive: baseline lengkap ----
  baseline_data <- eventReactive(input$processTAReg2, {
    use_rds <- isTRUE(input$load_rds)
    
    if (use_rds) {
      req(input$descriptive_rds_file)
      
      raw <- readRDS(input$descriptive_rds_file$datapath)
      
      if (!is.list(raw)) {
        stop("File .rds harus berisi list baseline dari modul deskriptif.")
      }
      # Kalau dibungkus $baseline
      if (is.null(raw$gdp_baseline) && !is.null(raw$baseline)) {
        raw <- raw$baseline
      }
      if (is.null(raw$gdp_baseline)) {
        stop("Struktur .rds tidak dikenali (tidak ada $gdp_baseline).")
      }
      
      baseline <- list()
      
      # ---- Direct fields ----
      baseline$gdp_baseline             <- raw$gdp_baseline
      baseline$intermediate_consumption <- raw$intermediate_consumption
      baseline$full_io_table            <- raw$full_io_table
      baseline$code_registry            <- raw$code_registry
      baseline$growth_rate              <- raw$growth_rate
      baseline$constrained              <- raw$constrained
      baseline$unconstrained            <- raw$unconstrained
      
      # ---- P_default: fallback dari land_proportion ----
      baseline$P_default <- if (!is.null(raw$P_default)) {
        as.matrix(raw$P_default)
      } else if (!is.null(raw$land_proportion)) {
        as.matrix(raw$land_proportion)
      } else {
        stop("P_default / land_proportion tidak ditemukan di .rds.")
      }
      storage.mode(baseline$P_default) <- "numeric"
      
      # ---- Multipliers: nested $multipliers ----
      if (!is.null(raw$multipliers)) {
        baseline$income_multiplier <- raw$multipliers$income
        baseline$labour_multiplier <- raw$multipliers$labour
        baseline$SUB_multiplier    <- raw$multipliers$sub
        baseline$output_multiplier <- raw$multipliers$output
      } else {
        baseline$income_multiplier <- raw$income_multiplier
        baseline$labour_multiplier <- raw$labour_multiplier
        baseline$SUB_multiplier    <- raw$SUB_multiplier
        baseline$output_multiplier <- raw$output_multiplier
      }
      
      # ---- Linkage: nested $linkage ----
      if (!is.null(raw$linkage)) {
        baseline$linkage_table   <- raw$linkage
        baseline$leontief_matrix <- raw$linkage$Leontief
      } else {
        baseline$linkage_table   <- raw$linkage_table
        baseline$leontief_matrix <- raw$leontief_matrix
      }
      
      # ---- sector_idx ----
      if (!is.null(raw$sector_idx)) {
        baseline$sector_idx <- raw$sector_idx
      } else if (!is.null(baseline$code_registry) && 
                 "Type" %in% colnames(baseline$code_registry)) {
        baseline$sector_idx <- which(baseline$code_registry$Type == "Sector")
      } else {
        baseline$sector_idx <- seq_len(nrow(baseline$gdp_baseline))
      }
      
      # ---- VALIDASI ----
      required <- c("gdp_baseline", "intermediate_consumption", "P_default",
                    "income_multiplier", "labour_multiplier", "SUB_multiplier",
                    "output_multiplier", "linkage_table", "leontief_matrix",
                    "growth_rate", "code_registry")
      
      nulls <- required[vapply(required, function(nm) is.null(baseline[[nm]]), logical(1))]
      if (length(nulls) > 0) {
        stop("Field wajib NULL di .rds: ", paste(nulls, collapse = ", "))
      }
      
      message(sprintf("✓ Baseline .rds: %d sektor × %d tutupan lahan, multipliers OK",
                      nrow(baseline$P_default), ncol(baseline$P_default)))
      
    } else {
      req(input$io_output_file)
      baseline <- load_projection_baseline(
        path = input$io_output_file$datapath,
        projection_area_file_bau = input$projection_area_file$datapath
      )
    }
    
    # ---- Load area_matrix_bau dari Excel ----
    req(input$projection_area_file)
    sc_bau <- load_scenario_inputs(
      list(
        luc_projection    = input$projection_area_file$datapath,
        land_distribution = NULL
      ),
      P_default = baseline$P_default
    )
    
    if (is.null(sc_bau$area_matrix_scenario)) {
      stop("Gagal membaca area_matrix dari file BAU: ", input$projection_area_file$name)
    }
    
    list(
      baseline        = baseline,
      area_matrix_bau = sc_bau$area_matrix_scenario
    )
  }, ignoreNULL = FALSE)
  
  # ---- Reactive: hasil proyeksi ----
  hasil_proyeksi <- eventReactive(input$processTAReg2, {
    
    bl <- baseline_data()
    req(bl$baseline, bl$area_matrix_bau)
    
    baseline        <- bl$baseline
    area_matrix_bau <- bl$area_matrix_bau
    
    # --- Args Asumsi 1 ---
    args_a1 <- list()
    if (!is.null(a1_file())) {
      sc_a1 <- load_scenario_inputs(
        list(
          luc_projection    = a1_file()$datapath,
          land_distribution = a1_file()$datapath
        ),
        P_default = baseline$P_default
      )
      args_a1$area_matrix_scenario <- sc_a1$area_matrix_scenario
      args_a1$P_scenario           <- sc_a1$P
    }
    
    # --- Args Asumsi 2 ---
    args_a2 <- list()
    if (!is.null(a2_file())) {
      sc_a2 <- load_scenario_inputs(
        list(lpc = a2_file()$datapath),
        P_default = baseline$P_default
      )
      args_a2$lpc_scenario    <- sc_a2$lpc_scenario
      args_a2$lpc_target_year <- input$end_year
    }
    
    # --- Args Asumsi 3 ---
    args_a3 <- list()
    if (!is.null(a3_file())) {
      sc_a3 <- load_scenario_inputs(
        list(io_changes = a3_file()$datapath),
        P_default = baseline$P_default
      )
      args_a3$io_changes <- sc_a3$io_changes
    }
    
    options(initial_year = input$initial_year)
    
    # --- Panggil run_projection dengan tryCatch ---
    hasil <- tryCatch(
      do.call(
        run_projection_scenario,
        c(
          list(
            gdp_baseline      = baseline$gdp_baseline,
            income_multiplier = baseline$income_multiplier,
            labour_multiplier = baseline$labour_multiplier,
            SUB_multiplier    = baseline$SUB_multiplier,
            output_multiplier = baseline$output_multiplier,
            linkage_table     = baseline$linkage_table,
            leontief_matrix   = baseline$leontief_matrix,
            full_io_table     = baseline$intermediate_consumption,
            sector_idx        = baseline$sector_idx,
            P_baseline        = baseline$P_default,
            area_matrix_bau   = area_matrix_bau,
            growth_rate       = baseline$growth_rate,
            constrained       = baseline$constrained,
            unconstrained     = baseline$unconstrained
          ),
          args_a1,
          args_a2,
          args_a3
        )
      ),
      error = function(e) {
        warning("[hasil_proyeksi] Error: ", conditionMessage(e))
        NULL
      }
    )
    
    # --- DEBUG (hapus setelah stabil) ---
    cat("==== DEBUG hasil_proyeksi ====\n")
    cat("class(hasil):", paste(class(hasil), collapse = ", "), "\n")
    cat("is.null(hasil):", is.null(hasil), "\n")
    if (!is.null(hasil)) {
      cat("nrow(hasil):", nrow(hasil), "\n")
      cat("colnames(hasil):", paste(colnames(hasil), collapse = ", "), "\n")
      cat("active:", paste(attr(hasil, "active_assumptions"), collapse = " + "), "\n")
      cat("sector_names length:", length(attr(hasil, "sector_names")), "\n")
    }
    cat("==============================\n")
    
    if (is.null(hasil)) return(NULL)
    
    list(
      scenario  = hasil,
      active    = attr(hasil, "active_assumptions"),
      baseline_meta = list(
        sector_names = attr(hasil, "sector_names"),
        years        = attr(hasil, "years")
      )
    )
  }, ignoreNULL = FALSE)
  
  # ---- Reactive: proyeksi BAU (untuk perbandingan) ----
  hasil_bau <- eventReactive(input$processTAReg2, {
    
    bl <- baseline_data()
    req(bl$baseline, bl$area_matrix_bau)
    
    baseline        <- bl$baseline
    area_matrix_bau <- bl$area_matrix_bau
    
    options(initial_year = input$initial_year)
    
    hasil <- tryCatch(
      run_projection_scenario(
        gdp_baseline      = baseline$gdp_baseline,
        income_multiplier = baseline$income_multiplier,
        labour_multiplier = baseline$labour_multiplier,
        SUB_multiplier    = baseline$SUB_multiplier,
        output_multiplier = baseline$output_multiplier,
        linkage_table     = baseline$linkage_table,
        leontief_matrix   = baseline$leontief_matrix,
        full_io_table     = baseline$intermediate_consumption,
        sector_idx        = baseline$sector_idx,
        P_baseline        = baseline$P_default,
        area_matrix_bau   = area_matrix_bau,
        growth_rate       = baseline$growth_rate,
        constrained       = baseline$constrained,
        unconstrained     = baseline$unconstrained
      ),
      error = function(e) {
        warning("[hasil_bau] Error: ", conditionMessage(e))
        NULL
      }
    )
    
    # --- DEBUG ---
    cat("==== DEBUG hasil_bau ====\n")
    cat("class(hasil):", paste(class(hasil), collapse = ", "), "\n")
    cat("is.null(hasil):", is.null(hasil), "\n")
    if (!is.null(hasil)) {
      cat("nrow(hasil):", nrow(hasil), "\n")
    }
    cat("=========================\n")
    
    if (is.null(hasil)) return(NULL)
    
    list(
      scenario  = hasil,
      active    = attr(hasil, "active_assumptions"),
      baseline_meta = list(
        sector_names = attr(hasil, "sector_names"),
        years        = attr(hasil, "years")
      )
    )
  }, ignoreNULL = FALSE)
  
  # ==========================================================================
  # VISUALISASI
  # ==========================================================================
  
  agg_by_year <- function(df, indicator_col) {
    if (is.null(df)) return(NULL)
    if (!is.data.frame(df) && !is_tibble(df)) return(NULL)
    if (nrow(df) == 0) return(NULL)
    if (!indicator_col %in% names(df)) return(NULL)
    
    df |>
      dplyr::group_by(YEAR) |>
      dplyr::summarise(
        Total = sum(.data[[indicator_col]], na.rm = TRUE),
        .groups = "drop"
      )
  }

  output$projection_plot <- renderPlot({
    res_sc  <- hasil_proyeksi()
    res_bau <- hasil_bau()
    
    req(res_sc,  res_bau)
    req(!is.null(res_sc$scenario),  !is.null(res_bau$scenario))
    
    ind_col   <- indicator_meta[[input$chart_indicator]]$col
    ind_label <- indicator_meta[[input$chart_indicator]]$label
    
    df_bau <- agg_by_year(res_bau$scenario, ind_col)
    df_sc  <- agg_by_year(res_sc$scenario,  ind_col)
    
    req(df_bau, df_sc)
    
    df_bau <- df_bau |> dplyr::mutate(Seri = "BAU")
    df_sc  <- df_sc  |> dplyr::mutate(Seri = scen_label())
    df_plot <- dplyr::bind_rows(df_bau, df_sc)
    
    if (identical(res_sc$active, "BAU")) {
      df_plot <- df_bau
      df_plot$Seri <- factor(df_plot$Seri, levels = "BAU")
    } else {
      df_plot$Seri <- factor(df_plot$Seri, levels = c("BAU", scen_label()))
    }
    
    p <- ggplot(df_plot, aes(x = YEAR, y = Total, color = Seri, group = Seri)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = unique(df_plot$YEAR)) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Proyeksi", ind_label),
        x     = "Tahun",
        y     = ind_label,
        color = NULL
      )
    
    if (!identical(res_sc$active, "BAU")) {
      p + scale_color_manual(
        values = setNames(c("#7f8c8d", "#27ae60"), c("BAU", scen_label()))
      )
    } else {
      p + scale_color_manual(values = c("BAU" = "#7f8c8d"))
    }
  })
  
  output$recap_table <- renderTable({
    res_sc  <- hasil_proyeksi()
    res_bau <- hasil_bau()
    
    req(res_sc,  res_bau)
    req(!is.null(res_sc$scenario),  !is.null(res_bau$scenario))
    
    ind_col <- indicator_meta[[input$chart_indicator]]$col
    df_bau  <- agg_by_year(res_bau$scenario, ind_col)
    df_sc   <- agg_by_year(res_sc$scenario,  ind_col)
    
    req(df_bau, df_sc)
    
    merged <- merge(df_bau, df_sc, by = "YEAR",
                    suffixes = c("_bau", "_sc"))
    merged$Selisih <- sprintf("%+.2f", (merged$Total_sc / merged$Total_bau - 1) * 100)
    
    out <- data.frame(
      Tahun    = as.character(merged$YEAR),
      BAU      = formatC(round(merged$Total_bau), format = "f", digits = 0, big.mark = ","),
      Skenario = formatC(round(merged$Total_sc),  format = "f", digits = 0, big.mark = ","),
      Selisih  = merged$Selisih,
      stringsAsFactors = FALSE
    )
    names(out) <- c("Tahun", "BAU", scen_label(), "Selisih (%)")
    out
  })
  
  # ---- Linkage (hanya jika Asumsi 3 aktif) ----
  output$linkage_section <- renderUI({
    res <- hasil_proyeksi()
    if (is.null(res) || !("A3" %in% res$active)) {
      return(p("Linkage dan Output Multiplier muncul jika Asumsi 3: Hilirisasi diisi.",
               class = "note"))
    }
    tagList(
      h5("Linkage dan Output Multiplier (Asumsi 3: Hilirisasi)"),
      tableOutput("linkage_table"),
      plotOutput("linkage_chart", height = "350px")
    )
  })
  
  linkage_df <- reactive({
    req(hasil_proyeksi())
    res <- hasil_proyeksi()
    req("A3" %in% res$active)
    
    bl_before <- attr(hasil_bau()$scenario, "backward_linkage")
    bl_after  <- attr(res$scenario,         "backward_linkage")
    sec_names <- attr(res$scenario, "sector_names")
    req(!is.null(sec_names), !is.null(bl_before), !is.null(bl_after))
    
    data.frame(
      Sektor     = sec_names,
      BL_Sebelum = as.numeric(bl_before),
      BL_Sesudah = as.numeric(bl_after),
      stringsAsFactors = FALSE
    )
  })
  
  output$linkage_table <- renderTable({
    df <- linkage_df()
    df <- df[order(-abs(df$BL_Sesudah - df$BL_Sebelum)), ]
    df <- head(df, 10)
    df$Delta <- sprintf("%+.3f", df$BL_Sesudah - df$BL_Sebelum)
    names(df) <- c("Sektor", "BL (Sebelum)", "BL (Sesudah)", "ΔBL")
    df
  })
  
  output$linkage_chart <- renderPlot({
    df <- linkage_df()
    df <- df[order(-abs(df$BL_Sesudah - df$BL_Sebelum)), ]
    df <- head(df, 10)
    
    df_long <- tidyr::pivot_longer(
      df, cols = c("BL_Sebelum", "BL_Sesudah"),
      names_to = "Status", values_to = "Indeks"
    )
    df_long$Status <- factor(
      ifelse(df_long$Status == "BL_Sebelum", "Sebelum", "Sesudah"),
      levels = c("Sebelum", "Sesudah")
    )
    
    ggplot(df_long, aes(x = reorder(Sektor, Indeks), y = Indeks, fill = Status)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      coord_flip() +
      scale_fill_manual(values = c(Sebelum = "#bdc3c7", Sesudah = "#27ae60")) +
      theme_minimal(base_size = 12) +
      labs(title = "Perubahan Backward Linkage (Top 10)",
           x = NULL, y = "Indeks Backward Linkage", fill = NULL)
  })
  
  # ==========================================================================
  # RUN HANDLER & DOWNLOAD
  # ==========================================================================
  
  status_text <- reactiveVal("Belum ada proses dijalankan.")
  error_text  <- reactiveVal("")
  
  observeEvent(input$processTAReg2, {
    problems <- character(0)
    
    if (isTRUE(input$load_rds)) {
      if (is.null(input$descriptive_rds_file)) {
        problems <- c(problems, "File .rds Analisis Deskriptif belum dipilih.")
      }
    } else if (is.null(input$io_output_file)) {
      problems <- c(problems, "File IO Analysis Output belum dipilih.")
    }
    
    if (is.null(input$projection_area_file)) {
      problems <- c(problems, "File proyeksi luas lahan BAU belum dipilih.")
    }
    
    per <- tryCatch(periods(), error = function(e) NULL)
    if (is.null(per)) {
      problems <- c(problems, "Periode proyeksi belum valid.")
    }
    
    if (n_assumptions() > 0 && !nzchar(trimws(input$scenario_name))) {
      problems <- c(problems, "Nama skenario belum diisi.")
    }
    
    if (!is.null(a1_file()) && !("luc_projection" %in% a1_sheets())) {
      problems <- c(problems, "File Asumsi 1 tidak punya sheet luc_projection.")
    }
    
    d <- output_dir()
    if (is.null(d) || length(d) == 0) {
      problems <- c(problems, "Folder output belum dipilih.")
    }
    
    if (length(problems) > 0) {
      error_text(paste("-", problems, collapse = "\n"))
      status_text("Proses dihentikan. Lengkapi input berikut.")
      return()
    }
    
    # --- Eksekusi proyeksi ---
    error_text("")
    status_text("Menjalankan proyeksi...")
    
    withProgress(message = "Menjalankan analisis...", value = 0, {
      
      incProgress(0.1, detail = "Memuat baseline")
      
      result <- tryCatch({
        res <- hasil_proyeksi()
        list(ok = TRUE, res = res)
      }, error = function(e) {
        list(ok = FALSE, msg = conditionMessage(e))
      })
      
      if (!result$ok) {
        error_text(paste("Error:", result$msg))
        status_text("Proyeksi gagal. Periksa log error di bawah.")
        return()
      }
      
      incProgress(0.2, detail = "Menyiapkan tabel rekapitulasi")
      
      active <- result$res$active
      
      # --- Siapkan data untuk report ---
      indicator_labels <- list(
        "GDP"     = "PDRB (Rp Juta)",
        "LABOUR"  = "Serapan Tenaga Kerja (Orang)",
        "INCOME"  = "Pendapatan Tenaga Kerja (Rp Juta)",
        "SURPLUS" = "Surplus Usaha Bruto (Rp Juta)",
        "OUTPUT"  = "Output Total (Rp Juta)"
      )
      
      # --- Render HTML report ---
      incProgress(0.3, detail = "Rendering HTML report")
      
      output_file <- paste0(
        "Projection_Report_",
        gsub("[^A-Za-z0-9_-]+", "_", trimws(input$scenario_name)),
        "_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html"
      )
      
      # Deteksi lokasi template
      report_template <- if (file.exists("shiny/ta-regeco-projection.Rmd")) {
        "shiny/ta-regeco-projection.Rmd"
      } else {
        "../../report_template/ta-regeco-projection.Rmd"
      }
      
      start_time <- Sys.time()
      
      tryCatch({
        rmarkdown::render(
          input       = report_template,
          output_file = output_file,
          output_dir  = output_dir(),
          params = list(
            start_time         = format(start_time, "%Y-%m-%d %H:%M:%S"),
            end_time           = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            scenario_name      = scen_label(),
            assumption_active  = active,
            initial_year       = input$initial_year,
            end_year           = input$end_year,
            time_step          = input$time_step,
            unit               = "Rp Juta",       # sesuaikan dengan baseline Anda
            location           = "Bukecap", # sesuaikan dari baseline
            years              = result$res$baseline_meta$years,
            sector_names       = result$res$baseline_meta$sector_names,
            result_scenario    = as.data.frame(result$res$scenario),
            result_bau         = as.data.frame(hasil_bau()$scenario),
            recap_scenario     = as.data.frame(agg_by_year(result$res$scenario, "GDP")),
            recap_bau          = as.data.frame(agg_by_year(hasil_bau()$scenario, "GDP")),
            indicator_labels   = indicator_labels,
            linkage_before     = attr(hasil_bau()$scenario, "backward_linkage"),
            linkage_after      = attr(result$res$scenario,  "backward_linkage"),
            output_dir         = output_dir()
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
        
        incProgress(0.3, detail = "Report selesai")
        
        status_text(paste0(
          "✓ Proyeksi selesai.\n",
          "Skenario  : ", scen_label(), "\n",
          "Asumsi    : ", paste(active, collapse = " + "), "\n",
          "Periode   : ", paste(periods(), collapse = ", "), "\n",
          "Sektor    : ", length(result$res$baseline_meta$sector_names), "\n",
          "Report    : ", output_file
        ))
        
      }, error = function(e) {
        error_text(paste("Gagal render report:", conditionMessage(e)))
        status_text("Proyeksi selesai, tapi report gagal di-generate.")
      })
    })
    
    show("download_results")
    show("open_report")
    show("open_output_folder")
  })
  # ---- Download hasil proyeksi (.xlsx) ----
  output$download_results <- downloadHandler(
    filename = function() {
      nm <- gsub("[^A-Za-z0-9_-]+", "_", trimws(input$scenario_name))
      if (!nzchar(nm)) nm <- "Skenario"
      paste0("Hasil_Proyeksi_", nm, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(hasil_proyeksi(), hasil_bau())
      
      wb <- openxlsx::createWorkbook()
      
      recap_sc <- agg_by_year(hasil_proyeksi()$scenario, "GDP")
      openxlsx::addWorksheet(wb, "Recap_Skenario")
      openxlsx::writeData(wb, "Recap_Skenario", recap_sc)
      
      recap_bau <- agg_by_year(hasil_bau()$scenario, "GDP")
      openxlsx::addWorksheet(wb, "Recap_BAU")
      openxlsx::writeData(wb, "Recap_BAU", recap_bau)
      
      openxlsx::addWorksheet(wb, "Detail_Skenario")
      openxlsx::writeData(wb, "Detail_Skenario", hasil_proyeksi()$scenario)
      
      openxlsx::addWorksheet(wb, "Detail_BAU")
      openxlsx::writeData(wb, "Detail_BAU", hasil_bau()$scenario)
      
      meta <- data.frame(
        Parameter = c("Nama Skenario", "Asumsi Aktif", "Tahun Awal", "Tahun Akhir",
                      "Interval", "Jumlah Sektor", "Tanggal Proses"),
        Nilai = c(scen_label(),
                  paste(hasil_proyeksi()$active, collapse = " + "),
                  input$initial_year, input$end_year, input$time_step,
                  length(hasil_proyeksi()$baseline_meta$sector_names),
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      )
      openxlsx::addWorksheet(wb, "Metadata")
      openxlsx::writeData(wb, "Metadata", meta)
      
      if (!is.null(output_dir())) {
        out_path <- file.path(output_dir(), basename(file))
        try(openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE), silent = TRUE)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$status_messages <- renderText(status_text())
  output$error_messages  <- renderText(error_text())
  
  observeEvent(input$open_report, {
    d <- output_dir()
    if (is.null(d) || length(d) == 0) {
      showNotification("Folder output belum dipilih.", type = "warning")
      return()
    }
    
    # Cari file report HTML terbaru di folder output
    reports <- list.files(
      d,
      pattern    = "^Projection_Report.*\\.html$",
      full.names = TRUE
    )
    
    if (length(reports) == 0) {
      showNotification(
        "Belum ada file report di folder output. Jalankan Run Analysis terlebih dahulu.",
        type = "warning", duration = 5
      )
      return()
    }
    
    # Ambil file yang paling baru dimodifikasi
    report_path <- reports[which.max(file.info(reports)$mtime)]
    
    # Buka di browser default (cross-platform)
    tryCatch({
      if (.Platform$OS.type == "windows") {
        shell.exec(report_path)
      } else if (Sys.info()[["sysname"]] == "Darwin") {
        system2("open", shQuote(report_path))
      } else {
        system2("xdg-open", shQuote(report_path))
      }
      
      showNotification(
        paste("Membuka:", basename(report_path)),
        type = "message", duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Gagal membuka report:", conditionMessage(e)),
        type = "error", duration = 5
      )
    })
  })
  
  observeEvent(input$open_output_folder, {
    d <- output_dir()
    if (!is.null(d) && length(d) > 0 && dir.exists(d)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(normalizePath(d))
      } else if (Sys.info()[["sysname"]] == "Darwin") {
        system2("open", shQuote(normalizePath(d)))
      } else {
        system2("xdg-open", shQuote(normalizePath(d)))
      }
    }
  })
  
  observeEvent(input$returnButton, {
    js$closeWindow()
    stopApp()
  })
  
  # ---- User guide ----
  output$user_guide <- renderUI({
    guide_row <- function(a, b, c) tags$tr(tags$td(a), tags$td(b), tags$td(c))
    
    tagList(
      br(),
      h5("Alur penggunaan"),
      tags$ol(
        tags$li("Jalankan modul Analisis Deskriptif, lalu muat IO_Analysis_Output.xlsx atau file .rds-nya."),
        tags$li("Isi proyeksi luas lahan BAU, tahun awal, tahun akhir, dan interval."),
        tags$li("Isi Nama Skenario. Satu skenario dapat menggabungkan Asumsi 1, 2, dan 3. File pada tiap asumsi bersifat opsional."),
        tags$li("Untuk Asumsi 2, unduh Template Skenario (berisi nama sektor dan LPC baseline), isi, lalu unggah pada tab A2."),
        tags$li("Pilih folder output, lalu klik Run Analysis."),
        tags$li("Lihat hasil pada tab Scenario Summary: grafik perbandingan dengan BAU, rekapitulasi per indikator. Linkage dan Output Multiplier tampil jika Asumsi 3 dijalankan."),
        tags$li("Klik Download Results untuk mengekspor hasil ke file Excel.")
      ),
      h5("File asumsi"),
      tags$table(
        class = "table table-sm",
        tags$thead(tags$tr(tags$th("Asumsi"), tags$th("File"), tags$th("Isi"))),
        tags$tbody(
          guide_row(
            "1: Perubahan Lahan",
            "luc_projection_[nama].xlsx",
            "Sheet luc_projection (luas lahan per periode, wajib) dan land_proportion (distribusi lahan, opsional)."
          ),
          guide_row(
            "2: Peningkatan Produktivitas",
            "produktivitas_lpc.xlsx",
            "Dari template: sektor, LPC baseline, Cycle_Type, serta Increment_Rate dan Area_Implemented per periode."
          ),
          guide_row(
            "3: Hilirisasi",
            "perubahan_io.xlsx",
            "Perubahan nilai pada tabel I-O (konsumsi antara atau komponen final demand)."
          )
        )
      ),
      h5("Kombinasi skenario yang didukung"),
      tags$ul(
        tags$li("BAU saja"),
        tags$li("BAU + A1"),
        tags$li("BAU + A1 + A2"),
        tags$li("BAU + A1 + A3"),
        tags$li("BAU + A2"),
        tags$li("BAU + A2 + A3"),
        tags$li("BAU + A3")
      )
    )
  })
}

shinyApp(ui, server)