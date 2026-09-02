#' Find row alias in matrix or data frame rownames
#'
#' Searches for the first row name that matches one of the provided aliases,
#' tolerating differences in capitalization and leading/trailing spaces.
#'
#' @param data A matrix or data frame with row names.
#' @param aliases Character vector. Candidate row names.
#'
#' @return The first matching row name, or `NULL` if none is found.
#'
#' @keywords internal
find_row_alias <- function(data, aliases) {
  available_rows <- trimws(tolower(rownames(data)))
  aliases_clean  <- trimws(tolower(aliases))
  idx <- match(aliases_clean, available_rows)
  found <- aliases[which(!is.na(idx))[1]]
  if (length(found) > 0) return(found[1])
  return(NULL)
}

#' Validate Input-Output data with comprehensive exploded report
#'
#' Checks dimensions, sector names, data types, missing values, non‑negativity
#' (for IC and labour only), land proportions, land use table consistency,
#' growth rates, and the fundamental IO identity (warning only).
#' Prints a detailed validation report; stops only on critical failures.
#'
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param intermediate_consumption A numeric matrix of intermediate
#'   consumption (Sector x Sector).
#' @param final_demand A numeric matrix of final demand
#'   (Sector x Final Demand component).
#' @param added_value A numeric matrix of added value
#'   (Component x Sector).
#' @param labour A vector, one-column matrix, or one-column data frame/tibble
#'   containing labour per sector.
#' @param land_proportion A numeric matrix of land proportion
#'   (Sector x Land Cover Class).
#' @param landuse_table Optional data frame or tibble with at least columns
#'   "LC" (land cover name) and "area" (hectares). Number of rows must
#'   equal the number of columns of `land_proportion`. If `NULL` (default),
#'   the land use checks are reported as `SKIP` and not run.
#' @param growth_rate Optional numeric vector of length `n_sec` containing
#'   sectoral growth rates (in decimal, e.g., 0.02 for 2%). If `NULL`
#'   (default), the growth rate checks are reported as `SKIP` and not run.
#' @param tolerance_io Numeric; maximum allowed absolute relative difference
#'   for the IO identity. Default 0.01 (1%).
#' @param stop_on_fail Logical; if `TRUE` (default), stops on any `FAIL`.
#'
#' @return A tibble with validation results (invisibly if all pass).
#' @export
validate_io_data <- function(sector,
                             intermediate_consumption,
                             final_demand,
                             added_value,
                             labour,
                             land_proportion,
                             landuse_table = NULL,
                             growth_rate = NULL,
                             tolerance_io = 0.01,
                             stop_on_fail = TRUE) {
  
  # Helper functions 
  are_auto_names <- function(nms) {
    if (is.null(nms)) return(TRUE)
    default_pattern <- grepl("^(?:\\.\\.[0-9]+|\\.\\.\\.[0-9]+|[A-Za-z]+\\.?[0-9]+)$", nms)
    if (all(default_pattern)) return(TRUE)
    suppressWarnings({
      nums <- as.numeric(nms)
      if (!anyNA(nums)) return(TRUE)
    })
    FALSE
  }
  
  align_dimnames <- function(mat, expected_names, dim = c("row", "col")) {
    dim <- match.arg(dim)
    current_names <- if (dim == "row") rownames(mat) else colnames(mat)
    
    if (is.null(current_names) || are_auto_names(current_names)) {
      if (dim == "row") rownames(mat) <- expected_names
      else colnames(mat) <- expected_names
    } else {
      if (!identical(sort(current_names), sort(expected_names))) {
        stop(sprintf(
          "Dimension names on %s do not match sector list. Current names: %s",
          dim, paste(current_names, collapse = ", ")
        ))
      }
      if (dim == "row") {
        mat <- mat[expected_names, , drop = FALSE]
      } else {
        mat <- mat[, expected_names, drop = FALSE]
      }
    }
    mat
  }
  
  # Reporting framework
  report <- list()
  
  add_check <- function(component, check, status, details = NA_character_) {
    report <<- c(report, list(tibble::tibble(
      Component = component,
      Check = check,
      Status = status,
      Details = details
    )))
  }
  
  # Matrix validation helper
  validate_and_report_matrix <- function(mat, nrow_expected, ncol_expected,
                                         mat_name, check_negative) {
    # 1. Is matrix
    if (!is.matrix(mat)) {
      add_check(mat_name, "Is a matrix", "FAIL", "Object is not a matrix.")
      add_check(mat_name, "Rows match expected", "FAIL", "Skipped (not a matrix)")
      if (!is.null(ncol_expected)) {
        add_check(mat_name, "Columns match expected", "FAIL", "Skipped (not a matrix)")
      }
      add_check(mat_name, "Is numeric", "FAIL", "Skipped (not a matrix)")
      add_check(mat_name, "No missing (NA) values", "FAIL", "Skipped (not a matrix)")
      if (check_negative) {
        add_check(mat_name, "No negative values", "FAIL", "Skipped (not a matrix)")
      }
      return(invisible(FALSE))
    }
    add_check(mat_name, "Is a matrix", "PASS", NA)
    
    # 2. Rows
    if (nrow(mat) != nrow_expected) {
      add_check(mat_name, sprintf("Rows match expected (%d)", nrow_expected),
                "FAIL", sprintf("Has %d rows.", nrow(mat)))
    } else {
      add_check(mat_name, sprintf("Rows match expected (%d)", nrow_expected),
                "PASS", NA)
    }
    
    # 3. Columns 
    if (!is.null(ncol_expected)) {
      if (ncol(mat) != ncol_expected) {
        add_check(mat_name, sprintf("Columns match expected (%d)", ncol_expected),
                  "FAIL", sprintf("Has %d columns.", ncol(mat)))
      } else {
        add_check(mat_name, sprintf("Columns match expected (%d)", ncol_expected),
                  "PASS", NA)
      }
    }
    
    # 4. Numeric
    if (!is.numeric(mat)) {
      add_check(mat_name, "Is numeric", "FAIL",
                sprintf("Is type: %s", paste(class(mat), collapse = ", ")))
      add_check(mat_name, "No missing (NA) values", "FAIL", "Skipped (not numeric)")
      if (check_negative) {
        add_check(mat_name, "No negative values", "FAIL", "Skipped (not numeric)")
      }
      return(invisible(FALSE))
    }
    add_check(mat_name, "Is numeric", "PASS", NA)
    
    # 5. NA
    if (any(is.na(mat))) {
      add_check(mat_name, "No missing (NA) values", "FAIL",
                sprintf("Has %d NA(s).", sum(is.na(mat))))
    } else {
      add_check(mat_name, "No missing (NA) values", "PASS", NA)
    }
    
    # 6. Negative 
    if (check_negative) {
      if (any(mat < 0, na.rm = TRUE)) {
        add_check(mat_name, "No negative values", "FAIL",
                  sprintf("Has %d negative value(s).", sum(mat < 0, na.rm = TRUE)))
      } else {
        add_check(mat_name, "No negative values", "PASS", NA)
      }
    }
    
    invisible(TRUE)
  }
  
  
  # A. Sector ---------------------------------------------------------------
  if (!is.data.frame(sector) || ncol(sector) < 2) {
    add_check("sector", "Is a data frame with ≥2 columns", "FAIL",
              "Must be a data frame or tibble with at least 2 columns.")
  } else {
    add_check("sector", "Is a data frame with ≥2 columns", "PASS")
  }
  
  sector_names <- as.character(sector[[1]])
  n_sec <- length(sector_names)
  
  # Unique sector names
  if (any(duplicated(sector_names))) {
    dup <- paste(sector_names[duplicated(sector_names)], collapse = ", ")
    add_check("sector", "Unique sector names", "FAIL",
              paste("Duplicates found:", dup))
  } else {
    add_check("sector", "Unique sector names", "PASS")
  }
  
  # Non-empty and non-NA sector names
  if (any(is.na(sector_names) | sector_names == "")) {
    add_check("sector", "Sector names are non‑empty and non‑NA", "FAIL",
              "Empty or NA names found.")
  } else {
    add_check("sector", "Sector names are non‑empty and non‑NA", "PASS")
  }
  
  
  # B. Intermediate consumption ---------------------------------------------
  intermediate_consumption <- align_dimnames(intermediate_consumption, sector_names, "row")
  intermediate_consumption <- align_dimnames(intermediate_consumption, sector_names, "col")
  validate_and_report_matrix(intermediate_consumption, n_sec, n_sec,
                             "intermediate_consumption", check_negative = TRUE)
  
  
  # C. Final demand ---------------------------------------------------------
  n_fd <- ncol(final_demand)
  if (n_fd < 1) {
    add_check("final_demand", "At least one column", "FAIL",
              "Must have at least one column.")
  } else {
    add_check("final_demand", "At least one column", "PASS")
  }
  
  final_demand <- align_dimnames(final_demand, sector_names, "row")
  validate_and_report_matrix(final_demand, n_sec, n_fd,
                             "final_demand", check_negative = FALSE)
  
  
  # D. Added value ----------------------------------------------------------
  n_av <- nrow(added_value)
  if (n_av < 1) {
    add_check("added_value", "At least one row", "FAIL",
              "Must have at least one row.")
  } else {
    add_check("added_value", "At least one row", "PASS")
  }
  
  added_value <- align_dimnames(added_value, sector_names, "col")
  validate_and_report_matrix(added_value, n_av, n_sec,
                             "added_value", check_negative = FALSE)
  
  
  # E. Labour ---------------------------------------------------------------
  labour_vec <- NULL
  coercion_error <- NULL
  
  if (is.matrix(labour)) {
    if (ncol(labour) == 1) {
      labour_vec <- as.numeric(labour[, 1])
    } else {
      coercion_error <- "Matrix has more than one column"
    }
  } else if (is.data.frame(labour)) {
    if (ncol(labour) == 1) {
      labour_vec <- as.numeric(labour[[1]])
    } else {
      coercion_error <- "Data frame has more than one column"
    }
  } else if (is.vector(labour)) {
    labour_vec <- as.numeric(labour)
  } else {
    coercion_error <- paste("Unsupported type:", class(labour)[1])
  }
  
  if (!is.null(coercion_error)) {
    add_check("labour", sprintf("Length matches number of sectors (%d)", n_sec),
              "FAIL", paste("Coercion failed:", coercion_error))
    add_check("labour", "No missing (NA) values",
              "FAIL", paste("Coercion failed:", coercion_error))
    add_check("labour", "No negative values",
              "FAIL", paste("Coercion failed:", coercion_error))
  } else {
    # Length 
    if (length(labour_vec) != n_sec) {
      add_check("labour", sprintf("Length matches number of sectors (%d)", n_sec),
                "FAIL", sprintf("Length: %d", length(labour_vec)))
    } else {
      add_check("labour", sprintf("Length matches number of sectors (%d)", n_sec), "PASS")
    }
    
    # Missing values 
    if (any(is.na(labour_vec))) {
      na_count <- sum(is.na(labour_vec))
      add_check("labour", "No missing (NA) values", "FAIL",
                sprintf("Contains %d NA(s).", na_count))
    } else {
      add_check("labour", "No missing (NA) values", "PASS")
    }
    
    # Negative values 
    if (any(labour_vec < 0, na.rm = TRUE)) {
      neg_count <- sum(labour_vec < 0, na.rm = TRUE)
      add_check("labour", "No negative values", "FAIL",
                sprintf("Contains %d negative value(s).", neg_count))
    } else {
      add_check("labour", "No negative values", "PASS")
    }
  }
  
  # F. Land proportion ------------------------------------------------------
  land_proportion <- align_dimnames(land_proportion, sector_names, "row")
  validate_and_report_matrix(land_proportion, n_sec, NULL,
                             "land_proportion", check_negative = FALSE)
  
  # Column sums = 1
  if (is.matrix(land_proportion) && is.numeric(land_proportion) &&
      nrow(land_proportion) == n_sec) {
    col_sums <- colSums(land_proportion)
    if (any(abs(col_sums - 1) > 1e-6)) {
      bad <- paste(which(abs(col_sums - 1) > 1e-6), collapse = ", ")
      add_check("land_proportion", "Column sums equal 1", "FAIL",
                paste("Problem columns:", bad))
    } else {
      add_check("land_proportion", "Column sums equal 1", "PASS")
    }
    
    # Values in [0,1]
    if (any(land_proportion < 0 | land_proportion > 1, na.rm = TRUE)) {
      add_check("land_proportion", "All values in [0,1]", "FAIL",
                "Values outside range found.")
    } else {
      add_check("land_proportion", "All values in [0,1]", "PASS")
    }
  } else {
    add_check("land_proportion", "Column sums equal 1", "FAIL",
              "Skipped due to prior errors.")
    add_check("land_proportion", "All values in [0,1]", "FAIL",
              "Skipped due to prior errors.")
  }
  
  
  # G. Landuse table --------------------------------------------------------
  n_lc <- ncol(land_proportion) 
  
  if (is.null(landuse_table)) {
    add_check("landuse_table", "Supplied", "SKIP",
              "Not supplied; land use checks skipped.")
  } else if (!is.data.frame(landuse_table)) {
    add_check("landuse_table", "Is a data frame", "FAIL", "Not a data frame/tibble.")
  } else if (ncol(landuse_table) < 2) {
    add_check("landuse_table", "Has at least 2 columns (LC, area)", "FAIL",
              "Only ", ncol(landuse_table), " columns found.")
  } else {
    add_check("landuse_table", "Is a data frame", "PASS")
    add_check("landuse_table", "Has at least 2 columns (LC, area)", "PASS")
    
    # Check required columns exist
    required_cols <- c("LC", "area")
    missing <- required_cols[!required_cols %in% colnames(landuse_table)]
    if (length(missing) > 0) {
      add_check("landuse_table", "Contains columns 'LC' and 'area'", "FAIL",
                paste("Missing:", paste(missing, collapse = ", ")))
    } else {
      add_check("landuse_table", "Contains columns 'LC' and 'area'", "PASS")
    }
    
    # Rows match land cover columns
    if (nrow(landuse_table) != n_lc) {
      add_check("landuse_table", sprintf("Rows match land cover count (%d)", n_lc),
                "FAIL", sprintf("Has %d rows.", nrow(landuse_table)))
    } else {
      add_check("landuse_table", sprintf("Rows match land cover count (%d)", n_lc),
                "PASS")
    }
    
    # Landuse name: unique, non-NA, non-empty
    lc_vals <- as.character(landuse_table[["LC"]])
    if (any(is.na(lc_vals) | lc_vals == "")) {
      add_check("landuse_table", "Landuse name are non‑empty and non‑NA", "FAIL",
                "Empty or NA LC found.")
    } else if (any(duplicated(lc_vals))) {
      dup <- paste(lc_vals[duplicated(lc_vals)], collapse = ", ")
      add_check("landuse_table", "Landuse name are unique", "FAIL",
                paste("Duplicates:", dup))
    } else {
      add_check("landuse_table", "Landuse name are unique and non‑NA", "PASS")
    }
    
    # Area: numeric, positive, no NA
    area_vals <- landuse_table[["area"]]
    if (!is.numeric(area_vals)) {
      add_check("landuse_table", "Area is numeric", "FAIL",
                paste("Type:", class(area_vals)[1]))
    } else {
      add_check("landuse_table", "Area is numeric", "PASS")
      if (any(is.na(area_vals))) {
        add_check("landuse_table", "Area has no NA", "FAIL",
                  "Contains NA values.")
      } else {
        add_check("landuse_table", "Area has no NA", "PASS")
        if (any(area_vals <= 0)) {
          add_check("landuse_table", "Area is positive", "FAIL",
                    "Contains zero or negative values.")
        } else {
          add_check("landuse_table", "Area is positive", "PASS")
        }
      }
    }
    
    # Check that land_proportion columns match Landuse name
    lp_colnames <- colnames(land_proportion)
    if (!is.null(lp_colnames) && !are_auto_names(lp_colnames)) {
      if (!identical(sort(lp_colnames), sort(lc_vals))) {
        add_check("landuse_table", "Land cover names match land_proportion columns", "FAIL",
                  "Column names of land_proportion do not match landuse name.")
      } else {
        add_check("landuse_table", "Land cover names match land_proportion columns", "PASS")
      }
    } else {
      add_check("landuse_table", "Land cover names match land_proportion columns", "WARN")
    }
  }
  
  # H. Growth rate ----------------------------------------------------------
  if (is.null(growth_rate)) {
    add_check("growth_rate", "Supplied", "SKIP",
              "Not supplied; growth rate checks skipped.")
  } else if (!is.numeric(growth_rate)) {
    add_check("growth_rate", "Is numeric", "FAIL",
              paste("Type:", class(growth_rate)[1]))
  } else if (length(growth_rate) != n_sec) {
    add_check("growth_rate", sprintf("Length matches sectors (%d)", n_sec),
              "FAIL", sprintf("Length: %d", length(growth_rate)))
  } else {
    add_check("growth_rate", "Is numeric", "PASS")
    add_check("growth_rate", sprintf("Length matches sectors (%d)", n_sec),
              "PASS")
    if (any(is.na(growth_rate))) {
      add_check("growth_rate", "No NA values", "FAIL", "Contains NA values.")
    } else {
      add_check("growth_rate", "No NA values", "PASS")
    }
    if (any(growth_rate < 0)) {
      add_check("growth_rate", "Non‑negative values", "FAIL",
                "Contains negative growth rates.")
    } else {
      add_check("growth_rate", "Non‑negative values", "PASS")
    }
    if (any(growth_rate > 1)) {
      add_check("growth_rate", "Plausible range (≤1)", "WARN",
                "Some growth rates exceed 100%.")
    } else {
      add_check("growth_rate", "Plausible range (≤1)", "PASS")
    }
  }
  
  # I. Fundamental IO identity ----------------------------------------------
  if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption) &&
      is.matrix(final_demand) && is.numeric(final_demand) &&
      is.matrix(added_value) && is.numeric(added_value) &&
      nrow(intermediate_consumption) == n_sec &&
      nrow(final_demand) == n_sec &&
      ncol(added_value) == n_sec) {
    
    row_ic <- rowSums(intermediate_consumption)
    row_fd <- rowSums(final_demand)
    row_output <- row_ic + row_fd
    
    col_ic <- colSums(intermediate_consumption)
    col_av <- colSums(added_value)
    col_input <- col_ic + col_av
    
    denom <- pmax(abs(row_output), abs(col_input), 1e-9)
    rel_diff <- abs(row_output - col_input) / denom
    
    if (any(rel_diff > tolerance_io)) {
      bad <- which(rel_diff > tolerance_io)
      details <- sprintf(
        "Sectors exceeding %s%% tolerance: %s; Relative differences: %s",
        round(tolerance_io * 100, 2),
        paste(sector_names[bad], collapse = ", "),
        paste(signif(rel_diff[bad], 4), collapse = ", ")
      )
      add_check("Global", "IO identity (row output = column input)", "WARN", details)
    } else {
      add_check("Global", "IO identity (row output = column input)", "PASS")
    }
  } else {
    add_check("Global", "IO identity (row output = column input)", "FAIL",
              "Skipped due to earlier matrix errors.")
  }
  
  
  # J. Land‑based consistency -----------------------------------------------
  if ("Berbasis_Lahan" %in% colnames(sector)) {
    land_based <- sector$Berbasis_Lahan == "Ya"
    if (is.matrix(land_proportion) && nrow(land_proportion) == n_sec) {
      row_sums_land <- rowSums(land_proportion)
      
      zero_land <- which(land_based & row_sums_land == 0)
      if (length(zero_land) > 0) {
        details <- sprintf("Sectors: %s", paste(sector_names[zero_land], collapse = ", "))
        add_check("sector/land", "Land‑based sectors have positive land", "WARN", details)
      }
      
      pos_land <- which(!land_based & row_sums_land > 0)
      if (length(pos_land) > 0) {
        details <- sprintf("Sectors: %s", paste(sector_names[pos_land], collapse = ", "))
        add_check("sector/land", "Non‑land‑based sectors have zero land", "WARN", details)
      }
    }
  }
  
  
  # Prepare report ----------------------------------------------------------
  report_tbl <- dplyr::bind_rows(report)
  has_fail <- any(report_tbl$Status == "FAIL")
  
  if (has_fail && stop_on_fail) {
    stop("Validation FAILED. See report above for details.", call. = FALSE)
  }
  if (!has_fail) {
    message("All critical checks passed. ", n_sec, " sectors confirmed consistent.")
  } else {
    message("Validation completed with failures (stop_on_fail = FALSE).")
  }
  invisible(report_tbl)
}
