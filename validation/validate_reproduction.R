options(stringsAsFactors = FALSE)

compare_csv <- function(new_path, expected_path, tolerance = 1e-12,
                        ignore_columns = character()) {
  if (!file.exists(new_path) || !file.exists(expected_path)) return(FALSE)

  a <- read.csv(new_path, check.names = FALSE, stringsAsFactors = FALSE,
                fileEncoding = "UTF-8-BOM")
  b <- read.csv(expected_path, check.names = FALSE, stringsAsFactors = FALSE,
                fileEncoding = "UTF-8-BOM")

  ignore_columns <- intersect(ignore_columns, union(names(a), names(b)))
  a <- a[, setdiff(names(a), ignore_columns), drop = FALSE]
  b <- b[, setdiff(names(b), ignore_columns), drop = FALSE]

  if (!identical(names(a), names(b)) || !identical(dim(a), dim(b))) return(FALSE)

  for (nm in names(a)) {
    x <- a[[nm]]
    y <- b[[nm]]

    xn <- suppressWarnings(as.numeric(x))
    yn <- suppressWarnings(as.numeric(y))

    numeric_like <- all(is.na(x) | !is.na(xn)) &&
      all(is.na(y) | !is.na(yn))

    if (numeric_like) {
      both_na <- is.na(xn) & is.na(yn)
      one_na <- xor(is.na(xn), is.na(yn))
      d <- abs(xn - yn)
      d[both_na] <- 0
      d[one_na] <- Inf
      if (any(d > tolerance)) return(FALSE)
    } else {
      xx <- as.character(x); yy <- as.character(y)
      xx[is.na(x)] <- "<NA>"; yy[is.na(y)] <- "<NA>"
      if (!identical(xx, yy)) return(FALSE)
    }
  }

  TRUE
}

validate_reproduction <- function(project_root, repo_root) {
  final_root <- file.path(
    project_root, "manuscript", "04_final_freeze", "21_results_freeze_v1"
  )

  expected_frozen <- file.path(repo_root, "expected_results", "frozen_result_inputs")
  expected_scientific <- file.path(repo_root, "expected_results", "scientific_outputs")

  rows <- list()

  frozen_files <- list.files(expected_frozen, pattern = "\\.csv$", full.names = FALSE)

  for (fn in frozen_files) {
    ignored <- if (fn %in% c(
      "13_18_SDI_Candidate_Lock.csv",
      "14_18_SDI_Formula_Lock.csv",
      "15_18_SDI_Model_and_Threshold_Lock.csv"
    )) "Lock_Timestamp" else character()

    rows[[length(rows) + 1L]] <- data.frame(
      Category = "Frozen_Result_Input",
      File = fn,
      PASS = compare_csv(
        file.path(final_root, "06_frozen_result_inputs", fn),
        file.path(expected_frozen, fn),
        ignore_columns = ignored
      ),
      Note = if (length(ignored)) "Lock_Timestamp normalized" else "",
      stringsAsFactors = FALSE
    )
  }

  output_map <- c(
    "21_Table1_Cohorts.csv" = "01_tables/21_Table1_Cohorts.csv",
    "21_Table2A_Alpha.csv" = "01_tables/21_Table2A_Alpha.csv",
    "21_Table2B_Beta.csv" = "01_tables/21_Table2B_Beta.csv",
    "21_Table2C_Meta.csv" = "01_tables/21_Table2C_Meta.csv",
    "21_Table3A_SDI_Performance.csv" = "01_tables/21_Table3A_SDI_Performance.csv",
    "21_Table3B_SDI_Threshold.csv" = "01_tables/21_Table3B_SDI_Threshold.csv",
    "21_Table4A_Longitudinal.csv" = "01_tables/21_Table4A_Longitudinal.csv",
    "21_Table4B_Myocardial.csv" = "01_tables/21_Table4B_Myocardial.csv",
    "21_Figure2A_Beta_PERMANOVA_Source.csv" = "03_figure_source_data/21_Figure2A_Beta_PERMANOVA_Source.csv",
    "21_Figure2B_ICU_Meta_Source.csv" = "03_figure_source_data/21_Figure2B_ICU_Meta_Source.csv",
    "21_Figure2C_Healthy_Meta_Source.csv" = "03_figure_source_data/21_Figure2C_Healthy_Meta_Source.csv",
    "21_Figure3A_SDI_AUC_Source.csv" = "03_figure_source_data/21_Figure3A_SDI_AUC_Source.csv",
    "21_Figure3B_Candidate_Diagnosis_Source.csv" = "03_figure_source_data/21_Figure3B_Candidate_Diagnosis_Source.csv",
    "21_Figure3C_External_SDI_Distribution_Source.csv" = "03_figure_source_data/21_Figure3C_External_SDI_Distribution_Source.csv",
    "21_Figure4A_T01_Shannon_Source.csv" = "03_figure_source_data/21_Figure4A_T01_Shannon_Source.csv",
    "21_Figure4B_T03_Shannon_Source.csv" = "03_figure_source_data/21_Figure4B_T03_Shannon_Source.csv",
    "21_Figure4C_T06_Shannon_Source.csv" = "03_figure_source_data/21_Figure4C_T06_Shannon_Source.csv",
    "21_Figure4D_T01_SDI_Source.csv" = "03_figure_source_data/21_Figure4D_T01_SDI_Source.csv",
    "21_Beta_PERMANOVA_FDR_Field_Map.csv" = "04_audit_and_manifest/21_Beta_PERMANOVA_FDR_Field_Map.csv",
    "21_Final_Freeze_Assertions.csv" = "04_audit_and_manifest/21_Final_Freeze_Assertions.csv",
    "21_Status_Field_Scan.csv" = "04_audit_and_manifest/21_Status_Field_Scan.csv",
    "21_Manuscript_Claim_Registry.csv" = "05_text_and_claim_registry/21_Manuscript_Claim_Registry.csv",
    "21_Results_Number_Summary.csv" = "05_text_and_claim_registry/21_Results_Number_Summary.csv"
  )

  for (fn in names(output_map)) {
    rows[[length(rows) + 1L]] <- data.frame(
      Category = "Derived_Scientific_Output",
      File = fn,
      PASS = compare_csv(
        file.path(final_root, unname(output_map[[fn]])),
        file.path(expected_scientific, fn)
      ),
      Note = "",
      stringsAsFactors = FALSE
    )
  }

  pdfs <- c(
    "Figure_1_Study_Design_and_Analysis_Architecture.pdf",
    "Figure_2_Discovery_and_Meta_Analysis.pdf",
    "Figure_3_SDI_Development_and_External_Failure.pdf",
    "Figure_4_Longitudinal_and_Supportive_Analyses.pdf"
  )

  for (fn in pdfs) {
    p <- file.path(final_root, "02_figures", fn)
    rows[[length(rows) + 1L]] <- data.frame(
      Category = "Figure_Generation",
      File = fn,
      PASS = file.exists(p) && file.info(p)$size > 0,
      Note = "PDF binary hash is not used as a scientific equality gate",
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  validation_dir <- file.path(project_root, "validation")
  dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

  write.csv(
    out,
    file.path(validation_dir, "reproduction_checks.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  summary <- data.frame(
    N_Checks = nrow(out),
    N_PASS = sum(out$PASS %in% TRUE),
    All_Scientific_Checks_PASS = all(out$PASS %in% TRUE),
    stringsAsFactors = FALSE
  )

  write.csv(
    summary,
    file.path(validation_dir, "reproduction_summary.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  print(summary, row.names = FALSE)
  invisible(summary)
}
