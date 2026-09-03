# PUBLIC RELEASE v1.0.0
# Scientific logic preserved from the independently verified historical script15.
# Public-release change: the historical local project root was replaced by
# SEPSIS_V1_PROJECT_ROOT/getwd(); statistical logic is otherwise unchanged.

options(stringsAsFactors = FALSE)

# ============================================================
# Day 7: Freeze analysis sets and generate descriptive reports
#
# Reads Day 6 final cleaned genus-level files.
# Does not modify or overwrite Day 6 data.
# Does not run inferential statistics.
# ============================================================

root <- Sys.getenv("SEPSIS_V1_PROJECT_ROOT", unset = getwd())
root <- normalizePath(root, winslash = "/", mustWork = FALSE)
frozen_root <- file.path(root, "processed_data/final_frozen_v1")
set_root <- file.path(frozen_root, "analysis_sets_v1")
report_root <- file.path(
  root,
  "manuscript/02_data_inventory/15_analysis_set_freeze"
)

dir.create(set_root, recursive = TRUE, showWarnings = FALSE)
dir.create(report_root, recursive = TRUE, showWarnings = FALSE)

sink(
  file.path(report_root, "15_Analysis_Set_Freeze_Log.txt"),
  split = TRUE
)
on.exit(sink(), add = TRUE)

cat(
  "Day 7 analysis-set freeze started: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)

projects <- c(
  "PRJEB33360",
  "PRJNA691455",
  "PRJNA1010969",
  "PRJNA978257",
  "PRJNA430161",
  "PRJNA797231",
  "PRJNA912621"
)

cross_sectional_sets <- c(
  "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
  "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
  "A03_PRJNA691455_Sepsis_vs_Healthy",
  "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
  "A05_PRJNA1010969_External_Sepsis_vs_Trauma",
  "A06_PRJNA978257_Sepsis_vs_Healthy"
)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
wcsv <- function(x, path) {
  write.csv(
    x,
    path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

clean_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

safe_col <- function(df, nm, default = NA_character_) {
  if (nm %in% names(df)) {
    df[[nm]]
  } else {
    rep(default, nrow(df))
  }
}

first_nonmissing <- function(...) {
  xs <- list(...)
  out <- rep(NA_character_, length(xs[[1]]))

  for (x in xs) {
    x <- clean_text(x)
    take <- is.na(out) & !is.na(x)
    out[take] <- x[take]
  }

  out
}

standard_group <- function(df) {
  phenotype <- clean_text(safe_col(df, "Phenotype"))
  control <- clean_text(safe_col(df, "Control_Type"))

  raw <- ifelse(
    !is.na(phenotype) &
      grepl("sepsis", tolower(phenotype)) &
      !grepl("non", tolower(phenotype)),
    "Sepsis",
    first_nonmissing(control, phenotype)
  )

  lower <- tolower(ifelse(is.na(raw), "", raw))

  out <- raw
  out[grepl("non.?sepsis.*icu|nonseptic.*icu", lower)] <-
    "Non-sepsis ICU"
  out[grepl("healthy", lower)] <- "Healthy"
  out[grepl("trauma", lower)] <- "Trauma"
  out[
    grepl("^sepsis$|^septic$|sepsis patient", lower) &
      !grepl("non", lower)
  ] <- "Sepsis"

  out
}

genus_columns <- function(df) {
  names(df)[grepl("^Genus__", names(df))]
}

read_project <- function(project) {
  path <- file.path(
    frozen_root,
    project,
    paste0(
      project,
      "_analysis_ready_classified_genus.csv"
    )
  )

  if (!file.exists(path)) {
    stop("未找到Day 6最终文件：", path)
  }

  x <- read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  required <- c(
    "Project_ID",
    "Run_ID",
    "Cleaned_Target_Depth"
  )

  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop(
      project,
      " 缺少字段：",
      paste(missing_cols, collapse = ", ")
    )
  }

  x$Project_ID <- clean_text(x$Project_ID)
  x$Run_ID <- clean_text(x$Run_ID)
  x$Patient_ID <- clean_text(
    safe_col(x, "Patient_ID")
  )
  x$Specimen_ID <- clean_text(
    safe_col(x, "Specimen_ID")
  )
  x$Standard_Group <- standard_group(x)
  x$Cleaned_Target_Depth <- suppressWarnings(
    as.numeric(x$Cleaned_Target_Depth)
  )

  x
}

one_per_patient <- function(df) {
  if (nrow(df) == 0) return(df)

  patient_key <- clean_text(df$Patient_ID)
  patient_key[is.na(patient_key)] <- paste0(
    "RUN_ONLY::",
    df$Run_ID[is.na(patient_key)]
  )

  primary <- clean_text(
    safe_col(df, "Primary_Sample_Flag")
  )
  primary_rank <- ifelse(
    primary == "Yes",
    0,
    1
  )

  time_order <- suppressWarnings(
    as.numeric(
      safe_col(df, "Timepoint_Order", NA_real_)
    )
  )
  time_day <- suppressWarnings(
    as.numeric(
      safe_col(df, "Timepoint_Day", NA_real_)
    )
  )

  time_order[is.na(time_order)] <- Inf
  time_day[is.na(time_day)] <- Inf

  ord <- order(
    patient_key,
    primary_rank,
    time_order,
    time_day,
    df$Run_ID
  )

  z <- df[ord, , drop = FALSE]
  key_z <- patient_key[ord]

  z[!duplicated(key_z), , drop = FALSE]
}

make_binary_set <- function(
  df,
  set_id,
  group_a,
  group_b,
  analysis_family,
  model_use,
  project_role,
  one_sample_per_patient = TRUE,
  extra_filter = rep(TRUE, nrow(df))
) {
  keep <- !is.na(df$Standard_Group) &
    df$Standard_Group %in% c(group_a, group_b) &
    extra_filter &
    df$Cleaned_Target_Depth > 0

  z <- df[keep, , drop = FALSE]

  if (one_sample_per_patient) {
    z <- one_per_patient(z)
  }

  z$Analysis_Set <- set_id
  z$Comparison_Group <- z$Standard_Group
  z$Analysis_Family <- analysis_family
  z$Model_Use <- model_use
  z$Project_Role <- project_role
  z$Cross_Sectional_Independent <- ifelse(
    one_sample_per_patient,
    "Yes",
    "No"
  )

  z
}

make_support_set <- function(
  df,
  set_id,
  analysis_family,
  model_use,
  project_role,
  comparison_group,
  keep
) {
  z <- df[
    keep & df$Cleaned_Target_Depth > 0,
    ,
    drop = FALSE
  ]

  z$Analysis_Set <- set_id
  z$Comparison_Group <- comparison_group(z)
  z$Analysis_Family <- analysis_family
  z$Model_Use <- model_use
  z$Project_Role <- project_role
  z$Cross_Sectional_Independent <- "No"

  z
}

write_analysis_set <- function(df) {
  if (nrow(df) == 0) return(NULL)

  set_id <- unique(df$Analysis_Set)

  if (length(set_id) != 1) {
    stop("分析集对象中存在多个Analysis_Set。")
  }

  path <- file.path(
    set_root,
    paste0(set_id, ".csv")
  )

  wcsv(df, path)
  path
}

group_descriptive <- function(df) {
  genus_cols <- genus_columns(df)

  if (length(genus_cols) > 0) {
    genus_matrix <- as.matrix(df[, genus_cols, drop = FALSE])
    storage.mode(genus_matrix) <- "numeric"

    detected <- rowSums(
      genus_matrix > 0,
      na.rm = TRUE
    )

    classified_sum <- rowSums(
      genus_matrix,
      na.rm = TRUE
    )
  } else {
    detected <- rep(NA_real_, nrow(df))
    classified_sum <- rep(NA_real_, nrow(df))
  }

  df$Detected_Classified_Genera <- detected
  df$Classified_Genus_Relative_Sum <- classified_sum

  groups <- unique(df$Comparison_Group)

  do.call(
    rbind,
    lapply(groups, function(grp) {
      x <- df[
        df$Comparison_Group == grp,
        ,
        drop = FALSE
      ]

      patient_id <- clean_text(x$Patient_ID)

      data.frame(
        Analysis_Set = unique(x$Analysis_Set)[1],
        Project_ID = unique(x$Project_ID)[1],
        Analysis_Family = unique(x$Analysis_Family)[1],
        Model_Use = unique(x$Model_Use)[1],
        Comparison_Group = grp,
        Samples_N = nrow(x),
        Unique_Patients_N = length(
          unique(
            patient_id[!is.na(patient_id)]
          )
        ),
        Depth_Min = min(
          x$Cleaned_Target_Depth,
          na.rm = TRUE
        ),
        Depth_P25 = as.numeric(
          stats::quantile(
            x$Cleaned_Target_Depth,
            0.25,
            na.rm = TRUE
          )
        ),
        Depth_Median = stats::median(
          x$Cleaned_Target_Depth,
          na.rm = TRUE
        ),
        Depth_P75 = as.numeric(
          stats::quantile(
            x$Cleaned_Target_Depth,
            0.75,
            na.rm = TRUE
          )
        ),
        Depth_Max = max(
          x$Cleaned_Target_Depth,
          na.rm = TRUE
        ),
        Detected_Genera_Median = stats::median(
          x$Detected_Classified_Genera,
          na.rm = TRUE
        ),
        Detected_Genera_P25 = as.numeric(
          stats::quantile(
            x$Detected_Classified_Genera,
            0.25,
            na.rm = TRUE
          )
        ),
        Detected_Genera_P75 = as.numeric(
          stats::quantile(
            x$Detected_Classified_Genera,
            0.75,
            na.rm = TRUE
          )
        ),
        Classified_Relative_Sum_Median =
          stats::median(
            x$Classified_Genus_Relative_Sum,
            na.rm = TRUE
          ),
        stringsAsFactors = FALSE
      )
    })
  )
}

# ------------------------------------------------------------
# Load projects
# ------------------------------------------------------------
project_data <- setNames(
  lapply(projects, read_project),
  projects
)

# ------------------------------------------------------------
# Freeze explicit analysis sets
# ------------------------------------------------------------
analysis_sets <- list()

# A01: PRJEB33360 primary ICU comparison
x <- project_data[["PRJEB33360"]]
extra <- clean_text(safe_col(x, "Analysis_Role")) ==
  "Core_case_control" &
  clean_text(safe_col(x, "Primary_Sample_Flag")) ==
  "Yes"

analysis_sets[["A01_PRJEB33360_Sepsis_vs_NonSepsisICU"]] <-
  make_binary_set(
    x,
    "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
    "Sepsis",
    "Non-sepsis ICU",
    "Primary ICU case-control",
    "Development",
    "Core discovery",
    TRUE,
    extra
  )

# A02: PRJNA691455 first sample per patient, ICU control
x <- project_data[["PRJNA691455"]]

analysis_sets[["A02_PRJNA691455_Sepsis_vs_NonSepsisICU"]] <-
  make_binary_set(
    x,
    "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
    "Sepsis",
    "Non-sepsis ICU",
    "Primary ICU case-control",
    "Development",
    "Core discovery",
    TRUE
  )

# A03: PRJNA691455 first sample per patient, healthy control
analysis_sets[["A03_PRJNA691455_Sepsis_vs_Healthy"]] <-
  make_binary_set(
    x,
    "A03_PRJNA691455_Sepsis_vs_Healthy",
    "Sepsis",
    "Healthy",
    "Secondary healthy-control comparison",
    "Development",
    "Secondary discovery",
    TRUE
  )

# A04/A05: PRJNA1010969 locked external validation
x <- project_data[["PRJNA1010969"]]

analysis_sets[["A04_PRJNA1010969_External_Sepsis_vs_Healthy"]] <-
  make_binary_set(
    x,
    "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
    "Sepsis",
    "Healthy",
    "Locked external validation",
    "Locked_external_test",
    "External validation",
    TRUE
  )

analysis_sets[["A05_PRJNA1010969_External_Sepsis_vs_Trauma"]] <-
  make_binary_set(
    x,
    "A05_PRJNA1010969_External_Sepsis_vs_Trauma",
    "Sepsis",
    "Trauma",
    "Locked external validation",
    "Locked_external_test",
    "External validation",
    TRUE
  )

# A06: PRJNA978257 healthy-control comparison
x <- project_data[["PRJNA978257"]]

analysis_sets[["A06_PRJNA978257_Sepsis_vs_Healthy"]] <-
  make_binary_set(
    x,
    "A06_PRJNA978257_Sepsis_vs_Healthy",
    "Sepsis",
    "Healthy",
    "Primary healthy-control comparison",
    "Development",
    "Core discovery",
    TRUE
  )

# S01/S02: PRJEB33360 longitudinal main/sensitivity
x <- project_data[["PRJEB33360"]]
longitudinal <- clean_text(
  safe_col(x, "Analysis_Role")
) == "Longitudinal"

analysis_sets[["S01_PRJEB33360_Longitudinal_Main_ge1000"]] <-
  make_support_set(
    x,
    "S01_PRJEB33360_Longitudinal_Main_ge1000",
    "Longitudinal support",
    "Not_for_model",
    "Longitudinal support",
    function(z) {
      out <- clean_text(safe_col(z, "Timepoint_Raw"))
      out[is.na(out)] <- "Longitudinal"
      out
    },
    longitudinal &
      x$Cleaned_Target_Depth >= 1000
  )

analysis_sets[["S02_PRJEB33360_Longitudinal_Sensitivity_ge5000"]] <-
  make_support_set(
    x,
    "S02_PRJEB33360_Longitudinal_Sensitivity_ge5000",
    "Longitudinal sensitivity",
    "Not_for_model",
    "Longitudinal sensitivity",
    function(z) {
      out <- clean_text(safe_col(z, "Timepoint_Raw"))
      out[is.na(out)] <- "Longitudinal"
      out
    },
    longitudinal &
      x$Cleaned_Target_Depth >= 5000
  )

# S03: PRJNA691455 Sepsis longitudinal samples
x <- project_data[["PRJNA691455"]]

analysis_sets[["S03_PRJNA691455_Sepsis_Longitudinal"]] <-
  make_support_set(
    x,
    "S03_PRJNA691455_Sepsis_Longitudinal",
    "Longitudinal support",
    "Not_for_model",
    "Longitudinal support",
    function(z) {
      out <- clean_text(safe_col(z, "Timepoint_Raw"))
      out[is.na(out)] <- "Sepsis longitudinal"
      out
    },
    x$Standard_Group == "Sepsis"
  )

# S04: PRJNA430161 intervention/longitudinal support
x <- project_data[["PRJNA430161"]]

analysis_sets[["S04_PRJNA430161_Intervention_Longitudinal"]] <-
  make_support_set(
    x,
    "S04_PRJNA430161_Intervention_Longitudinal",
    "Intervention/longitudinal support",
    "Not_for_model",
    "Intervention support",
    function(z) {
      first_nonmissing(
        safe_col(z, "Timepoint_Raw"),
        safe_col(z, "Phenotype"),
        rep("Intervention sample", nrow(z))
      )
    },
    rep(TRUE, nrow(x))
  )

# S05: PRJNA797231 organ dysfunction
x <- project_data[["PRJNA797231"]]

analysis_sets[["S05_PRJNA797231_Organ_Dysfunction"]] <-
  make_support_set(
    x,
    "S05_PRJNA797231_Organ_Dysfunction",
    "Organ dysfunction support",
    "Not_for_model",
    "Organ dysfunction support",
    function(z) {
      first_nonmissing(
        safe_col(z, "Organ_Dysfunction_Status"),
        safe_col(z, "Phenotype"),
        rep("Organ dysfunction unresolved", nrow(z))
      )
    },
    rep(TRUE, nrow(x))
  )

# S06: PRJNA912621 organ dysfunction longitudinal
x <- project_data[["PRJNA912621"]]

analysis_sets[["S06_PRJNA912621_Organ_Dysfunction_Longitudinal"]] <-
  make_support_set(
    x,
    "S06_PRJNA912621_Organ_Dysfunction_Longitudinal",
    "Organ dysfunction longitudinal support",
    "Not_for_model",
    "Organ dysfunction longitudinal support",
    function(z) {
      status <- first_nonmissing(
        safe_col(z, "Organ_Dysfunction_Status"),
        safe_col(z, "Phenotype"),
        rep("Status unresolved", nrow(z))
      )
      tp <- clean_text(safe_col(z, "Timepoint_Raw"))
      ifelse(
        is.na(tp),
        status,
        paste(status, tp, sep = " | ")
      )
    },
    rep(TRUE, nrow(x))
  )

# ------------------------------------------------------------
# Write sets and manifests
# ------------------------------------------------------------
file_rows <- list()
membership_rows <- list()
summary_rows <- list()
descriptive_rows <- list()
qc_rows <- list()

fri <- 1L
mri <- 1L
sri <- 1L
dri <- 1L
qri <- 1L

expected_groups <- list(
  A01_PRJEB33360_Sepsis_vs_NonSepsisICU =
    c("Sepsis", "Non-sepsis ICU"),
  A02_PRJNA691455_Sepsis_vs_NonSepsisICU =
    c("Sepsis", "Non-sepsis ICU"),
  A03_PRJNA691455_Sepsis_vs_Healthy =
    c("Sepsis", "Healthy"),
  A04_PRJNA1010969_External_Sepsis_vs_Healthy =
    c("Sepsis", "Healthy"),
  A05_PRJNA1010969_External_Sepsis_vs_Trauma =
    c("Sepsis", "Trauma"),
  A06_PRJNA978257_Sepsis_vs_Healthy =
    c("Sepsis", "Healthy")
)

for (set_id in names(analysis_sets)) {
  z <- analysis_sets[[set_id]]

  path <- write_analysis_set(z)

  file_rows[[fri]] <- data.frame(
    Analysis_Set = set_id,
    Project_ID = if (nrow(z)) unique(z$Project_ID)[1] else NA_character_,
    File_Path = path,
    stringsAsFactors = FALSE
  )
  fri <- fri + 1L

  metadata_cols <- intersect(
    c(
      "Analysis_Set",
      "Project_ID",
      "Run_ID",
      "Patient_ID",
      "Specimen_ID",
      "Comparison_Group",
      "Phenotype",
      "Control_Type",
      "Primary_Sample_Flag",
      "Timepoint_Raw",
      "Timepoint_Day",
      "Timepoint_Order",
      "Analysis_Role",
      "Dataset_Split",
      "Organ_Dysfunction_Type",
      "Organ_Dysfunction_Status",
      "Cleaned_Target_Depth",
      "Analysis_Family",
      "Model_Use",
      "Project_Role",
      "Cross_Sectional_Independent"
    ),
    names(z)
  )

  membership_rows[[mri]] <- z[, metadata_cols, drop = FALSE]
  mri <- mri + 1L

  group_counts <- table(z$Comparison_Group)

  summary_rows[[sri]] <- data.frame(
    Analysis_Set = set_id,
    Project_ID = if (nrow(z)) unique(z$Project_ID)[1] else NA_character_,
    Analysis_Family = if (nrow(z)) unique(z$Analysis_Family)[1] else NA_character_,
    Model_Use = if (nrow(z)) unique(z$Model_Use)[1] else NA_character_,
    Project_Role = if (nrow(z)) unique(z$Project_Role)[1] else NA_character_,
    Samples_N = nrow(z),
    Unique_Patients_N = length(
      unique(
        z$Patient_ID[
          !is.na(z$Patient_ID) &
            nzchar(z$Patient_ID)
        ]
      )
    ),
    Groups_N = length(group_counts),
    Group_Summary = paste(
      names(group_counts),
      as.integer(group_counts),
      sep = "=",
      collapse = "; "
    ),
    Minimum_Depth = if (nrow(z)) {
      min(z$Cleaned_Target_Depth)
    } else {
      NA_real_
    },
    Median_Depth = if (nrow(z)) {
      median(z$Cleaned_Target_Depth)
    } else {
      NA_real_
    },
    Maximum_Depth = if (nrow(z)) {
      max(z$Cleaned_Target_Depth)
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
  sri <- sri + 1L

  if (nrow(z) > 0) {
    descriptive_rows[[dri]] <- group_descriptive(z)
    dri <- dri + 1L
  }

  # QC: duplicate Run IDs
  duplicate_runs <- unique(
    z$Run_ID[duplicated(z$Run_ID)]
  )

  qc_rows[[qri]] <- data.frame(
    Analysis_Set = set_id,
    Check_Name = "Duplicate_Run_ID_within_set",
    Status = if (length(duplicate_runs)) "FAIL" else "PASS",
    Details = if (length(duplicate_runs)) {
      paste(duplicate_runs, collapse = " | ")
    } else {
      "No duplicate Run_ID."
    },
    stringsAsFactors = FALSE
  )
  qri <- qri + 1L

  # QC: cross-sectional sets must contain one sample per patient
  if (set_id %in% cross_sectional_sets) {
    patient_key <- z$Patient_ID
    patient_key[
      is.na(patient_key) |
        !nzchar(patient_key)
    ] <- paste0(
      "RUN_ONLY::",
      z$Run_ID[
        is.na(z$Patient_ID) |
          !nzchar(z$Patient_ID)
      ]
    )

    duplicate_patients <- unique(
      patient_key[duplicated(patient_key)]
    )

    qc_rows[[qri]] <- data.frame(
      Analysis_Set = set_id,
      Check_Name = "One_sample_per_patient",
      Status = if (length(duplicate_patients)) "FAIL" else "PASS",
      Details = if (length(duplicate_patients)) {
        paste(duplicate_patients, collapse = " | ")
      } else {
        "Each patient contributes one sample."
      },
      stringsAsFactors = FALSE
    )
    qri <- qri + 1L

    expected <- expected_groups[[set_id]]
    observed <- unique(z$Comparison_Group)
    missing_expected <- setdiff(expected, observed)

    qc_rows[[qri]] <- data.frame(
      Analysis_Set = set_id,
      Check_Name = "Expected_groups_present",
      Status = if (length(missing_expected)) "FAIL" else "PASS",
      Details = if (length(missing_expected)) {
        paste(
          "Missing:",
          paste(missing_expected, collapse = " | ")
        )
      } else {
        paste(
          "Observed:",
          paste(sort(observed), collapse = " | ")
        )
      },
      stringsAsFactors = FALSE
    )
    qri <- qri + 1L

    min_group_n <- if (length(group_counts)) {
      min(group_counts)
    } else {
      0
    }

    qc_rows[[qri]] <- data.frame(
      Analysis_Set = set_id,
      Check_Name = "Minimum_group_size",
      Status = if (
        min_group_n < 5
      ) {
        "FAIL"
      } else if (
        min_group_n < 10
      ) {
        "REVIEW"
      } else {
        "PASS"
      },
      Details = paste0(
        "Minimum group N=",
        min_group_n
      ),
      stringsAsFactors = FALSE
    )
    qri <- qri + 1L
  }

  # QC: zero or missing cleaned depth
  bad_depth <- z$Run_ID[
    is.na(z$Cleaned_Target_Depth) |
      z$Cleaned_Target_Depth <= 0
  ]

  qc_rows[[qri]] <- data.frame(
    Analysis_Set = set_id,
    Check_Name = "Positive_cleaned_depth",
    Status = if (length(bad_depth)) "FAIL" else "PASS",
    Details = if (length(bad_depth)) {
      paste(bad_depth, collapse = " | ")
    } else {
      "All samples have positive cleaned depth."
    },
    stringsAsFactors = FALSE
  )
  qri <- qri + 1L
}

file_index <- do.call(rbind, file_rows)
membership <- do.call(rbind, membership_rows)
set_summary <- do.call(rbind, summary_rows)
group_stats <- do.call(rbind, descriptive_rows)
set_qc <- do.call(rbind, qc_rows)

# ------------------------------------------------------------
# Global development/external leakage audit
# ------------------------------------------------------------
model_membership <- membership[
  membership$Model_Use %in%
    c(
      "Development",
      "Locked_external_test"
    ),
  ,
  drop = FALSE
]

global_qc_rows <- list()

for (id_col in c("Patient_ID", "Specimen_ID")) {
  if (!id_col %in% names(model_membership)) next

  id_value <- clean_text(model_membership[[id_col]])
  valid <- !is.na(id_value)

  failures <- list()
  fi <- 1L

  for (id in unique(id_value[valid])) {
    rows <- model_membership[
      valid & id_value == id,
      ,
      drop = FALSE
    ]

    uses <- unique(rows$Model_Use)

    if (
      all(
        c(
          "Development",
          "Locked_external_test"
        ) %in% uses
      )
    ) {
      failures[[fi]] <- data.frame(
        Entity_ID = id,
        Projects = paste(
          unique(rows$Project_ID),
          collapse = " | "
        ),
        Analysis_Sets = paste(
          unique(rows$Analysis_Set),
          collapse = " | "
        ),
        stringsAsFactors = FALSE
      )
      fi <- fi + 1L
    }
  }

  if (length(failures) == 0) {
    global_qc_rows[[length(global_qc_rows) + 1]] <-
      data.frame(
        Analysis_Set = "GLOBAL",
        Check_Name = paste0(
          id_col,
          "_Development_vs_External_Leakage"
        ),
        Status = "PASS",
        Details = paste0(
          "No ",
          id_col,
          " appears in both Development and Locked_external_test."
        ),
        stringsAsFactors = FALSE
      )
  } else {
    failure_df <- do.call(rbind, failures)

    global_qc_rows[[length(global_qc_rows) + 1]] <-
      data.frame(
        Analysis_Set = "GLOBAL",
        Check_Name = paste0(
          id_col,
          "_Development_vs_External_Leakage"
        ),
        Status = "FAIL",
        Details = paste(
          apply(
            failure_df,
            1,
            paste,
            collapse = " :: "
          ),
          collapse = " || "
        ),
        stringsAsFactors = FALSE
      )
  }
}

if (length(global_qc_rows)) {
  set_qc <- rbind(
    set_qc,
    do.call(rbind, global_qc_rows)
  )
}

# ------------------------------------------------------------
# Prespecified analysis plan
# ------------------------------------------------------------
comparison_plan <- data.frame(
  Analysis_Set = c(
    "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
    "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
    "A03_PRJNA691455_Sepsis_vs_Healthy",
    "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
    "A05_PRJNA1010969_External_Sepsis_vs_Trauma",
    "A06_PRJNA978257_Sepsis_vs_Healthy",
    "S01_PRJEB33360_Longitudinal_Main_ge1000",
    "S02_PRJEB33360_Longitudinal_Sensitivity_ge5000",
    "S03_PRJNA691455_Sepsis_Longitudinal",
    "S04_PRJNA430161_Intervention_Longitudinal",
    "S05_PRJNA797231_Organ_Dysfunction",
    "S06_PRJNA912621_Organ_Dysfunction_Longitudinal"
  ),
  Planned_Use = c(
    "Primary ICU-specific discovery cohort",
    "Primary ICU-specific discovery cohort",
    "Secondary healthy-control discovery",
    "Locked external validation only",
    "Locked external validation only",
    "Primary healthy-control discovery cohort",
    "Longitudinal support",
    "Longitudinal depth sensitivity",
    "Longitudinal support",
    "Intervention support",
    "Organ dysfunction support",
    "Organ dysfunction longitudinal support"
  ),
  Included_In_Feature_Selection = c(
    "Yes", "Yes", "Secondary",
    "No", "No", "Yes",
    "No", "No", "No", "No", "No", "No"
  ),
  Included_In_Primary_ICU_Meta = c(
    "Yes", "Yes", "No",
    "No", "No", "No",
    "No", "No", "No", "No", "No", "No"
  ),
  Included_In_Healthy_Control_Meta = c(
    "No", "No", "Yes",
    "External validation only",
    "No", "Yes",
    "No", "No", "No", "No", "No", "No"
  ),
  Repeated_Measures = c(
    "No", "No", "No", "No", "No", "No",
    "Yes", "Yes", "Yes", "Yes", "No", "Yes"
  ),
  Statistical_Unit = c(
    "Patient", "Patient", "Patient", "Patient", "Patient", "Patient",
    "Patient with repeated samples",
    "Patient with repeated samples",
    "Patient with repeated samples",
    "Patient with repeated samples",
    "Patient",
    "Patient with repeated samples"
  ),
  Notes = c(
    "Sepsis M1 versus non-sepsis ICU.",
    "One earliest/primary Sepsis sample per patient.",
    "Same selected Sepsis baseline samples compared with Healthy.",
    "Do not use for genus selection or SDI training.",
    "Do not use for genus selection or SDI training.",
    "Sepsis versus Healthy.",
    "Cleaned depth >=1000.",
    "Cleaned depth >=5000.",
    "All Sepsis longitudinal samples; mixed models required.",
    "Supportive intervention/longitudinal analysis.",
    "Final groups use actual 18 versus 16 sample structure.",
    "Repeated-measures model required."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Final audit summary
# ------------------------------------------------------------
qc_by_set <- split(
  set_qc,
  set_qc$Analysis_Set
)

audit_rows <- lapply(
  names(qc_by_set),
  function(set_id) {
    z <- qc_by_set[[set_id]]
    fail_n <- sum(z$Status == "FAIL")
    review_n <- sum(z$Status == "REVIEW")

    data.frame(
      Analysis_Set = set_id,
      Fail_Checks = fail_n,
      Review_Checks = review_n,
      Final_Status = if (
        fail_n > 0
      ) {
        "FAIL"
      } else if (
        review_n > 0
      ) {
        "REVIEW"
      } else {
        "PASS"
      },
      Failed_Check_Names = paste(
        z$Check_Name[z$Status == "FAIL"],
        collapse = " | "
      ),
      Review_Check_Names = paste(
        z$Check_Name[z$Status == "REVIEW"],
        collapse = " | "
      ),
      Reviewer = "",
      Review_Date = "",
      Final_Decision = "",
      Decision_Notes = "",
      stringsAsFactors = FALSE
    )
  }
)

freeze_audit <- do.call(rbind, audit_rows)

# ------------------------------------------------------------
# Write reports
# ------------------------------------------------------------
wcsv(
  comparison_plan,
  file.path(
    report_root,
    "15_Comparison_Analysis_Plan.csv"
  )
)

wcsv(
  set_summary,
  file.path(
    report_root,
    "15_Analysis_Set_Summary.csv"
  )
)

wcsv(
  membership,
  file.path(
    report_root,
    "15_Analysis_Set_Membership.csv"
  )
)

wcsv(
  group_stats,
  file.path(
    report_root,
    "15_Group_Descriptive_Statistics.csv"
  )
)

wcsv(
  set_qc,
  file.path(
    report_root,
    "15_Analysis_Set_QC.csv"
  )
)

wcsv(
  freeze_audit,
  file.path(
    report_root,
    "15_Analysis_Set_Freeze_Audit.csv"
  )
)

wcsv(
  file_index,
  file.path(
    report_root,
    "15_Analysis_Set_File_Index.csv"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    report_root,
    "15_Analysis_Set_Freeze_SessionInfo.txt"
  )
)

files_to_hash <- unique(c(
  unlist(
    lapply(projects, function(project) {
      file.path(
        frozen_root,
        project,
        paste0(
          project,
          "_analysis_ready_classified_genus.csv"
        )
      )
    })
  ),
  list.files(
    set_root,
    full.names = TRUE
  ),
  list.files(
    report_root,
    full.names = TRUE
  )
))

writeLines(
  files_to_hash,
  file.path(
    report_root,
    "15_Files_To_Hash.txt"
  ),
  useBytes = TRUE
)

cat("\nDay 7 completed.\n")
cat("Analysis sets: ", set_root, "\n", sep = "")
cat("Reports: ", report_root, "\n", sep = "")
cat("Important outputs:\n")
cat("  15_Comparison_Analysis_Plan.csv\n")
cat("  15_Analysis_Set_Summary.csv\n")
cat("  15_Group_Descriptive_Statistics.csv\n")
cat("  15_Analysis_Set_QC.csv\n")
cat("  15_Analysis_Set_Freeze_Audit.csv\n")
