# PUBLIC RELEASE v1.0.0
# Scientific logic preserved from the independently verified historical script21.
# Public-release change: the historical local project root was replaced by
# SEPSIS_V1_PROJECT_ROOT/getwd(); statistical logic is otherwise unchanged.

options(stringsAsFactors = FALSE)

# ============================================================
# Day 13: Final results freeze, manuscript tables, figures,
# figure-source data, and claim registry.
#
# This script does NOT:
# - add new cohorts;
# - refit the SDI;
# - reverse the failed external-validation direction;
# - change Day 9 candidate selection;
# - reinterpret post-hoc analyses as confirmatory.
#
# It DOES:
# 1. Validate all required Day 7-12 outputs.
# 2. Freeze selected result files and create a manifest.
# 3. Generate main and supplementary Excel workbooks.
# 4. Generate manuscript-ready PDF figures.
# 5. Export figure-source data.
# 6. Create a claim registry with evidence level and wording.
# ============================================================

root <- Sys.getenv("SEPSIS_V1_PROJECT_ROOT", unset = getwd())
root <- normalizePath(root, winslash = "/", mustWork = FALSE)

day7_root <- file.path(
  root,
  "manuscript/02_data_inventory/15_analysis_set_freeze"
)

day8_root <- file.path(
  root,
  "manuscript/03_analysis/16_discovery_statistics"
)

day9_root <- file.path(
  root,
  "manuscript/03_analysis/17_harmonized_meta_analysis"
)

day10_root <- file.path(
  root,
  "manuscript/03_analysis/18_sdi_external_validation"
)

day10_lock_root <- file.path(
  day10_root,
  "locked_before_external_open"
)

day11_root <- file.path(
  root,
  "manuscript/03_analysis/19_sdi_failure_diagnosis"
)

day12_root <- file.path(
  root,
  "manuscript/03_analysis/20_longitudinal_support"
)

day10_sample_file <- file.path(
  root,
  "processed_data/day10_sdi_external_validation_v1",
  "18_All_SDI_Sample_Scores.csv"
)

day12_sample_file <- file.path(
  root,
  "processed_data/day12_longitudinal_support_v1",
  "20_All_Supportive_Sample_Metrics.csv"
)

freeze_root <- file.path(
  root,
  "manuscript/04_final_freeze/21_results_freeze_v1"
)

table_root <- file.path(
  freeze_root,
  "01_tables"
)

figure_root <- file.path(
  freeze_root,
  "02_figures"
)

source_root <- file.path(
  freeze_root,
  "03_figure_source_data"
)

audit_root <- file.path(
  freeze_root,
  "04_audit_and_manifest"
)

text_root <- file.path(
  freeze_root,
  "05_text_and_claim_registry"
)

frozen_input_root <- file.path(
  freeze_root,
  "06_frozen_result_inputs"
)

for (
  directory in c(
    freeze_root,
    table_root,
    figure_root,
    source_root,
    audit_root,
    text_root,
    frozen_input_root
  )
) {
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

sink(
  file.path(
    audit_root,
    "21_Final_Freeze_Log.txt"
  ),
  split = TRUE
)
on.exit(sink(), add = TRUE)

cat(
  "Day 13 final freeze started: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop(
    paste0(
      "缺少R包 openxlsx。\n",
      "请先在RStudio运行：install.packages('openxlsx')"
    )
  )
}

# ------------------------------------------------------------
# Core helpers
# ------------------------------------------------------------
wcsv <- function(x, path) {
  write.csv(
    x,
    path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

read_csv_strict <- function(path) {
  if (!file.exists(path)) {
    stop(
      "缺少必需文件：",
      path
    )
  }

  read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
}

as_bool <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  y <- tolower(
    trimws(
      as.character(x)
    )
  )

  y %in% c(
    "true",
    "t",
    "1",
    "yes",
    "y",
    "pass"
  )
}

safe_numeric <- function(x) {
  suppressWarnings(
    as.numeric(x)
  )
}

first_existing <- function(
  x,
  candidates,
  required = FALSE
) {
  hit <- candidates[
    candidates %in%
      names(x)
  ]

  if (
    length(hit) == 0
  ) {
    if (required) {
      stop(
        "缺少字段：",
        paste(
          candidates,
          collapse = " / "
        )
      )
    }

    return(NULL)
  }

  hit[1]
}

select_existing <- function(
  x,
  columns
) {
  columns <- columns[
    columns %in%
      names(x)
  ]

  x[
    ,
    columns,
    drop = FALSE
  ]
}

pretty_genus <- function(x) {
  x <- sub(
    "^Genus__",
    "",
    as.character(x)
  )

  x <- sub(
    "\\|Family__.*$",
    "",
    x
  )

  x
}

format_p <- function(x) {
  x <- safe_numeric(x)

  ifelse(
    is.na(x),
    "",
    ifelse(
      x < 0.001,
      "<0.001",
      formatC(
        x,
        format = "f",
        digits = 3
      )
    )
  )
}

format_num <- function(
  x,
  digits = 2
) {
  x <- safe_numeric(x)

  ifelse(
    is.na(x),
    "",
    formatC(
      x,
      format = "f",
      digits = digits
    )
  )
}

sheet_name_safe <- function(x) {
  x <- gsub(
    "[:\\\\/?*\\[\\]]",
    "_",
    x
  )

  substr(
    x,
    1,
    31
  )
}

unique_sheet_names <- function(x) {
  output <- character(
    length(x)
  )

  used <- character()

  for (i in seq_along(x)) {
    base <- sheet_name_safe(
      x[i]
    )

    candidate <- base
    suffix <- 1

    while (
      candidate %in%
        used
    ) {
      suffix_text <- paste0(
        "_",
        suffix
      )

      candidate <- paste0(
        substr(
          base,
          1,
          31 -
            nchar(
              suffix_text
            )
        ),
        suffix_text
      )

      suffix <- suffix + 1
    }

    output[i] <- candidate
    used <- c(
      used,
      candidate
    )
  }

  output
}

add_plain_sheet <- function(
  workbook,
  sheet_name,
  data,
  title = NULL,
  note = NULL,
  freeze_rows = 1
) {
  openxlsx::addWorksheet(
    workbook,
    sheet_name,
    gridLines = FALSE
  )

  current_row <- 1

  if (!is.null(title)) {
    openxlsx::writeData(
      workbook,
      sheet_name,
      title,
      startRow = current_row,
      startCol = 1
    )

    openxlsx::addStyle(
      workbook,
      sheet_name,
      openxlsx::createStyle(
        fontSize = 13,
        textDecoration = "bold",
        halign = "left"
      ),
      rows = current_row,
      cols = 1,
      gridExpand = TRUE
    )

    current_row <- current_row + 2
  }

  if (!is.null(note)) {
    openxlsx::writeData(
      workbook,
      sheet_name,
      note,
      startRow = current_row,
      startCol = 1
    )

    openxlsx::addStyle(
      workbook,
      sheet_name,
      openxlsx::createStyle(
        fontSize = 9,
        fontColour = "#555555",
        wrapText = TRUE,
        valign = "top"
      ),
      rows = current_row,
      cols = 1,
      gridExpand = TRUE
    )

    current_row <- current_row + 2
  }

  if (
    is.null(data) ||
      nrow(data) == 0
  ) {
    data <- data.frame(
      Message = "No rows available",
      stringsAsFactors = FALSE
    )
  }

  openxlsx::writeData(
    workbook,
    sheet_name,
    data,
    startRow = current_row,
    startCol = 1,
    withFilter = TRUE,
    keepNA = FALSE
  )

  header_style <- openxlsx::createStyle(
    fgFill = "#D9D9D9",
    textDecoration = "bold",
    border = "Bottom",
    borderColour = "#777777",
    halign = "center",
    valign = "center",
    wrapText = TRUE
  )

  body_style <- openxlsx::createStyle(
    border = "Bottom",
    borderColour = "#E6E6E6",
    valign = "top",
    wrapText = TRUE
  )

  openxlsx::addStyle(
    workbook,
    sheet_name,
    header_style,
    rows = current_row,
    cols = seq_len(
      ncol(data)
    ),
    gridExpand = TRUE
  )

  if (nrow(data) > 0) {
    openxlsx::addStyle(
      workbook,
      sheet_name,
      body_style,
      rows = (
        current_row + 1
      ):(
        current_row +
          nrow(data)
      ),
      cols = seq_len(
        ncol(data)
      ),
      gridExpand = TRUE
    )
  }

  openxlsx::freezePane(
    workbook,
    sheet_name,
    firstActiveRow =
      current_row + freeze_rows,
    firstActiveCol = 1
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = seq_len(
      ncol(data)
    ),
    widths = "auto"
  )

  # Cap excessive widths.
  for (column_index in seq_len(ncol(data))) {
    values <- c(
      names(data)[column_index],
      as.character(
        data[
          ,
          column_index
        ]
      )
    )

    maximum_length <- max(
      nchar(values),
      na.rm = TRUE
    )

    target_width <- min(
      max(
        10,
        maximum_length + 2
      ),
      38
    )

    openxlsx::setColWidths(
      workbook,
      sheet_name,
      cols = column_index,
      widths = target_width
    )
  }

  invisible(
    current_row
  )
}

add_sectioned_sheet <- function(
  workbook,
  sheet_name,
  title,
  sections
) {
  openxlsx::addWorksheet(
    workbook,
    sheet_name,
    gridLines = FALSE
  )

  openxlsx::writeData(
    workbook,
    sheet_name,
    title,
    startRow = 1,
    startCol = 1
  )

  openxlsx::addStyle(
    workbook,
    sheet_name,
    openxlsx::createStyle(
      fontSize = 14,
      textDecoration = "bold"
    ),
    rows = 1,
    cols = 1,
    gridExpand = TRUE
  )

  row_cursor <- 3

  section_style <- openxlsx::createStyle(
    fgFill = "#BFBFBF",
    textDecoration = "bold",
    border = "Bottom",
    borderColour = "#666666"
  )

  header_style <- openxlsx::createStyle(
    fgFill = "#E7E6E6",
    textDecoration = "bold",
    border = "Bottom",
    borderColour = "#888888",
    halign = "center",
    wrapText = TRUE
  )

  body_style <- openxlsx::createStyle(
    border = "Bottom",
    borderColour = "#ECECEC",
    valign = "top",
    wrapText = TRUE
  )

  for (
    section_name in names(
      sections
    )
  ) {
    section_data <- sections[[
      section_name
    ]]

    if (
      is.null(section_data) ||
        nrow(section_data) == 0
    ) {
      section_data <- data.frame(
        Message = "No rows available",
        stringsAsFactors = FALSE
      )
    }

    openxlsx::writeData(
      workbook,
      sheet_name,
      section_name,
      startRow = row_cursor,
      startCol = 1
    )

    openxlsx::addStyle(
      workbook,
      sheet_name,
      section_style,
      rows = row_cursor,
      cols = 1,
      gridExpand = TRUE
    )

    row_cursor <- row_cursor + 1

    openxlsx::writeData(
      workbook,
      sheet_name,
      section_data,
      startRow = row_cursor,
      startCol = 1,
      withFilter = FALSE,
      keepNA = FALSE
    )

    openxlsx::addStyle(
      workbook,
      sheet_name,
      header_style,
      rows = row_cursor,
      cols = seq_len(
        ncol(section_data)
      ),
      gridExpand = TRUE
    )

    if (nrow(section_data) > 0) {
      openxlsx::addStyle(
        workbook,
        sheet_name,
        body_style,
        rows = (
          row_cursor + 1
        ):(
          row_cursor +
            nrow(section_data)
        ),
        cols = seq_len(
          ncol(section_data)
        ),
        gridExpand = TRUE
      )
    }

    row_cursor <- row_cursor +
      nrow(section_data) + 3
  }

  openxlsx::freezePane(
    workbook,
    sheet_name,
    firstActiveRow = 3
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = 1:30,
    widths = 14
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = 1,
    widths = 28
  )
}

mean_se_by <- function(
  data,
  value_column,
  group_columns
) {
  value <- safe_numeric(
    data[[value_column]]
  )

  valid <- is.finite(
    value
  )

  data <- data[
    valid,
    ,
    drop = FALSE
  ]

  data$.value <- value[
    valid
  ]

  if (nrow(data) == 0) {
    return(
      data.frame()
    )
  }

  split_key <- interaction(
    data[
      ,
      group_columns,
      drop = FALSE
    ],
    drop = TRUE,
    lex.order = TRUE
  )

  split_data <- split(
    data,
    split_key
  )

  rows <- lapply(
    split_data,
    function(z) {
      grouping_values <- z[
        1,
        group_columns,
        drop = FALSE
      ]

      cbind(
        grouping_values,
        data.frame(
          N = sum(
            is.finite(
              z$.value
            )
          ),
          Mean = mean(
            z$.value,
            na.rm = TRUE
          ),
          SD = stats::sd(
            z$.value,
            na.rm = TRUE
          ),
          SE = stats::sd(
            z$.value,
            na.rm = TRUE
          ) /
            sqrt(
              sum(
                is.finite(
                  z$.value
                )
              )
            ),
          Median = stats::median(
            z$.value,
            na.rm = TRUE
          ),
          stringsAsFactors = FALSE
        )
      )
    }
  )

  do.call(
    rbind,
    rows
  )
}

draw_forest <- function(
  data,
  label_column,
  effect_column,
  lower_column,
  upper_column,
  title_text,
  x_label
) {
  if (
    is.null(data) ||
      nrow(data) == 0
  ) {
    plot.new()
    title(
      title_text
    )
    text(
      0.5,
      0.5,
      "No estimable rows"
    )
    return(
      invisible(NULL)
    )
  }

  effect <- safe_numeric(
    data[[effect_column]]
  )

  lower <- safe_numeric(
    data[[lower_column]]
  )

  upper <- safe_numeric(
    data[[upper_column]]
  )

  labels <- as.character(
    data[[label_column]]
  )

  order_index <- order(
    effect
  )

  effect <- effect[
    order_index
  ]

  lower <- lower[
    order_index
  ]

  upper <- upper[
    order_index
  ]

  labels <- labels[
    order_index
  ]

  y <- seq_along(
    effect
  )

  x_range <- range(
    c(
      lower,
      upper,
      0
    ),
    na.rm = TRUE
  )

  plot(
    effect,
    y,
    xlim = x_range,
    ylim = c(
      0.5,
      length(y) + 0.5
    ),
    yaxt = "n",
    ylab = "",
    xlab = x_label,
    pch = 16,
    main = title_text
  )

  segments(
    lower,
    y,
    upper,
    y
  )

  axis(
    2,
    at = y,
    labels = labels,
    las = 2,
    cex.axis = 0.65
  )

  abline(
    v = 0,
    lty = 2
  )
}

draw_auc_forest <- function(
  data,
  title_text
) {
  if (nrow(data) == 0) {
    plot.new()
    title(
      title_text
    )
    return(
      invisible(NULL)
    )
  }

  data <- data[
    order(
      data$AUC
    ),
    ,
    drop = FALSE
  ]

  y <- seq_len(
    nrow(data)
  )

  plot(
    data$AUC,
    y,
    xlim = c(
      0,
      1
    ),
    ylim = c(
      0.5,
      nrow(data) + 0.5
    ),
    yaxt = "n",
    ylab = "",
    xlab = "AUC (95% bootstrap CI)",
    main = title_text,
    pch = 16
  )

  segments(
    data$AUC_CI_Low,
    y,
    data$AUC_CI_High,
    y
  )

  axis(
    2,
    at = y,
    labels = data$Display_Label,
    las = 2,
    cex.axis = 0.7
  )

  abline(
    v = 0.5,
    lty = 2
  )
}

draw_mean_trajectory <- function(
  summary_data,
  title_text,
  y_label,
  group_column = NULL
) {
  if (
    is.null(summary_data) ||
      nrow(summary_data) == 0
  ) {
    plot.new()
    title(
      title_text
    )
    text(
      0.5,
      0.5,
      "No data"
    )
    return(
      invisible(NULL)
    )
  }

  summary_data$Time_Order_Resolved <-
    safe_numeric(
      summary_data$Time_Order_Resolved
    )

  summary_data <- summary_data[
    order(
      summary_data$Time_Order_Resolved
    ),
    ,
    drop = FALSE
  ]

  x_levels <- unique(
    summary_data[
      ,
      c(
        "Time_Label",
        "Time_Order_Resolved"
      ),
      drop = FALSE
    ]
  )

  x_levels <- x_levels[
    order(
      x_levels$Time_Order_Resolved
    ),
    ,
    drop = FALSE
  ]

  y_range <- range(
    c(
      summary_data$Mean -
        summary_data$SE,
      summary_data$Mean +
        summary_data$SE
    ),
    na.rm = TRUE
  )

  plot(
    NA,
    xlim = range(
      x_levels$Time_Order_Resolved
    ),
    ylim = y_range,
    xaxt = "n",
    xlab = "Timepoint",
    ylab = y_label,
    main = title_text
  )

  axis(
    1,
    at = x_levels$Time_Order_Resolved,
    labels = x_levels$Time_Label,
    las = 2,
    cex.axis = 0.75
  )

  if (is.null(group_column)) {
    z <- summary_data

    lines(
      z$Time_Order_Resolved,
      z$Mean,
      lwd = 2
    )

    points(
      z$Time_Order_Resolved,
      z$Mean,
      pch = 16
    )

    segments(
      z$Time_Order_Resolved,
      z$Mean - z$SE,
      z$Time_Order_Resolved,
      z$Mean + z$SE
    )
  } else {
    groups <- unique(
      as.character(
        summary_data[[group_column]]
      )
    )

    line_types <- seq_along(
      groups
    )

    point_types <- seq_along(
      groups
    ) + 14

    for (i in seq_along(groups)) {
      group_value <- groups[i]

      z <- summary_data[
        as.character(
          summary_data[[group_column]]
        ) ==
          group_value,
        ,
        drop = FALSE
      ]

      z <- z[
        order(
          z$Time_Order_Resolved
        ),
        ,
        drop = FALSE
      ]

      lines(
        z$Time_Order_Resolved,
        z$Mean,
        lwd = 2,
        lty = line_types[i]
      )

      points(
        z$Time_Order_Resolved,
        z$Mean,
        pch = point_types[i]
      )

      segments(
        z$Time_Order_Resolved,
        z$Mean - z$SE,
        z$Time_Order_Resolved,
        z$Mean + z$SE,
        lty = line_types[i]
      )
    }

    legend(
      "topright",
      legend = groups,
      lty = line_types,
      pch = point_types,
      bty = "n"
    )
  }
}

# ------------------------------------------------------------
# Required input inventory
# ------------------------------------------------------------
input_files <- data.frame(
  Key = c(
    "Analysis_Set_Summary",
    "Analysis_Set_QC",
    "Group_Descriptive",
    "Alpha_Tests",
    "Beta_PERMANOVA",
    "Beta_Dispersion",
    "Discovery_Audit",
    "Family_Harmonization",
    "Meta_Results",
    "Candidate_Grading",
    "Meta_Audit",
    "Global_Meta_Audit",
    "SDI_Candidate_Lock",
    "SDI_Formula_Lock",
    "SDI_Model_Lock",
    "SDI_Performance",
    "SDI_Threshold",
    "SDI_Group_Tests",
    "SDI_LOCO",
    "SDI_External_Audit",
    "Candidate_CrossSet_Diagnosis",
    "Exploratory_Score_Performance",
    "Leave_One_Genus_Out",
    "Diagnostic_Conclusion",
    "Diagnostic_Workflow_Audit",
    "Day12_Input_Audit",
    "Timepoint_Order_Audit",
    "Mixed_Model_Terms",
    "Mixed_Model_Coefficients",
    "Paired_Changes",
    "Myocardial_CrossSectional",
    "Timepoint_Group_Comparisons",
    "Day12_Analysis_Set_Audit",
    "Day12_Workflow_Audit",
    "Day10_Sample_Scores",
    "Day12_Sample_Metrics"
  ),
  Path = c(
    file.path(
      day7_root,
      "15_Analysis_Set_Summary.csv"
    ),
    file.path(
      day7_root,
      "15_Analysis_Set_QC.csv"
    ),
    file.path(
      day7_root,
      "15_Group_Descriptive_Statistics.csv"
    ),
    file.path(
      day8_root,
      "16_Alpha_Diversity_Tests.csv"
    ),
    file.path(
      day8_root,
      "16_Beta_Diversity_PERMANOVA.csv"
    ),
    file.path(
      day8_root,
      "16_Beta_Dispersion_Tests.csv"
    ),
    file.path(
      day8_root,
      "16_Discovery_Analysis_Audit.csv"
    ),
    file.path(
      day9_root,
      "17_Family_Feature_Harmonization.csv"
    ),
    file.path(
      day9_root,
      "17_Random_Fixed_Meta_Results.csv"
    ),
    file.path(
      day9_root,
      "17_Candidate_Genus_Grading.csv"
    ),
    file.path(
      day9_root,
      "17_Meta_Analysis_Audit.csv"
    ),
    file.path(
      day9_root,
      "17_Global_Meta_Workflow_Audit.csv"
    ),
    file.path(
      day10_lock_root,
      "18_SDI_Candidate_Lock.csv"
    ),
    file.path(
      day10_lock_root,
      "18_SDI_Formula_Lock.csv"
    ),
    file.path(
      day10_lock_root,
      "18_SDI_Model_and_Threshold_Lock.csv"
    ),
    file.path(
      day10_root,
      "18_SDI_Performance_Summary.csv"
    ),
    file.path(
      day10_root,
      "18_SDI_Locked_Threshold_Metrics.csv"
    ),
    file.path(
      day10_root,
      "18_SDI_Group_Comparison_Tests.csv"
    ),
    file.path(
      day10_root,
      "18_SDI_Internal_LOCO_Validation.csv"
    ),
    file.path(
      day10_root,
      "18_SDI_External_Validation_Audit.csv"
    ),
    file.path(
      day11_root,
      "19_Candidate_CrossSet_Diagnosis.csv"
    ),
    file.path(
      day11_root,
      "19_Exploratory_Score_Performance.csv"
    ),
    file.path(
      day11_root,
      "19_Leave_One_Genus_Out_Sensitivity.csv"
    ),
    file.path(
      day11_root,
      "19_Diagnostic_Conclusion.csv"
    ),
    file.path(
      day11_root,
      "19_Diagnostic_Workflow_Audit.csv"
    ),
    file.path(
      day12_root,
      "20_Input_Audit.csv"
    ),
    file.path(
      day12_root,
      "20_Timepoint_Order_Audit.csv"
    ),
    file.path(
      day12_root,
      "20_Mixed_Model_Overall_Terms.csv"
    ),
    file.path(
      day12_root,
      "20_Mixed_Model_Coefficients.csv"
    ),
    file.path(
      day12_root,
      "20_Paired_Timepoint_Changes.csv"
    ),
    file.path(
      day12_root,
      "20_Myocardial_Dysfunction_CrossSectional.csv"
    ),
    file.path(
      day12_root,
      "20_Timepoint_Group_Comparisons.csv"
    ),
    file.path(
      day12_root,
      "20_Analysis_Set_Audit.csv"
    ),
    file.path(
      day12_root,
      "20_Workflow_Audit.csv"
    ),
    day10_sample_file,
    day12_sample_file
  ),
  stringsAsFactors = FALSE
)

input_files$Exists <- file.exists(
  input_files$Path
)

if (
  any(
    !input_files$Exists
  )
) {
  missing <- input_files[
    !input_files$Exists,
    ,
    drop = FALSE
  ]

  wcsv(
    missing,
    file.path(
      audit_root,
      "21_Missing_Required_Files.csv"
    )
  )

  stop(
    paste0(
      "缺少",
      nrow(missing),
      "个Day 7-12必需文件。详见21_Missing_Required_Files.csv。"
    )
  )
}

# Read all required files.
data_list <- lapply(
  input_files$Path,
  read_csv_strict
)

names(data_list) <- input_files$Key

input_files$Rows <- vapply(
  data_list,
  nrow,
  integer(1)
)

input_files$Columns <- vapply(
  data_list,
  ncol,
  integer(1)
)

input_files$MD5 <- unname(
  tools::md5sum(
    input_files$Path
  )
)

wcsv(
  input_files,
  file.path(
    audit_root,
    "21_Required_Input_Inventory.csv"
  )
)

# ------------------------------------------------------------
# Freeze copies of selected result files
# ------------------------------------------------------------
for (
  i in seq_len(
    nrow(input_files)
  )
) {
  destination_name <- paste0(
    sprintf(
      "%02d",
      i
    ),
    "_",
    basename(
      input_files$Path[i]
    )
  )

  copied <- file.copy(
    input_files$Path[i],
    file.path(
      frozen_input_root,
      destination_name
    ),
    overwrite = TRUE
  )

  if (!copied) {
    stop(
      "无法复制冻结文件：",
      input_files$Path[i]
    )
  }
}

# ------------------------------------------------------------
# Audit scan
# ------------------------------------------------------------
audit_scan_rows <- list()

for (
  key in names(data_list)
) {
  data <- data_list[[key]]

  status_columns <- names(data)[
    grepl(
      "status|decision|unchanged|pass",
      names(data),
      ignore.case = TRUE
    )
  ]

  if (
    length(
      status_columns
    ) == 0
  ) {
    next
  }

  for (
    status_column in status_columns
  ) {
    values <- as.character(
      data[[status_column]]
    )

    audit_scan_rows[[
      length(
        audit_scan_rows
      ) + 1
    ]] <- data.frame(
      File_Key = key,
      Column = status_column,
      Rows = length(values),
      Explicit_FAIL_N = sum(
        grepl(
          "^FAIL$|FAILED",
          values,
          ignore.case = TRUE
        ),
        na.rm = TRUE
      ),
      REVIEW_N = sum(
        grepl(
          "REVIEW",
          values,
          ignore.case = TRUE
        ),
        na.rm = TRUE
      ),
      PASS_N = sum(
        grepl(
          "PASS|TRUE|UNCHANGED",
          values,
          ignore.case = TRUE
        ),
        na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }
}

audit_scan <- if (
  length(
    audit_scan_rows
  ) > 0
) {
  do.call(
    rbind,
    audit_scan_rows
  )
} else {
  data.frame()
}

wcsv(
  audit_scan,
  file.path(
    audit_root,
    "21_Status_Field_Scan.csv"
  )
)

# Mandatory workflow assertions.
day10_external_audit <- data_list[["SDI_External_Audit"]]

day11_workflow_audit <- data_list[["Diagnostic_Workflow_Audit"]]

day12_workflow_audit <- data_list[["Day12_Workflow_Audit"]]

timepoint_order_audit <- data_list[["Timepoint_Order_Audit"]]

assertions <- data.frame(
  Check = c(
    "Day10 external-validation workflow audit contains no FAIL",
    "Day11 diagnostic workflow audit contains no FAIL",
    "Day12 workflow audit contains no FAIL",
    "Day12 timepoint-order audit all PASS",
    "Day10 external validation remains failed in diagnostic conclusion"
  ),
  Passed = c(
    !any(
      grepl(
        "^FAIL$",
        unlist(
          day10_external_audit
        ),
        ignore.case = TRUE
      )
    ),
    !any(
      grepl(
        "^FAIL$",
        unlist(
          day11_workflow_audit
        ),
        ignore.case = TRUE
      )
    ),
    !any(
      grepl(
        "^FAIL$",
        unlist(
          day12_workflow_audit
        ),
        ignore.case = TRUE
      )
    ),
    "Status" %in%
      names(
        timepoint_order_audit
      ) &&
      all(
        timepoint_order_audit$Status ==
          "PASS"
      ),
    {
      diagnosis <- data_list[["Diagnostic_Conclusion"]]

      any(
        diagnosis$Item ==
          "Day10_confirmatory_external_validation" &
          diagnosis$Value ==
            "FAILED"
      )
    }
  ),
  stringsAsFactors = FALSE
)

assertions$Status <- ifelse(
  assertions$Passed,
  "PASS",
  "FAIL"
)

wcsv(
  assertions,
  file.path(
    audit_root,
    "21_Final_Freeze_Assertions.csv"
  )
)

if (
  any(
    !assertions$Passed
  )
) {
  stop(
    "最终冻结断言未全部通过。详见21_Final_Freeze_Assertions.csv。"
  )
}

# ------------------------------------------------------------
# Extract source datasets
# ------------------------------------------------------------
analysis_set_summary <- data_list[["Analysis_Set_Summary"]]

analysis_set_qc <- data_list[["Analysis_Set_QC"]]

group_descriptive <- data_list[["Group_Descriptive"]]

alpha_tests <- data_list[["Alpha_Tests"]]

beta_permanova <- data_list[["Beta_PERMANOVA"]]

# Day 8 stores the adjusted PERMANOVA p-value as P_FDR.
# Resolve known field names once and expose a unified numeric FDR
# column for tables, figures, and the manuscript claim registry.
beta_fdr_candidates <- c(
  "P_FDR",
  "FDR",
  "PERMANOVA_FDR",
  "Adjusted_P",
  "P_Adjusted",
  "q_value",
  "qvalue"
)

beta_fdr_column <- first_existing(
  beta_permanova,
  beta_fdr_candidates,
  required = TRUE
)

beta_permanova$FDR <- safe_numeric(
  beta_permanova[[beta_fdr_column]]
)

expected_beta_sets <- c(
  "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
  "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
  "A03_PRJNA691455_Sepsis_vs_Healthy",
  "A06_PRJNA978257_Sepsis_vs_Healthy"
)

beta_fdr_expected_rows <- beta_permanova[
  beta_permanova$Analysis_Set %in%
    expected_beta_sets,
  ,
  drop = FALSE
]

beta_fdr_mapping_pass <- (
  nrow(beta_fdr_expected_rows) ==
    length(expected_beta_sets) &&
  all(
    is.finite(
      beta_fdr_expected_rows$FDR
    )
  ) &&
  all(
    beta_fdr_expected_rows$FDR >= 0 &
      beta_fdr_expected_rows$FDR <= 1
  )
)

resolved_fdr_values <- beta_fdr_expected_rows$FDR[
  match(
    expected_beta_sets,
    beta_fdr_expected_rows$Analysis_Set
  )
]

beta_fdr_field_map <- data.frame(
  Source_File =
    "16_Beta_Diversity_PERMANOVA.csv",
  Source_Field =
    beta_fdr_column,
  Unified_Field =
    "FDR",
  Expected_Analysis_Set =
    expected_beta_sets,
  Resolved_Value =
    resolved_fdr_values,
  Status = ifelse(
    is.finite(
      resolved_fdr_values
    ),
    "PASS",
    "FAIL"
  ),
  stringsAsFactors = FALSE
)

wcsv(
  beta_fdr_field_map,
  file.path(
    audit_root,
    "21_Beta_PERMANOVA_FDR_Field_Map.csv"
  )
)

assertions <- rbind(
  assertions,
  data.frame(
    Check =
      "PERMANOVA adjusted-p-value field resolved for A01/A02/A03/A06",
    Passed =
      beta_fdr_mapping_pass,
    Status = ifelse(
      beta_fdr_mapping_pass,
      "PASS",
      "FAIL"
    ),
    stringsAsFactors = FALSE
  )
)

wcsv(
  assertions,
  file.path(
    audit_root,
    "21_Final_Freeze_Assertions.csv"
  )
)

if (!beta_fdr_mapping_pass) {
  stop(
    paste0(
      "PERMANOVA FDR字段映射失败。来源字段：",
      beta_fdr_column,
      "。详见21_Beta_PERMANOVA_FDR_Field_Map.csv。"
    )
  )
}

data_list[["Beta_PERMANOVA"]] <-
  beta_permanova

beta_dispersion <- data_list[["Beta_Dispersion"]]

meta_results <- data_list[["Meta_Results"]]

candidate_grading <- data_list[["Candidate_Grading"]]

candidate_lock <- data_list[["SDI_Candidate_Lock"]]

formula_lock <- data_list[["SDI_Formula_Lock"]]

model_lock <- data_list[["SDI_Model_Lock"]]

sdi_performance <- data_list[["SDI_Performance"]]

sdi_threshold <- data_list[["SDI_Threshold"]]

sdi_group_tests <- data_list[["SDI_Group_Tests"]]

sdi_loco <- data_list[["SDI_LOCO"]]

crossset_diagnosis <- data_list[["Candidate_CrossSet_Diagnosis"]]

exploratory_score_performance <- data_list[["Exploratory_Score_Performance"]]

leave_one_genus_out <- data_list[["Leave_One_Genus_Out"]]

mixed_terms <- data_list[["Mixed_Model_Terms"]]

mixed_coefficients <- data_list[["Mixed_Model_Coefficients"]]

paired_changes <- data_list[["Paired_Changes"]]

myocardial_results <- data_list[["Myocardial_CrossSectional"]]

timepoint_group_comparisons <- data_list[["Timepoint_Group_Comparisons"]]

day10_samples <- data_list[["Day10_Sample_Scores"]]

day12_samples <- data_list[["Day12_Sample_Metrics"]]

# ------------------------------------------------------------
# Main Table 1: cohort and analysis architecture
# ------------------------------------------------------------
table1_columns <- c(
  "Analysis_Set",
  "Project_ID",
  "Meta_Family",
  "Analysis_Role",
  "Comparison",
  "Group_A",
  "Group_B",
  "N_Group_A",
  "N_Group_B",
  "Total_N",
  "Sample_N",
  "Patient_N",
  "Locked_Status",
  "QC_Status",
  "Status"
)

table1 <- select_existing(
  analysis_set_summary,
  table1_columns
)

if (
  ncol(table1) < 3
) {
  table1 <- analysis_set_summary
}

table1$Manuscript_Role <- NA_character_

analysis_set_column <- first_existing(
  table1,
  c(
    "Analysis_Set",
    "Analysis_ID"
  )
)

if (!is.null(analysis_set_column)) {
  set_id <- as.character(
    table1[[analysis_set_column]]
  )

  table1$Manuscript_Role <- ifelse(
    grepl(
      "^A0[12]",
      set_id
    ),
    "Primary ICU-control discovery",
    ifelse(
      grepl(
        "^A0[36]",
        set_id
      ),
      "Healthy-control supplementary discovery",
      ifelse(
        grepl(
          "^A0[45]",
          set_id
        ),
        "Locked external validation",
        ifelse(
          grepl(
            "^S0[1-6]",
            set_id
          ),
          "Longitudinal/supportive",
          "Other"
        )
      )
    )
  )
}

# ------------------------------------------------------------
# Main Table 2: discovery and meta-analysis summary
# ------------------------------------------------------------
alpha_table <- select_existing(
  alpha_tests,
  c(
    "Analysis_Set",
    "Project_ID",
    "Meta_Family",
    "Metric",
    "Group_A",
    "Group_B",
    "Median_Group_A",
    "Median_Group_B",
    "P",
    "FDR",
    "Cliffs_Delta",
    "Status"
  )
)

beta_table <- select_existing(
  beta_permanova,
  c(
    "Analysis_Set",
    "Project_ID",
    "Meta_Family",
    "Group_A",
    "Group_B",
    "R2",
    "F",
    "P",
    "FDR",
    "Permutations",
    "Status"
  )
)

meta_results$Genus <- pretty_genus(
  meta_results$Genus_Feature
)

meta_results$Rank_P <- if (
  "Fixed_FDR" %in%
    names(meta_results)
) {
  safe_numeric(
    meta_results$Fixed_FDR
  )
} else {
  safe_numeric(
    meta_results$REML_FDR
  )
}

meta_results$Direction_Consistent_Bool <-
  if (
    "Direction_Consistent" %in%
      names(meta_results)
  ) {
    as_bool(
      meta_results$Direction_Consistent
    )
  } else {
    TRUE
  }

meta_key <- meta_results[
  meta_results$K >= 2 &
    meta_results$Direction_Consistent_Bool,
  ,
  drop = FALSE
]

meta_key <- meta_key[
  order(
    meta_key$Meta_Family,
    meta_key$Rank_P,
    -abs(
      safe_numeric(
        meta_key$REML_Effect
      )
    )
  ),
  ,
  drop = FALSE
]

meta_key <- do.call(
  rbind,
  lapply(
    split(
      meta_key,
      meta_key$Meta_Family
    ),
    function(z) {
      head(
        z,
        12
      )
    }
  )
)

meta_table <- select_existing(
  meta_key,
  c(
    "Meta_Family",
    "Genus",
    "K",
    "Direction",
    "Direction_Consistent",
    "REML_Effect",
    "REML_CI_Low",
    "REML_CI_High",
    "REML_P",
    "REML_FDR",
    "Fixed_FDR",
    "HK_P",
    "HK_FDR",
    "Tau2",
    "I2",
    "Candidate_Grade",
    "Proposed_SDI_Use"
  )
)

# ------------------------------------------------------------
# Main Table 3: SDI development and validation
# ------------------------------------------------------------
sdi_main <- sdi_performance[
  sdi_performance$Score ==
    "SDI_Equal_Balance",
  ,
  drop = FALSE
]

sdi_main$Result_Interpretation <- ifelse(
  grepl(
    "External",
    sdi_main$Evaluation_Role,
    ignore.case = TRUE
  ) |
    grepl(
      "^A0[45]",
      sdi_main$Dataset
    ),
  ifelse(
    sdi_main$AUC < 0.5,
    "External direction reversal; validation failed",
    "External result"
  ),
  "Development performance"
)

sdi_table <- select_existing(
  sdi_main,
  c(
    "Dataset",
    "Analysis_Set",
    "Evaluation_Role",
    "N",
    "Cases_N",
    "Controls_N",
    "AUC",
    "AUC_CI_Low",
    "AUC_CI_High",
    "Median_Cases",
    "Median_Controls",
    "Median_Difference",
    "Cliffs_Delta",
    "Wilcoxon_P",
    "Result_Interpretation"
  )
)

threshold_table <- sdi_threshold[
  sdi_threshold$Score ==
    "SDI_Equal_Balance",
  ,
  drop = FALSE
]

threshold_table <- select_existing(
  threshold_table,
  c(
    "Dataset",
    "Analysis_Set",
    "Score",
    "Threshold",
    "TP",
    "TN",
    "FP",
    "FN",
    "Sensitivity",
    "Specificity",
    "PPV",
    "NPV",
    "Accuracy",
    "Balanced_Accuracy"
  )
)

# ------------------------------------------------------------
# Main Table 4: longitudinal and support results
# ------------------------------------------------------------
key_metrics <- c(
  "Alpha_Observed",
  "Alpha_Shannon",
  "Alpha_InverseSimpson",
  "SDI_Equal_Balance",
  "SDI_MetaWeighted_Balance"
)

key_terms <- mixed_terms[
  mixed_terms$Metric %in%
    key_metrics &
    mixed_terms$Model_Term !=
      "(Intercept)",
  ,
  drop = FALSE
]

key_terms$Result_Category <- ifelse(
  grepl(
    "T01_",
    key_terms$Analysis_ID
  ),
  "Primary longitudinal",
  ifelse(
    grepl(
      "T02_",
      key_terms$Analysis_ID
    ),
    "Depth sensitivity",
    ifelse(
      grepl(
        "T03_",
        key_terms$Analysis_ID
      ),
      "Independent longitudinal support",
      ifelse(
        grepl(
          "T04_",
          key_terms$Analysis_ID
        ),
        "Intervention support",
        ifelse(
          grepl(
            "T06_",
            key_terms$Analysis_ID
          ),
          "Cholestasis longitudinal support",
          "Other"
        )
      )
    )
  )
)

longitudinal_table <- select_existing(
  key_terms,
  c(
    "Result_Category",
    "Analysis_ID",
    "Metric",
    "Model_Term",
    "Num_DF",
    "Den_DF",
    "F_Value",
    "P",
    "P_FDR",
    "N",
    "Patients_N",
    "Status"
  )
)

myocardial_table <- select_existing(
  myocardial_results[
    myocardial_results$Metric %in%
      key_metrics,
    ,
    drop = FALSE
  ],
  c(
    "Analysis_ID",
    "Endpoint",
    "Metric",
    "Yes_N",
    "No_N",
    "Median_Yes",
    "Median_No",
    "Median_Difference",
    "Cliffs_Delta_Yes_minus_No",
    "AUC_Yes_as_Case",
    "P",
    "P_FDR",
    "Status"
  )
)

# ------------------------------------------------------------
# Claim registry
# ------------------------------------------------------------
get_beta_row <- function(set_pattern) {
  z <- beta_permanova[
    grepl(
      set_pattern,
      beta_permanova$Analysis_Set
    ),
    ,
    drop = FALSE
  ]

  if (nrow(z) == 0) {
    return(NULL)
  }

  z[1, , drop = FALSE]
}

get_term_row <- function(
  analysis_pattern,
  metric,
  term_pattern
) {
  z <- mixed_terms[
    grepl(
      analysis_pattern,
      mixed_terms$Analysis_ID
    ) &
      mixed_terms$Metric ==
        metric &
      grepl(
        term_pattern,
        mixed_terms$Model_Term
      ),
    ,
    drop = FALSE
  ]

  if (nrow(z) == 0) {
    return(NULL)
  }

  z[
    order(
      safe_numeric(
        z$P_FDR
      )
    ),
    ,
    drop = FALSE
  ][1, , drop = FALSE]
}

get_coefficient_row <- function(
  analysis_pattern,
  metric,
  term_pattern
) {
  z <- mixed_coefficients[
    grepl(
      analysis_pattern,
      mixed_coefficients$Analysis_ID
    ) &
      mixed_coefficients$Metric ==
        metric &
      grepl(
        term_pattern,
        mixed_coefficients$Term
      ),
    ,
    drop = FALSE
  ]

  if (nrow(z) == 0) {
    return(NULL)
  }

  z[
    order(
      safe_numeric(
        z$P_FDR
      )
    ),
    ,
    drop = FALSE
  ][1, , drop = FALSE]
}

beta_a01 <- get_beta_row(
  "^A01"
)

beta_a02 <- get_beta_row(
  "^A02"
)

beta_a03 <- get_beta_row(
  "^A03"
)

beta_a06 <- get_beta_row(
  "^A06"
)

t01_shannon <- get_term_row(
  "^T01_",
  "Alpha_Shannon",
  "^Timepoint_Factor$"
)

t03_shannon <- get_term_row(
  "^T03_",
  "Alpha_Shannon",
  "^Timepoint_Factor$"
)

t06_shannon_interaction <- get_term_row(
  "^T06_",
  "Alpha_Shannon",
  "Timepoint_Factor:Status_Factor"
)

t01_day7_shannon <- get_coefficient_row(
  "^T01_",
  "Alpha_Shannon",
  "M2\\.7_ICU_day7"
)

development_equal <- sdi_main[
  sdi_main$Dataset ==
    "Development_Pooled",
  ,
  drop = FALSE
]

external_equal <- sdi_main[
  grepl(
    "^A0[45]",
    sdi_main$Dataset
  ),
  ,
  drop = FALSE
]

claim_registry <- data.frame(
  Claim_ID = c(
    "C01",
    "C02",
    "C03",
    "C04",
    "C05",
    "C06",
    "C07",
    "C08",
    "C09",
    "C10"
  ),
  Planned_Section = c(
    "Results: Discovery",
    "Results: Discovery",
    "Results: Genus meta-analysis",
    "Results: SDI development",
    "Results: External validation",
    "Results: Failure diagnosis",
    "Results: Longitudinal",
    "Results: Longitudinal support",
    "Results: Intervention and organ dysfunction",
    "Results: Organ dysfunction"
  ),
  Evidence_Level = c(
    "Primary discovery",
    "Secondary discovery",
    "Cross-cohort meta-analysis; k=2 per family",
    "Development only",
    "Prespecified external validation",
    "Post-hoc exploratory diagnosis",
    "Primary longitudinal supportive",
    "Independent small-cohort support",
    "Supportive exploratory",
    "Supportive longitudinal interaction"
  ),
  Claim_Status = c(
    "SUPPORTED_WITH_LIMITS",
    "SUPPORTED",
    "SUPPORTED_WITH_LIMITS",
    "SUPPORTED_IN_DEVELOPMENT",
    "FAILED",
    "SUPPORTED_AS_DIAGNOSIS_ONLY",
    "SUPPORTED",
    "TREND_ONLY",
    "NULL_OR_UNDERPOWERED",
    "SUPPORTED_WITH_LIMITS"
  ),
  Permitted_Wording = c(
    paste0(
      "Sepsis was associated with a small community-level difference ",
      "relative to non-sepsis ICU controls in the larger ICU cohort, ",
      "whereas the smaller ICU cohort did not independently confirm it."
    ),
    paste0(
      "Community-level separation was stronger in comparisons with ",
      "healthy controls than with ICU controls."
    ),
    paste0(
      "A subset of genera showed directionally concordant effects across ",
      "the two ICU-control cohorts, but inference is limited by only two cohorts."
    ),
    paste0(
      "The prespecified six-genus SDI achieved moderate discrimination ",
      "in pooled development data."
    ),
    paste0(
      "The locked SDI failed external validation and showed direction reversal ",
      "in both external comparisons."
    ),
    paste0(
      "Post-hoc diagnostics indicated that external failure was driven mainly ",
      "by systematic reversal among detectable candidate genera rather than ",
      "only by absent features."
    ),
    paste0(
      "Within PRJEB33360, genus richness and alpha diversity declined during ",
      "continued ICU stay, with the largest decrease near ICU day 7; ",
      "the depth sensitivity analysis was concordant."
    ),
    paste0(
      "PRJNA691455 showed a directionally similar decline in Shannon diversity, ",
      "but the small sample did not retain significance after multiple testing."
    ),
    paste0(
      "No reliable probiotic-intervention or myocardial-dysfunction association ",
      "was detected; these analyses were small and underpowered."
    ),
    paste0(
      "Cholestasis status was associated with a different Shannon-diversity ",
      "trajectory over time, driven mainly by divergence by day 7."
    )
  ),
  Prohibited_Wording = c(
    "A universal sepsis microbiome signature was confirmed.",
    "Healthy-control and ICU-control comparisons measure the same effect.",
    "The identified genera are definitive diagnostic biomarkers.",
    "The SDI is a validated diagnostic model.",
    "External validation was successful after reversing the score.",
    "Post-hoc alternatives represent independent validation.",
    "Longitudinal changes prove causal effects of sepsis.",
    "The small cohort independently confirms statistical significance.",
    "The intervention is ineffective or dysfunction has no biological association.",
    "Cholestasis causes microbiome decline."
  ),
  Numerical_Evidence = c(
    if (!is.null(beta_a01)) {
      paste0(
        "A01 R2=",
        format_num(
          beta_a01$R2,
          3
        ),
        ", FDR=",
        format_p(
          beta_a01$FDR
        ),
        "; A02 FDR=",
        if (!is.null(beta_a02)) {
          format_p(
            beta_a02$FDR
          )
        } else {
          "NA"
        }
      )
    } else {
      "See Table 2"
    },
    paste0(
      "A03 R2/FDR=",
      if (!is.null(beta_a03)) {
        paste0(
          format_num(
            beta_a03$R2,
            3
          ),
          "/",
          format_p(
            beta_a03$FDR
          )
        )
      } else {
        "NA"
      },
      "; A06 R2/FDR=",
      if (!is.null(beta_a06)) {
        paste0(
          format_num(
            beta_a06$R2,
            3
          ),
          "/",
          format_p(
            beta_a06$FDR
          )
        )
      } else {
        "NA"
      }
    ),
    paste0(
      "See 17_Random_Fixed_Meta_Results.csv; ",
      sum(
        meta_key$Meta_Family ==
          "ICU_control"
      ),
      " ICU-family genera retained in the display table."
    ),
    if (nrow(development_equal) > 0) {
      paste0(
        "Development AUC=",
        format_num(
          development_equal$AUC[1],
          3
        ),
        " (",
        format_num(
          development_equal$AUC_CI_Low[1],
          3
        ),
        "-",
        format_num(
          development_equal$AUC_CI_High[1],
          3
        ),
        ")"
      )
    } else {
      "See Table 3"
    },
    if (nrow(external_equal) > 0) {
      paste0(
        "External AUC range=",
        format_num(
          min(
            external_equal$AUC,
            na.rm = TRUE
          ),
          3
        ),
        "-",
        format_num(
          max(
            external_equal$AUC,
            na.rm = TRUE
          ),
          3
        )
      )
    } else {
      "See Table 3"
    },
    paste0(
      sum(
        crossset_diagnosis$Diagnostic_Label ==
          "COMPLETE_EXTERNAL_DIRECTION_REVERSAL",
        na.rm = TRUE
      ),
      " detectable genera fully reversed; ",
      sum(
        crossset_diagnosis$Diagnostic_Label ==
          "PROJECT_LEVEL_FEATURE_ABSENCE",
        na.rm = TRUE
      ),
      " genera absent at project level."
    ),
    paste0(
      "T01 Shannon overall FDR=",
      if (!is.null(t01_shannon)) {
        format_p(
          t01_shannon$P_FDR
        )
      } else {
        "NA"
      },
      "; Day 7 estimate=",
      if (!is.null(t01_day7_shannon)) {
        format_num(
          t01_day7_shannon$Estimate,
          2
        )
      } else {
        "NA"
      }
    ),
    paste0(
      "T03 Shannon overall FDR=",
      if (!is.null(t03_shannon)) {
        format_p(
          t03_shannon$P_FDR
        )
      } else {
        "NA"
      }
    ),
    "No predefined intervention or myocardial key metric retained FDR<0.05.",
    paste0(
      "T06 Shannon interaction FDR=",
      if (!is.null(t06_shannon_interaction)) {
        format_p(
          t06_shannon_interaction$P_FDR
        )
      } else {
        "NA"
      }
    )
  ),
  stringsAsFactors = FALSE
)

wcsv(
  claim_registry,
  file.path(
    text_root,
    "21_Manuscript_Claim_Registry.csv"
  )
)

# ------------------------------------------------------------
# Results-number summary
# ------------------------------------------------------------
results_number_summary <- data.frame(
  Topic = c(
    "Development SDI AUC",
    "External A04 SDI AUC",
    "External A05 SDI AUC",
    "External reversed detectable genera",
    "External project-absent genera",
    "T01 Shannon overall FDR",
    "T01 Day 7 Shannon coefficient",
    "T03 Shannon overall FDR",
    "T06 Shannon interaction FDR"
  ),
  Value = c(
    if (
      nrow(
        development_equal
      ) > 0
    ) {
      format_num(
        development_equal$AUC[1],
        3
      )
    } else {
      ""
    },
    {
      z <- external_equal[
        grepl(
          "^A04",
          external_equal$Dataset
        ),
        ,
        drop = FALSE
      ]

      if (
        nrow(z) > 0
      ) {
        format_num(
          z$AUC[1],
          3
        )
      } else {
        ""
      }
    },
    {
      z <- external_equal[
        grepl(
          "^A05",
          external_equal$Dataset
        ),
        ,
        drop = FALSE
      ]

      if (
        nrow(z) > 0
      ) {
        format_num(
          z$AUC[1],
          3
        )
      } else {
        ""
      }
    },
    sum(
      crossset_diagnosis$Diagnostic_Label ==
        "COMPLETE_EXTERNAL_DIRECTION_REVERSAL",
      na.rm = TRUE
    ),
    sum(
      crossset_diagnosis$Diagnostic_Label ==
        "PROJECT_LEVEL_FEATURE_ABSENCE",
      na.rm = TRUE
    ),
    if (!is.null(t01_shannon)) {
      format_p(
        t01_shannon$P_FDR
      )
    } else {
      ""
    },
    if (!is.null(t01_day7_shannon)) {
      paste0(
        format_num(
          t01_day7_shannon$Estimate,
          2
        ),
        " [",
        format_num(
          t01_day7_shannon$CI_Low,
          2
        ),
        ", ",
        format_num(
          t01_day7_shannon$CI_High,
          2
        ),
        "]"
      )
    } else {
      ""
    },
    if (!is.null(t03_shannon)) {
      format_p(
        t03_shannon$P_FDR
      )
    } else {
      ""
    },
    if (!is.null(t06_shannon_interaction)) {
      format_p(
        t06_shannon_interaction$P_FDR
      )
    } else {
      ""
    }
  ),
  Interpretation = c(
    "Moderate discrimination in development only",
    "Direction reversal; external validation failed",
    "Direction reversal; external validation failed",
    "Post-hoc failure diagnosis",
    "Feature harmonization limitation",
    "Primary longitudinal support",
    "Largest ICU-stay Shannon decline",
    "Directionally supportive trend",
    "Supportive organ-dysfunction trajectory interaction"
  ),
  stringsAsFactors = FALSE
)

wcsv(
  results_number_summary,
  file.path(
    text_root,
    "21_Results_Number_Summary.csv"
  )
)

# ------------------------------------------------------------
# Figure source data
# ------------------------------------------------------------
beta_source <- select_existing(
  beta_permanova,
  c(
    "Analysis_Set",
    "Project_ID",
    "Meta_Family",
    "Group_A",
    "Group_B",
    "R2",
    "P",
    "FDR"
  )
)

wcsv(
  beta_source,
  file.path(
    source_root,
    "21_Figure2A_Beta_PERMANOVA_Source.csv"
  )
)

icu_meta_source <- meta_results[
  meta_results$Meta_Family ==
    "ICU_control" &
    meta_results$K >= 2 &
    meta_results$Direction_Consistent_Bool,
  ,
  drop = FALSE
]

icu_meta_source <- icu_meta_source[
  order(
    icu_meta_source$Rank_P,
    -abs(
      safe_numeric(
        icu_meta_source$REML_Effect
      )
    )
  ),
  ,
  drop = FALSE
]

icu_meta_source <- head(
  icu_meta_source,
  12
)

healthy_meta_source <- meta_results[
  meta_results$Meta_Family ==
    "Healthy_control" &
    meta_results$K >= 2 &
    meta_results$Direction_Consistent_Bool,
  ,
  drop = FALSE
]

healthy_meta_source <- healthy_meta_source[
  order(
    healthy_meta_source$Rank_P,
    -abs(
      safe_numeric(
        healthy_meta_source$REML_Effect
      )
    )
  ),
  ,
  drop = FALSE
]

healthy_meta_source <- head(
  healthy_meta_source,
  12
)

wcsv(
  icu_meta_source,
  file.path(
    source_root,
    "21_Figure2B_ICU_Meta_Source.csv"
  )
)

wcsv(
  healthy_meta_source,
  file.path(
    source_root,
    "21_Figure2C_Healthy_Meta_Source.csv"
  )
)

auc_source <- sdi_main[
  sdi_main$Dataset %in%
    c(
      "Development_Pooled",
      "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
      "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
      "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
      "A05_PRJNA1010969_External_Sepsis_vs_Trauma"
    ),
  ,
  drop = FALSE
]

auc_source$Display_Label <- c(
  "Development pooled",
  "A01 ICU discovery",
  "A02 ICU discovery",
  "A04 external healthy",
  "A05 external trauma"
)[
  match(
    auc_source$Dataset,
    c(
      "Development_Pooled",
      "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
      "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
      "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
      "A05_PRJNA1010969_External_Sepsis_vs_Trauma"
    )
  )
]

wcsv(
  auc_source,
  file.path(
    source_root,
    "21_Figure3A_SDI_AUC_Source.csv"
  )
)

candidate_diagnosis_source <- data_list[["Candidate_CrossSet_Diagnosis"]]

wcsv(
  candidate_diagnosis_source,
  file.path(
    source_root,
    "21_Figure3B_Candidate_Diagnosis_Source.csv"
  )
)

external_score_source <- day10_samples[
  day10_samples$Analysis_Set %in%
    c(
      "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
      "A05_PRJNA1010969_External_Sepsis_vs_Trauma"
    ),
  ,
  drop = FALSE
]

wcsv(
  external_score_source,
  file.path(
    source_root,
    "21_Figure3C_External_SDI_Distribution_Source.csv"
  )
)

t01_source <- day12_samples[
  day12_samples$Analysis_ID ==
    "T01_PRJEB33360_Longitudinal_Main",
  ,
  drop = FALSE
]

t03_source <- day12_samples[
  day12_samples$Analysis_ID ==
    "T03_PRJNA691455_Sepsis_Longitudinal",
  ,
  drop = FALSE
]

t06_source <- day12_samples[
  day12_samples$Analysis_ID ==
    "T06_PRJNA912621_Cholestasis_Longitudinal",
  ,
  drop = FALSE
]

t01_shannon_source <- mean_se_by(
  t01_source,
  "Alpha_Shannon",
  c(
    "Time_Label",
    "Time_Order_Resolved"
  )
)

t03_shannon_source <- mean_se_by(
  t03_source,
  "Alpha_Shannon",
  c(
    "Time_Label",
    "Time_Order_Resolved"
  )
)

t06_shannon_source <- mean_se_by(
  t06_source,
  "Alpha_Shannon",
  c(
    "Time_Label",
    "Time_Order_Resolved",
    "Status_Resolved"
  )
)

t01_sdi_source <- mean_se_by(
  t01_source,
  "SDI_Equal_Balance",
  c(
    "Time_Label",
    "Time_Order_Resolved"
  )
)

wcsv(
  t01_shannon_source,
  file.path(
    source_root,
    "21_Figure4A_T01_Shannon_Source.csv"
  )
)

wcsv(
  t03_shannon_source,
  file.path(
    source_root,
    "21_Figure4B_T03_Shannon_Source.csv"
  )
)

wcsv(
  t06_shannon_source,
  file.path(
    source_root,
    "21_Figure4C_T06_Shannon_Source.csv"
  )
)

wcsv(
  t01_sdi_source,
  file.path(
    source_root,
    "21_Figure4D_T01_SDI_Source.csv"
  )
)

# ------------------------------------------------------------
# Main Figure 1: study and analysis architecture
# ------------------------------------------------------------
pdf(
  file.path(
    figure_root,
    "Figure_1_Study_Design_and_Analysis_Architecture.pdf"
  ),
  width = 11,
  height = 7
)

plot.new()
plot.window(
  xlim = c(
    0,
    1
  ),
  ylim = c(
    0,
    1
  )
)

box <- function(
  x1,
  y1,
  x2,
  y2,
  text_value,
  font = 1,
  fill = "white"
) {
  rect(
    x1,
    y1,
    x2,
    y2,
    col = fill,
    border = "black"
  )

  text(
    (
      x1 + x2
    ) / 2,
    (
      y1 + y2
    ) / 2,
    text_value,
    cex = 0.8,
    font = font
  )
}

arrow_line <- function(
  x1,
  y1,
  x2,
  y2
) {
  arrows(
    x1,
    y1,
    x2,
    y2,
    length = 0.08
  )
}

title(
  "Study design and prespecified analysis architecture"
)

box(
  0.04,
  0.78,
  0.22,
  0.92,
  "Seven human cohorts\nindependently processed",
  font = 2,
  fill = "#F2F2F2"
)

box(
  0.29,
  0.78,
  0.48,
  0.92,
  "Frozen genus tables\nand analysis sets",
  font = 2,
  fill = "#F2F2F2"
)

arrow_line(
  0.22,
  0.85,
  0.29,
  0.85
)

box(
  0.56,
  0.78,
  0.76,
  0.92,
  "Discovery analyses\nA01, A02, A03, A06",
  font = 2
)

arrow_line(
  0.48,
  0.85,
  0.56,
  0.85
)

box(
  0.80,
  0.78,
  0.96,
  0.92,
  "Family-specific\nmeta-analysis",
  font = 2
)

arrow_line(
  0.76,
  0.85,
  0.80,
  0.85
)

box(
  0.08,
  0.48,
  0.30,
  0.64,
  "ICU-control family\nA01 + A02\nPrimary sepsis-specific evidence",
  fill = "#F7F7F7"
)

box(
  0.39,
  0.48,
  0.61,
  0.64,
  "Healthy-control family\nA03 + A06\nGeneral dysbiosis context",
  fill = "#F7F7F7"
)

box(
  0.70,
  0.48,
  0.92,
  0.64,
  "Candidate genera and\nprespecified six-genus SDI",
  fill = "#F7F7F7"
)

arrow_line(
  0.19,
  0.64,
  0.19,
  0.73
)

arrow_line(
  0.50,
  0.64,
  0.50,
  0.73
)

arrow_line(
  0.61,
  0.56,
  0.70,
  0.56
)

box(
  0.08,
  0.17,
  0.30,
  0.33,
  "Locked external validation\nA04: healthy\nA05: trauma\nResult: failed",
  font = 2,
  fill = "#EDEDED"
)

box(
  0.39,
  0.17,
  0.61,
  0.33,
  "Post-hoc failure diagnosis\nDirection reversal and\nproject-level feature absence",
  fill = "#F7F7F7"
)

box(
  0.70,
  0.17,
  0.92,
  0.33,
  "Supportive analyses\nS01-S06\nLongitudinal, intervention,\norgan dysfunction",
  fill = "#F7F7F7"
)

arrow_line(
  0.81,
  0.48,
  0.19,
  0.33
)

arrow_line(
  0.30,
  0.25,
  0.39,
  0.25
)

arrow_line(
  0.81,
  0.48,
  0.81,
  0.33
)

text(
  0.5,
  0.05,
  paste0(
    "External validation data were not used for feature selection, ",
    "coefficient refitting, or threshold tuning."
  ),
  cex = 0.8
)

dev.off()

# ------------------------------------------------------------
# Main Figure 2: discovery and meta-analysis
# ------------------------------------------------------------
pdf(
  file.path(
    figure_root,
    "Figure_2_Discovery_and_Meta_Analysis.pdf"
  ),
  width = 13,
  height = 8
)

old_par <- par(
  mfrow = c(
    1,
    3
  ),
  mar = c(
    8,
    5,
    4,
    2
  )
)

beta_plot <- beta_source[
  beta_source$Analysis_Set %in%
    c(
      "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
      "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
      "A03_PRJNA691455_Sepsis_vs_Healthy",
      "A06_PRJNA978257_Sepsis_vs_Healthy"
    ),
  ,
  drop = FALSE
]

beta_plot <- beta_plot[
  match(
    c(
      "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
      "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
      "A03_PRJNA691455_Sepsis_vs_Healthy",
      "A06_PRJNA978257_Sepsis_vs_Healthy"
    ),
    beta_plot$Analysis_Set
  ),
  ,
  drop = FALSE
]

beta_r2_percent <- beta_plot$R2 * 100

bar_positions <- barplot(
  beta_r2_percent,
  names.arg = c(
    "A01\nICU",
    "A02\nICU",
    "A03\nHealthy",
    "A06\nHealthy"
  ),
  ylab = "PERMANOVA R² (%)",
  main = "A. Community-level effects",
  las = 1,
  ylim = c(
    0,
    max(
      beta_r2_percent,
      na.rm = TRUE
    ) * 1.22
  )
)

text(
  bar_positions,
  beta_r2_percent,
  labels = paste0(
    "BH FDR=",
    format_p(
      beta_plot$FDR
    )
  ),
  pos = 3,
  cex = 0.68,
  xpd = TRUE
)

draw_forest(
  transform(
    icu_meta_source,
    Genus = pretty_genus(
      Genus_Feature
    )
  ),
  "Genus",
  "REML_Effect",
  "REML_CI_Low",
  "REML_CI_High",
  "B. ICU-control meta-analysis",
  "Pooled CLR effect: Sepsis minus ICU control"
)

draw_forest(
  transform(
    healthy_meta_source,
    Genus = pretty_genus(
      Genus_Feature
    )
  ),
  "Genus",
  "REML_Effect",
  "REML_CI_Low",
  "REML_CI_High",
  "C. Healthy-control meta-analysis",
  "Pooled CLR effect: Sepsis minus healthy"
)

par(
  old_par
)

dev.off()

# ------------------------------------------------------------
# Main Figure 3: SDI development, failure, and diagnosis
# ------------------------------------------------------------
pdf(
  file.path(
    figure_root,
    "Figure_3_SDI_Development_and_External_Failure.pdf"
  ),
  width = 12,
  height = 9
)

old_par <- par(
  mfrow = c(
    2,
    2
  ),
  mar = c(
    5,
    7,
    4,
    2
  )
)

draw_auc_forest(
  auc_source,
  "A. Prespecified equal-weight SDI"
)

diagnosis_plot <- crossset_diagnosis
diagnosis_plot$Genus <- pretty_genus(
  diagnosis_plot$Genus_Feature
)

diagnosis_values <- cbind(
  Development =
    diagnosis_plot$Mean_Development_Expected_Signed_Effect,
  External =
    diagnosis_plot$Mean_External_Expected_Signed_Effect
)

barplot(
  t(
    diagnosis_values
  ),
  beside = TRUE,
  names.arg = diagnosis_plot$Genus,
  las = 2,
  cex.names = 0.65,
  ylab = "Expected-signed effect",
  main = "B. Candidate direction diagnosis"
)

abline(
  h = 0,
  lty = 2
)

legend(
  "topright",
  legend = c(
    "Development",
    "External"
  ),
  fill = c(
    "white",
    "gray"
  ),
  bty = "n"
)

for (
  comparison_set in c(
    "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
    "A05_PRJNA1010969_External_Sepsis_vs_Trauma"
  )
) {
  z <- external_score_source[
    external_score_source$Analysis_Set ==
      comparison_set,
    ,
    drop = FALSE
  ]

  label <- if (
    grepl(
      "Healthy",
      comparison_set
    )
  ) {
    "C. A04 external: Sepsis vs healthy"
  } else {
    "D. A05 external: Sepsis vs trauma"
  }

  boxplot(
    z$SDI_Equal_Balance ~
      z$Comparison_Group,
    xlab = "",
    ylab = "Locked equal-weight SDI",
    main = label
  )

  stripchart(
    z$SDI_Equal_Balance ~
      z$Comparison_Group,
    vertical = TRUE,
    method = "jitter",
    add = TRUE,
    pch = 16
  )
}

par(
  old_par
)

dev.off()

# ------------------------------------------------------------
# Main Figure 4: longitudinal and organ-dysfunction support
# ------------------------------------------------------------
pdf(
  file.path(
    figure_root,
    "Figure_4_Longitudinal_and_Supportive_Analyses.pdf"
  ),
  width = 12,
  height = 9
)

old_par <- par(
  mfrow = c(
    2,
    2
  ),
  mar = c(
    7,
    5,
    4,
    2
  )
)

draw_mean_trajectory(
  t01_shannon_source,
  "A. PRJEB33360 Shannon diversity",
  "Mean Shannon ± SE"
)

draw_mean_trajectory(
  t03_shannon_source,
  "B. PRJNA691455 Shannon diversity",
  "Mean Shannon ± SE"
)

draw_mean_trajectory(
  t06_shannon_source,
  "C. Cholestasis-associated Shannon trajectories",
  "Mean Shannon ± SE",
  "Status_Resolved"
)

draw_mean_trajectory(
  t01_sdi_source,
  "D. Locked SDI trajectory in PRJEB33360",
  "Mean equal-weight SDI ± SE"
)

par(
  old_par
)

dev.off()

# ------------------------------------------------------------
# Main tables workbook
# ------------------------------------------------------------
main_workbook <- openxlsx::createWorkbook(
  creator = "Sepsis microbiome meta-analysis"
)

add_plain_sheet(
  main_workbook,
  "Table1_Cohorts",
  table1,
  title = "Table 1. Frozen cohort and analysis-set architecture",
  note = paste0(
    "Discovery, external-validation, and supportive analysis roles are kept separate. ",
    "A04/A05 were locked external sets."
  )
)

add_sectioned_sheet(
  main_workbook,
  "Table2_Discovery_Meta",
  "Table 2. Discovery and genus-level meta-analysis",
  list(
    "Alpha-diversity tests" =
      alpha_table,
    "PERMANOVA results" =
      beta_table,
    "Top harmonized genus meta-analysis results" =
      meta_table
  )
)

add_sectioned_sheet(
  main_workbook,
  "Table3_SDI",
  "Table 3. Prespecified SDI development and external validation",
  list(
    "Discrimination performance" =
      sdi_table,
    "Locked-threshold performance" =
      threshold_table,
    "Locked formula" =
      formula_lock,
    "Locked model and threshold" =
      model_lock
  )
)

add_sectioned_sheet(
  main_workbook,
  "Table4_Longitudinal",
  "Table 4. Longitudinal and supportive analyses",
  list(
    "Mixed-model overall terms" =
      longitudinal_table,
    "Myocardial-dysfunction cross-sectional results" =
      myocardial_table
  )
)

add_plain_sheet(
  main_workbook,
  "Claim_Registry",
  claim_registry,
  title = "Manuscript claim registry",
  note = paste0(
    "Permitted wording preserves the evidence hierarchy. ",
    "Prohibited wording identifies overclaims that must not appear in the manuscript."
  )
)

openxlsx::saveWorkbook(
  main_workbook,
  file.path(
    table_root,
    "21_Main_Manuscript_Tables.xlsx"
  ),
  overwrite = TRUE
)

# Export main tables as CSVs as well.
wcsv(
  table1,
  file.path(
    table_root,
    "21_Table1_Cohorts.csv"
  )
)

wcsv(
  alpha_table,
  file.path(
    table_root,
    "21_Table2A_Alpha.csv"
  )
)

wcsv(
  beta_table,
  file.path(
    table_root,
    "21_Table2B_Beta.csv"
  )
)

wcsv(
  meta_table,
  file.path(
    table_root,
    "21_Table2C_Meta.csv"
  )
)

wcsv(
  sdi_table,
  file.path(
    table_root,
    "21_Table3A_SDI_Performance.csv"
  )
)

wcsv(
  threshold_table,
  file.path(
    table_root,
    "21_Table3B_SDI_Threshold.csv"
  )
)

wcsv(
  longitudinal_table,
  file.path(
    table_root,
    "21_Table4A_Longitudinal.csv"
  )
)

wcsv(
  myocardial_table,
  file.path(
    table_root,
    "21_Table4B_Myocardial.csv"
  )
)

# ------------------------------------------------------------
# Supplementary workbook
# ------------------------------------------------------------
supplement_workbook <- openxlsx::createWorkbook(
  creator = "Sepsis microbiome meta-analysis"
)

supplement_entries <- list(
  "Analysis_Set_Summary" =
    analysis_set_summary,
  "Analysis_Set_QC" =
    analysis_set_qc,
  "Group_Descriptive" =
    group_descriptive,
  "Alpha_Tests" =
    alpha_tests,
  "Beta_PERMANOVA" =
    beta_permanova,
  "Beta_Dispersion" =
    beta_dispersion,
  "Meta_Results" =
    meta_results,
  "Candidate_Grading" =
    candidate_grading,
  "SDI_Candidate_Lock" =
    candidate_lock,
  "SDI_Formula_Lock" =
    formula_lock,
  "SDI_Model_Lock" =
    model_lock,
  "SDI_Performance" =
    sdi_performance,
  "SDI_Threshold" =
    sdi_threshold,
  "SDI_Group_Tests" =
    sdi_group_tests,
  "SDI_LOCO" =
    sdi_loco,
  "Candidate_Diagnosis" =
    crossset_diagnosis,
  "Exploratory_Scores" =
    exploratory_score_performance,
  "Leave_One_Genus_Out" =
    leave_one_genus_out,
  "Timepoint_Order_Audit" =
    timepoint_order_audit,
  "Mixed_Model_Terms" =
    mixed_terms,
  "Mixed_Model_Coefficients" =
    mixed_coefficients,
  "Paired_Changes" =
    paired_changes,
  "Myocardial_Results" =
    myocardial_results,
  "Timepoint_Group_Comparisons" =
    timepoint_group_comparisons,
  "Claim_Registry" =
    claim_registry,
  "Input_Inventory" =
    input_files,
  "Freeze_Assertions" =
    assertions
)

supplement_sheet_names <- unique_sheet_names(
  names(
    supplement_entries
  )
)

for (
  i in seq_along(
    supplement_entries
  )
) {
  add_plain_sheet(
    supplement_workbook,
    supplement_sheet_names[i],
    supplement_entries[[i]],
    title = names(
      supplement_entries
    )[i]
  )
}

openxlsx::saveWorkbook(
  supplement_workbook,
  file.path(
    table_root,
    "21_Supplementary_Tables.xlsx"
  ),
  overwrite = TRUE
)

# ------------------------------------------------------------
# Final file map and freeze manifest
# ------------------------------------------------------------
file_map <- data.frame(
  Artifact = c(
    "Main manuscript tables",
    "Supplementary tables",
    "Figure 1",
    "Figure 2",
    "Figure 3",
    "Figure 4",
    "Figure source data folder",
    "Claim registry",
    "Results number summary",
    "Frozen input folder",
    "Audit and manifest folder"
  ),
  Path = c(
    file.path(
      table_root,
      "21_Main_Manuscript_Tables.xlsx"
    ),
    file.path(
      table_root,
      "21_Supplementary_Tables.xlsx"
    ),
    file.path(
      figure_root,
      "Figure_1_Study_Design_and_Analysis_Architecture.pdf"
    ),
    file.path(
      figure_root,
      "Figure_2_Discovery_and_Meta_Analysis.pdf"
    ),
    file.path(
      figure_root,
      "Figure_3_SDI_Development_and_External_Failure.pdf"
    ),
    file.path(
      figure_root,
      "Figure_4_Longitudinal_and_Supportive_Analyses.pdf"
    ),
    source_root,
    file.path(
      text_root,
      "21_Manuscript_Claim_Registry.csv"
    ),
    file.path(
      text_root,
      "21_Results_Number_Summary.csv"
    ),
    frozen_input_root,
    audit_root
  ),
  Intended_Use = c(
    "Main manuscript tables; edit journal formatting only",
    "Supplementary submission workbook",
    "Study design and analysis architecture",
    "Discovery and cross-cohort meta-analysis",
    "SDI development, failed external validation, and diagnosis",
    "Longitudinal and supportive results",
    "Reproducible source data for all main figures",
    "Evidence-level and wording control",
    "Numbers ready for Methods/Results writing",
    "Immutable copies of selected Day 7-12 outputs",
    "Input inventory, assertions, hashes, and session information"
  ),
  stringsAsFactors = FALSE
)

wcsv(
  file_map,
  file.path(
    freeze_root,
    "21_Final_Artifact_Map.csv"
  )
)

manifest_files <- list.files(
  freeze_root,
  recursive = TRUE,
  full.names = TRUE
)

manifest_files <- manifest_files[
  file.info(
    manifest_files
  )$isdir ==
    FALSE
]

manifest <- data.frame(
  File_Path =
    manifest_files,
  Relative_Path =
    sub(
      paste0(
        "^",
        gsub(
          "\\\\",
          "/",
          normalizePath(
            freeze_root,
            winslash = "/",
            mustWork = FALSE
          )
        ),
        "/?"
      ),
      "",
      gsub(
        "\\\\",
        "/",
        normalizePath(
          manifest_files,
          winslash = "/",
          mustWork = FALSE
        )
      )
    ),
  File_Size_Bytes =
    file.info(
      manifest_files
    )$size,
  Modified_Time =
    format(
      file.info(
        manifest_files
      )$mtime,
      "%Y-%m-%d %H:%M:%S"
    ),
  MD5 =
    unname(
      tools::md5sum(
        manifest_files
      )
    ),
  stringsAsFactors = FALSE
)

wcsv(
  manifest,
  file.path(
    audit_root,
    "21_Final_Freeze_MD5_Manifest.csv"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    audit_root,
    "21_Final_Freeze_SessionInfo.txt"
  )
)

writeLines(
  manifest_files,
  file.path(
    audit_root,
    "21_Files_To_SHA256.txt"
  ),
  useBytes = TRUE
)

readme_lines <- c(
  "DAY 13 FINAL RESULTS FREEZE",
  "",
  paste0(
    "Freeze timestamp: ",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  ),
  "",
  "Primary interpretation rules:",
  "1. ICU-control and healthy-control meta-analyses remain separate.",
  "2. Day 10 prespecified SDI external validation remains FAILED.",
  "3. Day 11 alternative scores remain POST-HOC exploratory diagnostics.",
  "4. Day 12 SDI and candidate-genus trajectories are supportive exploratory results.",
  "5. PRJEB33360 longitudinal alpha-diversity decline is the strongest supportive longitudinal result.",
  "6. S02 is a depth-sensitivity analysis, not an independent cohort.",
  "",
  "Generated artifacts:",
  "01_tables/21_Main_Manuscript_Tables.xlsx",
  "01_tables/21_Supplementary_Tables.xlsx",
  "02_figures/Figure_1_Study_Design_and_Analysis_Architecture.pdf",
  "02_figures/Figure_2_Discovery_and_Meta_Analysis.pdf",
  "02_figures/Figure_3_SDI_Development_and_External_Failure.pdf",
  "02_figures/Figure_4_Longitudinal_and_Supportive_Analyses.pdf",
  "03_figure_source_data/",
  "04_audit_and_manifest/",
  "05_text_and_claim_registry/",
  "06_frozen_result_inputs/",
  "",
  "After this freeze, numerical analysis should not be changed without creating a new version."
)

writeLines(
  readme_lines,
  file.path(
    freeze_root,
    "README_21_FINAL_RESULTS_FREEZE.txt"
  ),
  useBytes = TRUE
)

cat(
  "\n============================================================\n"
)
cat("Day 13 final freeze completed.\n")
cat(
  "Freeze root: ",
  freeze_root,
  "\n",
  sep = ""
)
cat("Main outputs:\n")
cat("  01_tables/21_Main_Manuscript_Tables.xlsx\n")
cat("  01_tables/21_Supplementary_Tables.xlsx\n")
cat("  02_figures/Figure_1_Study_Design_and_Analysis_Architecture.pdf\n")
cat("  02_figures/Figure_2_Discovery_and_Meta_Analysis.pdf\n")
cat("  02_figures/Figure_3_SDI_Development_and_External_Failure.pdf\n")
cat("  02_figures/Figure_4_Longitudinal_and_Supportive_Analyses.pdf\n")
cat("  05_text_and_claim_registry/21_Manuscript_Claim_Registry.csv\n")
cat("  05_text_and_claim_registry/21_Results_Number_Summary.csv\n")
cat("  04_audit_and_manifest/21_Beta_PERMANOVA_FDR_Field_Map.csv\n")
cat("All freeze assertions passed.\n")
cat(
  "Finished: ",
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n",
  sep = ""
)
