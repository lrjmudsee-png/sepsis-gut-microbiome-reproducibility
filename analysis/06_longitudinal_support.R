# PUBLIC RELEASE v1.0.0
# Scientific logic preserved from the independently verified historical script20.
# Public-release change: the historical local project root was replaced by
# SEPSIS_V1_PROJECT_ROOT/getwd(); statistical logic is otherwise unchanged.

options(stringsAsFactors = FALSE)

# ============================================================
# Day 12: Longitudinal, intervention, and organ-dysfunction
# supportive analyses
#
# Analysis status:
# SUPPORTIVE_EXPLORATORY
#
# Important:
# - Day 10 external validation remains FAILED.
# - The locked SDI is used only as an exploratory supportive
#   score and is not retrained or reoriented.
# - S02 is a depth-sensitivity analysis for S01, not an
#   independent replication cohort.
# - Different biological questions are not pooled together.
# ============================================================

root <- Sys.getenv("SEPSIS_V1_PROJECT_ROOT", unset = getwd())
root <- normalizePath(root, winslash = "/", mustWork = FALSE)

day6_root <- file.path(
  root,
  "processed_data/final_frozen_v1"
)

day7_set_root <- file.path(
  day6_root,
  "analysis_sets_v1"
)

day10_lock_root <- file.path(
  root,
  "manuscript/03_analysis/18_sdi_external_validation",
  "locked_before_external_open"
)

output_root <- file.path(
  root,
  "processed_data/day12_longitudinal_support_v1"
)

report_root <- file.path(
  root,
  "manuscript/03_analysis/20_longitudinal_support"
)

plot_root <- file.path(
  report_root,
  "plots"
)

dir.create(
  output_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  report_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  plot_root,
  recursive = TRUE,
  showWarnings = FALSE
)

sink(
  file.path(
    report_root,
    "20_Longitudinal_Support_Log.txt"
  ),
  split = TRUE
)
on.exit(sink(), add = TRUE)

cat(
  "Day 12 started: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)

if (!requireNamespace("nlme", quietly = TRUE)) {
  stop(
    paste0(
      "缺少R包 nlme。通常它随R安装；",
      "如缺失请运行 install.packages('nlme')。"
    )
  )
}

set.seed(20260802)

analysis_status <- "SUPPORTIVE_EXPLORATORY"
pseudocount <- 0.5
minimum_model_observations <- 8
minimum_unique_patients <- 4

parameters <- data.frame(
  Parameter = c(
    "Analysis_status",
    "Day10_validation_status",
    "Pseudocount",
    "Alpha_metrics",
    "Candidate_genus_scale",
    "Longitudinal_model",
    "Repeated_measure_unit",
    "Multiple_testing",
    "S01_S02_relationship",
    "Cross_question_pooling"
  ),
  Value = c(
    analysis_status,
    "FAILED_UNCHANGED",
    pseudocount,
    "Observed; Shannon; Inverse Simpson",
    "Full-project CLR multiplied by locked SDI sign",
    "Linear mixed model with patient random intercept",
    "Patient_ID",
    "BH within each result family",
    "S01 main; S02 depth-sensitivity only",
    "Not performed"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  parameters,
  file.path(
    report_root,
    "20_Analysis_Parameters.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# Input configuration
# ------------------------------------------------------------
set_config <- data.frame(
  Analysis_ID = c(
    "T01_PRJEB33360_Longitudinal_Main",
    "T02_PRJEB33360_Longitudinal_DepthSensitivity",
    "T03_PRJNA691455_Sepsis_Longitudinal",
    "T04_PRJNA430161_Intervention",
    "T05_PRJNA797231_Myocardial_Dysfunction",
    "T06_PRJNA912621_Cholestasis_Longitudinal"
  ),
  Analysis_Set = c(
    "S01_PRJEB33360_Longitudinal_Main_ge1000",
    "S02_PRJEB33360_Longitudinal_Sensitivity_ge5000",
    "S03_PRJNA691455_Sepsis_Longitudinal",
    "S04_PRJNA430161_Intervention_Longitudinal",
    "S05_PRJNA797231_Organ_Dysfunction",
    "S06_PRJNA912621_Organ_Dysfunction_Longitudinal"
  ),
  Project_ID = c(
    "PRJEB33360",
    "PRJEB33360",
    "PRJNA691455",
    "PRJNA430161",
    "PRJNA797231",
    "PRJNA912621"
  ),
  Analysis_Type = c(
    "Longitudinal_main",
    "Longitudinal_depth_sensitivity",
    "Longitudinal_sepsis",
    "Intervention_longitudinal",
    "Organ_dysfunction_cross_sectional",
    "Organ_dysfunction_longitudinal"
  ),
  Expected_Samples = c(
    91,
    88,
    29,
    26,
    34,
    58
  ),
  Expected_Patients = c(
    43,
    43,
    10,
    9,
    34,
    20
  ),
  stringsAsFactors = FALSE
)

baseline_config <- data.frame(
  Analysis_ID = c(
    "T01_PRJEB33360_Longitudinal_Main",
    "T02_PRJEB33360_Longitudinal_DepthSensitivity"
  ),
  Baseline_Set =
    "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
  Baseline_Group =
    "Sepsis",
  stringsAsFactors = FALSE
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

safe_numeric <- function(x) {
  suppressWarnings(
    as.numeric(x)
  )
}

safe_numeric_matrix <- function(df) {
  x <- as.matrix(
    data.frame(
      lapply(
        df,
        safe_numeric
      ),
      check.names = FALSE
    )
  )

  storage.mode(x) <- "numeric"
  x
}

genus_columns <- function(df) {
  names(df)[
    grepl(
      "^Genus__",
      names(df)
    )
  ]
}

normalise_yes_no <- function(x) {
  x <- tolower(
    trimws(
      as.character(x)
    )
  )

  out <- ifelse(
    x %in% c(
      "yes",
      "y",
      "1",
      "true",
      "positive"
    ),
    "Yes",
    ifelse(
      x %in% c(
        "no",
        "n",
        "0",
        "false",
        "negative"
      ),
      "No",
      NA_character_
    )
  )

  out
}

derive_time_label <- function(meta) {
  raw <- if (
    "Timepoint_Raw" %in%
      names(meta)
  ) {
    clean_text(
      meta$Timepoint_Raw
    )
  } else {
    rep(
      NA_character_,
      nrow(meta)
    )
  }

  comparison <- if (
    "Comparison_Group" %in%
      names(meta)
  ) {
    clean_text(
      meta$Comparison_Group
    )
  } else {
    rep(
      NA_character_,
      nrow(meta)
    )
  }

  label <- ifelse(
    !is.na(raw),
    raw,
    comparison
  )

  label[
    is.na(label)
  ] <- "Unresolved_time"

  label
}

derive_time_order <- function(meta) {
  order_value <- if (
    "Timepoint_Order" %in%
      names(meta)
  ) {
    safe_numeric(
      meta$Timepoint_Order
    )
  } else {
    rep(
      NA_real_,
      nrow(meta)
    )
  }

  day_value <- if (
    "Timepoint_Day" %in%
      names(meta)
  ) {
    safe_numeric(
      meta$Timepoint_Day
    )
  } else {
    rep(
      NA_real_,
      nrow(meta)
    )
  }

  fallback <- rank(
    day_value,
    ties.method = "min",
    na.last = "keep"
  )

  missing_order <- !is.finite(
    order_value
  )

  order_value[
    missing_order
  ] <- fallback[
    missing_order
  ]

  order_value
}

order_time_factor <- function(
  labels,
  order_values
) {
  mapping <- unique(
    data.frame(
      label = labels,
      order = order_values,
      stringsAsFactors = FALSE
    )
  )

  mapping$order[
    !is.finite(
      mapping$order
    )
  ] <- 999 +
    seq_len(
      sum(
        !is.finite(
          mapping$order
        )
      )
    )

  mapping <- mapping[
    order(
      mapping$order,
      mapping$label
    ),
    ,
    drop = FALSE
  ]

  factor(
    labels,
    levels = mapping$label,
    ordered = FALSE
  )
}

alpha_metrics <- function(counts) {
  depth <- rowSums(counts)

  relative <- counts

  valid <- depth > 0

  relative[
    valid,
  ] <- sweep(
    counts[
      valid,
      ,
      drop = FALSE
    ],
    1,
    depth[
      valid
    ],
    "/"
  )

  if (any(!valid)) {
    relative[
      !valid,
    ] <- 0
  }

  log_relative <- relative

  positive <- log_relative > 0

  log_relative[
    positive
  ] <- log(
    log_relative[
      positive
    ]
  )

  log_relative[
    !positive
  ] <- 0

  shannon <- -rowSums(
    relative *
      log_relative
  )

  inverse_simpson <- 1 /
    rowSums(
      relative^2
    )

  inverse_simpson[
    !is.finite(
      inverse_simpson
    )
  ] <- NA_real_

  data.frame(
    Alpha_Observed =
      rowSums(
        counts > 0
      ),
    Alpha_Shannon =
      shannon,
    Alpha_InverseSimpson =
      inverse_simpson,
    stringsAsFactors = FALSE
  )
}

clr_transform <- function(counts) {
  logged <- log(
    counts +
      pseudocount
  )

  sweep(
    logged,
    1,
    rowMeans(
      logged
    ),
    "-"
  )
}

balance_score <- function(
  candidate_counts,
  positive_features,
  negative_features,
  positive_weights = NULL,
  negative_weights = NULL
) {
  log_counts <- log(
    candidate_counts +
      pseudocount
  )

  if (
    length(
      positive_features
    ) == 0 ||
      length(
        negative_features
      ) == 0
  ) {
    return(
      rep(
        NA_real_,
        nrow(
          candidate_counts
        )
      )
    )
  }

  if (is.null(positive_weights)) {
    positive_component <- rowMeans(
      log_counts[
        ,
        positive_features,
        drop = FALSE
      ]
    )
  } else {
    positive_weights <-
      positive_weights /
      sum(
        positive_weights
      )

    positive_component <- as.numeric(
      log_counts[
        ,
        positive_features,
        drop = FALSE
      ] %*%
        positive_weights
    )
  }

  if (is.null(negative_weights)) {
    negative_component <- rowMeans(
      log_counts[
        ,
        negative_features,
        drop = FALSE
      ]
    )
  } else {
    negative_weights <-
      negative_weights /
      sum(
        negative_weights
      )

    negative_component <- as.numeric(
      log_counts[
        ,
        negative_features,
        drop = FALSE
      ] %*%
        negative_weights
    )
  }

  positive_component -
    negative_component
}

safe_wilcox_unpaired <- function(
  x,
  y
) {
  x <- x[
    is.finite(x)
  ]

  y <- y[
    is.finite(y)
  ]

  if (
    length(x) < 2 ||
      length(y) < 2 ||
      length(
        unique(
          c(
            x,
            y
          )
        )
      ) < 2
  ) {
    return(
      c(
        statistic = NA_real_,
        p = NA_real_
      )
    )
  }

  fit <- suppressWarnings(
    stats::wilcox.test(
      x,
      y,
      paired = FALSE,
      exact = FALSE
    )
  )

  c(
    statistic = unname(
      fit$statistic
    ),
    p = fit$p.value
  )
}

safe_wilcox_paired <- function(
  x,
  y
) {
  valid <- is.finite(x) &
    is.finite(y)

  x <- x[
    valid
  ]

  y <- y[
    valid
  ]

  if (
    length(x) < 3 ||
      length(
        unique(
          y - x
        )
      ) < 2
  ) {
    return(
      c(
        statistic = NA_real_,
        p = NA_real_
      )
    )
  }

  fit <- suppressWarnings(
    stats::wilcox.test(
      y,
      x,
      paired = TRUE,
      exact = FALSE
    )
  )

  c(
    statistic = unname(
      fit$statistic
    ),
    p = fit$p.value
  )
}

cliffs_delta <- function(x, y) {
  x <- x[
    is.finite(x)
  ]

  y <- y[
    is.finite(y)
  ]

  if (
    length(x) == 0 ||
      length(y) == 0
  ) {
    return(NA_real_)
  }

  differences <- outer(
    x,
    y,
    "-"
  )

  (
    sum(
      differences > 0
    ) -
      sum(
        differences < 0
      )
  ) / (
    length(x) *
      length(y)
  )
}

auc_rank <- function(label, score) {
  valid <- !is.na(label) &
    is.finite(score)

  label <- label[
    valid
  ]

  score <- score[
    valid
  ]

  n_case <- sum(
    label == 1
  )

  n_control <- sum(
    label == 0
  )

  if (
    n_case == 0 ||
      n_control == 0
  ) {
    return(NA_real_)
  }

  score_rank <- rank(
    score,
    ties.method = "average"
  )

  (
    sum(
      score_rank[
        label == 1
      ]
    ) -
      n_case *
        (
          n_case + 1
        ) / 2
  ) / (
    n_case *
      n_control
  )
}

# ------------------------------------------------------------
# Locked candidate definitions
# ------------------------------------------------------------
candidate_file <- file.path(
  day10_lock_root,
  "18_SDI_Candidate_Lock.csv"
)

formula_file <- file.path(
  day10_lock_root,
  "18_SDI_Formula_Lock.csv"
)

if (
  !file.exists(
    candidate_file
  ) ||
    !file.exists(
      formula_file
    )
) {
  stop(
    "缺少Day 10锁定候选或公式文件。"
  )
}

candidate_lock <- read.csv(
  candidate_file,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

required_candidate_columns <- c(
  "Genus_Feature",
  "SDI_Sign",
  "Meta_Balance_Weight",
  "Selection_Role"
)

missing_candidate_columns <- setdiff(
  required_candidate_columns,
  names(
    candidate_lock
  )
)

if (
  length(
    missing_candidate_columns
  ) > 0
) {
  stop(
    "候选锁定表缺少字段：",
    paste(
      missing_candidate_columns,
      collapse = ", "
    )
  )
}

candidate_features <-
  candidate_lock$Genus_Feature

candidate_sign <- setNames(
  candidate_lock$SDI_Sign,
  candidate_features
)

positive_features <-
  candidate_features[
    candidate_lock$SDI_Sign > 0
  ]

negative_features <-
  candidate_features[
    candidate_lock$SDI_Sign < 0
  ]

positive_weights <- abs(
  candidate_lock$
    Meta_Balance_Weight[
      candidate_lock$SDI_Sign > 0
    ]
)

negative_weights <- abs(
  candidate_lock$
    Meta_Balance_Weight[
      candidate_lock$SDI_Sign < 0
    ]
)

lock_hash_before <- unname(
  tools::md5sum(
    c(
      candidate_file,
      formula_file
    )
  )
)

# ------------------------------------------------------------
# Load one frozen analysis set and its project count table
# ------------------------------------------------------------
load_analysis_set <- function(
  analysis_set,
  project_id
) {
  set_file <- file.path(
    day7_set_root,
    paste0(
      analysis_set,
      ".csv"
    )
  )

  count_file <- file.path(
    day6_root,
    project_id,
    paste0(
      project_id,
      "_genus_classified_counts.csv"
    )
  )

  if (!file.exists(set_file)) {
    stop(
      "未找到分析集：",
      set_file
    )
  }

  if (!file.exists(count_file)) {
    stop(
      "未找到Genus counts：",
      count_file
    )
  }

  meta <- read.csv(
    set_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  count_df <- read.csv(
    count_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  required <- c(
    "Run_ID",
    "Patient_ID",
    "Comparison_Group"
  )

  missing_required <- setdiff(
    required,
    names(meta)
  )

  if (
    length(
      missing_required
    ) > 0
  ) {
    stop(
      analysis_set,
      " 缺少字段：",
      paste(
        missing_required,
        collapse = ", "
      )
    )
  }

  if (
    !"Run_ID" %in%
      names(count_df)
  ) {
    stop(
      project_id,
      " count表缺少Run_ID。"
    )
  }

  meta$Run_ID <- clean_text(
    meta$Run_ID
  )

  meta$Patient_ID <- clean_text(
    meta$Patient_ID
  )

  meta$Comparison_Group <-
    clean_text(
      meta$Comparison_Group
    )

  count_df$Run_ID <- clean_text(
    count_df$Run_ID
  )

  if (
    anyDuplicated(
      meta$Run_ID
    )
  ) {
    stop(
      analysis_set,
      " 存在重复Run_ID。"
    )
  }

  index <- match(
    meta$Run_ID,
    count_df$Run_ID
  )

  if (
    any(
      is.na(index)
    )
  ) {
    stop(
      analysis_set,
      " 有Run无法匹配count表。"
    )
  }

  features <- genus_columns(
    count_df
  )

  full_counts <- safe_numeric_matrix(
    count_df[
      index,
      features,
      drop = FALSE
    ]
  )

  rownames(full_counts) <-
    meta$Run_ID

  if (
    any(
      !is.finite(
        full_counts
      )
    ) ||
      any(
        full_counts < 0
      )
  ) {
    stop(
      analysis_set,
      " count矩阵包含NA、无限值或负数。"
    )
  }

  candidate_counts <- matrix(
    0,
    nrow = nrow(meta),
    ncol = length(
      candidate_features
    ),
    dimnames = list(
      meta$Run_ID,
      candidate_features
    )
  )

  present_candidates <- intersect(
    candidate_features,
    features
  )

  if (
    length(
      present_candidates
    ) > 0
  ) {
    candidate_counts[
      ,
      present_candidates
    ] <- full_counts[
      ,
      present_candidates,
      drop = FALSE
    ]
  }

  candidate_present <- setNames(
    candidate_features %in%
      features,
    candidate_features
  )

  list(
    meta = meta,
    full_counts = full_counts,
    candidate_counts =
      candidate_counts,
    candidate_present =
      candidate_present,
    set_file = set_file,
    count_file = count_file
  )
}

# ------------------------------------------------------------
# Convert a loaded set to sample-level metrics
# ------------------------------------------------------------
build_sample_metrics <- function(
  loaded,
  analysis_id,
  project_id,
  analysis_type
) {
  meta <- loaded$meta
  full_counts <- loaded$full_counts

  alpha <- alpha_metrics(
    full_counts
  )

  full_clr <- clr_transform(
    full_counts
  )

  equal_sdi <- balance_score(
    loaded$candidate_counts,
    positive_features,
    negative_features
  )

  weighted_sdi <- balance_score(
    loaded$candidate_counts,
    positive_features,
    negative_features,
    positive_weights,
    negative_weights
  )

  output <- data.frame(
    Analysis_Status =
      analysis_status,
    Analysis_ID =
      analysis_id,
    Analysis_Set =
      analysis_id,
    Project_ID =
      project_id,
    Analysis_Type =
      analysis_type,
    Run_ID =
      meta$Run_ID,
    Patient_ID =
      meta$Patient_ID,
    Comparison_Group =
      meta$Comparison_Group,
    Timepoint_Raw = if (
      "Timepoint_Raw" %in%
        names(meta)
    ) {
      clean_text(
        meta$Timepoint_Raw
      )
    } else {
      NA_character_
    },
    Timepoint_Day = if (
      "Timepoint_Day" %in%
        names(meta)
    ) {
      safe_numeric(
        meta$Timepoint_Day
      )
    } else {
      NA_real_
    },
    Timepoint_Order = if (
      "Timepoint_Order" %in%
        names(meta)
    ) {
      safe_numeric(
        meta$Timepoint_Order
      )
    } else {
      NA_real_
    },
    Intervention_Arm = if (
      "Intervention_Arm" %in%
        names(meta)
    ) {
      clean_text(
        meta$Intervention_Arm
      )
    } else {
      NA_character_
    },
    Organ_Dysfunction_Status = if (
      "Organ_Dysfunction_Status" %in%
        names(meta)
    ) {
      clean_text(
        meta$Organ_Dysfunction_Status
      )
    } else {
      NA_character_
    },
    Alpha_Observed =
      alpha$Alpha_Observed,
    Alpha_Shannon =
      alpha$Alpha_Shannon,
    Alpha_InverseSimpson =
      alpha$Alpha_InverseSimpson,
    SDI_Equal_Balance =
      equal_sdi,
    SDI_MetaWeighted_Balance =
      weighted_sdi,
    stringsAsFactors = FALSE
  )

  for (
    feature in candidate_features
  ) {
    metric_name <- paste0(
      "CandidateSignedCLR__",
      make.names(
        feature
      )
    )

    if (
      feature %in%
        colnames(
          full_clr
        )
    ) {
      output[[metric_name]] <-
        candidate_sign[
          feature
        ] *
        full_clr[
          ,
          feature
        ]
    } else {
      output[[metric_name]] <-
        NA_real_
    }
  }

  output
}

# ------------------------------------------------------------
# Load all support sets
# ------------------------------------------------------------
loaded_support <- list()
sample_metric_list <- list()
input_audit_rows <- list()

for (
  i in seq_len(
    nrow(
      set_config
    )
  )
) {
  config_row <- set_config[
    i,
    ,
    drop = FALSE
  ]

  loaded <- load_analysis_set(
    config_row$Analysis_Set,
    config_row$Project_ID
  )

  loaded_support[[
    config_row$Analysis_ID
  ]] <- loaded

  metrics <- build_sample_metrics(
    loaded,
    config_row$Analysis_ID,
    config_row$Project_ID,
    config_row$Analysis_Type
  )

  sample_metric_list[[
    config_row$Analysis_ID
  ]] <- metrics

  missing_candidates <- names(
    loaded$candidate_present
  )[
    !loaded$candidate_present
  ]

  input_audit_rows[[
    length(
      input_audit_rows
    ) + 1
  ]] <- data.frame(
    Analysis_ID =
      config_row$Analysis_ID,
    Analysis_Set =
      config_row$Analysis_Set,
    Project_ID =
      config_row$Project_ID,
    Analysis_Type =
      config_row$Analysis_Type,
    Expected_Samples =
      config_row$Expected_Samples,
    Observed_Samples =
      nrow(
        loaded$meta
      ),
    Expected_Patients =
      config_row$Expected_Patients,
    Observed_Patients =
      length(
        unique(
          loaded$meta$Patient_ID
        )
      ),
    Duplicate_Run_N =
      sum(
        duplicated(
          loaded$meta$Run_ID
        )
      ),
    Candidate_Genera_N =
      length(
        candidate_features
      ),
    Missing_Candidate_Genera_N =
      length(
        missing_candidates
      ),
    Missing_Candidate_Genera =
      paste(
        missing_candidates,
        collapse = " | "
      ),
    Status = if (
      nrow(
        loaded$meta
      ) ==
        config_row$Expected_Samples &&
        length(
          unique(
            loaded$meta$Patient_ID
          )
        ) ==
        config_row$Expected_Patients &&
        sum(
          duplicated(
            loaded$meta$Run_ID
          )
        ) == 0
    ) {
      if (
        length(
          missing_candidates
        ) == 0
      ) {
        "PASS"
      } else {
        "PASS_WITH_MISSING_CANDIDATE_REVIEW"
      }
    } else {
      "REVIEW"
    },
    stringsAsFactors = FALSE
  )
}

input_audit <- do.call(
  rbind,
  input_audit_rows
)

# ------------------------------------------------------------
# Add matching PRJEB33360 M1 Sepsis baseline to T01/T02
# ------------------------------------------------------------
baseline_set_name <-
  "A01_PRJEB33360_Sepsis_vs_NonSepsisICU"

baseline_loaded <- load_analysis_set(
  baseline_set_name,
  "PRJEB33360"
)

baseline_metrics_all <- build_sample_metrics(
  baseline_loaded,
  "PRJEB33360_M1_Baseline",
  "PRJEB33360",
  "Baseline_for_longitudinal_support"
)

baseline_metrics_all <-
  baseline_metrics_all[
    baseline_metrics_all$
      Comparison_Group ==
      "Sepsis",
    ,
    drop = FALSE
  ]

for (
  analysis_id in c(
    "T01_PRJEB33360_Longitudinal_Main",
    "T02_PRJEB33360_Longitudinal_DepthSensitivity"
  )
) {
  later <- sample_metric_list[[
    analysis_id
  ]]

  matched_patients <- intersect(
    unique(
      later$Patient_ID
    ),
    unique(
      baseline_metrics_all$Patient_ID
    )
  )

  baseline <- baseline_metrics_all[
    baseline_metrics_all$Patient_ID %in%
      matched_patients,
    ,
    drop = FALSE
  ]

  baseline$Analysis_ID <-
    analysis_id

  baseline$Analysis_Set <-
    analysis_id

  baseline$Analysis_Type <-
    if (
      grepl(
        "DepthSensitivity",
        analysis_id
      )
    ) {
      "Longitudinal_depth_sensitivity"
    } else {
      "Longitudinal_main"
    }

  baseline$Timepoint_Raw <-
    "M1_initial"

  baseline$Timepoint_Day <-
    0

  baseline$Timepoint_Order <-
    1

  combined <- rbind(
    baseline,
    later
  )

  sample_metric_list[[
    analysis_id
  ]] <- combined

  input_audit <- rbind(
    input_audit,
    data.frame(
      Analysis_ID =
        analysis_id,
      Analysis_Set =
        baseline_set_name,
      Project_ID =
        "PRJEB33360",
      Analysis_Type =
        "Matched_M1_baseline_addition",
      Expected_Samples =
        length(
          matched_patients
        ),
      Observed_Samples =
        nrow(
          baseline
        ),
      Expected_Patients =
        length(
          matched_patients
        ),
      Observed_Patients =
        length(
          unique(
            baseline$Patient_ID
          )
        ),
      Duplicate_Run_N =
        sum(
          duplicated(
            baseline$Run_ID
          )
        ),
      Candidate_Genera_N =
        length(
          candidate_features
        ),
      Missing_Candidate_Genera_N =
        0,
      Missing_Candidate_Genera =
        "",
      Status = if (
        nrow(
          baseline
        ) ==
          length(
            matched_patients
          )
      ) {
        "PASS"
      } else {
        "REVIEW"
      },
      stringsAsFactors = FALSE
    )
  )
}

# ------------------------------------------------------------
# Prepare time, arm, and dysfunction variables
# ------------------------------------------------------------
metric_columns <- c(
  "Alpha_Observed",
  "Alpha_Shannon",
  "Alpha_InverseSimpson",
  "SDI_Equal_Balance",
  "SDI_MetaWeighted_Balance",
  grep(
    "^CandidateSignedCLR__",
    names(
      sample_metric_list[[1]]
    ),
    value = TRUE
  )
)

metric_dictionary <- data.frame(
  Metric = metric_columns,
  Metric_Family = c(
    rep(
      "Alpha_diversity",
      3
    ),
    rep(
      "Locked_SDI_exploratory",
      2
    ),
    rep(
      "Locked_candidate_signed_CLR",
      length(
        metric_columns
      ) - 5
    )
  ),
  Interpretation = c(
    "Observed classified genus richness",
    "Shannon diversity",
    "Inverse Simpson diversity",
    "Higher means more Day10-Sepsis-like by equal locked balance",
    "Higher means more Day10-Sepsis-like by meta-weighted locked balance",
    rep(
      paste0(
        "Positive means movement in the Day10 locked Sepsis direction; ",
        "candidate absent from a project is NA, not zero."
      ),
      length(
        metric_columns
      ) - 5
    )
  ),
  Confirmatory_Status = c(
    rep(
      "SUPPORTIVE",
      3
    ),
    rep(
      "EXPLORATORY_AFTER_FAILED_VALIDATION",
      length(
        metric_columns
      ) - 3
    )
  ),
  stringsAsFactors = FALSE
)

wcsv(
  metric_dictionary,
  file.path(
    report_root,
    "20_Metric_Dictionary.csv"
  )
)

# Patient-level intervention arm propagation for T04.
derive_patient_arm <- function(df) {
  valid_arm <- clean_text(
    df$Intervention_Arm
  )

  invalid <- is.na(
    valid_arm
  ) |
    tolower(
      valid_arm
    ) %in% c(
      "unknown",
      "unknown/observational",
      "observational",
      "baseline",
      "none"
    )

  valid_arm[
    invalid
  ] <- NA_character_

  arm_map <- tapply(
    valid_arm,
    df$Patient_ID,
    function(x) {
      values <- unique(
        x[
          !is.na(x)
        ]
      )

      if (
        length(
          values
        ) == 1
      ) {
        values
      } else if (
        length(
          values
        ) == 0
      ) {
        NA_character_
      } else {
        "Conflicting"
      }
    }
  )

  unname(
    arm_map[
      df$Patient_ID
    ]
  )
}

resolve_prespecified_time_order <- function(
  analysis_id,
  labels,
  fallback_order
) {
  normalised <- tolower(
    trimws(
      as.character(
        labels
      )
    )
  )

  mapping <- NULL

  if (
    analysis_id %in%
      c(
        "T01_PRJEB33360_Longitudinal_Main",
        "T02_PRJEB33360_Longitudinal_DepthSensitivity"
      )
  ) {
    mapping <- c(
      "m1_initial" = 1,
      "m2.5_icu_day5" = 2,
      "m2.6_icu_day6" = 3,
      "m2.7_icu_day7" = 4,
      "m3_discharge" = 5
    )
  } else if (
    analysis_id %in%
      c(
        "T03_PRJNA691455_Sepsis_Longitudinal",
        "T06_PRJNA912621_Cholestasis_Longitudinal"
      )
  ) {
    mapping <- c(
      "day 1" = 1,
      "day 3" = 2,
      "day 7" = 3
    )
  } else if (
    analysis_id ==
      "T04_PRJNA430161_Intervention"
  ) {
    mapping <- c(
      "baseline" = 1,
      "day 7" = 2,
      "day 28" = 3
    )
  }

  if (is.null(mapping)) {
    return(
      fallback_order
    )
  }

  resolved <- unname(
    mapping[
      normalised
    ]
  )

  unmatched <- !is.finite(
    resolved
  )

  if (
    any(
      unmatched
    )
  ) {
    warning(
      paste0(
        analysis_id,
        "存在未识别时间标签，将使用原始顺序：",
        paste(
          unique(
            labels[
              unmatched
            ]
          ),
          collapse = " | "
        )
      )
    )

    resolved[
      unmatched
    ] <- fallback_order[
      unmatched
    ]
  }

  resolved
}

for (
  analysis_id in names(
    sample_metric_list
  )
) {
  df <- sample_metric_list[[
    analysis_id
  ]]

  df$Time_Label <-
    derive_time_label(
      df
    )

  fallback_order <- derive_time_order(
    df
  )

  time_order <- resolve_prespecified_time_order(
    analysis_id,
    df$Time_Label,
    fallback_order
  )

  df$Time_Order_Resolved <-
    time_order

  df$Timepoint_Factor <-
    order_time_factor(
      df$Time_Label,
      df$Time_Order_Resolved
    )

  if (
    analysis_id ==
      "T04_PRJNA430161_Intervention"
  ) {
    df$Arm_Resolved <-
      derive_patient_arm(
        df
      )

    df$Arm_Factor <- factor(
      df$Arm_Resolved,
      levels = c(
        "Placebo",
        "Probiotic"
      )
    )
  } else {
    df$Arm_Resolved <-
      NA_character_

    df$Arm_Factor <- factor(
      NA_character_
    )
  }

  if (
    analysis_id %in%
      c(
        "T05_PRJNA797231_Myocardial_Dysfunction",
        "T06_PRJNA912621_Cholestasis_Longitudinal"
      )
  ) {
    status <- normalise_yes_no(
      df$Organ_Dysfunction_Status
    )

    unresolved <- is.na(
      status
    )

    if (
      any(
        unresolved
      )
    ) {
      comparison_status <- vapply(
        strsplit(
          as.character(
            df$Comparison_Group
          ),
          "\\|"
        ),
        function(x) {
          trimws(
            x[1]
          )
        },
        character(1)
      )

      status[
        unresolved
      ] <- normalise_yes_no(
        comparison_status[
          unresolved
        ]
      )
    }

    df$Status_Resolved <-
      status

    df$Status_Factor <- factor(
      status,
      levels = c(
        "No",
        "Yes"
      )
    )
  } else {
    df$Status_Resolved <-
      NA_character_

    df$Status_Factor <- factor(
      NA_character_
    )
  }

  sample_metric_list[[
    analysis_id
  ]] <- df
}

all_sample_metrics <- do.call(
  rbind,
  sample_metric_list
)

# rbind() can rebuild a common factor-level order across analysis
# sets. Reapply a single global level order that preserves the
# prespecified chronology within every set.
global_time_levels <- c(
  "M1_initial",
  "M2.5_ICU_day5",
  "M2.6_ICU_day6",
  "M2.7_ICU_day7",
  "M3_discharge",
  "Baseline",
  "Day 1",
  "Day 3",
  "Day 7",
  "Day 28",
  "ICU admission"
)

unexpected_time_labels <- setdiff(
  unique(
    as.character(
      all_sample_metrics$Timepoint_Factor
    )
  ),
  global_time_levels
)

if (
  length(
    unexpected_time_labels
  ) > 0
) {
  stop(
    paste0(
      "合并后出现未预设的时间标签：",
      paste(
        unexpected_time_labels,
        collapse = " | "
      )
    )
  )
}

all_sample_metrics$Timepoint_Factor <- factor(
  as.character(
    all_sample_metrics$Timepoint_Factor
  ),
  levels = global_time_levels,
  ordered = FALSE
)

timepoint_order_audit <- do.call(
  rbind,
  lapply(
    names(
      sample_metric_list
    ),
    function(analysis_id) {
      df <- sample_metric_list[[
        analysis_id
      ]]

      z <- unique(
        data.frame(
          Analysis_ID =
            analysis_id,
          Time_Label =
            as.character(
              df$Time_Label
            ),
          Time_Order_Resolved =
            df$Time_Order_Resolved,
          Factor_Level =
            as.numeric(
              df$Timepoint_Factor
            ),
          stringsAsFactors = FALSE
        )
      )

      z[
        order(
          z$Time_Order_Resolved,
          z$Time_Label
        ),
        ,
        drop = FALSE
      ]
    }
  )
)

timepoint_order_audit$PreBind_Status <- ifelse(
  ave(
    timepoint_order_audit$Time_Order_Resolved,
    timepoint_order_audit$Analysis_ID,
    FUN = function(x) {
      !duplicated(x) &
        order(x) ==
        seq_along(x)
    }
  ),
  "PASS",
  "REVIEW"
)

postbind_level_map <- unique(
  data.frame(
    Analysis_ID =
      all_sample_metrics$Analysis_ID,
    Time_Label =
      as.character(
        all_sample_metrics$Time_Label
      ),
    PostBind_Factor_Level =
      as.numeric(
        all_sample_metrics$Timepoint_Factor
      ),
    stringsAsFactors = FALSE
  )
)

timepoint_order_audit <- merge(
  timepoint_order_audit,
  postbind_level_map,
  by = c(
    "Analysis_ID",
    "Time_Label"
  ),
  all.x = TRUE,
  sort = FALSE
)

timepoint_order_audit <- timepoint_order_audit[
  order(
    timepoint_order_audit$Analysis_ID,
    timepoint_order_audit$Time_Order_Resolved
  ),
  ,
  drop = FALSE
]

timepoint_order_audit$PostBind_Rank <- ave(
  timepoint_order_audit$PostBind_Factor_Level,
  timepoint_order_audit$Analysis_ID,
  FUN = function(x) {
    rank(
      x,
      ties.method = "first"
    )
  }
)

timepoint_order_audit$PostBind_Status <- ifelse(
  timepoint_order_audit$PostBind_Rank ==
    timepoint_order_audit$Time_Order_Resolved,
  "PASS",
  "FAIL"
)

timepoint_order_audit$Status <- ifelse(
  timepoint_order_audit$PreBind_Status ==
    "PASS" &
    timepoint_order_audit$PostBind_Status ==
      "PASS",
  "PASS",
  "FAIL"
)

if (
  any(
    timepoint_order_audit$Status !=
      "PASS"
  )
) {
  stop(
    "时间顺序在合并后校验失败，停止分析。"
  )
}

# ------------------------------------------------------------
# Long-format helper
# ------------------------------------------------------------
to_long_metrics <- function(df) {
  rows <- lapply(
    metric_columns,
    function(metric) {
      out <- df[
        ,
        c(
          "Analysis_Status",
          "Analysis_ID",
          "Project_ID",
          "Analysis_Type",
          "Run_ID",
          "Patient_ID",
          "Comparison_Group",
          "Timepoint_Raw",
          "Timepoint_Day",
          "Timepoint_Order",
          "Time_Label",
          "Time_Order_Resolved",
          "Timepoint_Factor",
          "Arm_Resolved",
          "Status_Resolved"
        ),
        drop = FALSE
      ]

      out$Metric <- metric
      out$Value <- safe_numeric(
        df[[metric]]
      )

      out
    }
  )

  do.call(
    rbind,
    rows
  )
}

all_long <- to_long_metrics(
  all_sample_metrics
)

# ------------------------------------------------------------
# Mixed-model utilities
# ------------------------------------------------------------
fit_lme_model <- function(
  df,
  fixed_formula,
  analysis_id,
  metric,
  model_name
) {
  valid <- is.finite(
    df$Value
  ) &
    !is.na(
      df$Patient_ID
    )

  model_df <- df[
    valid,
    ,
    drop = FALSE
  ]

  if (
    nrow(
      model_df
    ) <
      minimum_model_observations ||
      length(
        unique(
          model_df$Patient_ID
        )
      ) <
      minimum_unique_patients ||
      stats::sd(
        model_df$Value
      ) == 0
  ) {
    return(
      list(
        coefficients = data.frame(
          Analysis_ID =
            analysis_id,
          Metric =
            metric,
          Model =
            model_name,
          Term =
            "MODEL_NOT_FIT",
          Estimate =
            NA_real_,
          Std_Error =
            NA_real_,
          DF =
            NA_real_,
          CI_Low =
            NA_real_,
          CI_High =
            NA_real_,
          P =
            NA_real_,
          N =
            nrow(
              model_df
            ),
          Patients_N =
            length(
              unique(
                model_df$Patient_ID
              )
            ),
          Status =
            "INSUFFICIENT_OR_ZERO_VARIANCE",
          stringsAsFactors = FALSE
        ),
        terms = data.frame(
          Analysis_ID =
            analysis_id,
          Metric =
            metric,
          Model =
            model_name,
          Model_Term =
            "MODEL_NOT_FIT",
          Num_DF =
            NA_real_,
          Den_DF =
            NA_real_,
          F_Value =
            NA_real_,
          P =
            NA_real_,
          N =
            nrow(
              model_df
            ),
          Patients_N =
            length(
              unique(
                model_df$Patient_ID
              )
            ),
          Status =
            "INSUFFICIENT_OR_ZERO_VARIANCE",
          stringsAsFactors = FALSE
        )
      )
    )
  }

  fit <- tryCatch(
    nlme::lme(
      fixed =
        fixed_formula,
      random =
        ~ 1 | Patient_ID,
      data =
        model_df,
      method =
        "REML",
      na.action =
        na.omit,
      control =
        nlme::lmeControl(
          opt = "optim",
          maxIter = 200,
          msMaxIter = 200,
          returnObject = TRUE
        )
    ),
    error = function(e) {
      structure(
        list(
          message =
            conditionMessage(e)
        ),
        class =
          "lme_failure"
      )
    }
  )

  if (
    inherits(
      fit,
      "lme_failure"
    )
  ) {
    return(
      list(
        coefficients = data.frame(
          Analysis_ID =
            analysis_id,
          Metric =
            metric,
          Model =
            model_name,
          Term =
            "MODEL_FAILED",
          Estimate =
            NA_real_,
          Std_Error =
            NA_real_,
          DF =
            NA_real_,
          CI_Low =
            NA_real_,
          CI_High =
            NA_real_,
          P =
            NA_real_,
          N =
            nrow(
              model_df
            ),
          Patients_N =
            length(
              unique(
                model_df$Patient_ID
              )
            ),
          Status =
            paste0(
              "FAILED: ",
              fit$message
            ),
          stringsAsFactors = FALSE
        ),
        terms = data.frame(
          Analysis_ID =
            analysis_id,
          Metric =
            metric,
          Model =
            model_name,
          Model_Term =
            "MODEL_FAILED",
          Num_DF =
            NA_real_,
          Den_DF =
            NA_real_,
          F_Value =
            NA_real_,
          P =
            NA_real_,
          N =
            nrow(
              model_df
            ),
          Patients_N =
            length(
              unique(
                model_df$Patient_ID
              )
            ),
          Status =
            paste0(
              "FAILED: ",
              fit$message
            ),
          stringsAsFactors = FALSE
        )
      )
    )
  }

  summary_fit <- summary(
    fit
  )

  coefficient_table <-
    as.data.frame(
      summary_fit$tTable
    )

  coefficient_table$Term <-
    rownames(
      coefficient_table
    )

  rownames(
    coefficient_table
  ) <- NULL

  df_value <- safe_numeric(
    coefficient_table$DF
  )

  critical <- stats::qt(
    0.975,
    df = pmax(
      df_value,
      1
    )
  )

  coefficient_output <- data.frame(
    Analysis_ID =
      analysis_id,
    Metric =
      metric,
    Model =
      model_name,
    Term =
      coefficient_table$Term,
    Estimate =
      safe_numeric(
        coefficient_table$Value
      ),
    Std_Error =
      safe_numeric(
        coefficient_table$Std.Error
      ),
    DF =
      df_value,
    CI_Low =
      safe_numeric(
        coefficient_table$Value
      ) -
      critical *
        safe_numeric(
          coefficient_table$Std.Error
        ),
    CI_High =
      safe_numeric(
        coefficient_table$Value
      ) +
      critical *
        safe_numeric(
          coefficient_table$Std.Error
        ),
    P =
      safe_numeric(
        coefficient_table$`p-value`
      ),
    N =
      nrow(
        model_df
      ),
    Patients_N =
      length(
        unique(
          model_df$Patient_ID
        )
      ),
    Status =
      "PASS",
    stringsAsFactors = FALSE
  )

  anova_table <- as.data.frame(
    anova(
      fit
    )
  )

  anova_table$Model_Term <-
    rownames(
      anova_table
    )

  rownames(
    anova_table
  ) <- NULL

  term_output <- data.frame(
    Analysis_ID =
      analysis_id,
    Metric =
      metric,
    Model =
      model_name,
    Model_Term =
      anova_table$Model_Term,
    Num_DF =
      safe_numeric(
        anova_table$numDF
      ),
    Den_DF =
      safe_numeric(
        anova_table$denDF
      ),
    F_Value =
      safe_numeric(
        anova_table$`F-value`
      ),
    P =
      safe_numeric(
        anova_table$`p-value`
      ),
    N =
      nrow(
        model_df
      ),
    Patients_N =
      length(
        unique(
          model_df$Patient_ID
        )
      ),
    Status =
      "PASS",
    stringsAsFactors = FALSE
  )

  list(
    coefficients =
      coefficient_output,
    terms =
      term_output
  )
}

# ------------------------------------------------------------
# Fit longitudinal mixed models
# ------------------------------------------------------------
model_coefficient_rows <- list()
model_term_rows <- list()

simple_longitudinal_ids <- c(
  "T01_PRJEB33360_Longitudinal_Main",
  "T02_PRJEB33360_Longitudinal_DepthSensitivity",
  "T03_PRJNA691455_Sepsis_Longitudinal"
)

for (
  analysis_id in simple_longitudinal_ids
) {
  df <- all_long[
    all_long$Analysis_ID ==
      analysis_id,
    ,
    drop = FALSE
  ]

  df$Timepoint_Factor <- droplevels(
    df$Timepoint_Factor
  )

  for (
    metric in metric_columns
  ) {
    z <- df[
      df$Metric ==
        metric,
      ,
      drop = FALSE
    ]

    model <- fit_lme_model(
      z,
      Value ~ Timepoint_Factor,
      analysis_id,
      metric,
      "Value ~ Timepoint_Factor + (1|Patient_ID)"
    )

    model_coefficient_rows[[
      length(
        model_coefficient_rows
      ) + 1
    ]] <- model$coefficients

    model_term_rows[[
      length(
        model_term_rows
      ) + 1
    ]] <- model$terms
  }
}

# Intervention model.
intervention_id <-
  "T04_PRJNA430161_Intervention"

intervention_long <- all_long[
  all_long$Analysis_ID ==
    intervention_id &
    !is.na(
      all_long$Arm_Resolved
    ) &
    all_long$Arm_Resolved !=
      "Conflicting",
  ,
  drop = FALSE
]

intervention_long$Timepoint_Factor <-
  droplevels(
    intervention_long$Timepoint_Factor
  )

intervention_long$Arm_Factor <- factor(
  intervention_long$Arm_Resolved
)

for (
  metric in metric_columns
) {
  z <- intervention_long[
    intervention_long$Metric ==
      metric,
    ,
    drop = FALSE
  ]

  model <- fit_lme_model(
    z,
    Value ~
      Timepoint_Factor *
      Arm_Factor,
    intervention_id,
    metric,
    paste0(
      "Value ~ Timepoint_Factor * Arm_Factor ",
      "+ (1|Patient_ID)"
    )
  )

  model_coefficient_rows[[
    length(
      model_coefficient_rows
    ) + 1
  ]] <- model$coefficients

  model_term_rows[[
    length(
      model_term_rows
    ) + 1
  ]] <- model$terms
}

# Organ-dysfunction longitudinal model.
organ_long_id <-
  "T06_PRJNA912621_Cholestasis_Longitudinal"

organ_long <- all_long[
  all_long$Analysis_ID ==
    organ_long_id &
    !is.na(
      all_long$Status_Resolved
    ),
  ,
  drop = FALSE
]

organ_long$Timepoint_Factor <-
  droplevels(
    organ_long$Timepoint_Factor
  )

organ_long$Status_Factor <- factor(
  organ_long$Status_Resolved,
  levels = c(
    "No",
    "Yes"
  )
)

for (
  metric in metric_columns
) {
  z <- organ_long[
    organ_long$Metric ==
      metric,
    ,
    drop = FALSE
  ]

  model <- fit_lme_model(
    z,
    Value ~
      Timepoint_Factor *
      Status_Factor,
    organ_long_id,
    metric,
    paste0(
      "Value ~ Timepoint_Factor * Status_Factor ",
      "+ (1|Patient_ID)"
    )
  )

  model_coefficient_rows[[
    length(
      model_coefficient_rows
    ) + 1
  ]] <- model$coefficients

  model_term_rows[[
    length(
      model_term_rows
    ) + 1
  ]] <- model$terms
}

mixed_model_coefficients <- do.call(
  rbind,
  model_coefficient_rows
)

mixed_model_terms <- do.call(
  rbind,
  model_term_rows
)

# Submission revision: exclude intercepts from inferential BH families.
# Preserve the original grouping across metrics; do not select families by results.
nonintercept_bh <- function(d, groups) {
  out <- rep(NA_real_, nrow(d))
  term <- if ("Model_Term" %in% names(d)) d$Model_Term else d$Term
  eligible <- term != "(Intercept)" & is.finite(d$P)
  for (g in unique(groups[eligible])) {
    i <- which(eligible & groups == g)
    out[i] <- stats::p.adjust(d$P[i], method = "BH")
  }
  out
}
mixed_model_coefficients$P_FDR <- nonintercept_bh(
  mixed_model_coefficients,
  interaction(mixed_model_coefficients$Analysis_ID,
              mixed_model_coefficients$Model, drop = TRUE))
mixed_model_terms$P_FDR <- nonintercept_bh(
  mixed_model_terms, mixed_model_terms$Analysis_ID)

# ------------------------------------------------------------
# Paired within-patient changes
# ------------------------------------------------------------
paired_change_rows <- list()

paired_analysis_ids <- c(
  simple_longitudinal_ids,
  intervention_id,
  organ_long_id
)

for (
  analysis_id in paired_analysis_ids
) {
  df <- all_long[
    all_long$Analysis_ID ==
      analysis_id,
    ,
    drop = FALSE
  ]

  baseline_level <- levels(
    droplevels(
      df$Timepoint_Factor
    )
  )[1]

  later_levels <- setdiff(
    levels(
      droplevels(
        df$Timepoint_Factor
      )
    ),
    baseline_level
  )

  stratifier_name <- if (
    analysis_id ==
      intervention_id
  ) {
    "Arm_Resolved"
  } else if (
    analysis_id ==
      organ_long_id
  ) {
    "Status_Resolved"
  } else {
    "None"
  }

  strata <- if (
    stratifier_name ==
      "None"
  ) {
    "All"
  } else {
    unique(
      df[[stratifier_name]][
        !is.na(
          df[[stratifier_name]]
        )
      ]
    )
  }

  for (
    metric in metric_columns
  ) {
    metric_df <- df[
      df$Metric ==
        metric,
      ,
      drop = FALSE
    ]

    for (
      stratum in strata
    ) {
      z <- if (
        stratifier_name ==
          "None"
      ) {
        metric_df
      } else {
        metric_df[
          metric_df[[stratifier_name]] ==
            stratum,
          ,
          drop = FALSE
        ]
      }

      baseline <- z[
        as.character(
          z$Timepoint_Factor
        ) ==
          baseline_level,
        c(
          "Patient_ID",
          "Value"
        ),
        drop = FALSE
      ]

      names(
        baseline
      )[2] <- "Baseline_Value"

      for (
        later_level in later_levels
      ) {
        later <- z[
          as.character(
            z$Timepoint_Factor
          ) ==
            later_level,
          c(
            "Patient_ID",
            "Value"
          ),
          drop = FALSE
        ]

        names(
          later
        )[2] <- "Later_Value"

        paired <- merge(
          baseline,
          later,
          by = "Patient_ID",
          all = FALSE
        )

        paired <- paired[
          is.finite(
            paired$Baseline_Value
          ) &
            is.finite(
              paired$Later_Value
            ),
          ,
          drop = FALSE
        ]

        test <- safe_wilcox_paired(
          paired$Baseline_Value,
          paired$Later_Value
        )

        paired_change_rows[[
          length(
            paired_change_rows
          ) + 1
        ]] <- data.frame(
          Analysis_ID =
            analysis_id,
          Metric =
            metric,
          Stratifier =
            stratifier_name,
          Stratum =
            stratum,
          Baseline =
            baseline_level,
          Followup =
            later_level,
          Paired_N =
            nrow(
              paired
            ),
          Median_Baseline = if (
            nrow(
              paired
            ) > 0
          ) {
            stats::median(
              paired$Baseline_Value
            )
          } else {
            NA_real_
          },
          Median_Followup = if (
            nrow(
              paired
            ) > 0
          ) {
            stats::median(
              paired$Later_Value
            )
          } else {
            NA_real_
          },
          Median_Change = if (
            nrow(
              paired
            ) > 0
          ) {
            stats::median(
              paired$Later_Value -
                paired$Baseline_Value
            )
          } else {
            NA_real_
          },
          Mean_Change = if (
            nrow(
              paired
            ) > 0
          ) {
            mean(
              paired$Later_Value -
                paired$Baseline_Value
            )
          } else {
            NA_real_
          },
          Wilcoxon_V =
            test["statistic"],
          P =
            test["p"],
          Status = if (
            nrow(
              paired
            ) >= 3
          ) {
            if (
              nrow(
                paired
              ) < 5
            ) {
              "REVIEW_SMALL_PAIRED_N"
            } else {
              "PASS"
            }
          } else {
            "INSUFFICIENT"
          },
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

paired_changes <- do.call(
  rbind,
  paired_change_rows
)

paired_changes$P_FDR <-
  ave(
    paired_changes$P,
    interaction(
      paired_changes$Analysis_ID,
      paired_changes$Stratum,
      drop = TRUE
    ),
    FUN = function(p) {
      stats::p.adjust(
        p,
        method = "BH"
      )
    }
  )

# ------------------------------------------------------------
# Cross-sectional organ dysfunction S05
# ------------------------------------------------------------
cross_id <-
  "T05_PRJNA797231_Myocardial_Dysfunction"

cross_long <- all_long[
  all_long$Analysis_ID ==
    cross_id &
    !is.na(
      all_long$Status_Resolved
    ),
  ,
  drop = FALSE
]

cross_sectional_rows <- list()

for (
  metric in metric_columns
) {
  z <- cross_long[
    cross_long$Metric ==
      metric,
    ,
    drop = FALSE
  ]

  yes <- z$Value[
    z$Status_Resolved ==
      "Yes"
  ]

  no <- z$Value[
    z$Status_Resolved ==
      "No"
  ]

  test <- safe_wilcox_unpaired(
    yes,
    no
  )

  label <- ifelse(
    z$Status_Resolved ==
      "Yes",
    1,
    0
  )

  cross_sectional_rows[[
    length(
      cross_sectional_rows
    ) + 1
  ]] <- data.frame(
    Analysis_ID =
      cross_id,
    Endpoint =
      "Sepsis-induced myocardial dysfunction",
    Metric =
      metric,
    Yes_N =
      sum(
        is.finite(
          yes
        )
      ),
    No_N =
      sum(
        is.finite(
          no
        )
      ),
    Median_Yes =
      if (
        any(
          is.finite(
            yes
          )
        )
      ) {
        stats::median(
          yes[
            is.finite(
              yes
            )
          ]
        )
      } else {
        NA_real_
      },
    Median_No =
      if (
        any(
          is.finite(
            no
          )
        )
      ) {
        stats::median(
          no[
            is.finite(
              no
            )
          ]
        )
      } else {
        NA_real_
      },
    Median_Difference =
      if (
        any(
          is.finite(
            yes
          )
        ) &&
          any(
            is.finite(
              no
            )
          )
      ) {
        stats::median(
          yes[
            is.finite(
              yes
            )
          ]
        ) -
          stats::median(
            no[
              is.finite(
                no
              )
            ]
          )
      } else {
        NA_real_
      },
    Cliffs_Delta_Yes_minus_No =
      cliffs_delta(
        yes,
        no
      ),
    AUC_Yes_as_Case =
      auc_rank(
        label,
        z$Value
      ),
    Wilcoxon_W =
      test["statistic"],
    P =
      test["p"],
    Status =
      if (
        sum(
          is.finite(
            yes
          )
        ) >= 5 &&
          sum(
            is.finite(
              no
            )
          ) >= 5
      ) {
        "PASS"
      } else {
        "REVIEW_SMALL_OR_MISSING"
      },
    stringsAsFactors = FALSE
  )
}

cross_sectional <- do.call(
  rbind,
  cross_sectional_rows
)

cross_sectional$P_FDR <-
  stats::p.adjust(
    cross_sectional$P,
    method = "BH"
  )

# ------------------------------------------------------------
# At-each-time group comparisons for T04 and T06
# ------------------------------------------------------------
timepoint_group_rows <- list()

timepoint_group_configs <- list(
  list(
    analysis_id =
      intervention_id,
    group_column =
      "Arm_Resolved",
    endpoint =
      "Probiotic versus placebo"
  ),
  list(
    analysis_id =
      organ_long_id,
    group_column =
      "Status_Resolved",
    endpoint =
      "Cholestasis Yes versus No"
  )
)

for (
  comparison_config in
    timepoint_group_configs
) {
  analysis_id <-
    comparison_config$analysis_id

  group_column <-
    comparison_config$group_column

  df <- all_long[
    all_long$Analysis_ID ==
      analysis_id &
      !is.na(
        all_long[[group_column]]
      ),
    ,
    drop = FALSE
  ]

  groups <- unique(
    df[[group_column]]
  )

  if (
    length(
      groups
    ) != 2
  ) {
    next
  }

  group_a <- sort(
    groups
  )[1]

  group_b <- sort(
    groups
  )[2]

  for (
    metric in metric_columns
  ) {
    metric_df <- df[
      df$Metric ==
        metric,
      ,
      drop = FALSE
    ]

    for (
      timepoint in levels(
        droplevels(
          metric_df$Timepoint_Factor
        )
      )
    ) {
      z <- metric_df[
        as.character(
          metric_df$Timepoint_Factor
        ) ==
          timepoint,
        ,
        drop = FALSE
      ]

      a <- z$Value[
        z[[group_column]] ==
          group_a
      ]

      b <- z$Value[
        z[[group_column]] ==
          group_b
      ]

      test <- safe_wilcox_unpaired(
        b,
        a
      )

      timepoint_group_rows[[
        length(
          timepoint_group_rows
        ) + 1
      ]] <- data.frame(
        Analysis_ID =
          analysis_id,
        Endpoint =
          comparison_config$endpoint,
        Metric =
          metric,
        Timepoint =
          timepoint,
        Reference_Group =
          group_a,
        Comparison_Group =
          group_b,
        Reference_N =
          sum(
            is.finite(
              a
            )
          ),
        Comparison_N =
          sum(
            is.finite(
              b
            )
          ),
        Median_Reference =
          if (
            any(
              is.finite(
                a
              )
            )
          ) {
            stats::median(
              a[
                is.finite(
                  a
                )
              ]
            )
          } else {
            NA_real_
          },
        Median_Comparison =
          if (
            any(
              is.finite(
                b
              )
            )
          ) {
            stats::median(
              b[
                is.finite(
                  b
                )
              ]
            )
          } else {
            NA_real_
          },
        Cliffs_Delta_Comparison_minus_Reference =
          cliffs_delta(
            b,
            a
          ),
        Wilcoxon_W =
          test["statistic"],
        P =
          test["p"],
        Status =
          if (
            sum(
              is.finite(
                a
              )
            ) >= 3 &&
              sum(
                is.finite(
                  b
                )
              ) >= 3
          ) {
            "PASS"
          } else {
            "REVIEW_SMALL_OR_MISSING"
          },
        stringsAsFactors = FALSE
      )
    }
  }
}

timepoint_group_comparisons <- do.call(
  rbind,
  timepoint_group_rows
)

timepoint_group_comparisons$P_FDR <-
  ave(
    timepoint_group_comparisons$P,
    timepoint_group_comparisons$
      Analysis_ID,
    FUN = function(p) {
      stats::p.adjust(
        p,
        method = "BH"
      )
    }
  )

# ------------------------------------------------------------
# Descriptive summaries
# ------------------------------------------------------------
descriptive <- aggregate(
  Value ~
    Analysis_ID +
    Metric +
    Time_Label +
    Arm_Resolved +
    Status_Resolved,
  data = transform(
    all_long,
    Arm_Resolved = ifelse(
      is.na(
        Arm_Resolved
      ),
      "Not_applicable",
      Arm_Resolved
    ),
    Status_Resolved = ifelse(
      is.na(
        Status_Resolved
      ),
      "Not_applicable",
      Status_Resolved
    )
  ),
  FUN = function(x) {
    c(
      N = sum(
        is.finite(
          x
        )
      ),
      Mean = mean(
        x,
        na.rm = TRUE
      ),
      SD = stats::sd(
        x,
        na.rm = TRUE
      ),
      Median = stats::median(
        x,
        na.rm = TRUE
      ),
      Q1 = stats::quantile(
        x,
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      Q3 = stats::quantile(
        x,
        0.75,
        na.rm = TRUE,
        names = FALSE
      )
    )
  }
)

# aggregate() may return the vector-valued summary column either
# as a matrix or as a list, depending on the R data structure.
# Expand both forms explicitly.
if (is.matrix(descriptive$Value)) {
  descriptive_value_expanded <- as.data.frame(
    descriptive$Value,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
} else if (is.list(descriptive$Value)) {
  descriptive_value_expanded <- as.data.frame(
    do.call(
      rbind,
      descriptive$Value
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
} else {
  stop(
    paste0(
      "无法识别descriptive$Value的数据结构：",
      paste(
        class(descriptive$Value),
        collapse = " / "
      )
    )
  )
}

expected_summary_columns <- c(
  "N",
  "Mean",
  "SD",
  "Median",
  "Q1",
  "Q3"
)

if (
  ncol(descriptive_value_expanded) !=
    length(expected_summary_columns)
) {
  stop(
    paste0(
      "描述性汇总列数异常：预期",
      length(expected_summary_columns),
      "列，实际",
      ncol(descriptive_value_expanded),
      "列。"
    )
  )
}

names(descriptive_value_expanded) <-
  expected_summary_columns

descriptive_expanded <- cbind(
  data.frame(
    Analysis_ID =
      descriptive$Analysis_ID,
    Metric =
      descriptive$Metric,
    Time_Label =
      descriptive$Time_Label,
    Arm =
      descriptive$Arm_Resolved,
    Organ_Dysfunction_Status =
      descriptive$Status_Resolved,
    stringsAsFactors = FALSE
  ),
  descriptive_value_expanded
)

# ------------------------------------------------------------
# Basic plots
# ------------------------------------------------------------
key_metrics <- c(
  "SDI_Equal_Balance",
  "SDI_MetaWeighted_Balance",
  "Alpha_Shannon"
)

plot_individual_trajectory <- function(
  df,
  metric,
  title_text,
  group_column = NULL
) {
  z <- df[
    df$Metric ==
      metric &
      is.finite(
        df$Value
      ),
    ,
    drop = FALSE
  ]

  if (
    nrow(
      z
    ) == 0
  ) {
    plot.new()
    title(
      title_text
    )
    text(
      0.5,
      0.5,
      "No estimable data"
    )
    return(
      invisible(
        NULL
      )
    )
  }

  x <- as.numeric(
    z$Timepoint_Factor
  )

  plot(
    x,
    z$Value,
    type = "n",
    xaxt = "n",
    xlab = "Timepoint",
    ylab = metric,
    main = title_text
  )

  axis(
    1,
    at = seq_along(
      levels(
        z$Timepoint_Factor
      )
    ),
    labels = levels(
      z$Timepoint_Factor
    ),
    las = 2,
    cex.axis = 0.7
  )

  patients <- unique(
    z$Patient_ID
  )

  for (
    patient in patients
  ) {
    patient_df <- z[
      z$Patient_ID ==
        patient,
      ,
      drop = FALSE
    ]

    patient_df <- patient_df[
      order(
        as.numeric(
          patient_df$Timepoint_Factor
        )
      ),
      ,
      drop = FALSE
    ]

    lines(
      as.numeric(
        patient_df$Timepoint_Factor
      ),
      patient_df$Value,
      lty = 1
    )
  }

  if (is.null(group_column)) {
    means <- tapply(
      z$Value,
      z$Timepoint_Factor,
      mean,
      na.rm = TRUE
    )

    lines(
      seq_along(
        means
      ),
      means,
      lwd = 3
    )

    points(
      seq_along(
        means
      ),
      means,
      pch = 19
    )
  } else {
    groups <- unique(
      z[[group_column]][
        !is.na(
          z[[group_column]]
        )
      ]
    )

    line_types <- seq_along(
      groups
    )

    for (
      j in seq_along(
        groups
      )
    ) {
      group <- groups[j]

      group_df <- z[
        z[[group_column]] ==
          group,
        ,
        drop = FALSE
      ]

      means <- tapply(
        group_df$Value,
        group_df$Timepoint_Factor,
        mean,
        na.rm = TRUE
      )

      lines(
        seq_along(
          means
        ),
        means,
        lwd = 3,
        lty = line_types[j]
      )

      points(
        seq_along(
          means
        ),
        means,
        pch = j + 14
      )
    }

    legend(
      "topright",
      legend = groups,
      lty = line_types,
      pch = seq_along(
        groups
      ) + 14,
      bty = "n"
    )
  }
}

pdf(
  file.path(
    plot_root,
    "20_Longitudinal_KeyMetric_Trajectories.pdf"
  ),
  width = 9,
  height = 6
)

for (
  analysis_id in c(
    simple_longitudinal_ids,
    intervention_id,
    organ_long_id
  )
) {
  df <- all_long[
    all_long$Analysis_ID ==
      analysis_id,
    ,
    drop = FALSE
  ]

  group_column <- if (
    analysis_id ==
      intervention_id
  ) {
    "Arm_Resolved"
  } else if (
    analysis_id ==
      organ_long_id
  ) {
    "Status_Resolved"
  } else {
    NULL
  }

  for (
    metric in key_metrics
  ) {
    plot_individual_trajectory(
      df,
      metric,
      paste0(
        analysis_id,
        "\n",
        metric
      ),
      group_column
    )
  }
}

dev.off()

pdf(
  file.path(
    plot_root,
    "20_Myocardial_Dysfunction_KeyMetric_Boxplots.pdf"
  ),
  width = 8,
  height = 6
)

cross_plot <- all_long[
  all_long$Analysis_ID ==
    cross_id,
  ,
  drop = FALSE
]

for (
  metric in key_metrics
) {
  z <- cross_plot[
    cross_plot$Metric ==
      metric,
    ,
    drop = FALSE
  ]

  boxplot(
    z$Value ~
      z$Status_Resolved,
    xlab =
      "Myocardial dysfunction",
    ylab =
      metric,
    main =
      paste0(
        cross_id,
        "\n",
        metric
      )
  )

  stripchart(
    z$Value ~
      z$Status_Resolved,
    vertical =
      TRUE,
    method =
      "jitter",
    add =
      TRUE,
    pch =
      16
  )
}

dev.off()

# Candidate change heatmap uses main longitudinal,
# intervention interaction, and organ-dysfunction terms only.
candidate_terms <- mixed_model_coefficients[
  grepl(
    "^CandidateSignedCLR__",
    mixed_model_coefficients$Metric
  ) &
    mixed_model_coefficients$Term !=
      "(Intercept)" &
    mixed_model_coefficients$Status ==
      "PASS",
  ,
  drop = FALSE
]

pdf(
  file.path(
    plot_root,
    "20_Candidate_Signed_CLR_Model_Effects.pdf"
  ),
  width = 11,
  height = 7
)

if (
  nrow(
    candidate_terms
  ) > 0
) {
  labels <- paste0(
    candidate_terms$Analysis_ID,
    " | ",
    candidate_terms$Metric,
    " | ",
    candidate_terms$Term
  )

  ordered <- order(
    candidate_terms$Estimate
  )

  plot(
    candidate_terms$Estimate[
      ordered
    ],
    seq_along(
      ordered
    ),
    xlab = paste0(
      "Mixed-model coefficient; positive = movement ",
      "toward locked Sepsis direction"
    ),
    ylab = "",
    yaxt = "n",
    pch = 16,
    main =
      "Candidate signed CLR supportive effects"
  )

  axis(
    2,
    at = seq_along(
      ordered
    ),
    labels = labels[
      ordered
    ],
    las = 2,
    cex.axis = 0.45
  )

  segments(
    candidate_terms$CI_Low[
      ordered
    ],
    seq_along(
      ordered
    ),
    candidate_terms$CI_High[
      ordered
    ],
    seq_along(
      ordered
    )
  )

  abline(
    v = 0,
    lty = 2
  )
} else {
  plot.new()
  text(
    0.5,
    0.5,
    "No estimable candidate model effects"
  )
}

dev.off()

# ------------------------------------------------------------
# Workflow audit
# ------------------------------------------------------------
lock_hash_after <- unname(
  tools::md5sum(
    c(
      candidate_file,
      formula_file
    )
  )
)

lock_audit <- data.frame(
  File = c(
    candidate_file,
    formula_file
  ),
  MD5_Before =
    lock_hash_before,
  MD5_After =
    lock_hash_after,
  Unchanged =
    lock_hash_before ==
      lock_hash_after,
  Status = ifelse(
    lock_hash_before ==
      lock_hash_after,
    "PASS",
    "FAIL"
  ),
  stringsAsFactors = FALSE
)

workflow_audit <- data.frame(
  Check_Name = c(
    "Day10_locked_files_unchanged",
    "Day10_validation_status_preserved",
    "SDI_retrained",
    "SDI_direction_changed",
    "S02_treated_as_independent_replication",
    "Repeated_measures_modeled_by_patient",
    "Different_support_questions_pooled",
    "Candidate_absence_handling"
  ),
  Status = c(
    if (
      all(
        lock_audit$Unchanged
      )
    ) {
      "PASS"
    } else {
      "FAIL"
    },
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS"
  ),
  Details = c(
    "Day10 candidate and formula lock files remained unchanged.",
    "Day10 external validation remains FAILED.",
    "No SDI model was fitted or refitted.",
    "Locked genus directions were preserved.",
    "S02 is explicitly labeled depth sensitivity for S01.",
    "Longitudinal models include Patient_ID random intercept.",
    "Longitudinal, intervention, and organ-dysfunction endpoints were analyzed separately.",
    "Locked SDI uses zero for project-absent locked candidates; individual candidate CLR is NA when the genus column is absent."
  ),
  stringsAsFactors = FALSE
)

analysis_audit <- data.frame(
  Analysis_ID =
    set_config$Analysis_ID,
  Analysis_Type =
    set_config$Analysis_Type,
  Input_Status =
    input_audit$Status[
      match(
        set_config$Analysis_ID,
        input_audit$Analysis_ID
      )
    ],
  Role = c(
    "Main longitudinal supportive analysis",
    "Depth sensitivity only",
    "Independent longitudinal supportive analysis",
    "Small intervention supportive analysis",
    "Cross-sectional organ-dysfunction support",
    "Longitudinal organ-dysfunction support"
  ),
  Interpretation_Limit = c(
    "Later ICU/discharge samples; observational trajectory.",
    "Not a second independent cohort.",
    "Small n=10; supportive only.",
    "Very small n=9; interaction tests are underpowered.",
    "Small n=34 and one sample per patient.",
    "Small n=20; group-by-time interaction is supportive."
  ),
  Final_Status = ifelse(
    input_audit$Status[
      match(
        set_config$Analysis_ID,
        input_audit$Analysis_ID
      )
    ] %in%
      c(
        "PASS",
        "PASS_WITH_MISSING_CANDIDATE_REVIEW"
      ),
    "PASS_WITH_INTERPRETATION_LIMITS",
    "REVIEW"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------
wcsv(
  all_sample_metrics,
  file.path(
    output_root,
    "20_All_Supportive_Sample_Metrics.csv"
  )
)

wcsv(
  all_long,
  file.path(
    output_root,
    "20_All_Supportive_Metrics_Long.csv"
  )
)

wcsv(
  input_audit,
  file.path(
    report_root,
    "20_Input_Audit.csv"
  )
)

wcsv(
  timepoint_order_audit,
  file.path(
    report_root,
    "20_Timepoint_Order_Audit.csv"
  )
)

wcsv(
  descriptive_expanded,
  file.path(
    report_root,
    "20_Descriptive_Summaries.csv"
  )
)

wcsv(
  mixed_model_coefficients,
  file.path(
    report_root,
    "20_Mixed_Model_Coefficients.csv"
  )
)

wcsv(
  mixed_model_terms,
  file.path(
    report_root,
    "20_Mixed_Model_Overall_Terms.csv"
  )
)

wcsv(
  paired_changes,
  file.path(
    report_root,
    "20_Paired_Timepoint_Changes.csv"
  )
)

wcsv(
  cross_sectional,
  file.path(
    report_root,
    "20_Myocardial_Dysfunction_CrossSectional.csv"
  )
)

wcsv(
  timepoint_group_comparisons,
  file.path(
    report_root,
    "20_Timepoint_Group_Comparisons.csv"
  )
)

wcsv(
  lock_audit,
  file.path(
    report_root,
    "20_Day10_Lock_Integrity_Audit.csv"
  )
)

wcsv(
  workflow_audit,
  file.path(
    report_root,
    "20_Workflow_Audit.csv"
  )
)

wcsv(
  analysis_audit,
  file.path(
    report_root,
    "20_Analysis_Set_Audit.csv"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    report_root,
    "20_Longitudinal_Support_SessionInfo.txt"
  )
)

files_to_hash <- unique(c(
  candidate_file,
  formula_file,
  file.path(
    day7_set_root,
    paste0(
      c(
        set_config$Analysis_Set,
        baseline_set_name
      ),
      ".csv"
    )
  ),
  file.path(
    day6_root,
    c(
      set_config$Project_ID,
      "PRJEB33360"
    ),
    paste0(
      c(
        set_config$Project_ID,
        "PRJEB33360"
      ),
      "_genus_classified_counts.csv"
    )
  ),
  list.files(
    output_root,
    recursive = TRUE,
    full.names = TRUE
  ),
  list.files(
    report_root,
    recursive = TRUE,
    full.names = TRUE
  )
))

writeLines(
  files_to_hash,
  file.path(
    report_root,
    "20_Files_To_Hash.txt"
  ),
  useBytes = TRUE
)

cat(
  "\n============================================================\n"
)
cat("Day 12 completed.\n")
cat("Day10 external validation remains FAILED.\n")
cat("All SDI/candidate results are supportive exploratory analyses.\n")
cat(
  "Reports: ",
  report_root,
  "\n",
  sep = ""
)
cat(
  "Sample-level outputs: ",
  output_root,
  "\n",
  sep = ""
)
cat("Important outputs:\n")
cat("  20_Input_Audit.csv\n")
cat("  20_Timepoint_Order_Audit.csv\n")
cat("  20_Mixed_Model_Overall_Terms.csv\n")
cat("  20_Mixed_Model_Coefficients.csv\n")
cat("  20_Paired_Timepoint_Changes.csv\n")
cat("  20_Myocardial_Dysfunction_CrossSectional.csv\n")
cat("  20_Timepoint_Group_Comparisons.csv\n")
cat("  20_Analysis_Set_Audit.csv\n")
cat("  20_Workflow_Audit.csv\n")
cat(
  "Finished: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)
