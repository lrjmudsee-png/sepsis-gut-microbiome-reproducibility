# PUBLIC RELEASE v1.0.0
# Scientific logic preserved from the independently verified historical script18.
# Public-release change: the historical local project root was replaced by
# SEPSIS_V1_PROJECT_ROOT/getwd(); statistical logic is otherwise unchanged.

options(stringsAsFactors = FALSE)

# ============================================================
# Day 10: Lock SDI formula and perform external validation
#
# Strict sequence:
# 1. Read Day 9 ICU-control meta-analysis results.
# 2. Select and lock candidate genera using prespecified rules.
# 3. Build and lock the SDI formula using A01/A02 only.
# 4. Fit and lock the development logistic model and threshold.
# 5. Only after all lock files are written, open A04/A05.
# 6. Evaluate without feature selection, coefficient refitting,
#    threshold retuning, or other external-data-driven changes.
#
# Primary SDI:
#   Equal-weight log balance
#
# Secondary SDI:
#   Meta-effect-weighted log balance
#
# Positive score means a more Sepsis-like dysbiosis pattern.
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

day9_report_root <- file.path(
  root,
  "manuscript/03_analysis/17_harmonized_meta_analysis"
)

output_root <- file.path(
  root,
  "processed_data/day10_sdi_external_validation_v1"
)

report_root <- file.path(
  root,
  "manuscript/03_analysis/18_sdi_external_validation"
)

plot_root <- file.path(
  report_root,
  "plots"
)

lock_root <- file.path(
  report_root,
  "locked_before_external_open"
)

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(report_root, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_root, recursive = TRUE, showWarnings = FALSE)
dir.create(lock_root, recursive = TRUE, showWarnings = FALSE)

sink(
  file.path(
    report_root,
    "18_SDI_External_Validation_Log.txt"
  ),
  split = TRUE
)
on.exit(sink(), add = TRUE)

cat(
  "Day 10 started: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)

set.seed(20260730)

# ------------------------------------------------------------
# Prespecified parameters
# ------------------------------------------------------------
count_pseudocount <- 0.5
maximum_positive_genera <- 3
maximum_negative_genera <- 3
bootstrap_replicates <- 2000
primary_score_name <- "SDI_Equal_Balance"
secondary_score_name <- "SDI_MetaWeighted_Balance"

parameters <- data.frame(
  Parameter = c(
    "Candidate_source",
    "Primary_candidate_family",
    "Primary_candidate_grades",
    "Direction_completion_rule",
    "Maximum_positive_genera",
    "Maximum_negative_genera",
    "Primary_SDI",
    "Secondary_SDI",
    "Count_pseudocount",
    "Development_sets",
    "External_sets",
    "Primary_external_comparison",
    "Bootstrap_replicates",
    "External_data_retraining"
  ),
  Value = c(
    "Day 9 harmonized ICU-control meta-analysis",
    "ICU_control only",
    "ICU_Tier1_Robust then ICU_Tier2_Supportive",
    paste0(
      "If one direction is absent, add the highest-ranked ",
      "strict-stable ICU_Tier3_Directional genus in the ",
      "missing direction as a balance anchor."
    ),
    maximum_positive_genera,
    maximum_negative_genera,
    paste0(
      "Mean log(count+",
      count_pseudocount,
      ") of Sepsis-enriched genera minus mean log(count+",
      count_pseudocount,
      ") of Sepsis-depleted genera."
    ),
    paste0(
      "Within-direction weights proportional to absolute ICU REML ",
      "effect; positive weights sum to 1 and negative weights sum to 1."
    ),
    count_pseudocount,
    "A01+A02",
    "A04+A05",
    "A05 Sepsis vs Trauma; A04 Sepsis vs Healthy is supportive",
    bootstrap_replicates,
    "Forbidden"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  parameters,
  file.path(
    report_root,
    "18_SDI_Analysis_Parameters.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# Configuration
# ------------------------------------------------------------
development_config <- data.frame(
  Analysis_Set = c(
    "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
    "A02_PRJNA691455_Sepsis_vs_NonSepsisICU"
  ),
  Project_ID = c(
    "PRJEB33360",
    "PRJNA691455"
  ),
  Evaluation_Role = c(
    "Development_primary",
    "Development_primary"
  ),
  Group_A = "Sepsis",
  Group_B = "Non-sepsis ICU",
  stringsAsFactors = FALSE
)

external_config <- data.frame(
  Analysis_Set = c(
    "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
    "A05_PRJNA1010969_External_Sepsis_vs_Trauma"
  ),
  Project_ID = "PRJNA1010969",
  Evaluation_Role = c(
    "External_supportive_healthy",
    "External_primary_trauma"
  ),
  Group_A = "Sepsis",
  Group_B = c(
    "Healthy",
    "Trauma"
  ),
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

genus_columns <- function(df) {
  names(df)[grepl("^Genus__", names(df))]
}

safe_numeric_matrix <- function(df) {
  x <- as.matrix(
    data.frame(
      lapply(
        df,
        function(z) suppressWarnings(as.numeric(z))
      ),
      check.names = FALSE
    )
  )

  storage.mode(x) <- "numeric"
  x
}

auc_rank <- function(label, score) {
  valid <- !is.na(label) &
    is.finite(score)

  label <- label[valid]
  score <- score[valid]

  n1 <- sum(label == 1)
  n0 <- sum(label == 0)

  if (n1 == 0 || n0 == 0) {
    return(NA_real_)
  }

  ranks <- rank(
    score,
    ties.method = "average"
  )

  (
    sum(ranks[label == 1]) -
      n1 * (n1 + 1) / 2
  ) / (
    n1 * n0
  )
}

bootstrap_auc <- function(
  label,
  score,
  replicates = 2000
) {
  valid <- !is.na(label) &
    is.finite(score)

  label <- label[valid]
  score <- score[valid]

  case_index <- which(label == 1)
  control_index <- which(label == 0)

  if (
    length(case_index) < 2 ||
      length(control_index) < 2
  ) {
    return(
      c(
        lower = NA_real_,
        upper = NA_real_
      )
    )
  }

  values <- rep(
    NA_real_,
    replicates
  )

  for (i in seq_len(replicates)) {
    sampled <- c(
      sample(
        case_index,
        length(case_index),
        replace = TRUE
      ),
      sample(
        control_index,
        length(control_index),
        replace = TRUE
      )
    )

    values[i] <- auc_rank(
      label[sampled],
      score[sampled]
    )
  }

  stats::quantile(
    values,
    probs = c(
      0.025,
      0.975
    ),
    na.rm = TRUE,
    names = FALSE
  )
}

threshold_metrics <- function(
  label,
  score,
  threshold
) {
  valid <- !is.na(label) &
    is.finite(score)

  label <- label[valid]
  score <- score[valid]

  predicted <- as.integer(
    score >= threshold
  )

  tp <- sum(
    predicted == 1 &
      label == 1
  )

  tn <- sum(
    predicted == 0 &
      label == 0
  )

  fp <- sum(
    predicted == 1 &
      label == 0
  )

  fn <- sum(
    predicted == 0 &
      label == 1
  )

  sensitivity <- if (
    tp + fn > 0
  ) {
    tp / (tp + fn)
  } else {
    NA_real_
  }

  specificity <- if (
    tn + fp > 0
  ) {
    tn / (tn + fp)
  } else {
    NA_real_
  }

  ppv <- if (
    tp + fp > 0
  ) {
    tp / (tp + fp)
  } else {
    NA_real_
  }

  npv <- if (
    tn + fn > 0
  ) {
    tn / (tn + fn)
  } else {
    NA_real_
  }

  accuracy <- (
    tp + tn
  ) / length(label)

  balanced_accuracy <- mean(
    c(
      sensitivity,
      specificity
    ),
    na.rm = TRUE
  )

  data.frame(
    Threshold = threshold,
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn,
    Sensitivity = sensitivity,
    Specificity = specificity,
    PPV = ppv,
    NPV = npv,
    Accuracy = accuracy,
    Balanced_Accuracy =
      balanced_accuracy,
    stringsAsFactors = FALSE
  )
}

choose_youden_threshold <- function(
  label,
  score
) {
  candidates <- sort(
    unique(
      score[is.finite(score)]
    )
  )

  if (length(candidates) == 0) {
    return(NA_real_)
  }

  if (length(candidates) == 1) {
    return(candidates)
  }

  midpoints <- (
    candidates[-1] +
      candidates[
        -length(candidates)
      ]
  ) / 2

  thresholds <- c(
    Inf,
    rev(midpoints),
    -Inf
  )

  results <- lapply(
    thresholds,
    function(threshold) {
      m <- threshold_metrics(
        label,
        score,
        threshold
      )

      m$Youden <- m$Sensitivity +
        m$Specificity - 1

      m
    }
  )

  table <- do.call(
    rbind,
    results
  )

  best <- table[
    table$Youden ==
      max(
        table$Youden,
        na.rm = TRUE
      ),
    ,
    drop = FALSE
  ]

  best <- best[
    order(
      -best$Balanced_Accuracy,
      abs(
        best$Sensitivity -
          best$Specificity
      )
    ),
    ,
    drop = FALSE
  ]

  best$Threshold[1]
}

cliffs_delta <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]

  if (
    length(x) == 0 ||
      length(y) == 0
  ) {
    return(NA_real_)
  }

  comparisons <- outer(
    x,
    y,
    "-"
  )

  (
    sum(comparisons > 0) -
      sum(comparisons < 0)
  ) / (
    length(x) * length(y)
  )
}

safe_wilcox <- function(x, y) {
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

score_formula_text <- function(
  positive_features,
  negative_features,
  positive_weights,
  negative_weights,
  pseudocount,
  weighted = FALSE
) {
  if (!weighted) {
    return(
      paste0(
        "mean(log(count+",
        pseudocount,
        ")) over positive genera [",
        paste(
          positive_features,
          collapse = "; "
        ),
        "] minus mean(log(count+",
        pseudocount,
        ")) over negative genera [",
        paste(
          negative_features,
          collapse = "; "
        ),
        "]"
      )
    )
  }

  positive_text <- paste(
    paste0(
      round(
        positive_weights,
        8
      ),
      "*log(",
      positive_features,
      "+",
      pseudocount,
      ")"
    ),
    collapse = " + "
  )

  negative_text <- paste(
    paste0(
      round(
        negative_weights,
        8
      ),
      "*log(",
      negative_features,
      "+",
      pseudocount,
      ")"
    ),
    collapse = " + "
  )

  paste0(
    "(",
    positive_text,
    ") - (",
    negative_text,
    ")"
  )
}

load_set_counts <- function(
  config_row,
  candidate_features
) {
  set_id <- config_row$Analysis_Set
  project <- config_row$Project_ID

  set_file <- file.path(
    day7_set_root,
    paste0(
      set_id,
      ".csv"
    )
  )

  count_file <- file.path(
    day6_root,
    project,
    paste0(
      project,
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

  if (length(missing_required) > 0) {
    stop(
      set_id,
      " 缺少字段：",
      paste(
        missing_required,
        collapse = ", "
      )
    )
  }

  if (!"Run_ID" %in% names(count_df)) {
    stop(
      project,
      " count表缺少Run_ID。"
    )
  }

  meta$Run_ID <- clean_text(
    meta$Run_ID
  )

  meta$Patient_ID <- clean_text(
    meta$Patient_ID
  )

  meta$Comparison_Group <- clean_text(
    meta$Comparison_Group
  )

  count_df$Run_ID <- clean_text(
    count_df$Run_ID
  )

  if (anyDuplicated(meta$Run_ID)) {
    stop(
      set_id,
      " 分析集存在重复Run_ID。"
    )
  }

  index <- match(
    meta$Run_ID,
    count_df$Run_ID
  )

  if (any(is.na(index))) {
    stop(
      set_id,
      " 有Run无法匹配count表。"
    )
  }

  available_features <- intersect(
    candidate_features,
    names(count_df)
  )

  missing_features <- setdiff(
    candidate_features,
    names(count_df)
  )

  counts <- matrix(
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

  if (
    length(
      available_features
    ) > 0
  ) {
    available_matrix <-
      safe_numeric_matrix(
        count_df[
          index,
          available_features,
          drop = FALSE
        ]
      )

    counts[
      ,
      available_features
    ] <- available_matrix
  }

  if (
    any(!is.finite(counts)) ||
      any(counts < 0)
  ) {
    stop(
      set_id,
      " candidate count矩阵包含NA、无限值或负数。"
    )
  }

  list(
    meta = meta,
    counts = counts,
    missing_features = missing_features,
    set_file = set_file,
    count_file = count_file
  )
}

score_dataset <- function(
  loaded,
  config_row,
  positive_features,
  negative_features,
  positive_weights,
  negative_weights
) {
  counts <- loaded$counts
  meta <- loaded$meta

  log_counts <- log(
    counts + count_pseudocount
  )

  positive_equal <- rowMeans(
    log_counts[
      ,
      positive_features,
      drop = FALSE
    ]
  )

  negative_equal <- rowMeans(
    log_counts[
      ,
      negative_features,
      drop = FALSE
    ]
  )

  equal_score <- positive_equal -
    negative_equal

  weighted_positive <- as.numeric(
    log_counts[
      ,
      positive_features,
      drop = FALSE
    ] %*%
      positive_weights
  )

  weighted_negative <- as.numeric(
    log_counts[
      ,
      negative_features,
      drop = FALSE
    ] %*%
      negative_weights
  )

  weighted_score <- weighted_positive -
    weighted_negative

  case_label <- ifelse(
    meta$Comparison_Group ==
      config_row$Group_A,
    1,
    ifelse(
      meta$Comparison_Group ==
        config_row$Group_B,
      0,
      NA_real_
    )
  )

  data.frame(
    Analysis_Set =
      config_row$Analysis_Set,
    Project_ID =
      config_row$Project_ID,
    Evaluation_Role =
      config_row$Evaluation_Role,
    Run_ID = meta$Run_ID,
    Patient_ID = meta$Patient_ID,
    Comparison_Group =
      meta$Comparison_Group,
    Case_Label = case_label,
    SDI_Equal_Balance =
      equal_score,
    SDI_MetaWeighted_Balance =
      weighted_score,
    Missing_Candidate_Features =
      paste(
        loaded$missing_features,
        collapse = " | "
      ),
    Missing_Candidate_Feature_N =
      length(
        loaded$missing_features
      ),
    stringsAsFactors = FALSE
  )
}

evaluate_score <- function(
  score_df,
  score_column,
  dataset_label,
  locked_threshold = NA_real_,
  locked_probability = NULL
) {
  label <- score_df$Case_Label
  score <- score_df[[score_column]]

  auc <- auc_rank(
    label,
    score
  )

  auc_ci <- bootstrap_auc(
    label,
    score,
    bootstrap_replicates
  )

  group_a_score <- score[
    label == 1
  ]

  group_b_score <- score[
    label == 0
  ]

  wilcox <- safe_wilcox(
    group_a_score,
    group_b_score
  )

  result <- data.frame(
    Dataset = dataset_label,
    Analysis_Set = paste(
      unique(
        score_df$Analysis_Set
      ),
      collapse = " | "
    ),
    Evaluation_Role = paste(
      unique(
        score_df$Evaluation_Role
      ),
      collapse = " | "
    ),
    Score = score_column,
    N = nrow(score_df),
    Cases_N = sum(
      label == 1
    ),
    Controls_N = sum(
      label == 0
    ),
    AUC = auc,
    AUC_CI_Low = auc_ci[1],
    AUC_CI_High = auc_ci[2],
    Mean_Cases = mean(
      group_a_score
    ),
    Mean_Controls = mean(
      group_b_score
    ),
    Median_Cases = stats::median(
      group_a_score
    ),
    Median_Controls = stats::median(
      group_b_score
    ),
    Median_Difference =
      stats::median(
        group_a_score
      ) -
      stats::median(
        group_b_score
      ),
    Cliffs_Delta =
      cliffs_delta(
        group_a_score,
        group_b_score
      ),
    Wilcoxon_W =
      wilcox["statistic"],
    Wilcoxon_P =
      wilcox["p"],
    stringsAsFactors = FALSE
  )

  threshold_result <- NULL

  if (is.finite(locked_threshold)) {
    threshold_result <- threshold_metrics(
      label,
      score,
      locked_threshold
    )

    threshold_result$Dataset <-
      dataset_label

    threshold_result$Analysis_Set <-
      paste(
        unique(
          score_df$Analysis_Set
        ),
        collapse = " | "
      )

    threshold_result$Score <-
      score_column

    threshold_result <- threshold_result[
      ,
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
    ]
  }

  probability_result <- NULL

  if (!is.null(locked_probability)) {
    probability <- pmin(
      pmax(
        locked_probability,
        1e-8
      ),
      1 - 1e-8
    )

    probability_result <- data.frame(
      Dataset = dataset_label,
      Analysis_Set = paste(
        unique(
          score_df$Analysis_Set
        ),
        collapse = " | "
      ),
      Score = score_column,
      Probability_AUC = auc_rank(
        label,
        probability
      ),
      Brier_Score = mean(
        (
          probability - label
        )^2
      ),
      Log_Loss = -mean(
        label * log(probability) +
          (
            1 - label
          ) *
          log(
            1 - probability
          )
      ),
      stringsAsFactors = FALSE
    )
  }

  list(
    performance = result,
    threshold = threshold_result,
    probability = probability_result
  )
}

make_score_plot <- function(
  score_df,
  score_column,
  path
) {
  pdf(
    path,
    width = 9,
    height = 6
  )

  old_par <- par(no.readonly = TRUE)
  on.exit(
    {
      par(old_par)
      dev.off()
    },
    add = TRUE
  )

  group <- factor(
    score_df$Comparison_Group
  )

  boxplot(
    score_df[[score_column]] ~ group,
    xlab = "",
    ylab = score_column,
    main = paste0(
      paste(
        unique(
          score_df$Analysis_Set
        ),
        collapse = " | "
      ),
      "\n",
      score_column
    )
  )

  stripchart(
    score_df[[score_column]] ~ group,
    vertical = TRUE,
    method = "jitter",
    add = TRUE,
    pch = 16
  )
}

make_roc_plot <- function(
  score_df,
  score_column,
  path
) {
  label <- score_df$Case_Label
  score <- score_df[[score_column]]

  thresholds <- sort(
    unique(
      score[
        is.finite(score)
      ]
    ),
    decreasing = TRUE
  )

  thresholds <- c(
    Inf,
    thresholds,
    -Inf
  )

  roc_rows <- lapply(
    thresholds,
    function(threshold) {
      m <- threshold_metrics(
        label,
        score,
        threshold
      )

      data.frame(
        FPR = 1 - m$Specificity,
        TPR = m$Sensitivity
      )
    }
  )

  roc <- do.call(
    rbind,
    roc_rows
  )

  auc <- auc_rank(
    label,
    score
  )

  pdf(
    path,
    width = 6,
    height = 6
  )

  old_par <- par(no.readonly = TRUE)
  on.exit(
    {
      par(old_par)
      dev.off()
    },
    add = TRUE
  )

  plot(
    roc$FPR,
    roc$TPR,
    type = "l",
    xlim = c(
      0,
      1
    ),
    ylim = c(
      0,
      1
    ),
    xlab = "1 - Specificity",
    ylab = "Sensitivity",
    main = paste0(
      paste(
        unique(
          score_df$Analysis_Set
        ),
        collapse = " | "
      ),
      "\nAUC=",
      round(
        auc,
        3
      )
    )
  )

  abline(
    0,
    1,
    lty = 2
  )
}

# ------------------------------------------------------------
# Read Day 9 meta results only
# ------------------------------------------------------------
meta_file <- file.path(
  day9_report_root,
  "17_Random_Fixed_Meta_Results.csv"
)

if (!file.exists(meta_file)) {
  stop(
    "未找到Day 9 Meta结果：",
    meta_file
  )
}

meta_results <- read.csv(
  meta_file,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

required_meta_columns <- c(
  "Meta_Family",
  "Genus_Feature",
  "K",
  "Direction_Consistent",
  "Strict_Direction_Stable",
  "Fixed_FDR",
  "REML_Effect",
  "REML_FDR",
  "I2",
  "Candidate_Grade"
)

missing_meta_columns <- setdiff(
  required_meta_columns,
  names(meta_results)
)

if (length(missing_meta_columns) > 0) {
  stop(
    "Day 9结果缺少字段：",
    paste(
      missing_meta_columns,
      collapse = ", "
    )
  )
}

icu <- meta_results[
  meta_results$Meta_Family ==
    "ICU_control" &
    meta_results$K == 2 &
    meta_results$Direction_Consistent &
    meta_results$Strict_Direction_Stable &
    is.finite(
      meta_results$REML_Effect
    ),
  ,
  drop = FALSE
]

if (nrow(icu) == 0) {
  stop(
    "没有可用于SDI的ICU方向一致且严格过滤稳定的菌属。"
  )
}

grade_rank <- match(
  icu$Candidate_Grade,
  c(
    "ICU_Tier1_Robust",
    "ICU_Tier2_Supportive",
    "ICU_Tier3_Directional",
    "Insufficient_or_Inconsistent"
  )
)

grade_rank[
  is.na(
    grade_rank
  )
] <- 99

icu$Grade_Rank <- grade_rank
icu$Direction_Sign <- sign(
  icu$REML_Effect
)

icu <- icu[
  order(
    icu$Grade_Rank,
    icu$Fixed_FDR,
    icu$REML_FDR,
    -abs(
      icu$REML_Effect
    )
  ),
  ,
  drop = FALSE
]

core_pool <- icu[
  icu$Candidate_Grade %in%
    c(
      "ICU_Tier1_Robust",
      "ICU_Tier2_Supportive"
    ),
  ,
  drop = FALSE
]

select_direction <- function(
  pool,
  direction,
  maximum_n
) {
  x <- pool[
    pool$Direction_Sign ==
      direction,
    ,
    drop = FALSE
  ]

  head(
    x,
    maximum_n
  )
}

positive_selected <- select_direction(
  core_pool,
  1,
  maximum_positive_genera
)

negative_selected <- select_direction(
  core_pool,
  -1,
  maximum_negative_genera
)

# Add one strict-stable Tier3 balance anchor only when a direction
# is absent after Tier1/Tier2 selection.
if (nrow(positive_selected) == 0) {
  positive_anchor <- select_direction(
    icu[
      icu$Candidate_Grade ==
        "ICU_Tier3_Directional",
      ,
      drop = FALSE
    ],
    1,
    1
  )

  if (nrow(positive_anchor) > 0) {
    positive_anchor$Candidate_Grade <-
      "ICU_Tier3_Balance_Anchor"

    positive_selected <-
      positive_anchor
  }
}

if (nrow(negative_selected) == 0) {
  negative_anchor <- select_direction(
    icu[
      icu$Candidate_Grade ==
        "ICU_Tier3_Directional",
      ,
      drop = FALSE
    ],
    -1,
    1
  )

  if (nrow(negative_anchor) > 0) {
    negative_anchor$Candidate_Grade <-
      "ICU_Tier3_Balance_Anchor"

    negative_selected <-
      negative_anchor
  }
}

if (
  nrow(positive_selected) == 0 ||
    nrow(negative_selected) == 0
) {
  stop(
    paste0(
      "无法构建双向log-balance SDI：",
      "必须至少有1个Sepsis升高菌属和1个Sepsis降低菌属。"
    )
  )
}

selected <- rbind(
  positive_selected,
  negative_selected
)

selected$SDI_Direction <- ifelse(
  selected$REML_Effect > 0,
  "Sepsis_Enriched",
  "Sepsis_Depleted"
)

selected$SDI_Sign <- sign(
  selected$REML_Effect
)

selected$Selection_Role <- ifelse(
  selected$Candidate_Grade ==
    "ICU_Tier1_Robust",
  "Tier1",
  ifelse(
    selected$Candidate_Grade ==
      "ICU_Tier2_Supportive",
    "Tier2",
    "Balance_anchor"
  )
)

positive_features <-
  positive_selected$Genus_Feature

negative_features <-
  negative_selected$Genus_Feature

positive_weights <- abs(
  positive_selected$REML_Effect
)

negative_weights <- abs(
  negative_selected$REML_Effect
)

positive_weights <- positive_weights /
  sum(
    positive_weights
  )

negative_weights <- negative_weights /
  sum(
    negative_weights
  )

selected$Equal_Weight <- ifelse(
  selected$SDI_Sign > 0,
  1 / length(
    positive_features
  ),
  -1 / length(
    negative_features
  )
)

selected$Meta_Balance_Weight <- NA_real_

selected$Meta_Balance_Weight[
  selected$SDI_Sign > 0
] <- positive_weights[
  match(
    selected$Genus_Feature[
      selected$SDI_Sign > 0
    ],
    positive_features
  )
]

selected$Meta_Balance_Weight[
  selected$SDI_Sign < 0
] <- -negative_weights[
  match(
    selected$Genus_Feature[
      selected$SDI_Sign < 0
    ],
    negative_features
  )
]

selected$Locked_Before_External_Open <- "Yes"
selected$Lock_Timestamp <- format(
  Sys.time(),
  "%Y-%m-%d %H:%M:%S"
)

formula_lock <- data.frame(
  Score = c(
    primary_score_name,
    secondary_score_name
  ),
  Status = "LOCKED_BEFORE_EXTERNAL_OPEN",
  Formula = c(
    score_formula_text(
      positive_features,
      negative_features,
      positive_weights,
      negative_weights,
      count_pseudocount,
      FALSE
    ),
    score_formula_text(
      positive_features,
      negative_features,
      positive_weights,
      negative_weights,
      count_pseudocount,
      TRUE
    )
  ),
  Higher_Score_Means =
    "More Sepsis-like dysbiosis",
  Pseudocount = count_pseudocount,
  Positive_Genera_N =
    length(
      positive_features
    ),
  Negative_Genera_N =
    length(
      negative_features
    ),
  Lock_Timestamp = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  stringsAsFactors = FALSE
)

# Write candidate and formula locks before any external set is read.
candidate_lock_file <- file.path(
  lock_root,
  "18_SDI_Candidate_Lock.csv"
)

formula_lock_file <- file.path(
  lock_root,
  "18_SDI_Formula_Lock.csv"
)

wcsv(
  selected,
  candidate_lock_file
)

wcsv(
  formula_lock,
  formula_lock_file
)

candidate_md5 <- unname(
  tools::md5sum(
    candidate_lock_file
  )
)

formula_md5 <- unname(
  tools::md5sum(
    formula_lock_file
  )
)

lock_manifest <- data.frame(
  File = c(
    candidate_lock_file,
    formula_lock_file
  ),
  MD5_Before_External_Open = c(
    candidate_md5,
    formula_md5
  ),
  Timestamp = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  stringsAsFactors = FALSE
)

lock_manifest_file <- file.path(
  lock_root,
  "18_PreExternal_Lock_Manifest.csv"
)

wcsv(
  lock_manifest,
  lock_manifest_file
)

cat(
  "Candidate and formula lock files written before external opening.\n"
)

# ------------------------------------------------------------
# Development scoring: A01/A02 only
# ------------------------------------------------------------
all_candidate_features <- c(
  positive_features,
  negative_features
)

development_scores_list <- list()
development_input_audit <- list()

for (
  i in seq_len(
    nrow(
      development_config
    )
  )
) {
  config_row <- development_config[
    i,
    ,
    drop = FALSE
  ]

  loaded <- load_set_counts(
    config_row,
    all_candidate_features
  )

  development_scores_list[[
    length(
      development_scores_list
    ) + 1
  ]] <- score_dataset(
    loaded,
    config_row,
    positive_features,
    negative_features,
    positive_weights,
    negative_weights
  )

  development_input_audit[[
    length(
      development_input_audit
    ) + 1
  ]] <- data.frame(
    Analysis_Set =
      config_row$Analysis_Set,
    Project_ID =
      config_row$Project_ID,
    Stage = "Development_before_external_open",
    Samples_N =
      nrow(
        loaded$meta
      ),
    Candidate_Genera_N =
      length(
        all_candidate_features
      ),
    Missing_Candidate_Feature_N =
      length(
        loaded$missing_features
      ),
    Missing_Candidate_Features =
      paste(
        loaded$missing_features,
        collapse = " | "
      ),
    Status = if (
      length(
        loaded$missing_features
      ) == 0
    ) {
      "PASS"
    } else {
      "REVIEW_MISSING_AS_ZERO"
    },
    stringsAsFactors = FALSE
  )
}

development_scores <- do.call(
  rbind,
  development_scores_list
)

# Fit development logistic models.
equal_model <- stats::glm(
  Case_Label ~ SDI_Equal_Balance,
  data = development_scores,
  family = stats::binomial()
)

weighted_model <- stats::glm(
  Case_Label ~ SDI_MetaWeighted_Balance,
  data = development_scores,
  family = stats::binomial()
)

equal_threshold <- choose_youden_threshold(
  development_scores$Case_Label,
  development_scores$SDI_Equal_Balance
)

weighted_threshold <- choose_youden_threshold(
  development_scores$Case_Label,
  development_scores$SDI_MetaWeighted_Balance
)

model_lock <- data.frame(
  Score = c(
    primary_score_name,
    secondary_score_name
  ),
  Model = "Univariable logistic regression",
  Intercept = c(
    unname(
      stats::coef(
        equal_model
      )[1]
    ),
    unname(
      stats::coef(
        weighted_model
      )[1]
    )
  ),
  Slope = c(
    unname(
      stats::coef(
        equal_model
      )[2]
    ),
    unname(
      stats::coef(
        weighted_model
      )[2]
    )
  ),
  Locked_Raw_Score_Threshold = c(
    equal_threshold,
    weighted_threshold
  ),
  Threshold_Method =
    "Maximum Youden index in pooled A01+A02 development data",
  Development_Sets =
    "A01+A02",
  External_Refitting =
    "Forbidden",
  Lock_Timestamp = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  stringsAsFactors = FALSE
)

model_lock_file <- file.path(
  lock_root,
  "18_SDI_Model_and_Threshold_Lock.csv"
)

wcsv(
  model_lock,
  model_lock_file
)

saveRDS(
  list(
    equal_model = equal_model,
    weighted_model = weighted_model,
    equal_threshold = equal_threshold,
    weighted_threshold = weighted_threshold,
    positive_features =
      positive_features,
    negative_features =
      negative_features,
    positive_weights =
      positive_weights,
    negative_weights =
      negative_weights,
    pseudocount =
      count_pseudocount
  ),
  file.path(
    lock_root,
    "18_SDI_Locked_Model_Object.rds"
  )
)

model_md5 <- unname(
  tools::md5sum(
    model_lock_file
  )
)

model_manifest <- data.frame(
  File = model_lock_file,
  MD5_Before_External_Open =
    model_md5,
  Timestamp = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  stringsAsFactors = FALSE
)

wcsv(
  model_manifest,
  file.path(
    lock_root,
    "18_PreExternal_Model_Lock_Manifest.csv"
  )
)

cat(
  "Development model and thresholds locked before external opening.\n"
)

# ------------------------------------------------------------
# Internal leave-one-cohort-out threshold validation
# ------------------------------------------------------------
loco_rows <- list()

for (
  train_set in development_config$Analysis_Set
) {
  test_set <- setdiff(
    development_config$Analysis_Set,
    train_set
  )

  train <- development_scores[
    development_scores$Analysis_Set ==
      train_set,
    ,
    drop = FALSE
  ]

  test <- development_scores[
    development_scores$Analysis_Set ==
      test_set,
    ,
    drop = FALSE
  ]

  for (
    score_name in c(
      primary_score_name,
      secondary_score_name
    )
  ) {
    threshold <- choose_youden_threshold(
      train$Case_Label,
      train[[score_name]]
    )

    metrics <- threshold_metrics(
      test$Case_Label,
      test[[score_name]],
      threshold
    )

    loco_rows[[
      length(loco_rows) + 1
    ]] <- data.frame(
      Train_Set = train_set,
      Test_Set = test_set,
      Score = score_name,
      Train_Threshold =
        threshold,
      Test_AUC = auc_rank(
        test$Case_Label,
        test[[score_name]]
      ),
      Test_TP = metrics$TP,
      Test_TN = metrics$TN,
      Test_FP = metrics$FP,
      Test_FN = metrics$FN,
      Test_Sensitivity =
        metrics$Sensitivity,
      Test_Specificity =
        metrics$Specificity,
      Test_Balanced_Accuracy =
        metrics$Balanced_Accuracy,
      stringsAsFactors = FALSE
    )
  }
}

loco_results <- do.call(
  rbind,
  loco_rows
)

# ------------------------------------------------------------
# Verify pre-external locks remain unchanged.
# ------------------------------------------------------------
pre_external_verification <- data.frame(
  File = c(
    candidate_lock_file,
    formula_lock_file,
    model_lock_file
  ),
  MD5_At_Verification = c(
    unname(
      tools::md5sum(
        candidate_lock_file
      )
    ),
    unname(
      tools::md5sum(
        formula_lock_file
      )
    ),
    unname(
      tools::md5sum(
        model_lock_file
      )
    )
  ),
  Expected_MD5 = c(
    candidate_md5,
    formula_md5,
    model_md5
  ),
  Unchanged = c(
    unname(
      tools::md5sum(
        candidate_lock_file
      )
    ) == candidate_md5,
    unname(
      tools::md5sum(
        formula_lock_file
      )
    ) == formula_md5,
    unname(
      tools::md5sum(
        model_lock_file
      )
    ) == model_md5
  ),
  Verified_Before_External_Read =
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
  stringsAsFactors = FALSE
)

wcsv(
  pre_external_verification,
  file.path(
    lock_root,
    "18_PreExternal_Lock_Verification.csv"
  )
)

if (
  any(
    !pre_external_verification$Unchanged
  )
) {
  stop(
    "锁定文件在打开外部验证集前发生变化，停止。"
  )
}

# ------------------------------------------------------------
# External validation opening starts here.
# No selection, refitting, or threshold tuning below this line.
# ------------------------------------------------------------
external_open_timestamp <- format(
  Sys.time(),
  "%Y-%m-%d %H:%M:%S"
)

writeLines(
  c(
    paste0(
      "External validation opened after all candidate, formula, ",
      "model, and threshold locks were written and verified."
    ),
    paste0(
      "Timestamp: ",
      external_open_timestamp
    ),
    "External sets opened: A04 and A05.",
    "No external-data-driven feature selection or refitting permitted."
  ),
  file.path(
    report_root,
    "18_EXTERNAL_VALIDATION_OPENED_AFTER_LOCK.txt"
  )
)

cat(
  "External validation opening begins: ",
  external_open_timestamp,
  "\n",
  sep = ""
)

external_scores_list <- list()
external_input_audit <- list()

for (
  i in seq_len(
    nrow(
      external_config
    )
  )
) {
  config_row <- external_config[
    i,
    ,
    drop = FALSE
  ]

  loaded <- load_set_counts(
    config_row,
    all_candidate_features
  )

  external_scores_list[[
    length(
      external_scores_list
    ) + 1
  ]] <- score_dataset(
    loaded,
    config_row,
    positive_features,
    negative_features,
    positive_weights,
    negative_weights
  )

  external_input_audit[[
    length(
      external_input_audit
    ) + 1
  ]] <- data.frame(
    Analysis_Set =
      config_row$Analysis_Set,
    Project_ID =
      config_row$Project_ID,
    Stage = "External_after_lock",
    Samples_N =
      nrow(
        loaded$meta
      ),
    Candidate_Genera_N =
      length(
        all_candidate_features
      ),
    Missing_Candidate_Feature_N =
      length(
        loaded$missing_features
      ),
    Missing_Candidate_Features =
      paste(
        loaded$missing_features,
        collapse = " | "
      ),
    Status = if (
      length(
        loaded$missing_features
      ) == 0
    ) {
      "PASS"
    } else {
      "REVIEW_MISSING_AS_ZERO"
    },
    stringsAsFactors = FALSE
  )
}

external_scores <- do.call(
  rbind,
  external_scores_list
)

all_scores <- rbind(
  development_scores,
  external_scores
)

# Locked model probabilities.
all_scores$Probability_Equal_Locked <-
  stats::predict(
    equal_model,
    newdata = all_scores,
    type = "response"
  )

all_scores$Probability_MetaWeighted_Locked <-
  stats::predict(
    weighted_model,
    newdata = all_scores,
    type = "response"
  )

# ------------------------------------------------------------
# Performance evaluation
# ------------------------------------------------------------
performance_rows <- list()
threshold_rows <- list()
probability_rows <- list()
group_test_rows <- list()

dataset_definitions <- c(
  "Development_Pooled",
  development_config$Analysis_Set,
  external_config$Analysis_Set
)

for (
  dataset_name in dataset_definitions
) {
  if (
    dataset_name ==
      "Development_Pooled"
  ) {
    z <- all_scores[
      all_scores$Evaluation_Role ==
        "Development_primary",
      ,
      drop = FALSE
    ]
  } else {
    z <- all_scores[
      all_scores$Analysis_Set ==
        dataset_name,
      ,
      drop = FALSE
    ]
  }

  for (
    score_name in c(
      primary_score_name,
      secondary_score_name
    )
  ) {
    locked_threshold <- if (
      score_name ==
        primary_score_name
    ) {
      equal_threshold
    } else {
      weighted_threshold
    }

    locked_probability <- if (
      score_name ==
        primary_score_name
    ) {
      z$Probability_Equal_Locked
    } else {
      z$Probability_MetaWeighted_Locked
    }

    evaluation <- evaluate_score(
      z,
      score_name,
      dataset_name,
      locked_threshold,
      locked_probability
    )

    performance_rows[[
      length(performance_rows) + 1
    ]] <- evaluation$performance

    threshold_rows[[
      length(threshold_rows) + 1
    ]] <- evaluation$threshold

    probability_rows[[
      length(probability_rows) + 1
    ]] <- evaluation$probability

    case_scores <- z[
      z$Case_Label == 1,
      score_name
    ]

    control_scores <- z[
      z$Case_Label == 0,
      score_name
    ]

    wilcox <- safe_wilcox(
      case_scores,
      control_scores
    )

    group_test_rows[[
      length(group_test_rows) + 1
    ]] <- data.frame(
      Dataset = dataset_name,
      Analysis_Set = paste(
        unique(
          z$Analysis_Set
        ),
        collapse = " | "
      ),
      Score = score_name,
      Cases_N = length(
        case_scores
      ),
      Controls_N = length(
        control_scores
      ),
      Median_Cases =
        stats::median(
          case_scores
        ),
      Median_Controls =
        stats::median(
          control_scores
        ),
      Median_Difference =
        stats::median(
          case_scores
        ) -
        stats::median(
          control_scores
        ),
      Cliffs_Delta =
        cliffs_delta(
          case_scores,
          control_scores
        ),
      Wilcoxon_W =
        wilcox["statistic"],
      Wilcoxon_P =
        wilcox["p"],
      stringsAsFactors = FALSE
    )
  }
}

performance_summary <- do.call(
  rbind,
  performance_rows
)

threshold_summary <- do.call(
  rbind,
  threshold_rows
)

probability_summary <- do.call(
  rbind,
  probability_rows
)

group_tests <- do.call(
  rbind,
  group_test_rows
)

group_tests$Wilcoxon_FDR <-
  stats::p.adjust(
    group_tests$Wilcoxon_P,
    method = "BH"
  )

# ------------------------------------------------------------
# Plots
# ------------------------------------------------------------
for (
  set_id in c(
    development_config$Analysis_Set,
    external_config$Analysis_Set
  )
) {
  z <- all_scores[
    all_scores$Analysis_Set ==
      set_id,
    ,
    drop = FALSE
  ]

  make_score_plot(
    z,
    primary_score_name,
    file.path(
      plot_root,
      paste0(
        set_id,
        "_SDI_Equal_Distribution.pdf"
      )
    )
  )

  make_roc_plot(
    z,
    primary_score_name,
    file.path(
      plot_root,
      paste0(
        set_id,
        "_SDI_Equal_ROC.pdf"
      )
    )
  )

  make_score_plot(
    z,
    secondary_score_name,
    file.path(
      plot_root,
      paste0(
        set_id,
        "_SDI_MetaWeighted_Distribution.pdf"
      )
    )
  )

  make_roc_plot(
    z,
    secondary_score_name,
    file.path(
      plot_root,
      paste0(
        set_id,
        "_SDI_MetaWeighted_ROC.pdf"
      )
    )
  )
}

# ------------------------------------------------------------
# External-validation audit
# ------------------------------------------------------------
input_audit <- do.call(
  rbind,
  c(
    development_input_audit,
    external_input_audit
  )
)

post_external_lock_check <- data.frame(
  File = c(
    candidate_lock_file,
    formula_lock_file,
    model_lock_file
  ),
  Expected_MD5 = c(
    candidate_md5,
    formula_md5,
    model_md5
  ),
  MD5_After_External_Evaluation = c(
    unname(
      tools::md5sum(
        candidate_lock_file
      )
    ),
    unname(
      tools::md5sum(
        formula_lock_file
      )
    ),
    unname(
      tools::md5sum(
        model_lock_file
      )
    )
  ),
  Unchanged_After_External_Evaluation =
    c(
      unname(
        tools::md5sum(
          candidate_lock_file
        )
      ) == candidate_md5,
      unname(
        tools::md5sum(
          formula_lock_file
        )
      ) == formula_md5,
      unname(
        tools::md5sum(
          model_lock_file
        )
      ) == model_md5
    ),
  stringsAsFactors = FALSE
)

external_audit <- data.frame(
  Check_Name = c(
    "Candidate_lock_before_external",
    "Formula_lock_before_external",
    "Model_and_threshold_lock_before_external",
    "External_feature_selection",
    "External_coefficient_refitting",
    "External_threshold_retuning",
    "A04_read_only_after_lock",
    "A05_read_only_after_lock",
    "Primary_external_comparison",
    "Post_external_lock_unchanged"
  ),
  Status = c(
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    if (
      all(
        post_external_lock_check$
          Unchanged_After_External_Evaluation
      )
    ) {
      "PASS"
    } else {
      "FAIL"
    }
  ),
  Details = c(
    "Candidate genera were selected from Day 9 ICU-control meta-analysis only.",
    "Equal and meta-weighted balance formulas were written before external opening.",
    "Logistic coefficients and raw-score thresholds were locked using A01+A02 only.",
    "No feature was selected using A04/A05.",
    "No coefficient was refitted using A04/A05.",
    "No threshold was retuned using A04/A05.",
    "A04 was opened after lock verification.",
    "A05 was opened after lock verification.",
    "A05 Sepsis vs Trauma is the primary external clinical comparison.",
    "Candidate, formula, and model lock files remained unchanged."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Final outputs
# ------------------------------------------------------------
wcsv(
  all_scores,
  file.path(
    output_root,
    "18_All_SDI_Sample_Scores.csv"
  )
)

wcsv(
  input_audit,
  file.path(
    report_root,
    "18_SDI_Input_Audit.csv"
  )
)

wcsv(
  performance_summary,
  file.path(
    report_root,
    "18_SDI_Performance_Summary.csv"
  )
)

wcsv(
  threshold_summary,
  file.path(
    report_root,
    "18_SDI_Locked_Threshold_Metrics.csv"
  )
)

wcsv(
  probability_summary,
  file.path(
    report_root,
    "18_SDI_Locked_Model_Probability_Metrics.csv"
  )
)

wcsv(
  group_tests,
  file.path(
    report_root,
    "18_SDI_Group_Comparison_Tests.csv"
  )
)

wcsv(
  loco_results,
  file.path(
    report_root,
    "18_SDI_Internal_LOCO_Validation.csv"
  )
)

wcsv(
  post_external_lock_check,
  file.path(
    report_root,
    "18_PostExternal_Lock_Check.csv"
  )
)

wcsv(
  external_audit,
  file.path(
    report_root,
    "18_SDI_External_Validation_Audit.csv"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    report_root,
    "18_SDI_External_Validation_SessionInfo.txt"
  )
)

files_to_hash <- unique(c(
  meta_file,
  candidate_lock_file,
  formula_lock_file,
  model_lock_file,
  file.path(
    day7_set_root,
    paste0(
      c(
        development_config$Analysis_Set,
        external_config$Analysis_Set
      ),
      ".csv"
    )
  ),
  file.path(
    day6_root,
    c(
      development_config$Project_ID,
      external_config$Project_ID
    ),
    paste0(
      c(
        development_config$Project_ID,
        external_config$Project_ID
      ),
      "_genus_classified_counts.csv"
    )
  ),
  list.files(
    output_root,
    full.names = TRUE,
    recursive = TRUE
  ),
  list.files(
    report_root,
    full.names = TRUE,
    recursive = TRUE
  )
))

writeLines(
  files_to_hash,
  file.path(
    report_root,
    "18_Files_To_Hash.txt"
  ),
  useBytes = TRUE
)

cat(
  "\n============================================================\n"
)
cat("Day 10 completed.\n")
cat(
  "Locked candidate and formula files: ",
  lock_root,
  "\n",
  sep = ""
)
cat(
  "Numerical outputs: ",
  output_root,
  "\n",
  sep = ""
)
cat(
  "Reports and plots: ",
  report_root,
  "\n",
  sep = ""
)
cat("Important outputs:\n")
cat("  locked_before_external_open/18_SDI_Candidate_Lock.csv\n")
cat("  locked_before_external_open/18_SDI_Formula_Lock.csv\n")
cat("  locked_before_external_open/18_SDI_Model_and_Threshold_Lock.csv\n")
cat("  18_SDI_Performance_Summary.csv\n")
cat("  18_SDI_Locked_Threshold_Metrics.csv\n")
cat("  18_SDI_Internal_LOCO_Validation.csv\n")
cat("  18_SDI_External_Validation_Audit.csv\n")
cat(
  "Finished: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)
