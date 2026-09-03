# PUBLIC RELEASE v1.0.0
# Scientific logic preserved from the independently verified historical script19.
# Public-release change: the historical local project root was replaced by
# SEPSIS_V1_PROJECT_ROOT/getwd(); statistical logic is otherwise unchanged.

options(stringsAsFactors = FALSE)

# ============================================================
# Day 11: Diagnose SDI external-validation failure
#
# This is a POST-HOC EXPLORATORY diagnostic workflow.
#
# It does NOT:
# - change the locked Day 10 candidate list;
# - change locked directions or weights;
# - refit the locked model;
# - retune the locked threshold;
# - relabel the failed external validation as successful.
#
# It DOES:
# 1. Audit each locked genus across A01/A02/A04/A05.
# 2. Identify project-level feature absence and direction reversal.
# 3. Decompose the SDI group difference into genus contributions.
# 4. Run leave-one-genus-out sensitivity analyses.
# 5. Test post-hoc exploratory score variants:
#    - project-present-feature balance;
#    - all-project-common-feature balance;
#    - within-sample rank balance.
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

day10_report_root <- file.path(
  root,
  "manuscript/03_analysis/18_sdi_external_validation"
)

day10_lock_root <- file.path(
  day10_report_root,
  "locked_before_external_open"
)

day10_score_file <- file.path(
  root,
  "processed_data/day10_sdi_external_validation_v1",
  "18_All_SDI_Sample_Scores.csv"
)

output_root <- file.path(
  root,
  "processed_data/day11_sdi_failure_diagnosis_v1"
)

report_root <- file.path(
  root,
  "manuscript/03_analysis/19_sdi_failure_diagnosis"
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
    "19_SDI_Failure_Diagnosis_Log.txt"
  ),
  split = TRUE
)
on.exit(sink(), add = TRUE)

cat(
  "Day 11 diagnosis started: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)

set.seed(20260731)

# ------------------------------------------------------------
# Parameters
# ------------------------------------------------------------
pseudocount <- 0.5
bootstrap_replicates <- 2000

parameters <- data.frame(
  Parameter = c(
    "Analysis_status",
    "Sets_read",
    "Locked_candidate_source",
    "Pseudocount",
    "Bootstrap_replicates",
    "External_labels_used",
    "Confirmatory_claim_allowed",
    "Model_refitting_allowed",
    "Threshold_retuning_allowed"
  ),
  Value = c(
    "POSTHOC_EXPLORATORY_DIAGNOSIS",
    "A01;A02;A04;A05",
    "Day10 locked candidate file",
    pseudocount,
    bootstrap_replicates,
    "Yes, for failure diagnosis only",
    "No",
    "No",
    "No"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  parameters,
  file.path(
    report_root,
    "19_Diagnostic_Parameters.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# Set configuration
# ------------------------------------------------------------
set_config <- data.frame(
  Analysis_Set = c(
    "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
    "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
    "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
    "A05_PRJNA1010969_External_Sepsis_vs_Trauma"
  ),
  Project_ID = c(
    "PRJEB33360",
    "PRJNA691455",
    "PRJNA1010969",
    "PRJNA1010969"
  ),
  Stage = c(
    "Development",
    "Development",
    "External",
    "External"
  ),
  Group_A = "Sepsis",
  Group_B = c(
    "Non-sepsis ICU",
    "Non-sepsis ICU",
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

  n_case <- sum(label == 1)
  n_control <- sum(label == 0)

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
    n_case * n_control
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

  case_index <- which(
    label == 1
  )

  control_index <- which(
    label == 0
  )

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
    index <- c(
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
      label[index],
      score[index]
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

cliffs_delta <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]

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
    sum(differences > 0) -
      sum(differences < 0)
  ) / (
    length(x) * length(y)
  )
}

safe_wilcox <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]

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
      exact = FALSE,
      correct = TRUE
    )
  )

  c(
    statistic = unname(
      fit$statistic
    ),
    p = fit$p.value
  )
}

welch_effect <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]

  n_x <- length(x)
  n_y <- length(y)

  if (
    n_x < 2 ||
      n_y < 2
  ) {
    return(
      c(
        effect = NA_real_,
        se = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p = NA_real_
      )
    )
  }

  var_x <- stats::var(x)
  var_y <- stats::var(y)

  if (!is.finite(var_x)) var_x <- 0
  if (!is.finite(var_y)) var_y <- 0

  effect <- mean(x) - mean(y)
  se <- sqrt(
    var_x / n_x +
      var_y / n_y
  )

  if (
    !is.finite(se) ||
      se <= 0
  ) {
    return(
      c(
        effect = effect,
        se = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p = NA_real_
      )
    )
  }

  denominator <- (
    (var_x / n_x)^2 /
      (n_x - 1)
  ) + (
    (var_y / n_y)^2 /
      (n_y - 1)
  )

  df <- if (
    is.finite(denominator) &&
      denominator > 0
  ) {
    (
      var_x / n_x +
        var_y / n_y
    )^2 /
      denominator
  } else {
    NA_real_
  }

  statistic <- effect / se

  p <- if (
    is.finite(df)
  ) {
    2 * stats::pt(
      abs(statistic),
      df = df,
      lower.tail = FALSE
    )
  } else {
    2 * stats::pnorm(
      abs(statistic),
      lower.tail = FALSE
    )
  }

  c(
    effect = effect,
    se = se,
    ci_low =
      effect - 1.96 * se,
    ci_high =
      effect + 1.96 * se,
    p = p
  )
}

rank_rows <- function(matrix_data) {
  if (ncol(matrix_data) == 1) {
    return(
      matrix(
        0.5,
        nrow = nrow(matrix_data),
        ncol = 1,
        dimnames = dimnames(
          matrix_data
        )
      )
    )
  }

  ranked <- t(
    apply(
      matrix_data,
      1,
      function(x) {
        (
          rank(
            x,
            ties.method = "average"
          ) - 1
        ) / (
          length(x) - 1
        )
      }
    )
  )

  colnames(ranked) <- colnames(
    matrix_data
  )

  rownames(ranked) <- rownames(
    matrix_data
  )

  ranked
}

balance_score <- function(
  value_matrix,
  positive_features,
  negative_features,
  positive_weights = NULL,
  negative_weights = NULL
) {
  if (
    length(positive_features) == 0 ||
      length(negative_features) == 0
  ) {
    return(
      rep(
        NA_real_,
        nrow(value_matrix)
      )
    )
  }

  if (is.null(positive_weights)) {
    positive_component <- rowMeans(
      value_matrix[
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
      value_matrix[
        ,
        positive_features,
        drop = FALSE
      ] %*%
        positive_weights
    )
  }

  if (is.null(negative_weights)) {
    negative_component <- rowMeans(
      value_matrix[
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
      value_matrix[
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

evaluate_score <- function(
  score_df,
  score_column,
  variant_status
) {
  label <- score_df$Case_Label
  score <- score_df[[score_column]]

  case_score <- score[
    label == 1
  ]

  control_score <- score[
    label == 0
  ]

  auc <- auc_rank(
    label,
    score
  )

  auc_ci <- bootstrap_auc(
    label,
    score,
    bootstrap_replicates
  )

  wilcox <- safe_wilcox(
    case_score,
    control_score
  )

  data.frame(
    Analysis_Status =
      variant_status,
    Analysis_Set =
      unique(
        score_df$Analysis_Set
      ),
    Project_ID =
      unique(
        score_df$Project_ID
      ),
    Stage =
      unique(
        score_df$Stage
      ),
    Comparison = paste0(
      "Sepsis vs ",
      unique(
        score_df$Control_Group
      )
    ),
    Score_Variant =
      score_column,
    N = nrow(score_df),
    Cases_N = sum(
      label == 1
    ),
    Controls_N = sum(
      label == 0
    ),
    AUC = auc,
    AUC_CI_Low =
      auc_ci[1],
    AUC_CI_High =
      auc_ci[2],
    Mean_Cases = mean(
      case_score
    ),
    Mean_Controls = mean(
      control_score
    ),
    Median_Cases =
      stats::median(
        case_score
      ),
    Median_Controls =
      stats::median(
        control_score
      ),
    Median_Difference =
      stats::median(
        case_score
      ) -
      stats::median(
        control_score
      ),
    Cliffs_Delta =
      cliffs_delta(
        case_score,
        control_score
      ),
    Wilcoxon_W =
      wilcox["statistic"],
    Wilcoxon_P =
      wilcox["p"],
    stringsAsFactors = FALSE
  )
}

load_set <- function(
  config_row,
  candidates
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
      "未找到count表：",
      count_file
    )
  }

  meta <- read.csv(
    set_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  counts_df <- read.csv(
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
      set_id,
      " 缺少字段：",
      paste(
        missing_required,
        collapse = ", "
      )
    )
  }

  if (
    !"Run_ID" %in%
      names(counts_df)
  ) {
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

  meta$Comparison_Group <-
    clean_text(
      meta$Comparison_Group
    )

  counts_df$Run_ID <- clean_text(
    counts_df$Run_ID
  )

  if (
    anyDuplicated(
      meta$Run_ID
    )
  ) {
    stop(
      set_id,
      " 存在重复Run_ID。"
    )
  }

  index <- match(
    meta$Run_ID,
    counts_df$Run_ID
  )

  if (
    any(
      is.na(index)
    )
  ) {
    stop(
      set_id,
      " 有Run无法匹配count表。"
    )
  }

  present <- candidates %in%
    names(counts_df)

  candidate_matrix <- matrix(
    0,
    nrow = nrow(meta),
    ncol = length(candidates),
    dimnames = list(
      meta$Run_ID,
      candidates
    )
  )

  present_features <- candidates[
    present
  ]

  if (
    length(
      present_features
    ) > 0
  ) {
    present_matrix <-
      safe_numeric_matrix(
        counts_df[
          index,
          present_features,
          drop = FALSE
        ]
      )

    candidate_matrix[
      ,
      present_features
    ] <- present_matrix
  }

  if (
    any(
      !is.finite(
        candidate_matrix
      )
    ) ||
      any(
        candidate_matrix < 0
      )
  ) {
    stop(
      set_id,
      " candidate矩阵包含NA、无限值或负数。"
    )
  }

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

  if (
    any(
      is.na(
        case_label
      )
    )
  ) {
    stop(
      set_id,
      " 包含预期比较之外的分组。"
    )
  }

  list(
    meta = meta,
    counts = candidate_matrix,
    present = setNames(
      present,
      candidates
    ),
    case_label = case_label,
    set_file = set_file,
    count_file = count_file
  )
}

# ------------------------------------------------------------
# Read locked Day 10 candidate definition
# ------------------------------------------------------------
candidate_file <- file.path(
  day10_lock_root,
  "18_SDI_Candidate_Lock.csv"
)

formula_file <- file.path(
  day10_lock_root,
  "18_SDI_Formula_Lock.csv"
)

model_file <- file.path(
  day10_lock_root,
  "18_SDI_Model_and_Threshold_Lock.csv"
)

for (
  path in c(
    candidate_file,
    formula_file,
    model_file
  )
) {
  if (!file.exists(path)) {
    stop(
      "缺少Day 10锁定文件：",
      path
    )
  }
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
  "Equal_Weight",
  "Meta_Balance_Weight",
  "SDI_Direction",
  "Selection_Role",
  "REML_Effect"
)

missing_candidate_columns <- setdiff(
  required_candidate_columns,
  names(candidate_lock)
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

candidates <- candidate_lock$Genus_Feature

positive_features <- candidate_lock$Genus_Feature[
  candidate_lock$SDI_Sign > 0
]

negative_features <- candidate_lock$Genus_Feature[
  candidate_lock$SDI_Sign < 0
]

positive_meta_weights <- abs(
  candidate_lock$Meta_Balance_Weight[
    candidate_lock$SDI_Sign > 0
  ]
)

negative_meta_weights <- abs(
  candidate_lock$Meta_Balance_Weight[
    candidate_lock$SDI_Sign < 0
  ]
)

expected_sign <- setNames(
  candidate_lock$SDI_Sign,
  candidate_lock$Genus_Feature
)

equal_weight <- setNames(
  candidate_lock$Equal_Weight,
  candidate_lock$Genus_Feature
)

meta_weight <- setNames(
  candidate_lock$Meta_Balance_Weight,
  candidate_lock$Genus_Feature
)

lock_hash_before <- unname(
  tools::md5sum(
    c(
      candidate_file,
      formula_file,
      model_file
    )
  )
)

# ------------------------------------------------------------
# Load four sets
# ------------------------------------------------------------
loaded_sets <- list()

for (
  i in seq_len(
    nrow(set_config)
  )
) {
  set_id <- set_config$Analysis_Set[i]

  loaded_sets[[set_id]] <-
    load_set(
      set_config[
        i,
        ,
        drop = FALSE
      ],
      candidates
    )

  cat(
    "Loaded ",
    set_id,
    ": ",
    nrow(
      loaded_sets[[set_id]]$meta
    ),
    " samples; present candidates=",
    sum(
      loaded_sets[[set_id]]$present
    ),
    "/",
    length(candidates),
    "\n",
    sep = ""
  )
}

# ------------------------------------------------------------
# Detectability audit
# ------------------------------------------------------------
detectability_rows <- list()

for (
  i in seq_len(
    nrow(set_config)
  )
) {
  config_row <- set_config[
    i,
    ,
    drop = FALSE
  ]

  set_id <- config_row$Analysis_Set
  loaded <- loaded_sets[[set_id]]

  for (
    feature in candidates
  ) {
    count <- loaded$counts[
      ,
      feature
    ]

    case_index <- loaded$case_label == 1
    control_index <- loaded$case_label == 0

    detectability_rows[[
      length(detectability_rows) + 1
    ]] <- data.frame(
      Analysis_Status =
        "POSTHOC_EXPLORATORY_DIAGNOSIS",
      Analysis_Set = set_id,
      Project_ID =
        config_row$Project_ID,
      Stage =
        config_row$Stage,
      Control_Group =
        config_row$Group_B,
      Genus_Feature =
        feature,
      Expected_SDI_Sign =
        expected_sign[feature],
      Expected_Direction = if (
        expected_sign[feature] > 0
      ) {
        "Higher_in_Sepsis"
      } else {
        "Lower_in_Sepsis"
      },
      Column_Present =
        loaded$present[feature],
      Cases_N = sum(
        case_index
      ),
      Controls_N = sum(
        control_index
      ),
      Detected_Cases = sum(
        count[
          case_index
        ] > 0
      ),
      Detected_Controls = sum(
        count[
          control_index
        ] > 0
      ),
      Prevalence_Cases = mean(
        count[
          case_index
        ] > 0
      ),
      Prevalence_Controls = mean(
        count[
          control_index
        ] > 0
      ),
      Total_Count = sum(
        count
      ),
      All_Zero = all(
        count == 0
      ),
      stringsAsFactors = FALSE
    )
  }
}

detectability <- do.call(
  rbind,
  detectability_rows
)

# ------------------------------------------------------------
# Candidate-level direction and contribution audit
# ------------------------------------------------------------
candidate_effect_rows <- list()

for (
  i in seq_len(
    nrow(set_config)
  )
) {
  config_row <- set_config[
    i,
    ,
    drop = FALSE
  ]

  set_id <- config_row$Analysis_Set
  loaded <- loaded_sets[[set_id]]

  log_counts <- log(
    loaded$counts +
      pseudocount
  )

  case_index <- loaded$case_label == 1
  control_index <- loaded$case_label == 0

  for (
    feature in candidates
  ) {
    case_value <- log_counts[
      case_index,
      feature
    ]

    control_value <- log_counts[
      control_index,
      feature
    ]

    effect <- welch_effect(
      case_value,
      control_value
    )

    signed_score <- expected_sign[
      feature
    ] * log_counts[
      ,
      feature
    ]

    signed_auc <- auc_rank(
      loaded$case_label,
      signed_score
    )

    signed_auc_ci <- bootstrap_auc(
      loaded$case_label,
      signed_score,
      bootstrap_replicates
    )

    wilcox <- safe_wilcox(
      case_value,
      control_value
    )

    raw_direction <- if (
      is.finite(
        effect["effect"]
      )
    ) {
      if (
        effect["effect"] > 0
      ) {
        "Higher_in_Sepsis"
      } else if (
        effect["effect"] < 0
      ) {
        "Lower_in_Sepsis"
      } else {
        "No_direction"
      }
    } else {
      "Not_estimable"
    }

    expected_signed_effect <-
      expected_sign[feature] *
      effect["effect"]

    candidate_effect_rows[[
      length(candidate_effect_rows) + 1
    ]] <- data.frame(
      Analysis_Status =
        "POSTHOC_EXPLORATORY_DIAGNOSIS",
      Analysis_Set = set_id,
      Project_ID =
        config_row$Project_ID,
      Stage =
        config_row$Stage,
      Control_Group =
        config_row$Group_B,
      Genus_Feature =
        feature,
      Selection_Role =
        candidate_lock$Selection_Role[
          match(
            feature,
            candidate_lock$Genus_Feature
          )
        ],
      Expected_SDI_Sign =
        expected_sign[feature],
      Expected_Direction = if (
        expected_sign[feature] > 0
      ) {
        "Higher_in_Sepsis"
      } else {
        "Lower_in_Sepsis"
      },
      Column_Present =
        loaded$present[feature],
      Mean_Log_Count_Cases =
        mean(
          case_value
        ),
      Mean_Log_Count_Controls =
        mean(
          control_value
        ),
      Raw_Log_Effect_Case_minus_Control =
        effect["effect"],
      Raw_Log_Effect_SE =
        effect["se"],
      Raw_Log_Effect_CI_Low =
        effect["ci_low"],
      Raw_Log_Effect_CI_High =
        effect["ci_high"],
      Raw_Log_Effect_P =
        effect["p"],
      Observed_Raw_Direction =
        raw_direction,
      Expected_Signed_Effect =
        expected_signed_effect,
      Direction_Matches_Lock =
        is.finite(
          expected_signed_effect
        ) &&
        expected_signed_effect > 0,
      Individual_Signed_AUC =
        signed_auc,
      Individual_Signed_AUC_CI_Low =
        signed_auc_ci[1],
      Individual_Signed_AUC_CI_High =
        signed_auc_ci[2],
      Wilcoxon_W =
        wilcox["statistic"],
      Wilcoxon_P =
        wilcox["p"],
      Equal_SDI_Component_Difference =
        equal_weight[feature] *
        effect["effect"],
      MetaWeighted_SDI_Component_Difference =
        meta_weight[feature] *
        effect["effect"],
      stringsAsFactors = FALSE
    )
  }
}

candidate_effects <- do.call(
  rbind,
  candidate_effect_rows
)

candidate_effects$Raw_Log_Effect_FDR <-
  ave(
    candidate_effects$Raw_Log_Effect_P,
    candidate_effects$Analysis_Set,
    FUN = function(p) {
      stats::p.adjust(
        p,
        method = "BH"
      )
    }
  )

candidate_effects$Wilcoxon_FDR <-
  ave(
    candidate_effects$Wilcoxon_P,
    candidate_effects$Analysis_Set,
    FUN = function(p) {
      stats::p.adjust(
        p,
        method = "BH"
      )
    }
  )

# ------------------------------------------------------------
# Candidate-level cross-set diagnosis
# ------------------------------------------------------------
direction_summary_rows <- list()

for (
  feature in candidates
) {
  z <- candidate_effects[
    candidate_effects$Genus_Feature ==
      feature,
    ,
    drop = FALSE
  ]

  development <- z[
    z$Stage ==
      "Development",
    ,
    drop = FALSE
  ]

  external <- z[
    z$Stage ==
      "External",
    ,
    drop = FALSE
  ]

  detect_z <- detectability[
    detectability$Genus_Feature ==
      feature,
    ,
    drop = FALSE
  ]

  direction_summary_rows[[
    length(direction_summary_rows) + 1
  ]] <- data.frame(
    Genus_Feature = feature,
    Expected_Direction =
      unique(
        z$Expected_Direction
      ),
    Selection_Role =
      unique(
        z$Selection_Role
      ),
    Development_Direction_Match_N =
      sum(
        development$Direction_Matches_Lock,
        na.rm = TRUE
      ),
    Development_Sets_N =
      nrow(
        development
      ),
    External_Direction_Match_N =
      sum(
        external$Direction_Matches_Lock,
        na.rm = TRUE
      ),
    External_Sets_N =
      nrow(
        external
      ),
    External_Complete_Reversal =
      nrow(
        external
      ) > 0 &&
      all(
        !external$Direction_Matches_Lock
      ),
    Missing_In_External_Project =
      any(
        external$Column_Present ==
          FALSE
      ),
    External_All_Zero =
      any(
        detect_z$Stage ==
          "External" &
          detect_z$All_Zero
      ),
    Mean_Development_Expected_Signed_Effect =
      mean(
        development$
          Expected_Signed_Effect,
        na.rm = TRUE
      ),
    Mean_External_Expected_Signed_Effect =
      mean(
        external$
          Expected_Signed_Effect,
        na.rm = TRUE
      ),
    Mean_External_Equal_Component =
      mean(
        external$
          Equal_SDI_Component_Difference,
        na.rm = TRUE
      ),
    Mean_External_MetaWeighted_Component =
      mean(
        external$
          MetaWeighted_SDI_Component_Difference,
        na.rm = TRUE
      ),
    Diagnostic_Label = if (
      any(
        external$Column_Present ==
          FALSE
      )
    ) {
      "PROJECT_LEVEL_FEATURE_ABSENCE"
    } else if (
      nrow(
        external
      ) > 0 &&
        all(
          !external$Direction_Matches_Lock
        )
    ) {
      "COMPLETE_EXTERNAL_DIRECTION_REVERSAL"
    } else if (
      nrow(
        external
      ) > 0 &&
        any(
          !external$Direction_Matches_Lock
        )
    ) {
      "PARTIAL_EXTERNAL_DIRECTION_REVERSAL"
    } else {
      "EXTERNAL_DIRECTION_CONCORDANT"
    },
    stringsAsFactors = FALSE
  )
}

direction_summary <- do.call(
  rbind,
  direction_summary_rows
)

# ------------------------------------------------------------
# Determine all-project-common features
# ------------------------------------------------------------
project_presence <- aggregate(
  Column_Present ~
    Project_ID +
    Genus_Feature,
  data = detectability,
  FUN = all
)

common_presence <- aggregate(
  Column_Present ~
    Genus_Feature,
  data = project_presence,
  FUN = all
)

common_features <- common_presence$
  Genus_Feature[
    common_presence$Column_Present
  ]

common_positive <- intersect(
  positive_features,
  common_features
)

common_negative <- intersect(
  negative_features,
  common_features
)

if (
  length(
    common_positive
  ) == 0 ||
    length(
      common_negative
    ) == 0
) {
  warning(
    paste0(
      "所有项目共同可检测特征无法形成双向balance；",
      "CommonFeature score将为NA。"
    )
  )
}

common_positive_weights <-
  positive_meta_weights[
    match(
      common_positive,
      positive_features
    )
  ]

common_negative_weights <-
  negative_meta_weights[
    match(
      common_negative,
      negative_features
    )
  ]

# ------------------------------------------------------------
# Recompute score variants
# ------------------------------------------------------------
score_rows <- list()

for (
  i in seq_len(
    nrow(set_config)
  )
) {
  config_row <- set_config[
    i,
    ,
    drop = FALSE
  ]

  set_id <- config_row$Analysis_Set
  loaded <- loaded_sets[[set_id]]

  log_counts <- log(
    loaded$counts +
      pseudocount
  )

  rank_all <- rank_rows(
    log_counts
  )

  present_features <- names(
    loaded$present
  )[
    loaded$present
  ]

  present_positive <- intersect(
    positive_features,
    present_features
  )

  present_negative <- intersect(
    negative_features,
    present_features
  )

  present_positive_weights <-
    positive_meta_weights[
      match(
        present_positive,
        positive_features
      )
    ]

  present_negative_weights <-
    negative_meta_weights[
      match(
        present_negative,
        negative_features
      )
    ]

  locked_equal <- balance_score(
    log_counts,
    positive_features,
    negative_features
  )

  locked_weighted <- balance_score(
    log_counts,
    positive_features,
    negative_features,
    positive_meta_weights,
    negative_meta_weights
  )

  project_present_equal <-
    balance_score(
      log_counts,
      present_positive,
      present_negative
    )

  project_present_weighted <-
    balance_score(
      log_counts,
      present_positive,
      present_negative,
      present_positive_weights,
      present_negative_weights
    )

  common_equal <- balance_score(
    log_counts,
    common_positive,
    common_negative
  )

  common_weighted <- balance_score(
    log_counts,
    common_positive,
    common_negative,
    common_positive_weights,
    common_negative_weights
  )

  locked_rank <- balance_score(
    rank_all,
    positive_features,
    negative_features
  )

  if (
    length(
      common_features
    ) > 0
  ) {
    rank_common <- rank_rows(
      log_counts[
        ,
        common_features,
        drop = FALSE
      ]
    )
  } else {
    rank_common <- matrix(
      NA_real_,
      nrow = nrow(
        log_counts
      ),
      ncol = 0
    )
  }

  common_rank <- balance_score(
    rank_common,
    common_positive,
    common_negative
  )

  score_rows[[
    length(score_rows) + 1
  ]] <- data.frame(
    Analysis_Status =
      "POSTHOC_EXPLORATORY_DIAGNOSIS",
    Analysis_Set = set_id,
    Project_ID =
      config_row$Project_ID,
    Stage =
      config_row$Stage,
    Control_Group =
      config_row$Group_B,
    Run_ID =
      loaded$meta$Run_ID,
    Patient_ID =
      loaded$meta$Patient_ID,
    Comparison_Group =
      loaded$meta$Comparison_Group,
    Case_Label =
      loaded$case_label,
    Locked_Equal_Balance =
      locked_equal,
    Locked_MetaWeighted_Balance =
      locked_weighted,
    ProjectPresent_Equal_Balance =
      project_present_equal,
    ProjectPresent_MetaWeighted_Balance =
      project_present_weighted,
    CommonFeature_Equal_Balance =
      common_equal,
    CommonFeature_MetaWeighted_Balance =
      common_weighted,
    LockedFeature_Rank_Balance =
      locked_rank,
    CommonFeature_Rank_Balance =
      common_rank,
    Project_Present_Positive_N =
      length(
        present_positive
      ),
    Project_Present_Negative_N =
      length(
        present_negative
      ),
    Common_Positive_N =
      length(
        common_positive
      ),
    Common_Negative_N =
      length(
        common_negative
      ),
    stringsAsFactors = FALSE
  )
}

all_scores <- do.call(
  rbind,
  score_rows
)

# Optional numerical audit against Day 10 locked scores.
score_reconstruction_audit <- data.frame()

if (
  file.exists(
    day10_score_file
  )
) {
  day10_scores <- read.csv(
    day10_score_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  compare <- merge(
    all_scores[
      ,
      c(
        "Analysis_Set",
        "Run_ID",
        "Locked_Equal_Balance",
        "Locked_MetaWeighted_Balance"
      )
    ],
    day10_scores[
      ,
      c(
        "Analysis_Set",
        "Run_ID",
        "SDI_Equal_Balance",
        "SDI_MetaWeighted_Balance"
      )
    ],
    by = c(
      "Analysis_Set",
      "Run_ID"
    ),
    all.x = TRUE,
    sort = FALSE
  )

  compare$Equal_Difference <-
    compare$Locked_Equal_Balance -
    compare$SDI_Equal_Balance

  compare$Weighted_Difference <-
    compare$Locked_MetaWeighted_Balance -
    compare$SDI_MetaWeighted_Balance

  score_reconstruction_audit <-
    aggregate(
      cbind(
        Absolute_Equal_Difference =
          abs(
            compare$Equal_Difference
          ),
        Absolute_Weighted_Difference =
          abs(
            compare$Weighted_Difference
          )
      ) ~ Analysis_Set,
      data = compare,
      FUN = max,
      na.rm = TRUE
    )

  score_reconstruction_audit$Status <-
    ifelse(
      score_reconstruction_audit$
        Absolute_Equal_Difference <
        1e-10 &
        score_reconstruction_audit$
          Absolute_Weighted_Difference <
          1e-10,
      "PASS",
      "FAIL"
    )
}

# ------------------------------------------------------------
# Evaluate score variants
# ------------------------------------------------------------
score_variants <- c(
  "Locked_Equal_Balance",
  "Locked_MetaWeighted_Balance",
  "ProjectPresent_Equal_Balance",
  "ProjectPresent_MetaWeighted_Balance",
  "CommonFeature_Equal_Balance",
  "CommonFeature_MetaWeighted_Balance",
  "LockedFeature_Rank_Balance",
  "CommonFeature_Rank_Balance"
)

score_performance_rows <- list()

for (
  set_id in unique(
    all_scores$Analysis_Set
  )
) {
  z <- all_scores[
    all_scores$Analysis_Set ==
      set_id,
    ,
    drop = FALSE
  ]

  for (
    score_name in score_variants
  ) {
    status <- if (
      score_name %in%
        c(
          "Locked_Equal_Balance",
          "Locked_MetaWeighted_Balance"
        )
    ) {
      "CONFIRMATORY_REFERENCE_RECALCULATION"
    } else {
      "POSTHOC_EXPLORATORY_NOT_VALIDATION"
    }

    score_performance_rows[[
      length(score_performance_rows) + 1
    ]] <- evaluate_score(
      z,
      score_name,
      status
    )
  }
}

score_performance <- do.call(
  rbind,
  score_performance_rows
)

score_performance$Wilcoxon_FDR <-
  ave(
    score_performance$Wilcoxon_P,
    score_performance$Analysis_Set,
    FUN = function(p) {
      stats::p.adjust(
        p,
        method = "BH"
      )
    }
  )

# ------------------------------------------------------------
# Leave-one-genus-out sensitivity
# ------------------------------------------------------------
loo_rows <- list()

for (
  i in seq_len(
    nrow(set_config)
  )
) {
  config_row <- set_config[
    i,
    ,
    drop = FALSE
  ]

  set_id <- config_row$Analysis_Set
  loaded <- loaded_sets[[set_id]]

  log_counts <- log(
    loaded$counts +
      pseudocount
  )

  full_equal <- balance_score(
    log_counts,
    positive_features,
    negative_features
  )

  full_weighted <- balance_score(
    log_counts,
    positive_features,
    negative_features,
    positive_meta_weights,
    negative_meta_weights
  )

  full_equal_auc <- auc_rank(
    loaded$case_label,
    full_equal
  )

  full_weighted_auc <- auc_rank(
    loaded$case_label,
    full_weighted
  )

  for (
    omitted in candidates
  ) {
    positive_remaining <- setdiff(
      positive_features,
      omitted
    )

    negative_remaining <- setdiff(
      negative_features,
      omitted
    )

    if (
      length(
        positive_remaining
      ) == 0 ||
        length(
          negative_remaining
        ) == 0
    ) {
      next
    }

    positive_remaining_weights <-
      positive_meta_weights[
        match(
          positive_remaining,
          positive_features
        )
      ]

    negative_remaining_weights <-
      negative_meta_weights[
        match(
          negative_remaining,
          negative_features
        )
      ]

    equal_score <- balance_score(
      log_counts,
      positive_remaining,
      negative_remaining
    )

    weighted_score <- balance_score(
      log_counts,
      positive_remaining,
      negative_remaining,
      positive_remaining_weights,
      negative_remaining_weights
    )

    loo_rows[[
      length(loo_rows) + 1
    ]] <- data.frame(
      Analysis_Status =
        "POSTHOC_EXPLORATORY_DIAGNOSIS",
      Analysis_Set = set_id,
      Project_ID =
        config_row$Project_ID,
      Stage =
        config_row$Stage,
      Control_Group =
        config_row$Group_B,
      Omitted_Genus =
        omitted,
      Omitted_Expected_Direction = if (
        expected_sign[omitted] > 0
      ) {
        "Higher_in_Sepsis"
      } else {
        "Lower_in_Sepsis"
      },
      Omitted_Column_Present =
        loaded$present[omitted],
      Full_Equal_AUC =
        full_equal_auc,
      LOO_Equal_AUC =
        auc_rank(
          loaded$case_label,
          equal_score
        ),
      Delta_Equal_AUC =
        auc_rank(
          loaded$case_label,
          equal_score
        ) -
        full_equal_auc,
      Full_Weighted_AUC =
        full_weighted_auc,
      LOO_Weighted_AUC =
        auc_rank(
          loaded$case_label,
          weighted_score
        ),
      Delta_Weighted_AUC =
        auc_rank(
          loaded$case_label,
          weighted_score
        ) -
        full_weighted_auc,
      Positive_Remaining_N =
        length(
          positive_remaining
        ),
      Negative_Remaining_N =
        length(
          negative_remaining
        ),
      stringsAsFactors = FALSE
    )
  }
}

loo_results <- do.call(
  rbind,
  loo_rows
)

# ------------------------------------------------------------
# Failure-mechanism summary
# ------------------------------------------------------------
external_performance <- score_performance[
  score_performance$Stage ==
    "External" &
    score_performance$Score_Variant %in%
      c(
        "Locked_Equal_Balance",
        "Locked_MetaWeighted_Balance"
      ),
  ,
  drop = FALSE
]

missing_external <- direction_summary[
  direction_summary$
    Missing_In_External_Project,
  ,
  drop = FALSE
]

reversed_external <- direction_summary[
  direction_summary$
    External_Complete_Reversal,
  ,
  drop = FALSE
]

partial_reversal_external <-
  direction_summary[
    direction_summary$
      Diagnostic_Label ==
      "PARTIAL_EXTERNAL_DIRECTION_REVERSAL",
    ,
    drop = FALSE
  ]

common_performance <- score_performance[
  score_performance$Stage ==
    "External" &
    score_performance$Score_Variant %in%
      c(
        "CommonFeature_Equal_Balance",
        "CommonFeature_MetaWeighted_Balance",
        "CommonFeature_Rank_Balance"
      ),
  ,
  drop = FALSE
]

diagnostic_conclusion <- data.frame(
  Item = c(
    "Day10_confirmatory_external_validation",
    "Locked_external_AUC_range",
    "Project_level_missing_candidate_genera",
    "Complete_external_direction_reversal_genera",
    "Partial_external_direction_reversal_genera",
    "All_project_common_positive_genera",
    "All_project_common_negative_genera",
    "Best_posthoc_external_AUC",
    "Best_posthoc_variant",
    "Can_posthoc_result_replace_failed_validation",
    "Required_interpretation"
  ),
  Value = c(
    "FAILED",
    paste0(
      round(
        min(
          external_performance$AUC,
          na.rm = TRUE
        ),
        3
      ),
      " to ",
      round(
        max(
          external_performance$AUC,
          na.rm = TRUE
        ),
        3
      )
    ),
    if (
      nrow(
        missing_external
      ) > 0
    ) {
      paste(
        missing_external$Genus_Feature,
        collapse = " | "
      )
    } else {
      "None"
    },
    if (
      nrow(
        reversed_external
      ) > 0
    ) {
      paste(
        reversed_external$Genus_Feature,
        collapse = " | "
      )
    } else {
      "None"
    },
    if (
      nrow(
        partial_reversal_external
      ) > 0
    ) {
      paste(
        partial_reversal_external$Genus_Feature,
        collapse = " | "
      )
    } else {
      "None"
    },
    paste(
      common_positive,
      collapse = " | "
    ),
    paste(
      common_negative,
      collapse = " | "
    ),
    if (
      nrow(
        common_performance
      ) > 0
    ) {
      round(
        max(
          common_performance$AUC,
          na.rm = TRUE
        ),
        3
      )
    } else {
      NA_character_
    },
    if (
      nrow(
        common_performance
      ) > 0
    ) {
      common_performance$
        Score_Variant[
          which.max(
            common_performance$AUC
          )
        ]
    } else {
      NA_character_
    },
    "No",
    paste0(
      "Day10 remains a failed prespecified external validation. ",
      "Day11 results are post-hoc diagnostics and hypothesis generation only."
    )
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Plots
# ------------------------------------------------------------
# 1. Candidate signed AUC by set.
pdf(
  file.path(
    plot_root,
    "19_Candidate_Signed_AUC_By_Set.pdf"
  ),
  width = 10,
  height = 7
)

auc_matrix <- xtabs(
  Individual_Signed_AUC ~
    Genus_Feature +
    Analysis_Set,
  data = candidate_effects
)

image(
  x = seq_len(
    ncol(
      auc_matrix
    )
  ),
  y = seq_len(
    nrow(
      auc_matrix
    )
  ),
  z = t(
    auc_matrix
  ),
  xlab = "",
  ylab = "",
  axes = FALSE,
  main = paste0(
    "Candidate signed AUC: >0.5 supports locked direction\n",
    "POST-HOC DIAGNOSTIC"
  )
)

axis(
  1,
  at = seq_len(
    ncol(
      auc_matrix
    )
  ),
  labels = colnames(
    auc_matrix
  ),
  las = 2,
  cex.axis = 0.65
)

axis(
  2,
  at = seq_len(
    nrow(
      auc_matrix
    )
  ),
  labels = rownames(
    auc_matrix
  ),
  las = 2,
  cex.axis = 0.65
)

for (
  row_i in seq_len(
    nrow(
      auc_matrix
    )
  )
) {
  for (
    col_i in seq_len(
      ncol(
        auc_matrix
      )
    )
  ) {
    text(
      col_i,
      row_i,
      labels = round(
        auc_matrix[
          row_i,
          col_i
        ],
        2
      ),
      cex = 0.7
    )
  }
}

dev.off()

# 2. Score variant AUC.
pdf(
  file.path(
    plot_root,
    "19_Score_Variant_AUC_Comparison.pdf"
  ),
  width = 11,
  height = 7
)

variant_sets <- unique(
  score_performance$Analysis_Set
)

variant_names <- unique(
  score_performance$Score_Variant
)

auc_variant_matrix <- matrix(
  NA_real_,
  nrow = length(
    variant_names
  ),
  ncol = length(
    variant_sets
  ),
  dimnames = list(
    variant_names,
    variant_sets
  )
)

for (
  i in seq_len(
    nrow(
      score_performance
    )
  )
) {
  auc_variant_matrix[
    score_performance$
      Score_Variant[i],
    score_performance$
      Analysis_Set[i]
  ] <- score_performance$AUC[i]
}

image(
  x = seq_len(
    ncol(
      auc_variant_matrix
    )
  ),
  y = seq_len(
    nrow(
      auc_variant_matrix
    )
  ),
  z = t(
    auc_variant_matrix
  ),
  xlab = "",
  ylab = "",
  axes = FALSE,
  main = paste0(
    "Score variant AUC comparison\n",
    "Exploratory variants are not external validation"
  )
)

axis(
  1,
  at = seq_len(
    ncol(
      auc_variant_matrix
    )
  ),
  labels = colnames(
    auc_variant_matrix
  ),
  las = 2,
  cex.axis = 0.65
)

axis(
  2,
  at = seq_len(
    nrow(
      auc_variant_matrix
    )
  ),
  labels = rownames(
    auc_variant_matrix
  ),
  las = 2,
  cex.axis = 0.65
)

for (
  row_i in seq_len(
    nrow(
      auc_variant_matrix
    )
  )
) {
  for (
    col_i in seq_len(
      ncol(
        auc_variant_matrix
      )
    )
  ) {
    text(
      col_i,
      row_i,
      labels = round(
        auc_variant_matrix[
          row_i,
          col_i
        ],
        2
      ),
      cex = 0.7
    )
  }
}

dev.off()

# 3. Leave-one-genus-out external AUC changes.
pdf(
  file.path(
    plot_root,
    "19_External_Leave_One_Genus_Out_AUC.pdf"
  ),
  width = 11,
  height = 7
)

external_loo <- loo_results[
  loo_results$Stage ==
    "External",
  ,
  drop = FALSE
]

if (
  nrow(
    external_loo
  ) > 0
) {
  groups <- interaction(
    external_loo$Analysis_Set,
    external_loo$Omitted_Genus,
    drop = TRUE
  )

  barplot(
    external_loo$Delta_Equal_AUC,
    names.arg = groups,
    las = 2,
    cex.names = 0.5,
    ylab = "Change in AUC after omitting genus",
    main = paste0(
      "External leave-one-genus-out sensitivity\n",
      "Positive value means AUC improved after omission"
    )
  )

  abline(
    h = 0,
    lty = 2
  )
} else {
  plot.new()
  text(
    0.5,
    0.5,
    "No external LOO results"
  )
}

dev.off()

# ------------------------------------------------------------
# Lock integrity audit
# ------------------------------------------------------------
lock_hash_after <- unname(
  tools::md5sum(
    c(
      candidate_file,
      formula_file,
      model_file
    )
  )
)

lock_audit <- data.frame(
  File = c(
    candidate_file,
    formula_file,
    model_file
  ),
  MD5_Before_Diagnosis =
    lock_hash_before,
  MD5_After_Diagnosis =
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
    "Locked_model_refitted",
    "Locked_threshold_retuned",
    "Locked_candidate_direction_changed",
    "External_labels_used_for_diagnosis",
    "Posthoc_variants_labeled_exploratory",
    "Day10_validation_status_preserved"
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
    "PASS"
  ),
  Details = c(
    "Candidate, formula, and model lock files were read-only and remained unchanged.",
    "No locked logistic coefficient was refitted.",
    "No locked threshold was retuned.",
    "No locked genus direction was changed.",
    "External labels were used only to diagnose failure mechanisms.",
    "All alternative score variants are labeled POSTHOC_EXPLORATORY_NOT_VALIDATION.",
    "The prespecified Day10 external validation remains FAILED."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Write outputs
# ------------------------------------------------------------
wcsv(
  detectability,
  file.path(
    report_root,
    "19_Candidate_Project_Detectability_Audit.csv"
  )
)

wcsv(
  candidate_effects,
  file.path(
    report_root,
    "19_Candidate_Direction_and_Contribution_Audit.csv"
  )
)

wcsv(
  direction_summary,
  file.path(
    report_root,
    "19_Candidate_CrossSet_Diagnosis.csv"
  )
)

wcsv(
  all_scores,
  file.path(
    output_root,
    "19_All_Diagnostic_Score_Variants.csv"
  )
)

wcsv(
  score_performance,
  file.path(
    report_root,
    "19_Exploratory_Score_Performance.csv"
  )
)

wcsv(
  loo_results,
  file.path(
    report_root,
    "19_Leave_One_Genus_Out_Sensitivity.csv"
  )
)

wcsv(
  diagnostic_conclusion,
  file.path(
    report_root,
    "19_Diagnostic_Conclusion.csv"
  )
)

wcsv(
  lock_audit,
  file.path(
    report_root,
    "19_Day10_Lock_Integrity_Audit.csv"
  )
)

wcsv(
  workflow_audit,
  file.path(
    report_root,
    "19_Diagnostic_Workflow_Audit.csv"
  )
)

if (
  nrow(
    score_reconstruction_audit
  ) > 0
) {
  wcsv(
    score_reconstruction_audit,
    file.path(
      report_root,
      "19_Day10_Score_Reconstruction_Audit.csv"
    )
  )
}

capture.output(
  sessionInfo(),
  file = file.path(
    report_root,
    "19_SDI_Failure_Diagnosis_SessionInfo.txt"
  )
)

files_to_hash <- unique(c(
  candidate_file,
  formula_file,
  model_file,
  day10_score_file,
  file.path(
    day7_set_root,
    paste0(
      set_config$Analysis_Set,
      ".csv"
    )
  ),
  file.path(
    day6_root,
    set_config$Project_ID,
    paste0(
      set_config$Project_ID,
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
    "19_Files_To_Hash.txt"
  ),
  useBytes = TRUE
)

cat(
  "\n============================================================\n"
)
cat("Day 11 completed.\n")
cat("Day 10 confirmatory external validation status remains FAILED.\n")
cat("All alternative variants are post-hoc exploratory diagnostics.\n")
cat(
  "Reports: ",
  report_root,
  "\n",
  sep = ""
)
cat(
  "Sample-level variants: ",
  output_root,
  "\n",
  sep = ""
)
cat("Important outputs:\n")
cat("  19_Candidate_Project_Detectability_Audit.csv\n")
cat("  19_Candidate_Direction_and_Contribution_Audit.csv\n")
cat("  19_Candidate_CrossSet_Diagnosis.csv\n")
cat("  19_Exploratory_Score_Performance.csv\n")
cat("  19_Leave_One_Genus_Out_Sensitivity.csv\n")
cat("  19_Diagnostic_Conclusion.csv\n")
cat("  19_Diagnostic_Workflow_Audit.csv\n")
cat(
  "Finished: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)
