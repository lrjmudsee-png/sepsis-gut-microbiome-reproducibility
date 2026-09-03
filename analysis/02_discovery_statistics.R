# PUBLIC RELEASE v1.0.0
# Scientific logic preserved from the independently verified historical script16.
# Public-release change: the historical local project root was replaced by
# SEPSIS_V1_PROJECT_ROOT/getwd(); statistical logic is otherwise unchanged.

options(stringsAsFactors = FALSE)

# ============================================================
# Day 8: Cohort-specific discovery statistics
#
# Analysed discovery sets:
#   A01 PRJEB33360: Sepsis vs Non-sepsis ICU
#   A02 PRJNA691455: Sepsis baseline vs Non-sepsis ICU
#   A03 PRJNA691455: Sepsis baseline vs Healthy
#   A06 PRJNA978257: Sepsis vs Healthy
#
# Locked and NOT read:
#   A04 PRJNA1010969: Sepsis vs Healthy
#   A05 PRJNA1010969: Sepsis vs Trauma
#
# Outputs:
#   - Input and filtering QC
#   - Alpha diversity
#   - Bray-Curtis PERMANOVA and dispersion tests
#   - PCoA coordinates
#   - Genus-level CLR effects, SE, p, FDR
#   - Wilcoxon corroboration and Cliff's delta
#   - Meta-ready cohort-level effects
#
# This script does not merge samples across projects.
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

output_root <- file.path(
  root,
  "processed_data/day8_discovery_statistics_v1"
)

report_root <- file.path(
  root,
  "manuscript/03_analysis/16_discovery_statistics"
)

plot_root <- file.path(
  report_root,
  "plots"
)

pcoa_root <- file.path(
  output_root,
  "pcoa_coordinates"
)

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(report_root, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_root, recursive = TRUE, showWarnings = FALSE)
dir.create(pcoa_root, recursive = TRUE, showWarnings = FALSE)

log_file <- file.path(
  report_root,
  "16_Discovery_Statistics_Log.txt"
)

sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat(
  "Day 8 discovery analysis started: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)

# ------------------------------------------------------------
# Required package
# ------------------------------------------------------------
if (!requireNamespace("vegan", quietly = TRUE)) {
  stop(
    paste0(
      "缺少R包 vegan。\n",
      "请先在RStudio运行： install.packages('vegan')"
    )
  )
}

set.seed(20260730)

# ------------------------------------------------------------
# Prespecified parameters
# ------------------------------------------------------------
prevalence_threshold <- 0.10
minimum_total_count <- 10
clr_pseudocount <- 0.5
permutations_n <- 9999
fdr_method <- "BH"

parameter_table <- data.frame(
  Parameter = c(
    "Discovery_sets",
    "Locked_external_sets",
    "Alpha_diversity_feature_rule",
    "Beta_and_DA_prevalence_threshold",
    "Beta_and_DA_minimum_total_count",
    "CLR_pseudocount",
    "PERMANOVA_permutations",
    "P_adjust_method",
    "Primary_meta_ready_effect",
    "Effect_direction"
  ),
  Value = c(
    "A01;A02;A03;A06",
    "A04;A05",
    "All taxonomy-cleaned classified genera before prevalence filtering",
    prevalence_threshold,
    minimum_total_count,
    clr_pseudocount,
    permutations_n,
    fdr_method,
    "Sepsis-minus-control CLR mean difference with Welch SE",
    "Positive values indicate higher abundance/diversity in Sepsis"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  parameter_table,
  file.path(report_root, "16_Analysis_Parameters.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# Analysis-set configuration
# ------------------------------------------------------------
set_config <- data.frame(
  Analysis_Set = c(
    "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
    "A02_PRJNA691455_Sepsis_vs_NonSepsisICU",
    "A03_PRJNA691455_Sepsis_vs_Healthy",
    "A06_PRJNA978257_Sepsis_vs_Healthy"
  ),
  Project_ID = c(
    "PRJEB33360",
    "PRJNA691455",
    "PRJNA691455",
    "PRJNA978257"
  ),
  Group_A = rep("Sepsis", 4),
  Group_B = c(
    "Non-sepsis ICU",
    "Non-sepsis ICU",
    "Healthy",
    "Healthy"
  ),
  Meta_Family = c(
    "ICU_control",
    "ICU_control",
    "Healthy_control",
    "Healthy_control"
  ),
  Discovery_Role = c(
    "Primary",
    "Primary",
    "Secondary",
    "Primary_small_cohort"
  ),
  stringsAsFactors = FALSE
)

locked_status <- data.frame(
  Analysis_Set = c(
    "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
    "A05_PRJNA1010969_External_Sepsis_vs_Trauma"
  ),
  Status = "LOCKED_NOT_READ",
  Reason = paste0(
    "External validation data remain locked until candidate genera, ",
    "meta-analysis rules, and SDI formula are fixed."
  ),
  stringsAsFactors = FALSE
)

write.csv(
  locked_status,
  file.path(report_root, "16_Locked_External_Set_Status.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
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
  out <- as.matrix(
    data.frame(
      lapply(
        df,
        function(x) suppressWarnings(as.numeric(x))
      ),
      check.names = FALSE
    )
  )

  storage.mode(out) <- "numeric"
  out
}

cliffs_delta <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]

  if (length(x) == 0 || length(y) == 0) {
    return(NA_real_)
  }

  comparisons <- outer(x, y, "-")

  (
    sum(comparisons > 0) -
      sum(comparisons < 0)
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
      length(unique(c(x, y))) < 2
  ) {
    return(
      list(
        statistic = NA_real_,
        p_value = NA_real_
      )
    )
  }

  fit <- suppressWarnings(
    stats::wilcox.test(
      x,
      y,
      alternative = "two.sided",
      exact = FALSE,
      correct = TRUE
    )
  )

  list(
    statistic = unname(fit$statistic),
    p_value = fit$p.value
  )
}

safe_t_test <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]

  if (
    length(x) < 2 ||
      length(y) < 2 ||
      stats::var(x) == 0 &&
        stats::var(y) == 0
  ) {
    return(
      list(
        statistic = NA_real_,
        df = NA_real_,
        p_value = NA_real_
      )
    )
  }

  fit <- suppressWarnings(
    stats::t.test(
      x,
      y,
      alternative = "two.sided",
      var.equal = FALSE
    )
  )

  list(
    statistic = unname(fit$statistic),
    df = unname(fit$parameter),
    p_value = fit$p.value
  )
}

welch_effect <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]

  nx <- length(x)
  ny <- length(y)

  if (nx < 2 || ny < 2) {
    return(
      list(
        effect = NA_real_,
        se = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_
      )
    )
  }

  vx <- stats::var(x)
  vy <- stats::var(y)

  if (!is.finite(vx)) vx <- 0
  if (!is.finite(vy)) vy <- 0

  effect <- mean(x) - mean(y)
  se <- sqrt(vx / nx + vy / ny)

  if (!is.finite(se) || se <= 0) {
    return(
      list(
        effect = effect,
        se = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_
      )
    )
  }

  list(
    effect = effect,
    se = se,
    ci_low = effect - 1.96 * se,
    ci_high = effect + 1.96 * se
  )
}

bh_adjust <- function(p) {
  out <- rep(NA_real_, length(p))
  valid <- is.finite(p)

  if (any(valid)) {
    out[valid] <- stats::p.adjust(
      p[valid],
      method = fdr_method
    )
  }

  out
}

group_summary_string <- function(group) {
  tab <- table(group)

  paste(
    names(tab),
    as.integer(tab),
    sep = "=",
    collapse = "; "
  )
}

alpha_metrics <- function(count_matrix) {
  if (nrow(count_matrix) == 0) {
    return(data.frame())
  }

  data.frame(
    Observed_Genus_Richness = rowSums(
      count_matrix > 0,
      na.rm = TRUE
    ),
    Shannon = vegan::diversity(
      count_matrix,
      index = "shannon"
    ),
    Inverse_Simpson = vegan::diversity(
      count_matrix,
      index = "invsimpson"
    ),
    stringsAsFactors = FALSE
  )
}

prevalence_filter <- function(
  count_matrix,
  group,
  group_a,
  group_b
) {
  group_a_index <- group == group_a
  group_b_index <- group == group_b

  prevalence_a <- colMeans(
    count_matrix[group_a_index, , drop = FALSE] > 0
  )

  prevalence_b <- colMeans(
    count_matrix[group_b_index, , drop = FALSE] > 0
  )

  total_count <- colSums(
    count_matrix,
    na.rm = TRUE
  )

  keep <- pmax(
    prevalence_a,
    prevalence_b
  ) >= prevalence_threshold &
    total_count >= minimum_total_count

  data.frame(
    Genus_Feature = colnames(count_matrix),
    Prevalence_Group_A = as.numeric(prevalence_a),
    Prevalence_Group_B = as.numeric(prevalence_b),
    Maximum_Group_Prevalence = as.numeric(
      pmax(
        prevalence_a,
        prevalence_b
      )
    ),
    Total_Count = as.numeric(total_count),
    Keep_for_Beta_and_DA = keep,
    stringsAsFactors = FALSE
  )
}

clr_transform <- function(count_matrix, pseudocount) {
  log_matrix <- log(
    count_matrix + pseudocount
  )

  sweep(
    log_matrix,
    1,
    rowMeans(log_matrix),
    "-"
  )
}

relative_from_counts <- function(count_matrix) {
  depth <- rowSums(count_matrix)
  out <- count_matrix

  valid <- depth > 0

  out[valid, ] <- sweep(
    count_matrix[valid, , drop = FALSE],
    1,
    depth[valid],
    "/"
  )

  if (any(!valid)) {
    out[!valid, ] <- 0
  }

  out
}

make_alpha_plot <- function(
  alpha_df,
  group_a,
  group_b,
  set_id,
  path
) {
  pdf(
    path,
    width = 12,
    height = 4.5
  )

  old_par <- par(no.readonly = TRUE)
  on.exit(
    {
      par(old_par)
      dev.off()
    },
    add = TRUE
  )

  par(mfrow = c(1, 3))

  metrics <- c(
    "Observed_Genus_Richness",
    "Shannon",
    "Inverse_Simpson"
  )

  for (metric in metrics) {
    boxplot(
      alpha_df[[metric]] ~
        factor(
          alpha_df$Comparison_Group,
          levels = c(group_b, group_a)
        ),
      main = metric,
      xlab = "",
      ylab = metric,
      outline = TRUE
    )

    stripchart(
      alpha_df[[metric]] ~
        factor(
          alpha_df$Comparison_Group,
          levels = c(group_b, group_a)
        ),
      vertical = TRUE,
      method = "jitter",
      add = TRUE,
      pch = 16
    )
  }

  mtext(
    set_id,
    outer = TRUE,
    line = -1
  )
}

make_pcoa_plot <- function(
  pcoa_df,
  set_id,
  axis1_pct,
  axis2_pct,
  path
) {
  pdf(
    path,
    width = 7,
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

  group_factor <- factor(
    pcoa_df$Comparison_Group
  )

  plot(
    pcoa_df$PCoA1,
    pcoa_df$PCoA2,
    pch = as.integer(group_factor),
    xlab = paste0(
      "PCoA1 (",
      round(axis1_pct, 1),
      "%)"
    ),
    ylab = paste0(
      "PCoA2 (",
      round(axis2_pct, 1),
      "%)"
    ),
    main = set_id
  )

  legend(
    "topright",
    legend = levels(group_factor),
    pch = seq_along(levels(group_factor)),
    bty = "n"
  )
}

make_effect_plot <- function(
  effect_df,
  set_id,
  path
) {
  valid <- is.finite(effect_df$CLR_Effect) &
    is.finite(effect_df$CLR_FDR)

  pdf(
    path,
    width = 7,
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

  if (!any(valid)) {
    plot.new()
    title(set_id)
    text(
      0.5,
      0.5,
      "No valid genus effects"
    )
    return(invisible(NULL))
  }

  y <- -log10(
    pmax(
      effect_df$CLR_FDR[valid],
      .Machine$double.xmin
    )
  )

  plot(
    effect_df$CLR_Effect[valid],
    y,
    xlab = "CLR mean difference: Sepsis minus control",
    ylab = "-log10(BH FDR)",
    main = set_id,
    pch = 16
  )

  abline(
    h = -log10(0.05),
    lty = 2
  )

  abline(
    v = 0,
    lty = 3
  )
}

# ------------------------------------------------------------
# Result containers
# ------------------------------------------------------------
input_qc_rows <- list()
filter_summary_rows <- list()
filter_detail_rows <- list()
alpha_sample_rows <- list()
alpha_group_rows <- list()
alpha_test_rows <- list()
beta_rows <- list()
dispersion_rows <- list()
pcoa_rows <- list()
genus_effect_rows <- list()
meta_ready_rows <- list()
file_index_rows <- list()
audit_rows <- list()

# ------------------------------------------------------------
# Process discovery sets independently
# ------------------------------------------------------------
for (i in seq_len(nrow(set_config))) {
  set_id <- set_config$Analysis_Set[i]
  project <- set_config$Project_ID[i]
  group_a <- set_config$Group_A[i]
  group_b <- set_config$Group_B[i]
  meta_family <- set_config$Meta_Family[i]
  discovery_role <- set_config$Discovery_Role[i]

  cat(
    "\n============================================================\n"
  )
  cat(
    "Processing ",
    set_id,
    "\n",
    sep = ""
  )

  set_file <- file.path(
    day7_set_root,
    paste0(set_id, ".csv")
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
      "未找到Day 7分析集：",
      set_file
    )
  }

  if (!file.exists(count_file)) {
    stop(
      "未找到Day 6 Genus counts：",
      count_file
    )
  }

  set_df <- read.csv(
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

  required_set_cols <- c(
    "Run_ID",
    "Patient_ID",
    "Comparison_Group",
    "Cleaned_Target_Depth"
  )

  missing_set_cols <- setdiff(
    required_set_cols,
    names(set_df)
  )

  if (length(missing_set_cols) > 0) {
    stop(
      set_id,
      " 分析集缺少字段：",
      paste(
        missing_set_cols,
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

  set_df$Run_ID <- clean_text(
    set_df$Run_ID
  )

  set_df$Patient_ID <- clean_text(
    set_df$Patient_ID
  )

  set_df$Comparison_Group <- clean_text(
    set_df$Comparison_Group
  )

  set_df$Cleaned_Target_Depth <-
    suppressWarnings(
      as.numeric(
        set_df$Cleaned_Target_Depth
      )
    )

  count_df$Run_ID <- clean_text(
    count_df$Run_ID
  )

  if (anyDuplicated(set_df$Run_ID)) {
    stop(
      set_id,
      " 存在重复Run_ID。"
    )
  }

  if (anyDuplicated(count_df$Run_ID)) {
    stop(
      project,
      " count表存在重复Run_ID。"
    )
  }

  observed_groups <- sort(
    unique(
      set_df$Comparison_Group
    )
  )

  expected_groups <- sort(
    c(group_a, group_b)
  )

  missing_groups <- setdiff(
    expected_groups,
    observed_groups
  )

  if (length(missing_groups) > 0) {
    stop(
      set_id,
      " 缺少预期组：",
      paste(
        missing_groups,
        collapse = " | "
      )
    )
  }

  matched_index <- match(
    set_df$Run_ID,
    count_df$Run_ID
  )

  missing_runs <- set_df$Run_ID[
    is.na(matched_index)
  ]

  if (length(missing_runs) > 0) {
    stop(
      set_id,
      " 以下Run在count表中缺失：",
      paste(
        missing_runs,
        collapse = " | "
      )
    )
  }

  count_genus_cols <- genus_columns(
    count_df
  )

  relative_genus_cols <- genus_columns(
    set_df
  )

  common_genus <- intersect(
    count_genus_cols,
    relative_genus_cols
  )

  if (length(common_genus) == 0) {
    stop(
      set_id,
      " 未找到共同Genus特征。"
    )
  }

  count_matrix <- safe_numeric_matrix(
    count_df[
      matched_index,
      common_genus,
      drop = FALSE
    ]
  )

  rownames(count_matrix) <- set_df$Run_ID

  relative_matrix_existing <-
    safe_numeric_matrix(
      set_df[
        ,
        common_genus,
        drop = FALSE
      ]
    )

  rownames(relative_matrix_existing) <-
    set_df$Run_ID

  if (any(!is.finite(count_matrix))) {
    stop(
      set_id,
      " count矩阵包含NA或非数值。"
    )
  }

  if (any(count_matrix < 0)) {
    stop(
      set_id,
      " count矩阵包含负数。"
    )
  }

  count_depth <- rowSums(
    count_matrix
  )

  cleaned_depth <- set_df$Cleaned_Target_Depth

  count_exceeds_cleaned <- count_depth >
    cleaned_depth + 1e-8

  if (any(count_exceeds_cleaned)) {
    stop(
      set_id,
      " 分类到Genus的count之和超过Cleaned_Target_Depth。"
    )
  }

  relative_reconstructed <- sweep(
    count_matrix,
    1,
    cleaned_depth,
    "/"
  )

  relative_reconstructed[
    !is.finite(relative_reconstructed)
  ] <- 0

  maximum_relative_difference <- max(
    abs(
      relative_reconstructed -
        relative_matrix_existing
    ),
    na.rm = TRUE
  )

  patient_key <- set_df$Patient_ID
  patient_key[is.na(patient_key)] <- paste0(
    "RUN_ONLY::",
    set_df$Run_ID[
      is.na(patient_key)
    ]
  )

  duplicate_patient_n <- sum(
    duplicated(patient_key)
  )

  group_counts <- table(
    set_df$Comparison_Group
  )

  input_qc_rows[[length(input_qc_rows) + 1]] <-
    data.frame(
      Analysis_Set = set_id,
      Project_ID = project,
      Samples_N = nrow(set_df),
      Unique_Patients_N = length(
        unique(patient_key)
      ),
      Duplicate_Patient_Rows = duplicate_patient_n,
      Group_Summary = group_summary_string(
        set_df$Comparison_Group
      ),
      Count_Genus_Features = length(
        count_genus_cols
      ),
      Relative_Genus_Features = length(
        relative_genus_cols
      ),
      Common_Genus_Features = length(
        common_genus
      ),
      Missing_Count_Runs = length(
        missing_runs
      ),
      Negative_Count_Entries = sum(
        count_matrix < 0
      ),
      Zero_Count_Samples = sum(
        count_depth == 0
      ),
      Count_Exceeds_Cleaned_Depth = sum(
        count_exceeds_cleaned
      ),
      Maximum_Relative_Reconstruction_Difference =
        maximum_relative_difference,
      Status = if (
        duplicate_patient_n == 0 &&
          length(missing_runs) == 0 &&
          sum(count_matrix < 0) == 0 &&
          sum(count_depth == 0) == 0 &&
          sum(count_exceeds_cleaned) == 0 &&
          maximum_relative_difference < 1e-8
      ) {
        "PASS"
      } else {
        "FAIL"
      },
      stringsAsFactors = FALSE
    )

  # ----------------------------------------------------------
  # Alpha diversity: all classified genera before prevalence
  # filtering.
  # ----------------------------------------------------------
  alpha <- alpha_metrics(
    count_matrix
  )

  alpha_df <- data.frame(
    Analysis_Set = set_id,
    Project_ID = project,
    Run_ID = set_df$Run_ID,
    Patient_ID = set_df$Patient_ID,
    Comparison_Group = set_df$Comparison_Group,
    Cleaned_Target_Depth = cleaned_depth,
    Classified_Genus_Count_Depth = count_depth,
    alpha,
    stringsAsFactors = FALSE
  )

  alpha_sample_rows[[length(alpha_sample_rows) + 1]] <-
    alpha_df

  for (grp in c(group_a, group_b)) {
    z <- alpha_df[
      alpha_df$Comparison_Group == grp,
      ,
      drop = FALSE
    ]

    for (
      metric in c(
        "Observed_Genus_Richness",
        "Shannon",
        "Inverse_Simpson"
      )
    ) {
      alpha_group_rows[[
        length(alpha_group_rows) + 1
      ]] <- data.frame(
        Analysis_Set = set_id,
        Project_ID = project,
        Comparison_Group = grp,
        Metric = metric,
        N = nrow(z),
        Mean = mean(
          z[[metric]],
          na.rm = TRUE
        ),
        SD = stats::sd(
          z[[metric]],
          na.rm = TRUE
        ),
        Median = stats::median(
          z[[metric]],
          na.rm = TRUE
        ),
        P25 = as.numeric(
          stats::quantile(
            z[[metric]],
            0.25,
            na.rm = TRUE
          )
        ),
        P75 = as.numeric(
          stats::quantile(
            z[[metric]],
            0.75,
            na.rm = TRUE
          )
        ),
        Minimum = min(
          z[[metric]],
          na.rm = TRUE
        ),
        Maximum = max(
          z[[metric]],
          na.rm = TRUE
        ),
        stringsAsFactors = FALSE
      )
    }
  }

  alpha_test_set <- list()

  for (
    metric in c(
      "Observed_Genus_Richness",
      "Shannon",
      "Inverse_Simpson"
    )
  ) {
    x <- alpha_df[
      alpha_df$Comparison_Group == group_a,
      metric
    ]

    y <- alpha_df[
      alpha_df$Comparison_Group == group_b,
      metric
    ]

    wilcox_fit <- safe_wilcox(x, y)
    effect_fit <- welch_effect(x, y)

    alpha_test_set[[
      length(alpha_test_set) + 1
    ]] <- data.frame(
      Analysis_Set = set_id,
      Project_ID = project,
      Meta_Family = meta_family,
      Group_A = group_a,
      Group_B = group_b,
      Metric = metric,
      N_Group_A = length(x),
      N_Group_B = length(y),
      Median_Group_A = stats::median(
        x,
        na.rm = TRUE
      ),
      Median_Group_B = stats::median(
        y,
        na.rm = TRUE
      ),
      Median_Difference_A_minus_B =
        stats::median(
          x,
          na.rm = TRUE
        ) -
        stats::median(
          y,
          na.rm = TRUE
        ),
      Mean_Difference_A_minus_B =
        mean(
          x,
          na.rm = TRUE
        ) -
        mean(
          y,
          na.rm = TRUE
        ),
      Mean_Difference_SE = effect_fit$se,
      Mean_Difference_CI_Low =
        effect_fit$ci_low,
      Mean_Difference_CI_High =
        effect_fit$ci_high,
      Cliffs_Delta_A_minus_B =
        cliffs_delta(x, y),
      Wilcoxon_W = wilcox_fit$statistic,
      Wilcoxon_P = wilcox_fit$p_value,
      stringsAsFactors = FALSE
    )
  }

  alpha_test_set <- do.call(
    rbind,
    alpha_test_set
  )

  alpha_test_set$Wilcoxon_FDR <-
    bh_adjust(
      alpha_test_set$Wilcoxon_P
    )

  alpha_test_rows[[
    length(alpha_test_rows) + 1
  ]] <- alpha_test_set

  alpha_plot_path <- file.path(
    plot_root,
    paste0(
      set_id,
      "_Alpha_Diversity.pdf"
    )
  )

  make_alpha_plot(
    alpha_df,
    group_a,
    group_b,
    set_id,
    alpha_plot_path
  )

  # ----------------------------------------------------------
  # Feature filtering for beta diversity and differential
  # abundance.
  # ----------------------------------------------------------
  filter_detail <- prevalence_filter(
    count_matrix,
    set_df$Comparison_Group,
    group_a,
    group_b
  )

  filter_detail$Analysis_Set <- set_id
  filter_detail$Project_ID <- project
  filter_detail$Group_A <- group_a
  filter_detail$Group_B <- group_b

  filter_detail_rows[[
    length(filter_detail_rows) + 1
  ]] <- filter_detail

  kept_features <- filter_detail$Genus_Feature[
    filter_detail$Keep_for_Beta_and_DA
  ]

  filter_summary_rows[[
    length(filter_summary_rows) + 1
  ]] <- data.frame(
    Analysis_Set = set_id,
    Project_ID = project,
    Features_Before = ncol(
      count_matrix
    ),
    Features_After = length(
      kept_features
    ),
    Features_Removed = ncol(
      count_matrix
    ) - length(
      kept_features
    ),
    Prevalence_Threshold = prevalence_threshold,
    Minimum_Total_Count = minimum_total_count,
    Group_A = group_a,
    Group_B = group_b,
    Group_Summary = group_summary_string(
      set_df$Comparison_Group
    ),
    Status = if (
      length(kept_features) >= 2
    ) {
      "PASS"
    } else {
      "FAIL"
    },
    stringsAsFactors = FALSE
  )

  if (length(kept_features) < 2) {
    stop(
      set_id,
      " 过滤后少于2个Genus，无法继续。"
    )
  }

  filtered_counts <- count_matrix[
    ,
    kept_features,
    drop = FALSE
  ]

  filtered_relative <- relative_from_counts(
    filtered_counts
  )

  zero_filtered_samples <- rowSums(
    filtered_counts
  ) == 0

  if (any(zero_filtered_samples)) {
    stop(
      set_id,
      " 过滤后出现零总计样本：",
      paste(
        rownames(filtered_counts)[
          zero_filtered_samples
        ],
        collapse = " | "
      )
    )
  }

  # ----------------------------------------------------------
  # Beta diversity: Bray-Curtis, PERMANOVA, dispersion, PCoA.
  # ----------------------------------------------------------
  bray <- vegan::vegdist(
    filtered_relative,
    method = "bray"
  )

  beta_meta <- data.frame(
    Comparison_Group = factor(
      set_df$Comparison_Group,
      levels = c(
        group_b,
        group_a
      )
    )
  )

  permanova <- vegan::adonis2(
    bray ~ Comparison_Group,
    data = beta_meta,
    permutations = permutations_n,
    by = "margin"
  )

  beta_rows[[
    length(beta_rows) + 1
  ]] <- data.frame(
    Analysis_Set = set_id,
    Project_ID = project,
    Meta_Family = meta_family,
    Group_A = group_a,
    Group_B = group_b,
    Samples_N = nrow(set_df),
    Features_N = ncol(
      filtered_counts
    ),
    Distance = "Bray-Curtis",
    Permutations = permutations_n,
    Df = permanova[1, "Df"],
    SumOfSqs = permanova[1, "SumOfSqs"],
    R2 = permanova[1, "R2"],
    F = permanova[1, "F"],
    P = permanova[1, "Pr(>F)"],
    stringsAsFactors = FALSE
  )

  dispersion <- vegan::betadisper(
    bray,
    group = beta_meta$Comparison_Group
  )

  dispersion_perm <- vegan::permutest(
    dispersion,
    permutations = permutations_n
  )

  dispersion_tab <- dispersion_perm$tab

  dispersion_rows[[
    length(dispersion_rows) + 1
  ]] <- data.frame(
    Analysis_Set = set_id,
    Project_ID = project,
    Group_A = group_a,
    Group_B = group_b,
    Permutations = permutations_n,
    F = dispersion_tab[1, "F"],
    P = dispersion_tab[1, "Pr(>F)"],
    Interpretation = if (
      is.finite(
        dispersion_tab[1, "Pr(>F)"]
      ) &&
        dispersion_tab[1, "Pr(>F)"] <
          0.05
    ) {
      paste0(
        "REVIEW: group dispersions differ; ",
        "interpret PERMANOVA cautiously."
      )
    } else {
      "PASS: no evidence of unequal group dispersion."
    },
    stringsAsFactors = FALSE
  )

  pcoa <- stats::cmdscale(
    bray,
    k = 2,
    eig = TRUE,
    add = TRUE
  )

  pcoa_points <- as.data.frame(
    pcoa$points
  )

  names(pcoa_points) <- c(
    "PCoA1",
    "PCoA2"
  )

  positive_eig <- pcoa$eig[
    pcoa$eig > 0
  ]

  axis1_pct <- if (
    length(positive_eig) >= 1
  ) {
    100 * pcoa$eig[1] /
      sum(positive_eig)
  } else {
    NA_real_
  }

  axis2_pct <- if (
    length(positive_eig) >= 2
  ) {
    100 * pcoa$eig[2] /
      sum(positive_eig)
  } else {
    NA_real_
  }

  pcoa_df <- data.frame(
    Analysis_Set = set_id,
    Project_ID = project,
    Run_ID = set_df$Run_ID,
    Patient_ID = set_df$Patient_ID,
    Comparison_Group =
      set_df$Comparison_Group,
    PCoA1 = pcoa_points$PCoA1,
    PCoA2 = pcoa_points$PCoA2,
    PCoA1_Variance_Percent =
      axis1_pct,
    PCoA2_Variance_Percent =
      axis2_pct,
    stringsAsFactors = FALSE
  )

  pcoa_rows[[
    length(pcoa_rows) + 1
  ]] <- pcoa_df

  pcoa_file <- file.path(
    pcoa_root,
    paste0(
      set_id,
      "_PCoA_Coordinates.csv"
    )
  )

  wcsv(
    pcoa_df,
    pcoa_file
  )

  pcoa_plot_path <- file.path(
    plot_root,
    paste0(
      set_id,
      "_Bray_PCoA.pdf"
    )
  )

  make_pcoa_plot(
    pcoa_df,
    set_id,
    axis1_pct,
    axis2_pct,
    pcoa_plot_path
  )

  # ----------------------------------------------------------
  # Genus differential effects.
  #
  # Primary meta-ready effect:
  #   mean CLR in Sepsis - mean CLR in control
  #
  # Corroborating statistics:
  #   raw relative abundance Wilcoxon
  #   Cliff's delta
  #   prevalence difference
  # ----------------------------------------------------------
  clr_matrix <- clr_transform(
    filtered_counts,
    clr_pseudocount
  )

  effect_set <- list()

  for (
    feature in colnames(
      filtered_counts
    )
  ) {
    group_a_index <-
      set_df$Comparison_Group ==
        group_a

    group_b_index <-
      set_df$Comparison_Group ==
        group_b

    clr_a <- clr_matrix[
      group_a_index,
      feature
    ]

    clr_b <- clr_matrix[
      group_b_index,
      feature
    ]

    rel_a <- relative_matrix_existing[
      group_a_index,
      feature
    ]

    rel_b <- relative_matrix_existing[
      group_b_index,
      feature
    ]

    count_a <- filtered_counts[
      group_a_index,
      feature
    ]

    count_b <- filtered_counts[
      group_b_index,
      feature
    ]

    effect_fit <- welch_effect(
      clr_a,
      clr_b
    )

    t_fit <- safe_t_test(
      clr_a,
      clr_b
    )

    wilcox_fit <- safe_wilcox(
      rel_a,
      rel_b
    )

    effect_set[[
      length(effect_set) + 1
    ]] <- data.frame(
      Analysis_Set = set_id,
      Project_ID = project,
      Meta_Family = meta_family,
      Discovery_Role = discovery_role,
      Group_A = group_a,
      Group_B = group_b,
      Genus_Feature = feature,
      N_Group_A = sum(
        group_a_index
      ),
      N_Group_B = sum(
        group_b_index
      ),
      Prevalence_Group_A = mean(
        count_a > 0
      ),
      Prevalence_Group_B = mean(
        count_b > 0
      ),
      Prevalence_Difference_A_minus_B =
        mean(
          count_a > 0
        ) -
        mean(
          count_b > 0
        ),
      Mean_Relative_Abundance_Group_A =
        mean(
          rel_a,
          na.rm = TRUE
        ),
      Mean_Relative_Abundance_Group_B =
        mean(
          rel_b,
          na.rm = TRUE
        ),
      Median_Relative_Abundance_Group_A =
        stats::median(
          rel_a,
          na.rm = TRUE
        ),
      Median_Relative_Abundance_Group_B =
        stats::median(
          rel_b,
          na.rm = TRUE
        ),
      CLR_Mean_Group_A = mean(
        clr_a,
        na.rm = TRUE
      ),
      CLR_Mean_Group_B = mean(
        clr_b,
        na.rm = TRUE
      ),
      CLR_Effect = effect_fit$effect,
      CLR_SE = effect_fit$se,
      CLR_CI_Low = effect_fit$ci_low,
      CLR_CI_High = effect_fit$ci_high,
      CLR_T = t_fit$statistic,
      CLR_Df = t_fit$df,
      CLR_P = t_fit$p_value,
      Relative_Wilcoxon_W =
        wilcox_fit$statistic,
      Relative_Wilcoxon_P =
        wilcox_fit$p_value,
      Relative_Cliffs_Delta =
        cliffs_delta(
          rel_a,
          rel_b
        ),
      Direction = if (
        is.finite(
          effect_fit$effect
        )
      ) {
        if (
          effect_fit$effect > 0
        ) {
          "Higher_in_Sepsis"
        } else if (
          effect_fit$effect < 0
        ) {
          "Lower_in_Sepsis"
        } else {
          "No_direction"
        }
      } else {
        "Not_estimable"
      },
      stringsAsFactors = FALSE
    )
  }

  effect_set <- do.call(
    rbind,
    effect_set
  )

  effect_set$CLR_FDR <- bh_adjust(
    effect_set$CLR_P
  )

  effect_set$Relative_Wilcoxon_FDR <-
    bh_adjust(
      effect_set$Relative_Wilcoxon_P
    )

  effect_set$Pass_Meta_SE_QC <-
    is.finite(
      effect_set$CLR_Effect
    ) &
    is.finite(
      effect_set$CLR_SE
    ) &
    effect_set$CLR_SE > 0

  effect_set$Discovery_Signal <- ifelse(
    effect_set$CLR_FDR < 0.05 &
      effect_set$Relative_Wilcoxon_FDR <
        0.05,
    "Concordant_FDR_lt_0.05",
    ifelse(
      effect_set$CLR_FDR < 0.05 |
        effect_set$Relative_Wilcoxon_FDR <
          0.05,
      "One_method_FDR_lt_0.05",
      "No_within_set_FDR_signal"
    )
  )

  genus_effect_rows[[
    length(genus_effect_rows) + 1
  ]] <- effect_set

  meta_ready_rows[[
    length(meta_ready_rows) + 1
  ]] <- effect_set[
    effect_set$Pass_Meta_SE_QC,
    c(
      "Analysis_Set",
      "Project_ID",
      "Meta_Family",
      "Discovery_Role",
      "Group_A",
      "Group_B",
      "Genus_Feature",
      "N_Group_A",
      "N_Group_B",
      "CLR_Effect",
      "CLR_SE",
      "CLR_CI_Low",
      "CLR_CI_High",
      "CLR_P",
      "CLR_FDR",
      "Relative_Wilcoxon_P",
      "Relative_Wilcoxon_FDR",
      "Relative_Cliffs_Delta",
      "Prevalence_Group_A",
      "Prevalence_Group_B",
      "Mean_Relative_Abundance_Group_A",
      "Mean_Relative_Abundance_Group_B",
      "Direction",
      "Discovery_Signal"
    ),
    drop = FALSE
  ]

  effect_file <- file.path(
    output_root,
    paste0(
      set_id,
      "_Genus_Effects.csv"
    )
  )

  wcsv(
    effect_set,
    effect_file
  )

  effect_plot_path <- file.path(
    plot_root,
    paste0(
      set_id,
      "_CLR_Effect_FDR.pdf"
    )
  )

  make_effect_plot(
    effect_set,
    set_id,
    effect_plot_path
  )

  file_index_rows[[
    length(file_index_rows) + 1
  ]] <- data.frame(
    Analysis_Set = set_id,
    Project_ID = project,
    File_Type = c(
      "Input_analysis_set",
      "Input_genus_counts",
      "Genus_effects",
      "PCoA_coordinates",
      "Alpha_plot",
      "PCoA_plot",
      "Effect_plot"
    ),
    File_Path = c(
      set_file,
      count_file,
      effect_file,
      pcoa_file,
      alpha_plot_path,
      pcoa_plot_path,
      effect_plot_path
    ),
    stringsAsFactors = FALSE
  )

  # ----------------------------------------------------------
  # Set-level audit
  # ----------------------------------------------------------
  current_input_qc <- input_qc_rows[[
    length(input_qc_rows)
  ]]

  current_filter_qc <- filter_summary_rows[[
    length(filter_summary_rows)
  ]]

  current_beta <- beta_rows[[
    length(beta_rows)
  ]]

  current_disp <- dispersion_rows[[
    length(dispersion_rows)
  ]]

  fail_reasons <- character()
  review_reasons <- character()

  if (
    current_input_qc$Status != "PASS"
  ) {
    fail_reasons <- c(
      fail_reasons,
      "Input consistency QC failed"
    )
  }

  if (
    current_filter_qc$Status != "PASS"
  ) {
    fail_reasons <- c(
      fail_reasons,
      "Too few genera after filtering"
    )
  }

  if (
    !is.finite(
      current_beta$P
    )
  ) {
    fail_reasons <- c(
      fail_reasons,
      "PERMANOVA was not estimable"
    )
  }

  if (
    is.finite(
      current_disp$P
    ) &&
      current_disp$P < 0.05
  ) {
    review_reasons <- c(
      review_reasons,
      "Unequal multivariate dispersion"
    )
  }

  if (
    min(group_counts) < 10
  ) {
    review_reasons <- c(
      review_reasons,
      "At least one group has fewer than 10 samples"
    )
  }

  audit_rows[[
    length(audit_rows) + 1
  ]] <- data.frame(
    Analysis_Set = set_id,
    Project_ID = project,
    Input_QC = current_input_qc$Status,
    Filter_QC = current_filter_qc$Status,
    PERMANOVA_Estimable = is.finite(
      current_beta$P
    ),
    Dispersion_P = current_disp$P,
    Minimum_Group_N = min(
      group_counts
    ),
    Final_Status = if (
      length(fail_reasons) > 0
    ) {
      "FAIL"
    } else if (
      length(review_reasons) > 0
    ) {
      "REVIEW"
    } else {
      "PASS"
    },
    Fail_Reasons = paste(
      fail_reasons,
      collapse = " | "
    ),
    Review_Reasons = paste(
      review_reasons,
      collapse = " | "
    ),
    Reviewer = "",
    Review_Date = "",
    Final_Decision = "",
    Decision_Notes = "",
    stringsAsFactors = FALSE
  )

  cat(
    "Samples: ",
    nrow(set_df),
    "; genera before/after filtering: ",
    ncol(count_matrix),
    "/",
    length(kept_features),
    "\n",
    sep = ""
  )
}

# ------------------------------------------------------------
# Combine reports
# ------------------------------------------------------------
input_qc <- do.call(
  rbind,
  input_qc_rows
)

filter_summary <- do.call(
  rbind,
  filter_summary_rows
)

filter_detail <- do.call(
  rbind,
  filter_detail_rows
)

alpha_sample <- do.call(
  rbind,
  alpha_sample_rows
)

alpha_group <- do.call(
  rbind,
  alpha_group_rows
)

alpha_tests <- do.call(
  rbind,
  alpha_test_rows
)

beta_results <- do.call(
  rbind,
  beta_rows
)

dispersion_results <- do.call(
  rbind,
  dispersion_rows
)

pcoa_all <- do.call(
  rbind,
  pcoa_rows
)

genus_effects <- do.call(
  rbind,
  genus_effect_rows
)

meta_ready <- do.call(
  rbind,
  meta_ready_rows
)

file_index <- do.call(
  rbind,
  file_index_rows
)

analysis_audit <- do.call(
  rbind,
  audit_rows
)

# BH correction across the four discovery-set PERMANOVA tests.
beta_results$P_FDR <- bh_adjust(
  beta_results$P
)

dispersion_results$P_FDR <- bh_adjust(
  dispersion_results$P
)

# ------------------------------------------------------------
# Direction-consistency preview
#
# This is descriptive only. Formal random-effects meta-analysis
# occurs on Day 9 and is stratified by Meta_Family.
# ------------------------------------------------------------
direction_preview_rows <- list()

for (
  family in unique(
    meta_ready$Meta_Family
  )
) {
  family_df <- meta_ready[
    meta_ready$Meta_Family == family,
    ,
    drop = FALSE
  ]

  for (
    feature in unique(
      family_df$Genus_Feature
    )
  ) {
    z <- family_df[
      family_df$Genus_Feature == feature,
      ,
      drop = FALSE
    ]

    positive_n <- sum(
      z$CLR_Effect > 0,
      na.rm = TRUE
    )

    negative_n <- sum(
      z$CLR_Effect < 0,
      na.rm = TRUE
    )

    direction_preview_rows[[
      length(direction_preview_rows) + 1
    ]] <- data.frame(
      Meta_Family = family,
      Genus_Feature = feature,
      Cohorts_With_Effect = nrow(z),
      Positive_Effects = positive_n,
      Negative_Effects = negative_n,
      Direction_Consistent = (
        positive_n == nrow(z) ||
          negative_n == nrow(z)
      ),
      Sets = paste(
        z$Analysis_Set,
        collapse = " | "
      ),
      stringsAsFactors = FALSE
    )
  }
}

direction_preview <- if (
  length(direction_preview_rows) > 0
) {
  do.call(
    rbind,
    direction_preview_rows
  )
} else {
  data.frame()
}

# ------------------------------------------------------------
# Write final outputs
# ------------------------------------------------------------
wcsv(
  input_qc,
  file.path(
    report_root,
    "16_Input_Consistency_QC.csv"
  )
)

wcsv(
  filter_summary,
  file.path(
    report_root,
    "16_Feature_Filter_Summary.csv"
  )
)

wcsv(
  filter_detail,
  file.path(
    output_root,
    "16_Feature_Filter_Details.csv"
  )
)

wcsv(
  alpha_sample,
  file.path(
    output_root,
    "16_Alpha_Diversity_Sample_Level.csv"
  )
)

wcsv(
  alpha_group,
  file.path(
    report_root,
    "16_Alpha_Diversity_Group_Summary.csv"
  )
)

wcsv(
  alpha_tests,
  file.path(
    report_root,
    "16_Alpha_Diversity_Tests.csv"
  )
)

wcsv(
  beta_results,
  file.path(
    report_root,
    "16_Beta_Diversity_PERMANOVA.csv"
  )
)

wcsv(
  dispersion_results,
  file.path(
    report_root,
    "16_Beta_Dispersion_Tests.csv"
  )
)

wcsv(
  pcoa_all,
  file.path(
    output_root,
    "16_All_PCoA_Coordinates.csv"
  )
)

wcsv(
  genus_effects,
  file.path(
    output_root,
    "16_All_Discovery_Genus_Effects.csv"
  )
)

wcsv(
  meta_ready,
  file.path(
    report_root,
    "16_Meta_Ready_Genus_Effects.csv"
  )
)

wcsv(
  direction_preview,
  file.path(
    report_root,
    "16_Direction_Consistency_Preview.csv"
  )
)

wcsv(
  analysis_audit,
  file.path(
    report_root,
    "16_Discovery_Analysis_Audit.csv"
  )
)

wcsv(
  file_index,
  file.path(
    report_root,
    "16_Discovery_Output_File_Index.csv"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    report_root,
    "16_Discovery_Statistics_SessionInfo.txt"
  )
)

files_to_hash <- unique(c(
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
    "16_Files_To_Hash.txt"
  ),
  useBytes = TRUE
)

cat(
  "\n============================================================\n"
)
cat("Day 8 completed.\n")
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
cat("External validation sets remained locked and unread.\n")
cat("Important files:\n")
cat("  16_Input_Consistency_QC.csv\n")
cat("  16_Feature_Filter_Summary.csv\n")
cat("  16_Alpha_Diversity_Tests.csv\n")
cat("  16_Beta_Diversity_PERMANOVA.csv\n")
cat("  16_Beta_Dispersion_Tests.csv\n")
cat("  16_Meta_Ready_Genus_Effects.csv\n")
cat("  16_Discovery_Analysis_Audit.csv\n")
cat(
  "Finished: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)
