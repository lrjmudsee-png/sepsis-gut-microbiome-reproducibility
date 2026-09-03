# PUBLIC RELEASE v1.0.0
# Scientific logic preserved from the independently verified historical script17.
# Public-release change: the historical local project root was replaced by
# SEPSIS_V1_PROJECT_ROOT/getwd(); statistical logic is otherwise unchanged.

options(stringsAsFactors = FALSE)

# ============================================================
# Day 9: Family-harmonized genus meta-analysis
#
# IMPORTANT:
# CLR coordinates depend on the feature set used as the
# denominator/geometric-mean reference. Day 8 used set-specific
# filtering. Before meta-analysis, this script therefore:
#
# 1. Reopens only discovery sets A01, A02, A03, A06.
# 2. Applies the prespecified filter independently in each set.
# 3. Takes the intersection of eligible genera within each
#    control-family:
#       ICU_control:     A01 + A02
#       Healthy_control: A03 + A06
# 4. Recomputes CLR effects in every cohort using the SAME
#    family-specific genus feature space.
# 5. Fits fixed-effect, REML random-effect, and Knapp-Hartung
#    sensitivity models.
#
# Locked external validation sets A04/A05 are not read.
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

day8_root <- file.path(
  root,
  "processed_data/day8_discovery_statistics_v1"
)

day8_report_root <- file.path(
  root,
  "manuscript/03_analysis/16_discovery_statistics"
)

output_root <- file.path(
  root,
  "processed_data/day9_harmonized_meta_v1"
)

report_root <- file.path(
  root,
  "manuscript/03_analysis/17_harmonized_meta_analysis"
)

plot_root <- file.path(
  report_root,
  "plots"
)

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(report_root, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_root, recursive = TRUE, showWarnings = FALSE)

sink(
  file.path(
    report_root,
    "17_Harmonized_Meta_Analysis_Log.txt"
  ),
  split = TRUE
)
on.exit(sink(), add = TRUE)

cat(
  "Day 9 harmonized meta-analysis started: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)

# ------------------------------------------------------------
# Required package
# ------------------------------------------------------------
if (!requireNamespace("metafor", quietly = TRUE)) {
  stop(
    paste0(
      "缺少R包 metafor。\n",
      "请先在RStudio运行： install.packages('metafor')"
    )
  )
}

set.seed(20260730)

# ------------------------------------------------------------
# Prespecified parameters
# ------------------------------------------------------------
primary_prevalence_threshold <- 0.10
minimum_total_count <- 10
strict_minimum_detected_samples <- 2
clr_pseudocount <- 0.5
heterogeneity_review_i2 <- 75
fdr_method <- "BH"

parameters <- data.frame(
  Parameter = c(
    "Discovery_sets_read",
    "Locked_external_sets_not_read",
    "Meta_families",
    "Primary_feature_filter",
    "Strict_feature_filter",
    "CLR_pseudocount",
    "Primary_random_effect_method",
    "Primary_random_effect_test",
    "Small_k_sensitivity_test",
    "Fixed_effect_sensitivity",
    "Multiple_testing",
    "Heterogeneity_review_I2",
    "Primary_effect_direction"
  ),
  Value = c(
    "A01;A02;A03;A06",
    "A04;A05",
    "ICU_control=A01+A02; Healthy_control=A03+A06",
    paste0(
      "Within every set: maximum group prevalence >= ",
      primary_prevalence_threshold,
      " and total count >= ",
      minimum_total_count,
      "; then intersect eligible genera across both sets."
    ),
    paste0(
      "Within every set: detected in >= ",
      strict_minimum_detected_samples,
      " samples in either group and total count >= ",
      minimum_total_count,
      "; then intersect across both sets."
    ),
    clr_pseudocount,
    "REML",
    "z",
    "Knapp-Hartung",
    "Inverse-variance fixed effect",
    fdr_method,
    heterogeneity_review_i2,
    "Positive = higher in Sepsis; negative = lower in Sepsis"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  parameters,
  file.path(
    report_root,
    "17_Meta_Analysis_Parameters.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

candidate_rules <- data.frame(
  Candidate_Grade = c(
    "ICU_Tier1_Robust",
    "ICU_Tier2_Supportive",
    "ICU_Tier3_Directional",
    "Healthy_Robust_Dysbiosis",
    "Healthy_Heterogeneous_Dysbiosis",
    "Insufficient_or_Inconsistent"
  ),
  Rule = c(
    paste0(
      "ICU family; k=2; same direction; strict-filter stable; ",
      "fixed-effect FDR<0.05; REML-z FDR<0.10; I2<=",
      heterogeneity_review_i2,
      "%."
    ),
    paste0(
      "ICU family; k=2; same direction; strict-filter stable; ",
      "I2<=",
      heterogeneity_review_i2,
      "%; and fixed P<0.05 or REML-z P<0.05 or any harmonized ",
      "cohort FDR<0.05."
    ),
    paste0(
      "ICU family; k=2; same direction; strict-filter stable; ",
      "does not satisfy Tier1/Tier2."
    ),
    paste0(
      "Healthy-control family; k=2; same direction; strict-filter ",
      "stable; fixed FDR<0.05; REML-z FDR<0.10; I2<=",
      heterogeneity_review_i2,
      "%."
    ),
    paste0(
      "Healthy-control family; same direction and fixed FDR<0.05, ",
      "but I2>",
      heterogeneity_review_i2,
      "% or strict instability."
    ),
    "Fewer than two effects, opposite directions, or no stable evidence."
  ),
  Proposed_SDI_Use = c(
    "Primary_candidate",
    "Secondary_candidate",
    "Not_for_core_SDI",
    "Contextual_only_not_core_SDI",
    "Contextual_only_not_core_SDI",
    "Not_for_core_SDI"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  candidate_rules,
  file.path(
    report_root,
    "17_Candidate_Grading_Rules.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

locked_status <- data.frame(
  Analysis_Set = c(
    "A04_PRJNA1010969_External_Sepsis_vs_Healthy",
    "A05_PRJNA1010969_External_Sepsis_vs_Trauma"
  ),
  Status = "LOCKED_NOT_READ",
  Reason = paste0(
    "External validation remains locked until the Day 9 ",
    "candidate proposal is reviewed and the SDI construction ",
    "rule is frozen."
  ),
  stringsAsFactors = FALSE
)

write.csv(
  locked_status,
  file.path(
    report_root,
    "17_Locked_External_Set_Status.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# Configuration
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
  Meta_Family = c(
    "ICU_control",
    "ICU_control",
    "Healthy_control",
    "Healthy_control"
  ),
  Group_A = rep("Sepsis", 4),
  Group_B = c(
    "Non-sepsis ICU",
    "Non-sepsis ICU",
    "Healthy",
    "Healthy"
  ),
  Discovery_Role = c(
    "Primary",
    "Primary",
    "Secondary",
    "Primary_small_cohort"
  ),
  stringsAsFactors = FALSE
)

family_config <- list(
  ICU_control = c(
    "A01_PRJEB33360_Sepsis_vs_NonSepsisICU",
    "A02_PRJNA691455_Sepsis_vs_NonSepsisICU"
  ),
  Healthy_control = c(
    "A03_PRJNA691455_Sepsis_vs_Healthy",
    "A06_PRJNA978257_Sepsis_vs_Healthy"
  )
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

bh_adjust <- function(p) {
  out <- rep(NA_real_, length(p))
  ok <- is.finite(p)

  if (any(ok)) {
    out[ok] <- stats::p.adjust(
      p[ok],
      method = fdr_method
    )
  }

  out
}

relative_by_cleaned_depth <- function(counts, cleaned_depth) {
  out <- counts
  valid <- cleaned_depth > 0

  out[valid, ] <- sweep(
    counts[valid, , drop = FALSE],
    1,
    cleaned_depth[valid],
    "/"
  )

  if (any(!valid)) {
    out[!valid, ] <- 0
  }

  out
}

clr_transform <- function(counts, pseudocount) {
  logged <- log(
    counts + pseudocount
  )

  sweep(
    logged,
    1,
    rowMeans(logged),
    "-"
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
        ci_high = NA_real_,
        t = NA_real_,
        df = NA_real_,
        p = NA_real_
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
        ci_high = NA_real_,
        t = NA_real_,
        df = NA_real_,
        p = NA_real_
      )
    )
  }

  t_value <- effect / se

  denominator <- (
    (vx / nx)^2 / (nx - 1)
  ) + (
    (vy / ny)^2 / (ny - 1)
  )

  df <- if (
    is.finite(denominator) &&
      denominator > 0
  ) {
    (vx / nx + vy / ny)^2 /
      denominator
  } else {
    NA_real_
  }

  p <- if (is.finite(df)) {
    2 * stats::pt(
      abs(t_value),
      df = df,
      lower.tail = FALSE
    )
  } else {
    2 * stats::pnorm(
      abs(t_value),
      lower.tail = FALSE
    )
  }

  list(
    effect = effect,
    se = se,
    ci_low = effect - 1.96 * se,
    ci_high = effect + 1.96 * se,
    t = t_value,
    df = df,
    p = p
  )
}

hedges_g_effect <- function(x, y) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]

  n1 <- length(x)
  n0 <- length(y)

  if (n1 < 2 || n0 < 2) {
    return(
      list(
        g = NA_real_,
        se = NA_real_
      )
    )
  }

  s1 <- stats::sd(x)
  s0 <- stats::sd(y)

  pooled_var <- (
    (n1 - 1) * s1^2 +
      (n0 - 1) * s0^2
  ) / (
    n1 + n0 - 2
  )

  if (
    !is.finite(pooled_var) ||
      pooled_var <= 0
  ) {
    return(
      list(
        g = NA_real_,
        se = NA_real_
      )
    )
  }

  d <- (
    mean(x) - mean(y)
  ) / sqrt(pooled_var)

  df <- n1 + n0 - 2

  correction <- if (df > 1) {
    1 - 3 / (4 * df - 1)
  } else {
    1
  }

  g <- correction * d

  variance_g <- (
    (n1 + n0) / (n1 * n0)
  ) + (
    g^2 / (
      2 * (n1 + n0 - 2)
    )
  )

  list(
    g = g,
    se = sqrt(variance_g)
  )
}

feature_filter_table <- function(
  counts,
  group,
  group_a,
  group_b
) {
  a_index <- group == group_a
  b_index <- group == group_b

  detected_a <- colSums(
    counts[a_index, , drop = FALSE] > 0
  )

  detected_b <- colSums(
    counts[b_index, , drop = FALSE] > 0
  )

  n_a <- sum(a_index)
  n_b <- sum(b_index)

  prevalence_a <- detected_a / n_a
  prevalence_b <- detected_b / n_b
  total_count <- colSums(counts)

  primary_keep <- pmax(
    prevalence_a,
    prevalence_b
  ) >= primary_prevalence_threshold &
    total_count >= minimum_total_count

  strict_keep <- pmax(
    detected_a,
    detected_b
  ) >= strict_minimum_detected_samples &
    total_count >= minimum_total_count

  data.frame(
    Genus_Feature = colnames(counts),
    N_Group_A = n_a,
    N_Group_B = n_b,
    Detected_Group_A = as.numeric(
      detected_a
    ),
    Detected_Group_B = as.numeric(
      detected_b
    ),
    Prevalence_Group_A = as.numeric(
      prevalence_a
    ),
    Prevalence_Group_B = as.numeric(
      prevalence_b
    ),
    Total_Count = as.numeric(
      total_count
    ),
    Primary_Eligible = primary_keep,
    Strict_Eligible = strict_keep,
    stringsAsFactors = FALSE
  )
}

load_analysis_set <- function(set_row) {
  set_id <- set_row$Analysis_Set
  project <- set_row$Project_ID

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
    "Comparison_Group",
    "Cleaned_Target_Depth"
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

  meta$Cleaned_Target_Depth <-
    suppressWarnings(
      as.numeric(
        meta$Cleaned_Target_Depth
      )
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

  if (anyDuplicated(count_df$Run_ID)) {
    stop(
      project,
      " count表存在重复Run_ID。"
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

  features <- genus_columns(
    count_df
  )

  counts <- safe_numeric_matrix(
    count_df[
      index,
      features,
      drop = FALSE
    ]
  )

  rownames(counts) <- meta$Run_ID

  if (
    any(!is.finite(counts)) ||
      any(counts < 0)
  ) {
    stop(
      set_id,
      " count矩阵包含NA、无限值或负数。"
    )
  }

  if (any(rowSums(counts) <= 0)) {
    stop(
      set_id,
      " 存在分类Genus总计数为0的样本。"
    )
  }

  group_a <- set_row$Group_A
  group_b <- set_row$Group_B

  observed <- unique(
    meta$Comparison_Group
  )

  if (
    !all(
      c(group_a, group_b) %in% observed
    )
  ) {
    stop(
      set_id,
      " 缺少预期组。"
    )
  }

  filter_table <- feature_filter_table(
    counts,
    meta$Comparison_Group,
    group_a,
    group_b
  )

  list(
    set_id = set_id,
    project = project,
    family = set_row$Meta_Family,
    group_a = group_a,
    group_b = group_b,
    discovery_role = set_row$Discovery_Role,
    meta = meta,
    counts = counts,
    filter = filter_table,
    set_file = set_file,
    count_file = count_file
  )
}

extract_rma <- function(fit) {
  if (is.null(fit)) {
    return(
      list(
        estimate = NA_real_,
        se = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p = NA_real_,
        tau2 = NA_real_,
        i2 = NA_real_,
        h2 = NA_real_,
        qe = NA_real_,
        qep = NA_real_,
        pi_low = NA_real_,
        pi_high = NA_real_
      )
    )
  }

  prediction <- tryCatch(
    metafor::predict.rma(
      fit
    ),
    error = function(e) NULL
  )

  list(
    estimate = as.numeric(fit$b[1]),
    se = as.numeric(fit$se[1]),
    ci_low = as.numeric(fit$ci.lb[1]),
    ci_high = as.numeric(fit$ci.ub[1]),
    p = as.numeric(fit$pval[1]),
    tau2 = as.numeric(fit$tau2),
    i2 = as.numeric(fit$I2),
    h2 = as.numeric(fit$H2),
    qe = as.numeric(fit$QE),
    qep = as.numeric(fit$QEp),
    pi_low = if (
      !is.null(prediction) &&
        "pi.lb" %in% names(prediction)
    ) {
      as.numeric(prediction$pi.lb[1])
    } else {
      NA_real_
    },
    pi_high = if (
      !is.null(prediction) &&
        "pi.ub" %in% names(prediction)
    ) {
      as.numeric(prediction$pi.ub[1])
    } else {
      NA_real_
    }
  )
}

fit_meta_models <- function(yi, sei) {
  valid <- is.finite(yi) &
    is.finite(sei) &
    sei > 0

  yi <- yi[valid]
  sei <- sei[valid]

  if (length(yi) < 2) {
    return(
      list(
        fixed = extract_rma(NULL),
        random_z = extract_rma(NULL),
        random_hk = extract_rma(NULL)
      )
    )
  }

  fixed_fit <- tryCatch(
    metafor::rma.uni(
      yi = yi,
      sei = sei,
      method = "FE",
      test = "z"
    ),
    error = function(e) NULL
  )

  random_z_fit <- tryCatch(
    metafor::rma.uni(
      yi = yi,
      sei = sei,
      method = "REML",
      test = "z"
    ),
    error = function(e) NULL
  )

  random_hk_fit <- tryCatch(
    metafor::rma.uni(
      yi = yi,
      sei = sei,
      method = "REML",
      test = "knha"
    ),
    error = function(e) NULL
  )

  list(
    fixed = extract_rma(
      fixed_fit
    ),
    random_z = extract_rma(
      random_z_fit
    ),
    random_hk = extract_rma(
      random_hk_fit
    )
  )
}

calculate_cohort_effects <- function(
  loaded_set,
  family_features,
  feature_space
) {
  meta <- loaded_set$meta
  counts <- loaded_set$counts[
    ,
    family_features,
    drop = FALSE
  ]

  clr <- clr_transform(
    counts,
    clr_pseudocount
  )

  relative <- relative_by_cleaned_depth(
    counts,
    meta$Cleaned_Target_Depth
  )

  group_a_index <-
    meta$Comparison_Group ==
      loaded_set$group_a

  group_b_index <-
    meta$Comparison_Group ==
      loaded_set$group_b

  rows <- list()

  for (feature in family_features) {
    clr_a <- clr[
      group_a_index,
      feature
    ]

    clr_b <- clr[
      group_b_index,
      feature
    ]

    relative_a <- relative[
      group_a_index,
      feature
    ]

    relative_b <- relative[
      group_b_index,
      feature
    ]

    count_a <- counts[
      group_a_index,
      feature
    ]

    count_b <- counts[
      group_b_index,
      feature
    ]

    md <- welch_effect(
      clr_a,
      clr_b
    )

    smd <- hedges_g_effect(
      clr_a,
      clr_b
    )

    rows[[
      length(rows) + 1
    ]] <- data.frame(
      Feature_Space = feature_space,
      Meta_Family = loaded_set$family,
      Analysis_Set = loaded_set$set_id,
      Project_ID = loaded_set$project,
      Discovery_Role =
        loaded_set$discovery_role,
      Group_A = loaded_set$group_a,
      Group_B = loaded_set$group_b,
      Genus_Feature = feature,
      N_Group_A = sum(
        group_a_index
      ),
      N_Group_B = sum(
        group_b_index
      ),
      Detected_Group_A = sum(
        count_a > 0
      ),
      Detected_Group_B = sum(
        count_b > 0
      ),
      Prevalence_Group_A = mean(
        count_a > 0
      ),
      Prevalence_Group_B = mean(
        count_b > 0
      ),
      Mean_Relative_Group_A = mean(
        relative_a
      ),
      Mean_Relative_Group_B = mean(
        relative_b
      ),
      CLR_Mean_Group_A = mean(
        clr_a
      ),
      CLR_Mean_Group_B = mean(
        clr_b
      ),
      CLR_Effect = md$effect,
      CLR_SE = md$se,
      CLR_CI_Low = md$ci_low,
      CLR_CI_High = md$ci_high,
      CLR_T = md$t,
      CLR_Df = md$df,
      CLR_P = md$p,
      Hedges_g = smd$g,
      Hedges_g_SE = smd$se,
      Direction = if (
        is.finite(md$effect)
      ) {
        if (
          md$effect > 0
        ) {
          "Higher_in_Sepsis"
        } else if (
          md$effect < 0
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

  out <- do.call(
    rbind,
    rows
  )

  out$CLR_FDR <- bh_adjust(
    out$CLR_P
  )

  out
}

meta_from_cohort_effects <- function(
  cohort_effects,
  feature_space
) {
  rows <- list()

  for (
    family in unique(
      cohort_effects$Meta_Family
    )
  ) {
    family_df <- cohort_effects[
      cohort_effects$Meta_Family == family &
        cohort_effects$Feature_Space ==
          feature_space,
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

      models <- fit_meta_models(
        z$CLR_Effect,
        z$CLR_SE
      )

      smd_models <- fit_meta_models(
        z$Hedges_g,
        z$Hedges_g_SE
      )

      positive_n <- sum(
        z$CLR_Effect > 0,
        na.rm = TRUE
      )

      negative_n <- sum(
        z$CLR_Effect < 0,
        na.rm = TRUE
      )

      direction_consistent <- (
        positive_n == nrow(z) ||
          negative_n == nrow(z)
      )

      rows[[
        length(rows) + 1
      ]] <- data.frame(
        Feature_Space = feature_space,
        Meta_Family = family,
        Genus_Feature = feature,
        K = nrow(z),
        Sets = paste(
          z$Analysis_Set,
          collapse = " | "
        ),
        Projects = paste(
          z$Project_ID,
          collapse = " | "
        ),
        Positive_Effects = positive_n,
        Negative_Effects = negative_n,
        Direction_Consistent =
          direction_consistent,
        Minimum_Cohort_Effect = min(
          z$CLR_Effect,
          na.rm = TRUE
        ),
        Maximum_Cohort_Effect = max(
          z$CLR_Effect,
          na.rm = TRUE
        ),
        Minimum_Cohort_CLR_FDR = min(
          z$CLR_FDR,
          na.rm = TRUE
        ),
        Fixed_Effect =
          models$fixed$estimate,
        Fixed_SE =
          models$fixed$se,
        Fixed_CI_Low =
          models$fixed$ci_low,
        Fixed_CI_High =
          models$fixed$ci_high,
        Fixed_P =
          models$fixed$p,
        REML_Effect =
          models$random_z$estimate,
        REML_SE =
          models$random_z$se,
        REML_CI_Low =
          models$random_z$ci_low,
        REML_CI_High =
          models$random_z$ci_high,
        REML_P =
          models$random_z$p,
        REML_Prediction_Low =
          models$random_z$pi_low,
        REML_Prediction_High =
          models$random_z$pi_high,
        Tau2 =
          models$random_z$tau2,
        I2 =
          models$random_z$i2,
        H2 =
          models$random_z$h2,
        Q =
          models$random_z$qe,
        Q_P =
          models$random_z$qep,
        HK_Effect =
          models$random_hk$estimate,
        HK_SE =
          models$random_hk$se,
        HK_CI_Low =
          models$random_hk$ci_low,
        HK_CI_High =
          models$random_hk$ci_high,
        HK_P =
          models$random_hk$p,
        SMD_Fixed_Effect =
          smd_models$fixed$estimate,
        SMD_Fixed_P =
          smd_models$fixed$p,
        SMD_REML_Effect =
          smd_models$random_z$estimate,
        SMD_REML_P =
          smd_models$random_z$p,
        SMD_Direction_Consistent = (
          sum(
            z$Hedges_g > 0,
            na.rm = TRUE
          ) == nrow(z) ||
            sum(
              z$Hedges_g < 0,
              na.rm = TRUE
            ) == nrow(z)
        ),
        Direction = if (
          is.finite(
            models$random_z$estimate
          )
        ) {
          if (
            models$random_z$estimate > 0
          ) {
            "Higher_in_Sepsis"
          } else if (
            models$random_z$estimate < 0
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
  }

  out <- do.call(
    rbind,
    rows
  )

  for (
    family in unique(
      out$Meta_Family
    )
  ) {
    idx <- out$Meta_Family == family

    out$Fixed_FDR[idx] <-
      bh_adjust(
        out$Fixed_P[idx]
      )

    out$REML_FDR[idx] <-
      bh_adjust(
        out$REML_P[idx]
      )

    out$HK_FDR[idx] <-
      bh_adjust(
        out$HK_P[idx]
      )

    out$SMD_Fixed_FDR[idx] <-
      bh_adjust(
        out$SMD_Fixed_P[idx]
      )

    out$SMD_REML_FDR[idx] <-
      bh_adjust(
        out$SMD_REML_P[idx]
      )
  }

  out
}

make_meta_overview_plot <- function(
  meta_df,
  family,
  path
) {
  z <- meta_df[
    meta_df$Meta_Family == family,
    ,
    drop = FALSE
  ]

  valid <- is.finite(z$REML_Effect) &
    is.finite(z$Fixed_FDR)

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
    title(family)
    text(
      0.5,
      0.5,
      "No estimable effects"
    )
    return(invisible(NULL))
  }

  y <- -log10(
    pmax(
      z$Fixed_FDR[valid],
      .Machine$double.xmin
    )
  )

  plot(
    z$REML_Effect[valid],
    y,
    xlab = "REML pooled CLR effect: Sepsis minus control",
    ylab = "-log10 fixed-effect BH FDR",
    main = paste0(
      family,
      " harmonized meta-analysis"
    ),
    pch = 16
  )

  abline(
    v = 0,
    lty = 3
  )

  abline(
    h = -log10(0.05),
    lty = 2
  )
}

make_forest_pdf <- function(
  family,
  cohort_effects,
  meta_df,
  path,
  maximum_genera = 20
) {
  family_meta <- meta_df[
    meta_df$Meta_Family == family &
      meta_df$K >= 2,
    ,
    drop = FALSE
  ]

  family_meta <- family_meta[
    order(
      family_meta$Fixed_FDR,
      family_meta$REML_FDR,
      -abs(
        family_meta$REML_Effect
      )
    ),
    ,
    drop = FALSE
  ]

  family_meta <- head(
    family_meta,
    maximum_genera
  )

  pdf(
    path,
    width = 8.5,
    height = 6
  )

  if (nrow(family_meta) == 0) {
    plot.new()
    title(family)
    text(
      0.5,
      0.5,
      "No genera available"
    )
    dev.off()
    return(invisible(NULL))
  }

  for (
    feature in family_meta$Genus_Feature
  ) {
    z <- cohort_effects[
      cohort_effects$Meta_Family == family &
        cohort_effects$Genus_Feature == feature &
        cohort_effects$Feature_Space ==
          "Primary_harmonized",
      ,
      drop = FALSE
    ]

    fit <- tryCatch(
      metafor::rma.uni(
        yi = z$CLR_Effect,
        sei = z$CLR_SE,
        method = "REML",
        test = "z"
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      next
    }

    metafor::forest(
      fit,
      slab = paste0(
        z$Project_ID,
        " / ",
        z$Analysis_Set
      ),
      xlab = "CLR effect: Sepsis minus control",
      header = c(
        "Cohort",
        "Effect [95% CI]"
      )
    )

    title(
      main = feature,
      sub = family
    )
  }

  dev.off()
}

# ------------------------------------------------------------
# Load all discovery sets
# ------------------------------------------------------------
loaded_sets <- list()

for (i in seq_len(nrow(set_config))) {
  set_id <- set_config$Analysis_Set[i]

  loaded_sets[[set_id]] <-
    load_analysis_set(
      set_config[i, , drop = FALSE]
    )

  cat(
    "Loaded ",
    set_id,
    ": ",
    nrow(
      loaded_sets[[set_id]]$meta
    ),
    " samples, ",
    ncol(
      loaded_sets[[set_id]]$counts
    ),
    " classified genera.\n",
    sep = ""
  )
}

# ------------------------------------------------------------
# Family-specific feature harmonization
# ------------------------------------------------------------
harmonization_rows <- list()
filter_detail_rows <- list()
family_features <- list()
strict_family_features <- list()

for (
  family in names(
    family_config
  )
) {
  sets <- family_config[[family]]

  primary_lists <- list()
  strict_lists <- list()

  for (set_id in sets) {
    loaded <- loaded_sets[[set_id]]
    filter_table <- loaded$filter

    filter_table$Analysis_Set <-
      set_id
    filter_table$Project_ID <-
      loaded$project
    filter_table$Meta_Family <-
      family

    filter_detail_rows[[
      length(filter_detail_rows) + 1
    ]] <- filter_table

    primary_lists[[set_id]] <-
      filter_table$Genus_Feature[
        filter_table$Primary_Eligible
      ]

    strict_lists[[set_id]] <-
      filter_table$Genus_Feature[
        filter_table$Strict_Eligible
      ]
  }

  common_primary <- Reduce(
    intersect,
    primary_lists
  )

  common_strict <- Reduce(
    intersect,
    strict_lists
  )

  family_features[[family]] <-
    common_primary

  strict_family_features[[family]] <-
    common_strict

  harmonization_rows[[
    length(harmonization_rows) + 1
  ]] <- data.frame(
    Meta_Family = family,
    Analysis_Sets = paste(
      sets,
      collapse = " | "
    ),
    Primary_Eligible_Set_1 = length(
      primary_lists[[sets[1]]]
    ),
    Primary_Eligible_Set_2 = length(
      primary_lists[[sets[2]]]
    ),
    Primary_Common_Genera = length(
      common_primary
    ),
    Strict_Eligible_Set_1 = length(
      strict_lists[[sets[1]]]
    ),
    Strict_Eligible_Set_2 = length(
      strict_lists[[sets[2]]]
    ),
    Strict_Common_Genera = length(
      common_strict
    ),
    Primary_Genera_List = paste(
      common_primary,
      collapse = " | "
    ),
    Strict_Genera_List = paste(
      common_strict,
      collapse = " | "
    ),
    Status = if (
      length(common_primary) >= 2 &&
        length(common_strict) >= 2
    ) {
      "PASS"
    } else {
      "FAIL"
    },
    stringsAsFactors = FALSE
  )
}

harmonization_summary <- do.call(
  rbind,
  harmonization_rows
)

filter_details <- do.call(
  rbind,
  filter_detail_rows
)

if (
  any(
    harmonization_summary$Status !=
      "PASS"
  )
) {
  stop(
    "至少一个Meta家族的共同特征不足。"
  )
}

# ------------------------------------------------------------
# Recompute cohort effects on common feature spaces
# ------------------------------------------------------------
cohort_effect_rows <- list()

for (
  family in names(
    family_config
  )
) {
  for (
    set_id in family_config[[family]]
  ) {
    loaded <- loaded_sets[[set_id]]

    cohort_effect_rows[[
      length(cohort_effect_rows) + 1
    ]] <- calculate_cohort_effects(
      loaded,
      family_features[[family]],
      "Primary_harmonized"
    )

    cohort_effect_rows[[
      length(cohort_effect_rows) + 1
    ]] <- calculate_cohort_effects(
      loaded,
      strict_family_features[[family]],
      "Strict_harmonized"
    )
  }
}

cohort_effects <- do.call(
  rbind,
  cohort_effect_rows
)

# ------------------------------------------------------------
# Formal meta-analyses
# ------------------------------------------------------------
primary_meta <- meta_from_cohort_effects(
  cohort_effects,
  "Primary_harmonized"
)

strict_meta <- meta_from_cohort_effects(
  cohort_effects,
  "Strict_harmonized"
)

# ------------------------------------------------------------
# Strict-filter stability
# ------------------------------------------------------------
strict_key <- strict_meta[
  ,
  c(
    "Meta_Family",
    "Genus_Feature",
    "K",
    "Direction_Consistent",
    "Fixed_Effect",
    "Fixed_P",
    "Fixed_FDR",
    "REML_Effect",
    "REML_P",
    "REML_FDR",
    "HK_P",
    "HK_FDR",
    "Tau2",
    "I2"
  ),
  drop = FALSE
]

names(strict_key)[
  !(names(strict_key) %in%
      c(
        "Meta_Family",
        "Genus_Feature"
      ))
] <- paste0(
  "Strict_",
  names(strict_key)[
    !(names(strict_key) %in%
        c(
          "Meta_Family",
          "Genus_Feature"
        ))
  ]
)

meta_combined <- merge(
  primary_meta,
  strict_key,
  by = c(
    "Meta_Family",
    "Genus_Feature"
  ),
  all.x = TRUE,
  sort = FALSE
)

meta_combined$Strict_Filter_Available <-
  !is.na(
    meta_combined$Strict_K
  )

meta_combined$Strict_Direction_Stable <-
  meta_combined$Strict_Filter_Available &
  is.finite(
    meta_combined$REML_Effect
  ) &
  is.finite(
    meta_combined$Strict_REML_Effect
  ) &
  sign(
    meta_combined$REML_Effect
  ) ==
    sign(
      meta_combined$Strict_REML_Effect
    ) &
  meta_combined$Strict_Direction_Consistent

# ------------------------------------------------------------
# Grade family-level evidence
# ------------------------------------------------------------
meta_combined$Candidate_Grade <-
  "Insufficient_or_Inconsistent"

icu <- meta_combined$Meta_Family ==
  "ICU_control"

healthy <- meta_combined$Meta_Family ==
  "Healthy_control"

icu_tier1 <- icu &
  meta_combined$K == 2 &
  meta_combined$Direction_Consistent &
  meta_combined$Strict_Direction_Stable &
  meta_combined$Fixed_FDR < 0.05 &
  meta_combined$REML_FDR < 0.10 &
  meta_combined$I2 <=
    heterogeneity_review_i2

meta_combined$Candidate_Grade[
  icu_tier1
] <- "ICU_Tier1_Robust"

icu_tier2 <- icu &
  !icu_tier1 &
  meta_combined$K == 2 &
  meta_combined$Direction_Consistent &
  meta_combined$Strict_Direction_Stable &
  meta_combined$I2 <=
    heterogeneity_review_i2 &
  (
    meta_combined$Fixed_P < 0.05 |
      meta_combined$REML_P < 0.05 |
      meta_combined$Minimum_Cohort_CLR_FDR <
        0.05
  )

meta_combined$Candidate_Grade[
  icu_tier2
] <- "ICU_Tier2_Supportive"

icu_tier3 <- icu &
  !icu_tier1 &
  !icu_tier2 &
  meta_combined$K == 2 &
  meta_combined$Direction_Consistent &
  meta_combined$Strict_Direction_Stable

meta_combined$Candidate_Grade[
  icu_tier3
] <- "ICU_Tier3_Directional"

healthy_robust <- healthy &
  meta_combined$K == 2 &
  meta_combined$Direction_Consistent &
  meta_combined$Strict_Direction_Stable &
  meta_combined$Fixed_FDR < 0.05 &
  meta_combined$REML_FDR < 0.10 &
  meta_combined$I2 <=
    heterogeneity_review_i2

meta_combined$Candidate_Grade[
  healthy_robust
] <- "Healthy_Robust_Dysbiosis"

healthy_heterogeneous <- healthy &
  !healthy_robust &
  meta_combined$K == 2 &
  meta_combined$Direction_Consistent &
  meta_combined$Fixed_FDR < 0.05

meta_combined$Candidate_Grade[
  healthy_heterogeneous
] <-
  "Healthy_Heterogeneous_Dysbiosis"

meta_combined$Proposed_SDI_Use <- ifelse(
  meta_combined$Candidate_Grade ==
    "ICU_Tier1_Robust",
  "Primary_candidate",
  ifelse(
    meta_combined$Candidate_Grade ==
      "ICU_Tier2_Supportive",
    "Secondary_candidate",
    "Not_for_core_SDI"
  )
)

meta_combined$Proposed_SDI_Sign <- ifelse(
  meta_combined$REML_Effect > 0,
  1,
  ifelse(
    meta_combined$REML_Effect < 0,
    -1,
    NA_real_
  )
)

# ------------------------------------------------------------
# Combine ICU and Healthy context for each genus
# ------------------------------------------------------------
context_columns <- c(
  "Genus_Feature",
  "Candidate_Grade",
  "Proposed_SDI_Use",
  "Proposed_SDI_Sign",
  "Direction",
  "Direction_Consistent",
  "Strict_Direction_Stable",
  "Fixed_Effect",
  "Fixed_P",
  "Fixed_FDR",
  "REML_Effect",
  "REML_P",
  "REML_FDR",
  "HK_P",
  "HK_FDR",
  "Tau2",
  "I2",
  "Minimum_Cohort_CLR_FDR"
)

icu_context <- meta_combined[
  meta_combined$Meta_Family ==
    "ICU_control",
  context_columns,
  drop = FALSE
]

healthy_context <- meta_combined[
  meta_combined$Meta_Family ==
    "Healthy_control",
  context_columns,
  drop = FALSE
]

names(icu_context)[-1] <- paste0(
  "ICU_",
  names(icu_context)[-1]
)

names(healthy_context)[-1] <- paste0(
  "Healthy_",
  names(healthy_context)[-1]
)

candidate_grading <- merge(
  icu_context,
  healthy_context,
  by = "Genus_Feature",
  all = TRUE,
  sort = FALSE
)

candidate_grading$Context_Relationship <- ifelse(
  is.na(
    candidate_grading$ICU_REML_Effect
  ) |
    is.na(
      candidate_grading$Healthy_REML_Effect
    ),
  "One_context_not_estimable",
  ifelse(
    sign(
      candidate_grading$ICU_REML_Effect
    ) ==
      sign(
        candidate_grading$Healthy_REML_Effect
      ),
    "Same_direction",
    ifelse(
      candidate_grading$ICU_Fixed_FDR <
        0.05 &
        candidate_grading$Healthy_Fixed_FDR <
          0.05,
      "Significant_opposite_contexts",
      "Opposite_direction_without_dual_significance"
    )
  )
)

candidate_grading$Final_Proposed_SDI_Use <-
  ifelse(
    candidate_grading$ICU_Candidate_Grade ==
      "ICU_Tier1_Robust",
    "Primary_candidate",
    ifelse(
      candidate_grading$ICU_Candidate_Grade ==
        "ICU_Tier2_Supportive",
      "Secondary_candidate",
      "Not_for_core_SDI"
    )
  )

candidate_grading$Final_Proposed_SDI_Direction <-
  ifelse(
    candidate_grading$ICU_REML_Effect > 0,
    "Higher_in_Sepsis",
    ifelse(
      candidate_grading$ICU_REML_Effect < 0,
      "Lower_in_Sepsis",
      NA_character_
    )
  )

candidate_grading$Final_Proposed_SDI_Sign <-
  ifelse(
    candidate_grading$ICU_REML_Effect > 0,
    1,
    ifelse(
      candidate_grading$ICU_REML_Effect < 0,
      -1,
      NA_real_
    )
  )

candidate_lock_proposal <- candidate_grading[
  candidate_grading$Final_Proposed_SDI_Use %in%
    c(
      "Primary_candidate",
      "Secondary_candidate"
    ),
  ,
  drop = FALSE
]

candidate_lock_proposal <- candidate_lock_proposal[
  order(
    candidate_lock_proposal$Final_Proposed_SDI_Use,
    candidate_lock_proposal$ICU_Fixed_FDR,
    candidate_lock_proposal$ICU_REML_FDR
  ),
  ,
  drop = FALSE
]

candidate_lock_proposal$Reviewer <- ""
candidate_lock_proposal$Review_Date <- ""
candidate_lock_proposal$Lock_Decision <- ""
candidate_lock_proposal$Decision_Notes <- ""

# ------------------------------------------------------------
# Leave-one-out sensitivity
#
# With two cohorts, excluding one leaves a single cohort and no
# formal pooled model. We report the remaining cohort estimate
# transparently rather than pretending it is a meta-analysis.
# ------------------------------------------------------------
leave_one_out_rows <- list()

primary_cohort_effects <- cohort_effects[
  cohort_effects$Feature_Space ==
    "Primary_harmonized",
  ,
  drop = FALSE
]

for (
  family in unique(
    primary_cohort_effects$Meta_Family
  )
) {
  family_df <- primary_cohort_effects[
    primary_cohort_effects$Meta_Family ==
      family,
    ,
    drop = FALSE
  ]

  family_sets <- unique(
    family_df$Analysis_Set
  )

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

    for (
      excluded_set in family_sets
    ) {
      remaining <- z[
        z$Analysis_Set != excluded_set,
        ,
        drop = FALSE
      ]

      if (nrow(remaining) == 1) {
        leave_one_out_rows[[
          length(leave_one_out_rows) + 1
        ]] <- data.frame(
          Meta_Family = family,
          Genus_Feature = feature,
          Excluded_Set = excluded_set,
          Remaining_Set =
            remaining$Analysis_Set,
          Remaining_Project =
            remaining$Project_ID,
          Remaining_K = 1,
          Estimate =
            remaining$CLR_Effect,
          SE =
            remaining$CLR_SE,
          CI_Low =
            remaining$CLR_CI_Low,
          CI_High =
            remaining$CLR_CI_High,
          P =
            remaining$CLR_P,
          FDR =
            remaining$CLR_FDR,
          Direction =
            remaining$Direction,
          Interpretation =
            "SINGLE_COHORT_ONLY_NOT_META_ANALYSIS",
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

leave_one_out <- do.call(
  rbind,
  leave_one_out_rows
)

# ------------------------------------------------------------
# Compare Day 8 set-specific CLR effects with Day 9 harmonized
# CLR effects. This is an audit, not a statistical test.
# ------------------------------------------------------------
day8_meta_file <- file.path(
  day8_report_root,
  "16_Meta_Ready_Genus_Effects.csv"
)

effect_comparison <- data.frame()

if (file.exists(day8_meta_file)) {
  day8_effects <- read.csv(
    day8_meta_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  comparison_key <- primary_cohort_effects[
    ,
    c(
      "Analysis_Set",
      "Project_ID",
      "Meta_Family",
      "Genus_Feature",
      "CLR_Effect",
      "CLR_SE",
      "CLR_P",
      "CLR_FDR"
    ),
    drop = FALSE
  ]

  names(comparison_key)[
    names(comparison_key) %in%
      c(
        "CLR_Effect",
        "CLR_SE",
        "CLR_P",
        "CLR_FDR"
      )
  ] <- paste0(
    "Harmonized_",
    names(comparison_key)[
      names(comparison_key) %in%
        c(
          "CLR_Effect",
          "CLR_SE",
          "CLR_P",
          "CLR_FDR"
        )
    ]
  )

  day8_key <- day8_effects[
    ,
    c(
      "Analysis_Set",
      "Project_ID",
      "Meta_Family",
      "Genus_Feature",
      "CLR_Effect",
      "CLR_SE",
      "CLR_P",
      "CLR_FDR"
    ),
    drop = FALSE
  ]

  names(day8_key)[
    names(day8_key) %in%
      c(
        "CLR_Effect",
        "CLR_SE",
        "CLR_P",
        "CLR_FDR"
      )
  ] <- paste0(
    "Day8_SetSpecific_",
    names(day8_key)[
      names(day8_key) %in%
        c(
          "CLR_Effect",
          "CLR_SE",
          "CLR_P",
          "CLR_FDR"
        )
    ]
  )

  effect_comparison <- merge(
    comparison_key,
    day8_key,
    by = c(
      "Analysis_Set",
      "Project_ID",
      "Meta_Family",
      "Genus_Feature"
    ),
    all.x = TRUE,
    sort = FALSE
  )

  effect_comparison$Effect_Difference <-
    effect_comparison$Harmonized_CLR_Effect -
    effect_comparison$Day8_SetSpecific_CLR_Effect

  effect_comparison$Direction_Changed <-
    is.finite(
      effect_comparison$Harmonized_CLR_Effect
    ) &
    is.finite(
      effect_comparison$Day8_SetSpecific_CLR_Effect
    ) &
    sign(
      effect_comparison$Harmonized_CLR_Effect
    ) !=
      sign(
        effect_comparison$Day8_SetSpecific_CLR_Effect
      )
}

# ------------------------------------------------------------
# Plots
# ------------------------------------------------------------
for (
  family in names(
    family_config
  )
) {
  make_meta_overview_plot(
    meta_combined,
    family,
    file.path(
      plot_root,
      paste0(
        "17_",
        family,
        "_Meta_Overview.pdf"
      )
    )
  )

  make_forest_pdf(
    family,
    cohort_effects,
    meta_combined,
    file.path(
      plot_root,
      paste0(
        "17_",
        family,
        "_Top_Forest_Plots.pdf"
      )
    ),
    maximum_genera = 20
  )
}

# ------------------------------------------------------------
# Analysis audit
# ------------------------------------------------------------
audit_rows <- list()

for (
  family in names(
    family_config
  )
) {
  harmonization <- harmonization_summary[
    harmonization_summary$Meta_Family ==
      family,
    ,
    drop = FALSE
  ]

  family_primary <- meta_combined[
    meta_combined$Meta_Family ==
      family,
    ,
    drop = FALSE
  ]

  fail_reasons <- character()
  review_reasons <- character()

  if (
    harmonization$Status != "PASS"
  ) {
    fail_reasons <- c(
      fail_reasons,
      "Feature harmonization failed"
    )
  }

  if (
    any(
      family_primary$K != 2
    )
  ) {
    fail_reasons <- c(
      fail_reasons,
      "At least one pooled genus does not have k=2"
    )
  }

  if (
    any(
      !is.finite(
        family_primary$REML_Effect
      )
    )
  ) {
    fail_reasons <- c(
      fail_reasons,
      "At least one REML effect is not estimable"
    )
  }

  if (
    any(
      family_primary$I2 >
        heterogeneity_review_i2,
      na.rm = TRUE
    )
  ) {
    review_reasons <- c(
      review_reasons,
      paste0(
        "Some genera have I2>",
        heterogeneity_review_i2,
        "%"
      )
    )
  }

  if (
    family == "Healthy_control"
  ) {
    review_reasons <- c(
      review_reasons,
      paste0(
        "A06 is a small 6-vs-7 cohort; ",
        "leave-one-out leaves only A03 and is ",
        "not a formal meta-analysis"
      )
    )
  }

  audit_rows[[
    length(audit_rows) + 1
  ]] <- data.frame(
    Meta_Family = family,
    Analysis_Sets = harmonization$Analysis_Sets,
    Primary_Common_Genera =
      harmonization$Primary_Common_Genera,
    Strict_Common_Genera =
      harmonization$Strict_Common_Genera,
    Meta_Genera_K2 = sum(
      family_primary$K == 2
    ),
    Direction_Consistent_Genera = sum(
      family_primary$Direction_Consistent
    ),
    Strict_Stable_Genera = sum(
      family_primary$Strict_Direction_Stable,
      na.rm = TRUE
    ),
    Tier1_or_Healthy_Robust = sum(
      family_primary$Candidate_Grade %in%
        c(
          "ICU_Tier1_Robust",
          "Healthy_Robust_Dysbiosis"
        )
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
}

analysis_audit <- do.call(
  rbind,
  audit_rows
)

global_audit <- data.frame(
  Check_Name = c(
    "Locked_external_sets_read",
    "Discovery_sets_read",
    "Family_specific_harmonization",
    "Random_effect_model",
    "Small_k_sensitivity",
    "Fixed_effect_sensitivity",
    "Strict_filter_sensitivity",
    "Candidate_lock_status"
  ),
  Status = c(
    "PASS",
    "PASS",
    if (
      all(
        harmonization_summary$Status ==
          "PASS"
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
    "REVIEW_REQUIRED"
  ),
  Details = c(
    "A04 and A05 were not configured or read.",
    "Only A01, A02, A03, and A06 were read.",
    paste0(
      "CLR was recomputed using the same common genus set ",
      "within each Meta_Family."
    ),
    "REML random-effects model with z-based inference.",
    paste0(
      "Knapp-Hartung estimates and confidence intervals were ",
      "reported because each family contains only two cohorts."
    ),
    "Inverse-variance fixed-effect results were reported.",
    paste0(
      "Effects were recomputed on a stricter family-common ",
      "feature set."
    ),
    paste0(
      "17_Candidate_Genus_Lock_Proposal.csv is a proposal. ",
      "External validation remains locked until manual review."
    )
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------
wcsv(
  harmonization_summary,
  file.path(
    report_root,
    "17_Family_Feature_Harmonization.csv"
  )
)

wcsv(
  filter_details,
  file.path(
    output_root,
    "17_Set_Level_Feature_Filter_Details.csv"
  )
)

wcsv(
  cohort_effects,
  file.path(
    output_root,
    "17_Cohort_Effects_Harmonized_CLR.csv"
  )
)

wcsv(
  meta_combined,
  file.path(
    report_root,
    "17_Random_Fixed_Meta_Results.csv"
  )
)

wcsv(
  strict_meta,
  file.path(
    report_root,
    "17_Strict_Filter_Meta_Results.csv"
  )
)

wcsv(
  leave_one_out,
  file.path(
    report_root,
    "17_Leave_One_Out_Sensitivity.csv"
  )
)

wcsv(
  candidate_grading,
  file.path(
    report_root,
    "17_Candidate_Genus_Grading.csv"
  )
)

wcsv(
  candidate_lock_proposal,
  file.path(
    report_root,
    "17_Candidate_Genus_Lock_Proposal.csv"
  )
)

wcsv(
  analysis_audit,
  file.path(
    report_root,
    "17_Meta_Analysis_Audit.csv"
  )
)

wcsv(
  global_audit,
  file.path(
    report_root,
    "17_Global_Meta_Workflow_Audit.csv"
  )
)

if (nrow(effect_comparison) > 0) {
  wcsv(
    effect_comparison,
    file.path(
      report_root,
      "17_Day8_vs_Harmonized_Effect_Audit.csv"
    )
  )
}

capture.output(
  sessionInfo(),
  file = file.path(
    report_root,
    "17_Harmonized_Meta_SessionInfo.txt"
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
    "17_Files_To_Hash.txt"
  ),
  useBytes = TRUE
)

cat(
  "\n============================================================\n"
)
cat("Day 9 completed.\n")
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
cat("A04/A05 remained locked and unread.\n")
cat("Important outputs:\n")
cat("  17_Family_Feature_Harmonization.csv\n")
cat("  17_Random_Fixed_Meta_Results.csv\n")
cat("  17_Strict_Filter_Meta_Results.csv\n")
cat("  17_Candidate_Genus_Grading.csv\n")
cat("  17_Candidate_Genus_Lock_Proposal.csv\n")
cat("  17_Meta_Analysis_Audit.csv\n")
cat("  17_Global_Meta_Workflow_Audit.csv\n")
cat(
  "Finished: ",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)
