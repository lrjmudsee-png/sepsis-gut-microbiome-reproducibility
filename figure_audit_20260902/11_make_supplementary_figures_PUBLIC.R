# ============================================================
# Sepsis V1 — PUBLIC Supplementary Figure Reproduction v1.1
# File: 11_make_supplementary_figures_PUBLIC_v1_1.R
# v1.1: Windows/TRE path-manifest fix; scientific/plot logic unchanged.
#
# PURPOSE
#   Reproduce Supplementary Figures S1–S3 from the PUBLIC package
#   using frozen, package-relative source files only.
#
# SCIENTIFIC BOUNDARY
#   - NO statistical model is refit.
#   - NO frozen result is overwritten.
#   - S1 uses frozen cohort/analysis-set summary values.
#   - S2 plots descriptive mean-difference 95% CIs; formal inference
#     remains Wilcoxon + Benjamini–Hochberg FDR in the frozen source.
#   - S3 is post-hoc leave-one-genus-out failure/robustness diagnosis,
#     equal-weight SDI only; project-level absent genera remain absent,
#     never treated as zero effect.
#
# REQUIRED PUBLIC PACKAGE FILES
#   expected_results/frozen_result_inputs/
#     01_15_Analysis_Set_Summary.csv
#     04_16_Alpha_Diversity_Tests.csv
#     23_19_Leave_One_Genus_Out_Sensitivity.csv
#
# USAGE
#   From the package root:
#     Rscript figure_audit_20260902/11_make_supplementary_figures_PUBLIC.R .
#
#   Or from anywhere:
#     Rscript 11_make_supplementary_figures_PUBLIC.R "/path/to/package-root"
#
#   Optional argument 2 = output directory.
# ============================================================

options(stringsAsFactors = FALSE)

# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------
required_pkgs <- c("ggplot2", "dplyr", "patchwork", "scales")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    paste0(
      "Missing R packages: ",
      paste(missing_pkgs, collapse = ", "),
      "\nInstall them with:\n",
      "install.packages(c(",
      paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
      "), repos = \"https://cloud.r-project.org\")"
    )
  )
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(scales)
})

# ------------------------------------------------------------
# 1. Package-root discovery
# ------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

script_dir <- function() {
  full <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", full, value = TRUE)
  if (length(hit)) {
    p <- sub("^--file=", "", hit[[1]])
    return(dirname(normalizePath(p, winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

required_rel <- c(
  "expected_results/frozen_result_inputs/01_15_Analysis_Set_Summary.csv",
  "expected_results/frozen_result_inputs/04_16_Alpha_Diversity_Tests.csv",
  "expected_results/frozen_result_inputs/23_19_Leave_One_Genus_Out_Sensitivity.csv"
)

is_pkg_root <- function(x) {
  all(file.exists(file.path(x, required_rel)))
}

find_pkg_root <- function() {
  if (length(args) >= 1L && nzchar(args[[1]])) {
    p <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
    if (!is_pkg_root(p)) {
      stop(
        "Argument 1 is not a valid public package root:\n", p,
        "\nRequired files were not all found under expected_results/frozen_result_inputs/."
      )
    }
    return(p)
  }

  starts <- unique(c(script_dir(), getwd()))
  for (st in starts) {
    cur <- normalizePath(st, winslash = "/", mustWork = FALSE)
    for (i in 0:6) {
      if (is_pkg_root(cur)) return(cur)
      par <- dirname(cur)
      if (identical(par, cur)) break
      cur <- par
    }
  }

  stop(
    "Could not auto-detect package root.\n",
    "Pass the public package root explicitly as argument 1."
  )
}

PKG_ROOT <- find_pkg_root()
SRC_ROOT <- file.path(PKG_ROOT, "expected_results", "frozen_result_inputs")

OUT_DIR <- if (length(args) >= 2L && nzchar(args[[2]])) {
  normalizePath(args[[2]], winslash = "/", mustWork = FALSE)
} else {
  file.path(PKG_ROOT, "supplementary_figures_reproduced_public")
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cohort_file <- file.path(SRC_ROOT, "01_15_Analysis_Set_Summary.csv")
alpha_file  <- file.path(SRC_ROOT, "04_16_Alpha_Diversity_Tests.csv")
loo_file    <- file.path(SRC_ROOT, "23_19_Leave_One_Genus_Out_Sensitivity.csv")

cat("Public package root:\n", PKG_ROOT, "\n\n", sep = "")
cat("Supplementary Figure output:\n", OUT_DIR, "\n\n", sep = "")

# ------------------------------------------------------------
# 2. Helpers
# ------------------------------------------------------------
read_csv_strict <- function(path) {
  if (!file.exists(path)) stop("Required input file not found:\n", path)

  tryCatch(
    read.csv(
      path,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM"
    ),
    error = function(e) {
      read.csv(
        path,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
  )
}

require_columns <- function(dat, cols, object_name) {
  missing_cols <- setdiff(cols, names(dat))
  if (length(missing_cols) > 0) {
    stop(
      object_name,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
}

pretty_genus <- function(x) {
  x <- sub("^Genus__", "", as.character(x))
  x <- sub("\\|Family__.*$", "", x)
  x
}

theme_v1 <- function(base_size = 10.5) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = base_size + 2,
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        size = base_size - 0.5,
        colour = "#4A4A4A",
        lineheight = 1.08,
        margin = margin(b = 8)
      ),
      axis.title = element_text(face = "plain"),
      axis.text = element_text(colour = "#222222"),
      strip.background = element_rect(fill = "#F2F2F2", colour = NA),
      strip.text = element_text(face = "bold", colour = "#222222"),
      legend.title = element_blank(),
      legend.position = "top",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 16, 10, 10)
    )
}

save_plot <- function(p, filename_stub, width, height) {
  pdf_path <- file.path(OUT_DIR, paste0(filename_stub, ".pdf"))
  tif_path <- file.path(OUT_DIR, paste0(filename_stub, ".tiff"))

  ggsave(
    filename = pdf_path,
    plot = p,
    width = width,
    height = height,
    units = "in",
    device = cairo_pdf
  )

  tiff_ok <- TRUE
  tryCatch(
    ggsave(
      filename = tif_path,
      plot = p,
      width = width,
      height = height,
      units = "in",
      dpi = 600,
      device = "tiff",
      compression = "lzw",
      bg = "white"
    ),
    error = function(e) {
      tiff_ok <<- FALSE
      warning(
        "TIFF export failed on this system; PDF was still created.\n",
        conditionMessage(e)
      )
    }
  )

  invisible(c(pdf = pdf_path, tiff = if (tiff_ok) tif_path else NA_character_))
}

close_enough <- function(x, y, tol = 1e-6) {
  is.finite(x) && is.finite(y) && abs(x - y) <= tol
}

COL_DEV <- "#2F6F8F"
COL_EXT <- "#C65D4B"
COL_ICU <- "#4C78A8"
COL_HEALTHY <- "#E07A5F"
COL_NEUTRAL <- "#737373"

audit_rows <- list()

add_audit <- function(check, passed, observed, expected) {
  audit_rows[[length(audit_rows) + 1]] <<- data.frame(
    Check = check,
    Passed = isTRUE(passed),
    Status = ifelse(isTRUE(passed), "PASS", "FAIL"),
    Observed = as.character(observed),
    Expected = as.character(expected),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 3. Read frozen public source files
# ------------------------------------------------------------
cohort <- read_csv_strict(cohort_file)
alpha  <- read_csv_strict(alpha_file)
loo    <- read_csv_strict(loo_file)

require_columns(
  cohort,
  c(
    "Analysis_Set", "Project_ID", "Analysis_Family", "Model_Use",
    "Samples_N", "Median_Depth"
  ),
  basename(cohort_file)
)

require_columns(
  alpha,
  c(
    "Analysis_Set", "Meta_Family", "Metric",
    "Mean_Difference_A_minus_B",
    "Mean_Difference_CI_Low",
    "Mean_Difference_CI_High",
    "Wilcoxon_FDR"
  ),
  basename(alpha_file)
)

require_columns(
  loo,
  c(
    "Analysis_Set", "Stage", "Control_Group", "Omitted_Genus",
    "Omitted_Column_Present", "Full_Equal_AUC",
    "LOO_Equal_AUC", "Delta_Equal_AUC"
  ),
  basename(loo_file)
)

# Input MD5 manifest (package-relative, not author-machine paths)
input_paths <- c(cohort_file, alpha_file, loo_file)

# Build package-relative paths WITHOUT regular expressions.
# This is intentionally regex-free for Windows/R TRE compatibility.
abs_inputs <- normalizePath(
  input_paths,
  winslash = "/",
  mustWork = TRUE
)
root_norm <- normalizePath(
  PKG_ROOT,
  winslash = "/",
  mustWork = TRUE
)
root_prefix <- paste0(root_norm, "/")

relative_inputs <- vapply(
  abs_inputs,
  function(p) {
    if (startsWith(p, root_prefix)) {
      substring(p, nchar(root_prefix) + 1L)
    } else if (identical(p, root_norm)) {
      "."
    } else {
      basename(p)
    }
  },
  character(1)
)

input_manifest <- data.frame(
  Input = basename(input_paths),
  Relative_Path = unname(relative_inputs),
  MD5 = unname(tools::md5sum(input_paths)),
  stringsAsFactors = FALSE
)

write.csv(
  input_manifest,
  file.path(OUT_DIR, "PUBLIC_SuppFigure_Input_MD5.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
# 4. Supplementary Figure S1
# ============================================================
expected_a_sets <- c("A01", "A02", "A03", "A04", "A05", "A06")

s1 <- cohort %>%
  mutate(Set = sub("_.*$", "", Analysis_Set)) %>%
  filter(Set %in% expected_a_sets) %>%
  mutate(
    Set = factor(Set, levels = expected_a_sets),
    Stage = ifelse(
      Model_Use == "Locked_external_test",
      "External validation",
      "Development"
    )
  ) %>%
  arrange(Set)

# Exactly one row for each A01–A06
observed_sets <- as.character(s1$Set)
add_audit(
  "S1 contains exactly A01-A06 once each",
  nrow(s1) == 6 &&
    identical(observed_sets, expected_a_sets) &&
    !anyDuplicated(observed_sets),
  paste(observed_sets, collapse = ", "),
  paste(expected_a_sets, collapse = ", ")
)

expected_samples <- c(A01=146, A02=20, A03=20, A04=35, A05=36, A06=13)
observed_samples <- setNames(as.numeric(s1$Samples_N), observed_sets)

add_audit(
  "S1 sample counts match frozen values",
  all(observed_samples[names(expected_samples)] == expected_samples),
  paste(names(observed_samples), observed_samples, sep="=", collapse="; "),
  paste(names(expected_samples), expected_samples, sep="=", collapse="; ")
)

expected_depth <- c(
  A01=25265,
  A02=51210,
  A03=56015.5,
  A04=36204,
  A05=35152,
  A06=32084
)
observed_depth <- setNames(as.numeric(s1$Median_Depth), observed_sets)

add_audit(
  "S1 median sequencing depths match frozen values",
  all(abs(observed_depth[names(expected_depth)] - expected_depth) < 1e-8),
  paste(names(observed_depth), observed_depth, sep="=", collapse="; "),
  paste(names(expected_depth), expected_depth, sep="=", collapse="; ")
)

stage_map <- setNames(as.character(s1$Stage), observed_sets)
expected_stage <- c(
  A01="Development",
  A02="Development",
  A03="Development",
  A04="External validation",
  A05="External validation",
  A06="Development"
)
add_audit(
  "S1 development/external stage mapping is correct",
  all(stage_map[names(expected_stage)] == expected_stage),
  paste(names(stage_map), stage_map, sep="=", collapse="; "),
  paste(names(expected_stage), expected_stage, sep="=", collapse="; ")
)

s1$Label <- paste0(as.character(s1$Set), "\n", s1$Project_ID)
label_levels <- s1$Label[order(as.numeric(s1$Set))]
s1$Label <- factor(s1$Label, levels = label_levels)

write.csv(
  s1,
  file.path(OUT_DIR, "PUBLIC_SuppFigS1_Cohort_Source.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

p_s1a <- ggplot(
  s1,
  aes(x = Samples_N, y = Label, colour = Stage)
) +
  geom_segment(
    aes(x = 0, xend = Samples_N, yend = Label),
    linewidth = 0.65,
    alpha = 0.45
  ) +
  geom_point(size = 3.4) +
  geom_text(
    aes(label = Samples_N),
    hjust = -0.35,
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Development" = COL_DEV,
      "External validation" = COL_EXT
    )
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "A  Analysis-set sample size",
    x = "Samples, n",
    y = NULL
  ) +
  theme_v1()

p_s1b <- ggplot(
  s1,
  aes(x = Median_Depth, y = Label, colour = Stage)
) +
  geom_segment(
    aes(x = 0, xend = Median_Depth, yend = Label),
    linewidth = 0.65,
    alpha = 0.45
  ) +
  geom_point(size = 3.4) +
  geom_text(
    aes(label = comma(round(Median_Depth))),
    hjust = -0.25,
    size = 3.0,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(
      "Development" = COL_DEV,
      "External validation" = COL_EXT
    )
  ) +
  scale_x_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.20))
  ) +
  labs(
    title = "B  Median sequencing depth",
    x = "Reads per sample",
    y = NULL
  ) +
  theme_v1() +
  theme(legend.position = "none")

p_s1 <- (p_s1a | p_s1b) +
  plot_annotation(
    title = "Supplementary Figure S1. Characteristics of the analysis sets",
    subtitle = paste0(
      "Sample size and median sequencing depth are shown for the ",
      "development and locked external-validation analysis sets."
    )
  )

save_plot(
  p_s1,
  "Supp_Figure_S1_Cohort_Overview_PUBLIC",
  width = 10.8,
  height = 5.4
)

# ============================================================
# 5. Supplementary Figure S2
# ============================================================
alpha_sets <- c("A01", "A02", "A03", "A06")
alpha_metrics <- c(
  "Observed_Genus_Richness",
  "Shannon",
  "Inverse_Simpson"
)

s2 <- alpha %>%
  mutate(Set = sub("_.*$", "", Analysis_Set)) %>%
  filter(Set %in% alpha_sets, Metric %in% alpha_metrics) %>%
  mutate(
    Set = factor(Set, levels = rev(alpha_sets)),
    Metric = factor(Metric, levels = alpha_metrics),
    Comparator_family = ifelse(
      Meta_Family == "ICU_control",
      "ICU control",
      "Healthy control"
    )
  ) %>%
  arrange(Metric, Set)

add_audit(
  "S2 has 4 analysis sets x 3 alpha metrics",
  nrow(s2) == 12,
  nrow(s2),
  12
)

s2_keys <- paste(as.character(s2$Set), as.character(s2$Metric), sep="|")
expected_s2_keys <- as.vector(outer(
  rev(alpha_sets),
  alpha_metrics,
  FUN = function(a,b) paste(a,b,sep="|")
))
add_audit(
  "S2 contains every expected set-metric combination exactly once",
  length(unique(s2_keys)) == 12 &&
    setequal(s2_keys, expected_s2_keys),
  paste(sort(s2_keys), collapse="; "),
  paste(sort(expected_s2_keys), collapse="; ")
)

ci_ok <- (
  is.finite(as.numeric(s2$Mean_Difference_A_minus_B)) &
  is.finite(as.numeric(s2$Mean_Difference_CI_Low)) &
  is.finite(as.numeric(s2$Mean_Difference_CI_High)) &
  as.numeric(s2$Mean_Difference_CI_Low) <= as.numeric(s2$Mean_Difference_A_minus_B) &
  as.numeric(s2$Mean_Difference_A_minus_B) <= as.numeric(s2$Mean_Difference_CI_High)
)

add_audit(
  "S2 mean differences and descriptive CIs are finite and ordered",
  all(ci_ok),
  paste0("valid rows=", sum(ci_ok), "/", length(ci_ok)),
  "12/12"
)

add_audit(
  "S2 Wilcoxon FDR values are finite",
  all(is.finite(as.numeric(s2$Wilcoxon_FDR))),
  paste0("finite rows=", sum(is.finite(as.numeric(s2$Wilcoxon_FDR))), "/12"),
  "12/12"
)

# Anchor checks prevent accidental use of the reduced publication table
# that lacks the mean-difference CI fields.
anchor <- function(set, metric, column) {
  z <- s2[
    as.character(s2$Set) == set &
      as.character(s2$Metric) == metric,
    column
  ]
  if (length(z) != 1L) return(NA_real_)
  as.numeric(z[[1]])
}

add_audit(
  "S2 anchor: A01 observed mean difference",
  close_enough(anchor("A01","Observed_Genus_Richness","Mean_Difference_A_minus_B"), 5.745891, 5e-6),
  anchor("A01","Observed_Genus_Richness","Mean_Difference_A_minus_B"),
  "5.745891 ± 5e-6"
)

add_audit(
  "S2 anchor: A03 Shannon mean difference",
  close_enough(anchor("A03","Shannon","Mean_Difference_A_minus_B"), -0.4969, 5e-4),
  anchor("A03","Shannon","Mean_Difference_A_minus_B"),
  "-0.4969 ± 5e-4"
)

write.csv(
  s2,
  file.path(OUT_DIR, "PUBLIC_SuppFigS2_Alpha_Source.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

p_s2 <- ggplot(
  s2,
  aes(
    x = as.numeric(Mean_Difference_A_minus_B),
    y = Set,
    colour = Comparator_family
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.55,
    colour = COL_NEUTRAL
  ) +
  geom_segment(
    aes(
      x = as.numeric(Mean_Difference_CI_Low),
      xend = as.numeric(Mean_Difference_CI_High),
      yend = Set
    ),
    linewidth = 0.9
  ) +
  geom_point(size = 3.0) +
  facet_wrap(
    ~ Metric,
    scales = "free_x",
    nrow = 1,
    labeller = as_labeller(
      c(
        "Observed_Genus_Richness" = "Observed genus richness",
        "Shannon" = "Shannon diversity",
        "Inverse_Simpson" = "Inverse Simpson"
      )
    )
  ) +
  scale_colour_manual(
    values = c(
      "ICU control" = COL_ICU,
      "Healthy control" = COL_HEALTHY
    )
  ) +
  labs(
    title = "Supplementary Figure S2. Cross-sectional alpha-diversity effects",
    subtitle = paste0(
      "Points show mean Sepsis - comparator differences with descriptive 95% confidence intervals.\n",
      "Formal inference was based on Wilcoxon tests with Benjamini-Hochberg FDR correction."
    ),
    x = "Mean difference (Sepsis - comparator)",
    y = "Analysis set"
  ) +
  theme_v1() +
  theme(panel.spacing.x = grid::unit(1.0, "lines"))

save_plot(
  p_s2,
  "Supp_Figure_S2_Alpha_Diversity_Effects_PUBLIC",
  width = 11.4,
  height = 5.2
)

# ============================================================
# 6. Supplementary Figure S3
# ============================================================
genus_order <- c(
  "Bilophila",
  "Butyricimonas",
  "Parabacteroides",
  "Hoylesella",
  "Campylobacter",
  "Atopobium"
)

loo_sets <- c("A01", "A02", "A04", "A05")

facet_labels <- c(
  "A01" = "A01 (ICU control)",
  "A02" = "A02 (ICU control)",
  "A04" = "A04 (Healthy control)",
  "A05" = "A05 (Trauma control)"
)

s3 <- loo %>%
  mutate(
    Set = sub("_.*$", "", Analysis_Set),
    Genus = pretty_genus(Omitted_Genus),
    Column_present = tolower(as.character(Omitted_Column_Present)) %in%
      c("true", "t", "1", "yes", "y"),
    Genus = factor(Genus, levels = genus_order),
    Set = factor(Set, levels = loo_sets),
    Stage_short = ifelse(Stage == "External", "External", "Development"),
    Delta_plot = ifelse(
      Column_present,
      as.numeric(Delta_Equal_AUC),
      NA_real_
    )
  ) %>%
  filter(Set %in% loo_sets, Genus %in% genus_order) %>%
  arrange(Set, Genus)

add_audit(
  "S3 has 4 datasets x 6 omitted genera",
  nrow(s3) == 24,
  nrow(s3),
  24
)

s3_keys <- paste(as.character(s3$Set), as.character(s3$Genus), sep="|")
expected_s3_keys <- as.vector(outer(
  loo_sets,
  genus_order,
  FUN=function(a,b) paste(a,b,sep="|")
))
add_audit(
  "S3 contains every expected set-genus combination exactly once",
  length(unique(s3_keys)) == 24 &&
    setequal(s3_keys, expected_s3_keys),
  paste(sort(s3_keys), collapse="; "),
  paste(sort(expected_s3_keys), collapse="; ")
)

expected_absent_keys <- c(
  "A04|Campylobacter",
  "A04|Atopobium",
  "A05|Campylobacter",
  "A05|Atopobium"
)
observed_absent_keys <- s3_keys[!s3$Column_present]

add_audit(
  "S3 project-level absent genera are exactly the frozen four combinations",
  setequal(observed_absent_keys, expected_absent_keys) &&
    length(observed_absent_keys) == 4,
  paste(sort(observed_absent_keys), collapse="; "),
  paste(sort(expected_absent_keys), collapse="; ")
)

present_delta_ok <- all(is.finite(s3$Delta_plot[s3$Column_present]))
add_audit(
  "S3 present genera have finite equal-weight ΔAUC",
  present_delta_ok,
  paste0("finite present rows=", sum(is.finite(s3$Delta_plot[s3$Column_present]))),
  "20"
)

# Frozen equal-weight diagnostic range; shared axis must contain all bars.
delta_present <- s3$Delta_plot[s3$Column_present]
common_ylim <- c(-0.05, 0.11)

add_audit(
  "S3 common y-axis contains all frozen ΔAUC values",
  min(delta_present) >= common_ylim[1] &&
    max(delta_present) <= common_ylim[2],
  paste0(
    "min=", sprintf("%.6f", min(delta_present)),
    "; max=", sprintf("%.6f", max(delta_present))
  ),
  "within [-0.05, 0.11]"
)

s3_anchor <- function(set, genus) {
  z <- s3$Delta_plot[
    as.character(s3$Set) == set &
      as.character(s3$Genus) == genus
  ]
  if (length(z) != 1L) return(NA_real_)
  as.numeric(z[[1]])
}

add_audit(
  "S3 anchor: A01 Bilophila ΔAUC",
  close_enough(s3_anchor("A01","Bilophila"), -0.012091, 5e-6),
  s3_anchor("A01","Bilophila"),
  "-0.012091 ± 5e-6"
)

add_audit(
  "S3 anchor: A04 Bilophila ΔAUC",
  close_enough(s3_anchor("A04","Bilophila"), 0.106209, 5e-6),
  s3_anchor("A04","Bilophila"),
  "0.106209 ± 5e-6"
)

write.csv(
  s3,
  file.path(OUT_DIR, "PUBLIC_SuppFigS3_LOO_Source.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

p_s3 <- ggplot(
  s3,
  aes(
    x = Genus,
    y = Delta_plot,
    fill = Stage_short
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.55,
    colour = COL_NEUTRAL
  ) +
  geom_col(
    data = s3 %>% filter(Column_present),
    width = 0.68,
    alpha = 0.92
  ) +
  geom_text(
    data = s3 %>% filter(!Column_present),
    aes(x = Genus, y = 0.004, label = "\u00D7 absent"),
    inherit.aes = FALSE,
    vjust = 0,
    size = 3.0,
    colour = COL_NEUTRAL
  ) +
  facet_wrap(
    ~ Set,
    ncol = 2,
    scales = "fixed",
    labeller = as_labeller(facet_labels)
  ) +
  scale_fill_manual(
    values = c(
      "Development" = COL_DEV,
      "External" = COL_EXT
    )
  ) +
  coord_cartesian(
    ylim = common_ylim,
    clip = "off"
  ) +
  scale_y_continuous(
    breaks = seq(-0.05, 0.10, by = 0.05)
  ) +
  labs(
    title = "Supplementary Figure S3. Post-hoc leave-one-genus-out SDI diagnosis",
    subtitle = paste0(
      "Equal-weight SDI only. Positive \u0394AUC indicates improved discrimination after omission.\n",
      "This is a post-hoc robustness/failure diagnosis and does not constitute independent validation."
    ),
    x = NULL,
    y = expression(Delta * "AUC after genus omission")
  ) +
  theme_v1() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, face = "italic"),
    plot.margin = margin(10, 22, 10, 10)
  )

save_plot(
  p_s3,
  "Supp_Figure_S3_LOO_Posthoc_Diagnosis_PUBLIC",
  width = 10.8,
  height = 7.6
)

# ------------------------------------------------------------
# 7. Final audit + manifest
# ------------------------------------------------------------
audit <- bind_rows(audit_rows)

write.csv(
  audit,
  file.path(OUT_DIR, "PUBLIC_SuppFigure_Source_Audit.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (any(!audit$Passed)) {
  failed <- audit[!audit$Passed, , drop = FALSE]
  print(failed)
  stop(
    "Supplementary Figure source audit failed. See:\n",
    file.path(OUT_DIR, "PUBLIC_SuppFigure_Source_Audit.csv")
  )
}

manifest <- data.frame(
  Figure = c("Supplementary Figure S1","Supplementary Figure S2","Supplementary Figure S3"),
  Frozen_Source = c(
    "expected_results/frozen_result_inputs/01_15_Analysis_Set_Summary.csv",
    "expected_results/frozen_result_inputs/04_16_Alpha_Diversity_Tests.csv",
    "expected_results/frozen_result_inputs/23_19_Leave_One_Genus_Out_Sensitivity.csv"
  ),
  Statistical_Model_Refit = "NO",
  Public_Output = c(
    "Supp_Figure_S1_Cohort_Overview_PUBLIC.pdf/.tiff",
    "Supp_Figure_S2_Alpha_Diversity_Effects_PUBLIC.pdf/.tiff",
    "Supp_Figure_S3_LOO_Posthoc_Diagnosis_PUBLIC.pdf/.tiff"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  manifest,
  file.path(OUT_DIR, "PUBLIC_SuppFigure_Reproduction_Manifest.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

writeLines(
  capture.output(sessionInfo()),
  file.path(OUT_DIR, "PUBLIC_SuppFigure_Reproduction_sessionInfo.txt")
)

cat("\n============================================================\n")
cat("PUBLIC supplementary Figure reproduction complete.\n")
cat("Output directory:\n", OUT_DIR, "\n", sep = "")
cat("Audit checks: ", nrow(audit), "\n", sep = "")
cat("PASS: ", sum(audit$Passed), "\n", sep = "")
cat("FAIL: ", sum(!audit$Passed), "\n", sep = "")
cat("Statistical models refit: NO\n")
cat("Frozen results overwritten: NO\n")
cat("============================================================\n")
