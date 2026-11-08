suppressWarnings(Sys.setlocale("LC_ALL","English_United States.utf8"))
# ============================================================
# Sepsis V1 \u2014 PUBLIC main-figure reproduction v1.1
# Portable / package-relative version
#
# PURPOSE
#   Reproduce manuscript Figures 1\u20134 from the PUBLIC package only.
#   v1.1 fixes Figure 1 layout/architecture routing and Figure 3B
#   genus-to-status visual alignment; numerical sources are unchanged.
#   No author-specific drive path is used.
#
# REQUIRED PACKAGE STRUCTURE
#   <package-root>/
#     expected_results/scientific_outputs/
#     figure_audit_20260902/
#       V1_Figure1_Screening_Flow_FINAL_20260901.csv
#       08_make_main_figures_PUBLIC.R   [this file may live here]
#
# REQUIRED R PACKAGES
#   ggplot2, patchwork
#
# USAGE
#   From package root:
#     Rscript figure_audit_20260902/08_make_main_figures_PUBLIC.R
#
#   Or from anywhere:
#     Rscript 08_make_main_figures_PUBLIC.R "/path/to/package-root"
#
#   Optional second argument = output directory:
#     Rscript 08_make_main_figures_PUBLIC.R "/path/to/package-root" "/path/to/output"
#
# IMPORTANT
#   - No statistical model is refit.
#   - All numerical panels read frozen PUBLIC CSV sources.
#   - AUC < 0.5 is preserved; scores are never inverted.
#   - Project-level feature absence is not plotted as a zero effect.
#   - PRJNA912621 Figure 4C is labelled cholestasis/SIC, not generic
#     organ dysfunction.
# ============================================================

options(stringsAsFactors = FALSE)

# ------------------------------------------------------------
# 0. Utilities
# ------------------------------------------------------------
need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Required package '", pkg, "' is not installed.\n",
      "Install with: install.packages('", pkg, "')"
    )
  }
}
need_pkg("ggplot2")
need_pkg("patchwork")

library(ggplot2)
library(patchwork)

args <- if (exists("FIGURE_ARGS", inherits = TRUE)) FIGURE_ARGS else commandArgs(trailingOnly = TRUE)

script_dir <- function() {
  full <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", full, value = TRUE)
  if (length(hit)) {
    p <- sub("^--file=", "", hit[[1]])
    return(dirname(normalizePath(p, winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

is_pkg_root <- function(x) {
  dir.exists(file.path(x, "expected_results", "scientific_outputs")) &&
    file.exists(file.path(x, "figure_audit_20260902",
                          "V1_Figure1_Screening_Flow_FINAL_20260901.csv"))
}

find_pkg_root <- function() {
  if (length(args) >= 1L && nzchar(args[[1]])) {
    p <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
    if (!is_pkg_root(p)) {
      stop("Argument 1 is not a valid public package root: ", p)
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
    "Pass it explicitly as argument 1."
  )
}

PKG_ROOT <- find_pkg_root()
SRC_ROOT <- file.path(PKG_ROOT, "expected_results", "scientific_outputs")
AUDIT_ROOT <- file.path(PKG_ROOT, "figure_audit_20260902")

OUT_ROOT <- if (length(args) >= 2L && nzchar(args[[2]])) {
  normalizePath(args[[2]], winslash = "/", mustWork = FALSE)
} else {
  file.path(PKG_ROOT, "figures_reproduced_public")
}
dir.create(OUT_ROOT, recursive = TRUE, showWarnings = FALSE)

read_csv <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path)
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
           fileEncoding = "UTF-8-BOM")
}

require_cols <- function(x, cols, label) {
  miss <- setdiff(cols, names(x))
  if (length(miss)) {
    stop(label, " is missing columns: ", paste(miss, collapse = ", "))
  }
}

pretty_genus <- function(x) {
  x <- as.character(x)
  x <- sub("^Genus__", "", x)
  sub("\\|.*$", "", x)
}

close_enough <- function(x, y, tol = 1e-6) {
  is.finite(x) && is.finite(y) && abs(x - y) <= tol
}

get_summary_value <- function(summary_df, topic) {
  z <- summary_df$Value[summary_df$Topic == topic]
  if (length(z) != 1L) stop("Cannot uniquely resolve summary topic: ", topic)
  z[[1]]
}

COL <- list(
  dark = "#333333",
  grid = "#E6E6E6",
  icu = "#6D8EAA",
  healthy = "#73A86D",
  development = "#4C78A8",
  external = "#C95A49",
  sepsis = "#B85C50",
  trauma = "#E49B4A",
  positive = "#B95846",
  negative = "#4C78A8",
  neutral = "#9A9A9A",
  purple = "#7B62A3"
)

theme_pub <- function(base_size = 9) {
  theme_classic(base_size = base_size, base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle = element_text(size = base_size - 1, colour = "#555555"),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, colour = COL$dark),
      legend.text = element_text(size = base_size - 1),
      legend.title = element_text(size = base_size - 1),
      plot.margin = margin(7, 8, 7, 8)
    )
}

save_pub <- function(...) invisible(NULL)

# ------------------------------------------------------------
# 1. Read frozen public sources
# ------------------------------------------------------------
screen <- read_csv(file.path(
  AUDIT_ROOT, "V1_Figure1_Screening_Flow_FINAL_20260901.csv"
))
f2a <- read_csv(file.path(SRC_ROOT, "21_Figure2A_Beta_PERMANOVA_Source.csv"))
f2b <- read_csv(file.path(SRC_ROOT, "21_Figure2B_ICU_Meta_Source.csv"))
f2c <- read_csv(file.path(SRC_ROOT, "21_Figure2C_Healthy_Meta_Source.csv"))
f3a <- read_csv(file.path(SRC_ROOT, "21_Figure3A_SDI_AUC_Source.csv"))
f3b <- read_csv(file.path(SRC_ROOT, "21_Figure3B_Candidate_Diagnosis_Source.csv"))
f3c <- read_csv(file.path(SRC_ROOT, "21_Figure3C_External_SDI_Distribution_Source.csv"))
f4a <- read_csv(file.path(SRC_ROOT, "21_Figure4A_T01_Shannon_Source.csv"))
f4b <- read_csv(file.path(SRC_ROOT, "21_Figure4B_T03_Shannon_Source.csv"))
f4c <- read_csv(file.path(SRC_ROOT, "21_Figure4C_T06_Shannon_Source.csv"))
f4d <- read_csv(file.path(SRC_ROOT, "21_Figure4D_T01_SDI_Source.csv"))
numsum <- read_csv(file.path(SRC_ROOT, "21_Results_Number_Summary.csv"))

# ------------------------------------------------------------
# 2. Strict source assertions
# ------------------------------------------------------------
require_cols(screen, c("Metric", "Value"), "Figure 1 screening source")
screen$Value <- as.numeric(screen$Value)

sv <- function(m) {
  z <- screen$Value[screen$Metric == m]
  if (length(z) != 1L || !is.finite(z)) stop("Bad screening metric: ", m)
  z
}

SCR <- c(
  raw = sv("Raw_query_hit_rows"),
  unique = sv("Unique_BioProjects"),
  excluded = sv("Excluded"),
  eligible = sv("Eligible_reconstructed_search"),
  locked = sv("Locked_recovered_exact_query"),
  added = sv("Additional_eligible_retrospective"),
  known = sv("Known_item_recovered"),
  final = sv("Final_frozen_V1_analytic_set"),
  pending = sv("Pending")
)
EXP <- c(raw=197, unique=143, excluded=126, eligible=17,
         locked=6, added=11, known=1, final=7, pending=0)
if (!identical(as.numeric(SCR), as.numeric(EXP))) {
  stop("Figure 1 screening source does not match the frozen final flow.")
}
if (SCR["excluded"] + SCR["eligible"] != SCR["unique"]) stop("126 + 17 != 143")
if (SCR["locked"] + SCR["added"] != SCR["eligible"]) stop("6 + 11 != 17")
if (SCR["locked"] + SCR["known"] != SCR["final"]) stop("6 + 1 != 7")

require_cols(f2a, c("Analysis_Set","Meta_Family","R2","FDR"), "Figure 2A")
if (nrow(f2a) != 4L) stop("Figure 2A must have four rows.")

require_cols(f2b, c("Genus_Feature","REML_Effect","REML_CI_Low","REML_CI_High"), "Figure 2B")
require_cols(f2c, c("Genus_Feature","REML_Effect","REML_CI_Low","REML_CI_High"), "Figure 2C")

require_cols(f3a, c("Dataset","AUC","AUC_CI_Low","AUC_CI_High"), "Figure 3A")
if (!close_enough(f3a$AUC[f3a$Dataset=="Development_Pooled"], 0.731213, 1e-6)) {
  stop("Development pooled AUC changed.")
}
if (!close_enough(f3a$AUC[grepl("^A04", f3a$Dataset)], 0.200980, 1e-6)) {
  stop("A04 AUC changed.")
}
if (!close_enough(f3a$AUC[grepl("^A05", f3a$Dataset)], 0.271605, 1e-6)) {
  stop("A05 AUC changed.")
}

require_cols(f3b, c("Genus_Feature","Missing_In_External_Project",
                    "Mean_Development_Expected_Signed_Effect",
                    "Mean_External_Expected_Signed_Effect","Diagnostic_Label"),
             "Figure 3B")
if (sum(f3b$Diagnostic_Label=="COMPLETE_EXTERNAL_DIRECTION_REVERSAL") != 4L ||
    sum(f3b$Diagnostic_Label=="PROJECT_LEVEL_FEATURE_ABSENCE") != 2L) {
  stop("Figure 3B diagnosis is not the frozen 4 reversal + 2 absence.")
}

require_cols(f4a, c("Time_Label","Time_Order_Resolved","N","Mean","SE"), "Figure 4A")
require_cols(f4b, c("Time_Label","Time_Order_Resolved","N","Mean","SE"), "Figure 4B")
require_cols(f4c, c("Time_Label","Time_Order_Resolved","Status_Resolved","N","Mean","SE"), "Figure 4C")
require_cols(f4d, c("Time_Label","Time_Order_Resolved","N","Mean","SE"), "Figure 4D")

ncheck <- with(f4c, setNames(N, paste(Time_Label, Status_Resolved, sep="|")))
expected_n <- c("Day 1|No"=10, "Day 1|Yes"=10,
                "Day 3|No"=10, "Day 3|Yes"=10,
                "Day 7|No"=9,  "Day 7|Yes"=9)
if (!all(ncheck[names(expected_n)] == expected_n)) {
  stop("Figure 4C group-specific N differs from the frozen source.")
}

# ------------------------------------------------------------
