# ============================================================
# Reproduce manuscript results from the bundled validated checkpoint
#
# RECOMMENDED PUBLIC ENTRY POINT
#
# 1) Extract the complete repository anywhere on your computer.
# 2) In R/RStudio, set the working directory to THIS repository root, e.g.
#      setwd("D:/Sepsis_V1_public")   # EXAMPLE ONLY: use your own path
# 3) Run:
#      source("reproduce_manuscript_results.R")
#
# Command line:
#   Rscript reproduce_manuscript_results.R
#
# PATH NOTE
#   - The processed manuscript inputs are already bundled under data/.
#   - This runner deliberately uses repository-relative paths.
#   - Do NOT replace data/analysis/work paths with the authors' old local paths.
#   - The only personal path most users need is their own repository location
#     used in setwd(...) before source().
#
# This is the recommended public reproduction route.
# ============================================================

options(stringsAsFactors = FALSE)

repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f)) {
    return(dirname(normalizePath(sub("^--file=", "", f[1]),
                                  winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

REPO_ROOT <- repo_root()
WORK_ROOT <- file.path(REPO_ROOT, "work", "manuscript_reproduction")
INPUT_ROOT <- file.path(
  REPO_ROOT, "data", "analysis_ready_checkpoint", "final_frozen_v1"
)

required_packages <- c("vegan", "metafor", "nlme", "openxlsx")
missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop(
    "Missing required package(s): ", paste(missing, collapse = ", "),
    "\nRun source(\"environment/install_core_packages.R\") first."
  )
}

overwrite <- tolower(
  Sys.getenv("SEPSIS_V1_OVERWRITE", unset = "false")
) %in% c("1", "true", "yes")

if (dir.exists(WORK_ROOT)) {
  if (!overwrite) {
    stop(
      "The reproduction work directory already exists:\n", WORK_ROOT,
      "\nDelete it or rerun with SEPSIS_V1_OVERWRITE=true."
    )
  }
  unlink(WORK_ROOT, recursive = TRUE, force = TRUE)
}

dir.create(WORK_ROOT, recursive = TRUE, showWarnings = FALSE)

# Stage the validated compact inputs into the layout expected by the analysis scripts.
dest_root <- file.path(WORK_ROOT, "processed_data", "final_frozen_v1")
dir.create(dest_root, recursive = TRUE, showWarnings = FALSE)

projects <- c(
  "PRJEB33360", "PRJNA691455", "PRJNA978257", "PRJNA1010969",
  "PRJNA430161", "PRJNA797231", "PRJNA912621"
)

for (p in projects) {
  src_dir <- file.path(INPUT_ROOT, p)
  dst_dir <- file.path(dest_root, p)
  dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)

  files <- c(
    paste0(p, "_analysis_ready_classified_genus.csv"),
    paste0(p, "_genus_classified_counts.csv")
  )

  for (fn in files) {
    src <- file.path(src_dir, fn)
    dst <- file.path(dst_dir, fn)
    if (!file.exists(src)) stop("Bundled checkpoint file is missing: ", src)
    if (!file.copy(src, dst, overwrite = TRUE)) stop("Failed to stage: ", src)
  }
}

Sys.setenv(SEPSIS_V1_PROJECT_ROOT = WORK_ROOT)

modules <- c(
  "01_prepare_analysis_sets.R",
  "02_discovery_statistics.R",
  "03_harmonized_meta_analysis.R",
  "04_sdi_external_validation.R",
  "05_sdi_failure_diagnosis.R",
  "06_longitudinal_support.R",
  "07_generate_final_outputs.R"
)

for (m in modules) {
  cat("\n============================================================\n")
  cat("Running: ", m, "\n", sep = "")
  cat("============================================================\n")
  env <- new.env(parent = globalenv())
  source(file.path(REPO_ROOT, "analysis", m), local = env, echo = FALSE)
  while (sink.number() > 0) sink()
}

source(file.path(REPO_ROOT, "validation", "validate_reproduction.R"))

summary <- validate_reproduction(
  project_root = WORK_ROOT,
  repo_root = REPO_ROOT
)

capture.output(
  sessionInfo(),
  file = file.path(WORK_ROOT, "validation", "sessionInfo.txt")
)

cat("\n============================================================\n")
cat("Reproduction finished.\n")
cat("Outputs: ", WORK_ROOT, "\n", sep = "")
cat(
  "All scientific checks PASS: ",
  summary$All_Scientific_Checks_PASS[1],
  "\n",
  sep = ""
)
cat("============================================================\n")
