# ============================================================
# Optional raw FASTQ -> final reconstructed ASV entry point
#
# IMPORTANT: THIS IS NOT THE MANUSCRIPT-RESULT MASTER RUNNER.
# Exact manuscript-result reproduction uses:
#   source("reproduce_manuscript_results.R")
#
# This raw route is provided for upstream method transparency.
# It runs the project-specific DADA2 route to a reconstructed FINAL ASV table.
# Taxonomy reconstruction and analysis-ready checkpoint rebuilding are separate
# optional scripts under preprocessing/ (see README.md).
#
# PUBLIC PATH CONFIGURATION
# Raw FASTQ files are NOT bundled. You must provide your own raw-data path.
# Output can also be placed anywhere on your computer.
#
# Recommended Windows example (paths are examples only):
#   Rscript run_raw_preprocessing.R PRJEB33360 ^
#     --raw-root="F:/Sepsis_V1_raw_fastq" ^
#     --output-root="F:/Sepsis_V1_preprocessed"
#
# Expected raw-root layout:
#   F:/Sepsis_V1_raw_fastq/
#     PRJEB33360/
#     PRJNA691455/
#     PRJNA978257/
#     PRJNA1010969/
#     PRJNA430161/
#     PRJNA797231/
#     PRJNA912621/
#
# You may also point --raw-root directly to a single project's FASTQ folder.
# Do NOT copy any historical author path such as E:/sepsis_project.
# ============================================================

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)

get_flag <- function(name, default = NA_character_) {
  prefix <- paste0(name, "=")
  hit <- args[startsWith(args, prefix)]
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[1])
}

positional <- args[!startsWith(args, "--")]
if (!length(positional)) {
  stop(
    "Provide one BioProject accession followed by public path options.\n",
    "Example:\n",
    "Rscript run_raw_preprocessing.R PRJEB33360 ",
    "--raw-root=\"F:/Sepsis_V1_raw_fastq\" ",
    "--output-root=\"F:/Sepsis_V1_preprocessed\""
  )
}

project <- positional[1]
supported <- c(
  "PRJEB33360", "PRJNA691455", "PRJNA978257", "PRJNA1010969",
  "PRJNA430161", "PRJNA797231", "PRJNA912621"
)
if (!project %in% supported) stop("Unsupported BioProject: ", project)

raw_root <- get_flag("--raw-root")
output_root <- get_flag("--output-root")

if (is.na(raw_root) || !nzchar(raw_root)) {
  stop(
    "Missing --raw-root. Raw FASTQ files are external to this public package.\n",
    "Supply your own path, e.g. --raw-root=\"F:/Sepsis_V1_raw_fastq\"."
  )
}

raw_root <- normalizePath(raw_root, winslash = "/", mustWork = FALSE)
if (!dir.exists(raw_root)) stop("--raw-root does not exist: ", raw_root)

repo_root <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  if (length(f)) {
    return(dirname(normalizePath(sub("^--file=", "", f[1]),
                                  winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

ROOT <- repo_root()

if (is.na(output_root) || !nzchar(output_root)) {
  output_root <- file.path(ROOT, "work", "raw_preprocessing")
}
output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

# If --raw-root is the common parent, use <raw-root>/<BioProject>.
# If it already points directly to the selected BioProject folder, use it as-is.
raw_project_dir <- if (dir.exists(file.path(raw_root, project))) {
  file.path(raw_root, project)
} else {
  raw_root
}

cat("\n============================================================\n")
cat("Sepsis V1 raw DADA2 reconstruction\n")
cat("BioProject:        ", project, "\n", sep = "")
cat("Repository root:   ", ROOT, "\n", sep = "")
cat("Raw project path:  ", raw_project_dir, "\n", sep = "")
cat("Output root:       ", output_root, "\n", sep = "")
cat("============================================================\n\n")

# Public path variables consumed by the preprocessing scripts.
Sys.setenv(
  SEPSIS_V1_PROJECT_ROOT = ROOT,
  SEPSIS_V1_RAW_DATA_ROOT = raw_root,
  SEPSIS_V1_RAW_PROJECT_DIR = raw_project_dir,
  SEPSIS_V1_RAW_OUTPUT_ROOT = output_root
)

rscript <- file.path(R.home("bin"), "Rscript")

if (project == "PRJNA797231") {
  target <- file.path(ROOT, "preprocessing", "02_PRJNA797231_raw_to_final_asv.R")
  status <- system2(rscript, target)
} else {
  target <- file.path(ROOT, "preprocessing", "01_run_project_dada2.R")
  status <- system2(rscript, c(target, project))
}

if (status != 0) stop("Raw preprocessing exited with code ", status)

cat("\nRaw DADA2 reconstruction completed.\n")
cat("Outputs were written under: ", file.path(output_root, project), "\n", sep = "")
cat("See README.md before running optional taxonomy/checkpoint reconstruction.\n")
