# Sepsis V1 — fresh SILVA 138.2 taxonomy reconstruction (method-reproduction mode)
#
# IMPORTANT:
# The exact manuscript pipeline uses the verified historical taxonomy checkpoints.
# This script is provided to reproduce the stated taxonomy METHOD from final ASV tables.
# Fresh assignTaxonomy() calls can differ slightly from historical taxonomy output.
#
# Required:
#   dada2
#   SILVA 138.2 toGenus training set:
#   SILVA_138_2/silva_nr99_v138.2_toGenus_trainset.fa.gz

options(stringsAsFactors = FALSE)

ROOT <- Sys.getenv("SEPSIS_V1_PROJECT_ROOT", unset = getwd())
ROOT <- normalizePath(ROOT, winslash = "/", mustWork = FALSE)

if (!requireNamespace("dada2", quietly = TRUE)) {
  stop("Package 'dada2' is required.")
}

# PUBLIC PATH CONFIGURATION: large SILVA files are normally kept outside the
# repository. Set SEPSIS_V1_SILVA_REF to your own local reference path.
ref <- Sys.getenv("SEPSIS_V1_SILVA_REF", unset = "")
if (!nzchar(ref)) {
  ref <- file.path(
    ROOT,
    "SILVA_138_2",
    "silva_nr99_v138.2_toGenus_trainset.fa.gz"
  )
}
ref <- normalizePath(ref, winslash = "/", mustWork = FALSE)

if (!file.exists(ref)) {
  stop(
    "SILVA 138.2 reference not found: ", ref,
    "\nSet your own path before running, e.g.:\n",
    'Sys.setenv(SEPSIS_V1_SILVA_REF = "F:/references/SILVA_138_2/silva_nr99_v138.2_toGenus_trainset.fa.gz")'
  )
}

project_ids <- c(
  "PRJEB33360","PRJNA691455","PRJNA978257","PRJNA1010969",
  "PRJNA430161","PRJNA797231","PRJNA912621"
)

# Optional public ASV root. When set to the --output-root used by
# run_raw_preprocessing.R, this script reads the reconstructed final ASV RDS files.
ASV_ROOT <- Sys.getenv("SEPSIS_V1_ASV_ROOT", unset = "")

if (nzchar(ASV_ROOT)) {
  ASV_ROOT <- normalizePath(ASV_ROOT, winslash = "/", mustWork = FALSE)
  seqtab_paths <- vapply(project_ids, function(p) {
    if (identical(p, "PRJNA797231")) {
      file.path(ASV_ROOT, p, paste0(p, "_seqtab_final_END_TO_END_RECONSTRUCTED.rds"))
    } else {
      file.path(ASV_ROOT, p, paste0(p, "_seqtab_final_RECONSTRUCTED.rds"))
    }
  }, character(1))
} else {
  # Backward-compatible historical repository layout.
  seqtab_paths <- file.path(
    ROOT,
    c(
      "data/PRJEB33360/03_dada2/PRJEB33360_seqtab_final.rds",
      "data/PRJNA691455/03_dada2_rerun_v2/PRJNA691455_seqtab_final.rds",
      "data/PRJNA978257/03_dada2/PRJNA978257_seqtab_final.rds",
      "data/PRJNA1010969/03_dada2_rerun_v2/PRJNA1010969_seqtab_final.rds",
      "data/PRJNA430161/03_dada2/PRJNA430161_seqtab_final.rds",
      "data/PRJNA797231/03_dada2/PRJNA797231_seqtab_final.rds",
      "data/PRJNA912621/03_dada2/PRJNA912621_seqtab_final.rds"
    )
  )
}

projects <- data.frame(
  Project_ID = project_ids,
  Seqtab = seqtab_paths,
  stringsAsFactors = FALSE
)

TAXONOMY_OUTPUT_ROOT <- Sys.getenv("SEPSIS_V1_TAXONOMY_OUTPUT_ROOT", unset = "")
if (!nzchar(TAXONOMY_OUTPUT_ROOT)) {
  TAXONOMY_OUTPUT_ROOT <- file.path(ROOT, "fresh_taxonomy_method_reproduction")
}
TAXONOMY_OUTPUT_ROOT <- normalizePath(TAXONOMY_OUTPUT_ROOT, winslash = "/", mustWork = FALSE)
dir.create(TAXONOMY_OUTPUT_ROOT, recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(projects))) {
  p <- projects$Project_ID[i]
  s <- projects$Seqtab[i]

  if (!file.exists(s)) {
    warning(p, ": final ASV checkpoint not found; skipping.")
    next
  }

  seqtab <- as.matrix(readRDS(s))
  asvs <- colnames(seqtab)

  tax <- dada2::assignTaxonomy(
    asvs,
    refFasta = ref,
    minBoot = 50,
    tryRC = TRUE,
    multithread = TRUE
  )

  out_dir <- file.path(TAXONOMY_OUTPUT_ROOT, p)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  out <- data.frame(
    ASV_Sequence = rownames(tax),
    tax,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  write.csv(
    out,
    file.path(out_dir, paste0(p, "_taxonomy_silva1382_FRESH.csv")),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

cat("Fresh taxonomy method reproduction complete.\n")
cat("Do not substitute these files for the historical taxonomy checkpoints when exact manuscript reproduction is required.\n")
