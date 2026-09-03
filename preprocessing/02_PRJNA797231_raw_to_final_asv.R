# ============================================================
# Sepsis V1 — PRJNA797231 end-to-end reconstructed DADA2
# File: 00F_PRJNA797231_raw_to_final_end_to_end.R
#
# PURPOSE
#   Reproduce PRJNA797231 directly from the archived raw FASTQ files
#   to the final ASV table, without using the archived filtered FASTQs.
#
# Reconstructed filtering rule established by Step 00D:
#   truncLen = c(240, 240)
#   trimLeft = c(0, 0)
#   maxN = 0
#   maxEE = c(2, 2)
#   truncQ = 2
#   rm.phix = FALSE
#
# NOTE:
#   In Step 00D, rm.phix=TRUE and FALSE produced identical retained-read
#   counts across all 34 samples, so the historical rm.phix setting could
#   not be uniquely identified. FALSE is used here as the selected
#   reconstructed setting. The decisive validation is the final canonical
#   ASV-table SHA256 against the verified final-ASV fingerprint/reference.
#
# Recovered historical post-processing:
#   merged ASV length 440–468 bp
#   total ASV count >= 50
#   removeBimeraDenovo(method="consensus", multithread=FALSE)
#
# This script NEVER overwrites the archived original final RDS.
# ============================================================

options(stringsAsFactors = FALSE)

required <- c("dada2", "digest")
missing_pkgs <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages(library(dada2))

PROJECT_ROOT <- Sys.getenv("SEPSIS_V1_PROJECT_ROOT", unset = getwd())
PROJECT_ROOT <- normalizePath(PROJECT_ROOT, winslash = "/", mustWork = FALSE)
PROJECT_ID <- "PRJNA797231"
PROJECT_DIR <- file.path(PROJECT_ROOT, "data", PROJECT_ID)

# PUBLIC PATH CONFIGURATION (path plumbing only; DADA2 parameters are unchanged).
RAW_PROJECT_DIR <- Sys.getenv("SEPSIS_V1_RAW_PROJECT_DIR", unset = "")
RAW_OUTPUT_ROOT <- Sys.getenv("SEPSIS_V1_RAW_OUTPUT_ROOT", unset = "")

RAW_DIR <- if (nzchar(RAW_PROJECT_DIR)) {
  normalizePath(RAW_PROJECT_DIR, winslash = "/", mustWork = FALSE)
} else {
  file.path(PROJECT_DIR, "fastq_results")
}

ORIGINAL_FINAL_RDS <- file.path(
  PROJECT_DIR,
  "03_dada2",
  paste0(PROJECT_ID, "_seqtab_final.rds")
)

OUT_DIR <- if (nzchar(RAW_OUTPUT_ROOT)) {
  file.path(normalizePath(RAW_OUTPUT_ROOT, winslash = "/", mustWork = FALSE), PROJECT_ID)
} else {
  file.path(PROJECT_ROOT, "work", "raw_preprocessing", PROJECT_ID)
}
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

FILTERED_OUT <- file.path(OUT_DIR, "filtered_reconstructed")
dir.create(FILTERED_OUT, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

sample_id_from_raw <- function(x) {
  sub("_[12]\\.fastq(\\.gz)?$", "", basename(x), ignore.case = TRUE)
}

is_dna <- function(x) {
  if (is.null(x) || !length(x)) return(logical())
  grepl("^[ACGTN]+$", toupper(as.character(x))) &
    nchar(as.character(x)) >= 20
}

canonical_hash <- function(mat) {
  mat2 <- mat[
    order(rownames(mat)),
    order(colnames(mat)),
    drop = FALSE
  ]
  storage.mode(mat2) <- "numeric"
  digest::digest(mat2, algo = "sha256", serialize = TRUE)
}

standardize_seqtab <- function(x) {
  x <- as.matrix(x)
  row_ratio <- if (nrow(x)) mean(is_dna(rownames(x))) else 0
  col_ratio <- if (ncol(x)) mean(is_dna(colnames(x))) else 0

  if (col_ratio >= 0.7) return(x)
  if (row_ratio >= 0.7) return(t(x))

  stop("Could not determine sequence-table orientation.")
}

# ------------------------------------------------------------
# 1. Resolve raw paired FASTQs
# ------------------------------------------------------------

fnFs <- sort(list.files(
  RAW_DIR,
  pattern = "_1\\.fastq(\\.gz)?$",
  recursive = FALSE,
  full.names = TRUE,
  ignore.case = TRUE
))

fnRs <- sort(list.files(
  RAW_DIR,
  pattern = "_2\\.fastq(\\.gz)?$",
  recursive = FALSE,
  full.names = TRUE,
  ignore.case = TRUE
))

if (length(fnFs) != 34L || length(fnRs) != 34L) {
  stop(
    "Expected exactly 34 raw R1 and 34 raw R2 FASTQs in:\n",
    RAW_DIR,
    "\nFound R1=", length(fnFs), ", R2=", length(fnRs)
  )
}

sF <- unname(vapply(fnFs, sample_id_from_raw, character(1)))
sR <- unname(vapply(fnRs, sample_id_from_raw, character(1)))

if (!identical(sF, sR)) {
  debug <- data.frame(
    R1_File = basename(fnFs),
    R1_ID = sF,
    R2_File = basename(fnRs),
    R2_ID = sR,
    Match = sF == sR,
    stringsAsFactors = FALSE
  )
  write.csv(
    debug,
    file.path(OUT_DIR, "00F_raw_pairing_debug.csv"),
    row.names = FALSE
  )
  stop("Raw R1/R2 sample IDs do not align.")
}

sample.names <- sF
names(fnFs) <- sample.names
names(fnRs) <- sample.names

filtFs <- file.path(FILTERED_OUT, paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(FILTERED_OUT, paste0(sample.names, "_R_filt.fastq.gz"))

# Preserve sample IDs through DADA2 so no post-hoc row-name repair is needed.
names(filtFs) <- sample.names
names(filtRs) <- sample.names

# Remove stale candidate outputs from a previous incomplete run.
unlink(c(filtFs, filtRs), force = TRUE)

# ------------------------------------------------------------
# 2. Reconstructed raw FASTQ -> filtered FASTQ
# ------------------------------------------------------------

message("STEP 1/7: filterAndTrim from raw FASTQ ...")

filter_tracking <- dada2::filterAndTrim(
  fnFs,
  filtFs,
  fnRs,
  filtRs,
  truncLen = c(240, 240),
  trimLeft = c(0, 0),
  maxN = 0,
  maxEE = c(2, 2),
  truncQ = 2,
  rm.phix = FALSE,
  compress = TRUE,
  multithread = FALSE,
  verbose = TRUE
)

filter_tracking_df <- data.frame(
  Sample_ID = sample.names,
  Reads_In = filter_tracking[, "reads.in"],
  Reads_Out = filter_tracking[, "reads.out"],
  Retention = filter_tracking[, "reads.out"] / filter_tracking[, "reads.in"],
  stringsAsFactors = FALSE
)

write.csv(
  filter_tracking_df,
  file.path(OUT_DIR, "00F_filterAndTrim_tracking.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (any(filter_tracking_df$Reads_Out <= 0)) {
  stop("At least one sample has zero filtered reads.")
}

# ------------------------------------------------------------
# 3. Error learning
# ------------------------------------------------------------

set.seed(1)

message("STEP 2/7: learnErrors forward ...")
errF <- learnErrors(
  filtFs,
  multithread = FALSE
)

message("STEP 3/7: learnErrors reverse ...")
errR <- learnErrors(
  filtRs,
  multithread = FALSE
)

saveRDS(errF, file.path(OUT_DIR, "00F_errF.rds"))
saveRDS(errR, file.path(OUT_DIR, "00F_errR.rds"))

# ------------------------------------------------------------
# 4. Denoising + pair merging
# ------------------------------------------------------------

message("STEP 4/7: dada denoising ...")

dadaFs <- dada(
  filtFs,
  err = errF,
  multithread = FALSE
)

dadaRs <- dada(
  filtRs,
  err = errR,
  multithread = FALSE
)

message("STEP 5/7: mergePairs + makeSequenceTable ...")

mergers <- mergePairs(
  dadaFs,
  filtFs,
  dadaRs,
  filtRs,
  verbose = FALSE
)

seqtab_merged <- makeSequenceTable(mergers)

saveRDS(
  seqtab_merged,
  file.path(OUT_DIR, "00F_seqtab_merged_before_postfilter.rds")
)

# ------------------------------------------------------------
# 5. Recovered historical post-filtering
# ------------------------------------------------------------

message("STEP 6/7: historical length/abundance/chimera filtering ...")

seq_lengths <- nchar(getSequences(seqtab_merged))

seqtab_len <- seqtab_merged[
  ,
  seq_lengths >= 440 & seq_lengths <= 468,
  drop = FALSE
]

seqtab_abund <- seqtab_len[
  ,
  colSums(seqtab_len) >= 50,
  drop = FALSE
]

seqtab_final <- removeBimeraDenovo(
  seqtab_abund,
  method = "consensus",
  multithread = FALSE,
  verbose = TRUE
)

RECON_FINAL_RDS <- file.path(
  OUT_DIR,
  paste0(PROJECT_ID, "_seqtab_final_END_TO_END_RECONSTRUCTED.rds")
)

saveRDS(seqtab_final, RECON_FINAL_RDS)

# ------------------------------------------------------------
# 6. Optional exact validation against archived final V1 ASV table
# ------------------------------------------------------------
# The compact public repository does not bundle the larger historical final-ASV RDS.
# Therefore raw reconstruction must still finish successfully when that optional
# reference is absent. When the reference is supplied in the historical repository
# layout, the original exact-comparison logic is executed unchanged.

message("STEP 7/7: checking optional archived final-ASV validation reference ...")

recon <- standardize_seqtab(seqtab_final)
recon_hash <- canonical_hash(recon)

if (!file.exists(ORIGINAL_FINAL_RDS)) {
  status <- "RECONSTRUCTION_COMPLETE__REFERENCE_NOT_BUNDLED"

  write.csv(
    data.frame(
      Metric = c("N_samples", "N_ASVs", "Total_reads", "Canonical_table_SHA256"),
      Reconstructed = c(nrow(recon), ncol(recon), sum(recon), recon_hash),
      stringsAsFactors = FALSE
    ),
    file.path(OUT_DIR, "00F_END_TO_END_comparison.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  writeLines(
    capture.output(sessionInfo()),
    file.path(OUT_DIR, "00F_END_TO_END_SessionInfo.txt")
  )

  writeLines(
    c(
      paste0("Project_ID: ", PROJECT_ID),
      paste0("Status: ", status),
      "Archived_reference_RDS_present: FALSE",
      paste0("Reconstructed_SHA256: ", recon_hash),
      paste0("Reconstructed_samples: ", nrow(recon)),
      paste0("Reconstructed_ASVs: ", ncol(recon)),
      paste0("Reconstructed_total_reads: ", sum(recon)),
      "Note: exact historical equality cannot be re-checked without the optional archived final-ASV RDS."
    ),
    file.path(OUT_DIR, "00F_END_TO_END_STATUS.txt")
  )

  cat("\n============================================================\n")
  cat("PRJNA797231 RAW -> FINAL ASV reconstruction complete\n")
  cat("Status: ", status, "\n", sep = "")
  cat("Rebuilt: ", nrow(recon), " samples, ", ncol(recon),
      " ASVs, ", sum(recon), " reads\n", sep = "")
  cat("Reconstructed canonical SHA256: ", recon_hash, "\n", sep = "")
  cat("Archived validation RDS was not bundled; reconstruction output was retained.\n")
  cat("Output directory:\n", OUT_DIR, "\n", sep = "")
  cat("============================================================\n")

} else {
  message("Archived final-ASV reference found; running exact validation ...")

  orig <- standardize_seqtab(readRDS(ORIGINAL_FINAL_RDS))
  orig_hash <- canonical_hash(orig)

  sample_equal <- setequal(rownames(orig), rownames(recon))
  asv_equal <- setequal(colnames(orig), colnames(recon))
  hash_equal <- identical(orig_hash, recon_hash)

  common_samples <- intersect(rownames(orig), rownames(recon))
  common_asvs <- intersect(colnames(orig), colnames(recon))

  if (length(common_samples) && length(common_asvs)) {
    o <- orig[common_samples, common_asvs, drop = FALSE]
    r <- recon[common_samples, common_asvs, drop = FALSE]
    max_abs_diff <- max(abs(o - r))
    total_abs_diff <- sum(abs(o - r))
  } else {
    max_abs_diff <- NA_real_
    total_abs_diff <- NA_real_
  }

  comparison <- data.frame(
    Metric = c(
      "N_samples",
      "N_ASVs",
      "Total_reads",
      "Median_sample_depth",
      "Sample_ID_set_equal",
      "ASV_sequence_set_equal",
      "Canonical_table_SHA256",
      "Canonical_hash_equal",
      "Common_samples",
      "Common_ASVs",
      "Max_abs_count_difference_on_overlap",
      "Total_abs_count_difference_on_overlap"
    ),
    Original = c(
      nrow(orig), ncol(orig), sum(orig), median(rowSums(orig)),
      TRUE, TRUE, orig_hash, TRUE, nrow(orig), ncol(orig), 0, 0
    ),
    End_to_end_reconstructed = c(
      nrow(recon), ncol(recon), sum(recon), median(rowSums(recon)),
      sample_equal, asv_equal, recon_hash, hash_equal,
      length(common_samples), length(common_asvs),
      max_abs_diff, total_abs_diff
    ),
    stringsAsFactors = FALSE
  )

  write.csv(
    comparison,
    file.path(OUT_DIR, "00F_END_TO_END_comparison.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  all_samples <- union(rownames(orig), rownames(recon))
  orig_depth <- rowSums(orig)
  recon_depth <- rowSums(recon)

  depth_compare <- data.frame(
    Sample_ID = all_samples,
    Original_Depth = unname(orig_depth[match(all_samples, names(orig_depth))]),
    Reconstructed_Depth = unname(recon_depth[match(all_samples, names(recon_depth))]),
    stringsAsFactors = FALSE
  )
  depth_compare$Difference <- depth_compare$Reconstructed_Depth - depth_compare$Original_Depth

  write.csv(
    depth_compare,
    file.path(OUT_DIR, "00F_END_TO_END_depth_comparison.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  asv_compare <- data.frame(
    Original_ASVs = ncol(orig),
    Reconstructed_ASVs = ncol(recon),
    Shared_ASVs = length(common_asvs),
    Original_only_ASVs = length(setdiff(colnames(orig), colnames(recon))),
    Reconstructed_only_ASVs = length(setdiff(colnames(recon), colnames(orig))),
    stringsAsFactors = FALSE
  )

  write.csv(
    asv_compare,
    file.path(OUT_DIR, "00F_END_TO_END_ASV_overlap.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  writeLines(
    capture.output(sessionInfo()),
    file.path(OUT_DIR, "00F_END_TO_END_SessionInfo.txt")
  )

  status <- if (hash_equal && sample_equal && asv_equal &&
                isTRUE(max_abs_diff == 0) && isTRUE(total_abs_diff == 0)) {
    "END_TO_END_EXACT_REPRODUCTION"
  } else {
    "NOT_EXACT__REVIEW_OUTPUTS"
  }

  writeLines(
    c(
      paste0("Project_ID: ", PROJECT_ID),
      paste0("Status: ", status),
      paste0("Original_SHA256: ", orig_hash),
      paste0("Reconstructed_SHA256: ", recon_hash),
      paste0("Canonical_hash_equal: ", hash_equal),
      paste0("Sample_ID_set_equal: ", sample_equal),
      paste0("ASV_sequence_set_equal: ", asv_equal),
      paste0("Original_samples: ", nrow(orig)),
      paste0("Reconstructed_samples: ", nrow(recon)),
      paste0("Original_ASVs: ", ncol(orig)),
      paste0("Reconstructed_ASVs: ", ncol(recon)),
      paste0("Original_total_reads: ", sum(orig)),
      paste0("Reconstructed_total_reads: ", sum(recon))
    ),
    file.path(OUT_DIR, "00F_END_TO_END_STATUS.txt")
  )

  cat("\n============================================================\n")
  cat("PRJNA797231 RAW -> FINAL ASV reconstruction complete\n")
  cat("Status: ", status, "\n", sep = "")
  cat("Original : ", nrow(orig), " samples, ", ncol(orig),
      " ASVs, ", sum(orig), " reads\n", sep = "")
  cat("Rebuilt  : ", nrow(recon), " samples, ", ncol(recon),
      " ASVs, ", sum(recon), " reads\n", sep = "")
  cat("Canonical hash equal: ", hash_equal, "\n", sep = "")
  cat("Sample-ID set equal: ", sample_equal, "\n", sep = "")
  cat("ASV sequence set equal: ", asv_equal, "\n", sep = "")
  cat("Output directory:\n", OUT_DIR, "\n", sep = "")
  cat("============================================================\n")
}
