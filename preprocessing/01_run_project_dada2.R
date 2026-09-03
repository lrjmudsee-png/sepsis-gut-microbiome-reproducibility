# ============================================================
# Sepsis public-cohort reanalysis — six-project DADA2 worker
# File: 01_run_one_v1_dada2_project.R
#
# Purpose
#   Re-run ONE of the six remaining projects from raw FASTQ to final ASV
#   using the recovered project-specific historical V1 route, then compare
#   the reconstructed ASV table with the archived V1 fingerprint/reference.
#
# Supported projects
#   PRJEB33360
#   PRJNA691455
#   PRJNA978257
#   PRJNA1010969
#   PRJNA430161
#   PRJNA912621
#
# IMPORTANT
#   - Original final RDS files are READ-ONLY.
#   - New outputs are written under the active project root in a reconstruction directory.
#   - This script stops at final ASV; taxonomy/statistics are done later.
#   - The `multithread` choice below is computational, not a biological
#     filtering parameter. It is FALSE by default so multiple projects can
#     run safely in parallel without CPU oversubscription.
# ============================================================

options(stringsAsFactors = FALSE)

PROJECT_ROOT <- Sys.getenv("SEPSIS_V1_PROJECT_ROOT", unset = getwd())
PROJECT_ROOT <- normalizePath(PROJECT_ROOT, winslash = "/", mustWork = FALSE)

# PUBLIC PATH CONFIGURATION (path plumbing only; scientific DADA2 parameters below
# are unchanged). These variables are normally set by run_raw_preprocessing.R.
RAW_DATA_ROOT <- Sys.getenv("SEPSIS_V1_RAW_DATA_ROOT", unset = "")
RAW_PROJECT_DIR <- Sys.getenv("SEPSIS_V1_RAW_PROJECT_DIR", unset = "")
RAW_OUTPUT_ROOT <- Sys.getenv("SEPSIS_V1_RAW_OUTPUT_ROOT", unset = "")
if (nzchar(RAW_DATA_ROOT)) RAW_DATA_ROOT <- normalizePath(RAW_DATA_ROOT, winslash = "/", mustWork = FALSE)
if (nzchar(RAW_PROJECT_DIR)) RAW_PROJECT_DIR <- normalizePath(RAW_PROJECT_DIR, winslash = "/", mustWork = FALSE)
if (nzchar(RAW_OUTPUT_ROOT)) RAW_OUTPUT_ROOT <- normalizePath(RAW_OUTPUT_ROOT, winslash = "/", mustWork = FALSE)

# Parallel-safe mode. When running projects one-by-one you may set TRUE.
DADA2_INTERNAL_MULTITHREAD <- FALSE

# Fixed seed makes the reconstructed run repeatable.
RECONSTRUCTION_SEED <- 1L

OUT_ROOT <- if (nzchar(RAW_OUTPUT_ROOT)) {
  RAW_OUTPUT_ROOT
} else {
  file.path(PROJECT_ROOT, "work", "raw_preprocessing")
}
dir.create(OUT_ROOT, recursive = TRUE, showWarnings = FALSE)

REQUIRED <- c("dada2", "digest")
MISSING <- REQUIRED[
  !vapply(REQUIRED, requireNamespace, logical(1), quietly = TRUE)
]
if (length(MISSING)) {
  stop("Missing R package(s): ", paste(MISSING, collapse = ", "))
}

suppressPackageStartupMessages(library(dada2))

# ------------------------------------------------------------
# Verified project-specific routes + reference fingerprints
# ------------------------------------------------------------

PROJECT_CONFIG <- list(

  PRJEB33360 = list(
    mode = "single_end",
    route = "historical_PRJEB33360_single_end",
    filter = list(
      truncLen = 450,
      trimLeft = 0,
      maxN = 0,
      maxEE = 2,
      truncQ = 2,
      rm.phix = TRUE
    ),
    learn = list(nbases = NULL, randomize = FALSE),
    dada_pool = FALSE,
    reference = file.path(
      PROJECT_ROOT, "data", "PRJEB33360", "03_dada2",
      "PRJEB33360_seqtab_final.rds"
    ),
    expected = list(
      samples = 239L,
      asvs = 49614L,
      reads = 5849419,
      sha256 = "4e430c0682d0612b2bc428fab5657e8f5f7428066f2527778b29fd6d25aecaa1"
    )
  ),

  PRJNA691455 = list(
    mode = "single_end_R1",
    route = "final_rerun_v2_R1_only",
    filter = list(
      truncLen = 250,
      trimLeft = 17,
      maxN = 0,
      maxEE = 2,
      truncQ = 2,
      rm.phix = TRUE
    ),
    learn = list(nbases = NULL, randomize = TRUE),
    dada_pool = "pseudo",
    reference = file.path(
      PROJECT_ROOT, "data", "PRJNA691455", "03_dada2_rerun_v2",
      "PRJNA691455_seqtab_final.rds"
    ),
    expected = list(
      samples = 49L,
      asvs = 2912L,
      reads = 2398535,
      sha256 = "b159ed8e802434a742638623729e8900efcd83a5788143e080a25eca8cae4ec9"
    )
  ),

  PRJNA978257 = list(
    mode = "paired_end",
    route = "historical_ENA_250bp_route",
    filter = list(
      truncLen = c(0, 0),
      trimLeft = c(0, 0),
      minLen = 50,
      maxN = 0,
      maxEE = c(2, 5),
      truncQ = 2,
      rm.phix = TRUE
    ),
    learn = list(nbases = 1e7, randomize = TRUE),
    dada_pool = FALSE,
    merge = list(),
    reference = file.path(
      PROJECT_ROOT, "data", "PRJNA978257", "03_dada2",
      "PRJNA978257_seqtab_final.rds"
    ),
    expected = list(
      samples = 13L,
      asvs = 4825L,
      reads = 417923,
      sha256 = "60c1fabe4c1bae4f959699cc986a60a137f95bcab9d0802c91d51bd059a88055"
    )
  ),

  PRJNA1010969 = list(
    mode = "paired_end",
    route = "final_rerun_v2_V4",
    filter = list(
      truncLen = c(220, 160),
      trimLeft = c(19, 20),
      maxN = 0,
      maxEE = c(2, 3),
      truncQ = 2,
      rm.phix = TRUE
    ),
    learn = list(nbases = NULL, randomize = TRUE),
    dada_pool = "pseudo",
    merge = list(minOverlap = 20, maxMismatch = 1),
    reference = file.path(
      PROJECT_ROOT, "data", "PRJNA1010969", "03_dada2_rerun_v2",
      "PRJNA1010969_seqtab_final.rds"
    ),
    expected = list(
      samples = 53L,
      asvs = 4614L,
      reads = 1941413,
      sha256 = "40969ce3c68da72eee10bccee7584b168a0d5a2886de7b587b3b7d42a0c6568a"
    )
  ),

  PRJNA430161 = list(
    mode = "paired_end",
    route = "historical_ENA_full_length_high_reverse_EE",
    filter = list(
      truncLen = c(0, 0),
      trimLeft = c(0, 0),
      minLen = 50,
      maxN = 0,
      maxEE = c(2, 15),
      truncQ = 2,
      rm.phix = TRUE
    ),
    learn = list(nbases = 1e7, randomize = TRUE),
    dada_pool = FALSE,
    merge = list(),
    reference = file.path(
      PROJECT_ROOT, "data", "PRJNA430161", "03_dada2",
      "PRJNA430161_seqtab_final.rds"
    ),
    expected = list(
      samples = 26L,
      asvs = 1049L,
      reads = 814672,
      sha256 = "8a7a8a36a59b4dda7b64d99dd7918e1977c19c050cdd61f00000ad8d5a1b91bb"
    )
  ),

  PRJNA912621 = list(
    mode = "paired_end",
    route = "historical_ENA_250bp_route",
    filter = list(
      truncLen = c(0, 0),
      trimLeft = c(0, 0),
      minLen = 50,
      maxN = 0,
      maxEE = c(2, 5),
      truncQ = 2,
      rm.phix = TRUE
    ),
    learn = list(nbases = 1e7, randomize = TRUE),
    dada_pool = FALSE,
    merge = list(),
    reference = file.path(
      PROJECT_ROOT, "data", "PRJNA912621", "03_dada2",
      "PRJNA912621_seqtab_final.rds"
    ),
    expected = list(
      samples = 58L,
      asvs = 20151L,
      reads = 2355050,
      sha256 = "f02b62687d99824dae6b708b525c8da0058541fd7c6b46f1614ee59653956422"
    )
  )
)

SUPPORTED_PROJECTS <- names(PROJECT_CONFIG)

# ------------------------------------------------------------
# General helpers
# ------------------------------------------------------------

extract_run <- function(path) {
  x <- basename(path)
  m <- regexpr("(SRR|ERR|DRR)[0-9]+", x, perl = TRUE)
  if (m[1] == -1) return(NA_character_)
  regmatches(x, m)
}

classify_direction <- function(path) {
  x <- basename(path)

  # Explicit common paired-end patterns.
  if (grepl("(_R?1[_\\.]|_1\\.fastq|_1\\.fq|\\.1\\.)", x, ignore.case = TRUE)) {
    return("R1")
  }
  if (grepl("(_R?2[_\\.]|_2\\.fastq|_2\\.fq|\\.2\\.)", x, ignore.case = TRUE)) {
    return("R2")
  }

  "UNRESOLVED"
}

candidate_raw_roots <- function(project) {
  public_roots <- character(0)
  if (nzchar(RAW_PROJECT_DIR)) public_roots <- c(public_roots, RAW_PROJECT_DIR)
  if (nzchar(RAW_DATA_ROOT)) {
    public_roots <- c(public_roots, file.path(RAW_DATA_ROOT, project))
    if (identical(toupper(basename(RAW_DATA_ROOT)), toupper(project))) {
      public_roots <- c(public_roots, RAW_DATA_ROOT)
    }
  }

  unique(c(
    public_roots,
    # Backward-compatible repository-relative layouts:
    file.path(PROJECT_ROOT, "data", project, "fastq_results"),
    file.path(PROJECT_ROOT, "data", project, "raw"),
    file.path(PROJECT_ROOT, "data", project, "01_raw"),
    file.path(PROJECT_ROOT, "data", project, "02_raw"),
    file.path(PROJECT_ROOT, "ENA", project),
    file.path(PROJECT_ROOT, "SRA", project),
    file.path(PROJECT_ROOT, project, "fastq_results"),
    file.path(PROJECT_ROOT, "data", project)
  ))
}

find_raw_fastqs <- function(project) {

  roots <- candidate_raw_roots(project)
  roots <- roots[dir.exists(roots)]

  if (!length(roots)) {
    stop(project, ": none of the expected raw-data directories exists.")
  }

  root_records <- list()

  for (root in roots) {
    f <- list.files(
      root,
      pattern = "\\.(fastq|fq)(\\.gz)?$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )

    if (!length(f)) next

    # Exclude any derived/intermediate sequence products.
    f <- f[
      !grepl(
        paste(
          c(
            "filtered", "trimmed", "cutadapt", "dada2",
            "denoised", "merged", "rerun", "reconstruction",
            "taxonomy", "chimera", "nochim"
          ),
          collapse = "|"
        ),
        f,
        ignore.case = TRUE
      )
    ]

    if (!length(f)) next

    runs <- vapply(f, extract_run, character(1))
    dirs <- vapply(f, classify_direction, character(1))

    root_records[[length(root_records) + 1L]] <- data.frame(
      Root = root,
      Path = f,
      Run_ID = runs,
      Direction = dirs,
      stringsAsFactors = FALSE
    )
  }

  if (!length(root_records)) {
    stop(project, ": no raw FASTQ files found after excluding derived files.")
  }

  all <- do.call(rbind, root_records)
  all <- all[!is.na(all$Run_ID), , drop = FALSE]

  if (!nrow(all)) {
    stop(project, ": FASTQs found, but no SRR/ERR/DRR run IDs could be parsed.")
  }

  # Select the root with the largest number of unique run/direction records.
  score <- aggregate(
    paste(Run_ID, Direction) ~ Root,
    data = all,
    FUN = function(x) length(unique(x))
  )
  names(score)[2] <- "Unique_Run_Direction"
  chosen_root <- score$Root[which.max(score$Unique_Run_Direction)]

  chosen <- all[all$Root == chosen_root, , drop = FALSE]

  # When duplicate physical copies exist inside one root, keep the shortest path
  # (usually the actual raw file rather than a nested copy).
  chosen$PathLength <- nchar(chosen$Path)
  chosen <- chosen[
    order(chosen$Run_ID, chosen$Direction, chosen$PathLength),
    ,
    drop = FALSE
  ]
  chosen <- chosen[
    !duplicated(paste(chosen$Run_ID, chosen$Direction)),
    ,
    drop = FALSE
  ]

  chosen$PathLength <- NULL

  list(
    root = chosen_root,
    manifest = chosen,
    root_audit = score
  )
}

is_dna <- function(x) {
  if (is.null(x) || !length(x)) return(logical())
  grepl("^[ACGTN]+$", toupper(as.character(x))) &
    nchar(as.character(x)) >= 20
}

standardize_seqtab <- function(x) {
  x <- as.matrix(x)

  col_ratio <- if (ncol(x)) mean(is_dna(colnames(x))) else 0
  row_ratio <- if (nrow(x)) mean(is_dna(rownames(x))) else 0

  if (is.finite(col_ratio) && col_ratio >= 0.7) return(x)
  if (is.finite(row_ratio) && row_ratio >= 0.7) return(t(x))

  stop("Could not identify sample x ASV orientation.")
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

getN <- function(x) sum(dada2::getUniques(x))

safe_write_csv <- function(x, path) {
  write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
}

# ------------------------------------------------------------
# Worker
# ------------------------------------------------------------

run_v1_project <- function(project) {

  if (!project %in% SUPPORTED_PROJECTS) {
    stop(
      "Unsupported project: ", project,
      "\nSupported: ", paste(SUPPORTED_PROJECTS, collapse = ", ")
    )
  }

  cfg <- PROJECT_CONFIG[[project]]
  out_dir <- file.path(OUT_ROOT, project)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  log_path <- file.path(out_dir, paste0(project, "_console_log.txt"))
  zz <- file(log_path, open = "wt")
  sink(zz, type = "output")
  sink(zz, type = "message")
  on.exit({
    try(sink(type = "message"), silent = TRUE)
    try(sink(type = "output"), silent = TRUE)
    try(close(zz), silent = TRUE)
  }, add = TRUE)

  cat("============================================================\n")
  cat("Project: ", project, "\n", sep = "")
  cat("Recovered route: ", cfg$route, "\n", sep = "")
  cat("Start: ", format(Sys.time()), "\n", sep = "")
  cat("Internal DADA2 multithread: ", DADA2_INTERNAL_MULTITHREAD, "\n", sep = "")
  cat("============================================================\n")

  set.seed(RECONSTRUCTION_SEED)

  raw <- find_raw_fastqs(project)

  safe_write_csv(
    raw$root_audit,
    file.path(out_dir, paste0(project, "_raw_root_audit.csv"))
  )
  safe_write_csv(
    raw$manifest,
    file.path(out_dir, paste0(project, "_raw_manifest_selected.csv"))
  )

  cat("Selected raw root: ", raw$root, "\n", sep = "")
  cat("Selected FASTQ records: ", nrow(raw$manifest), "\n", sep = "")

  # ----------------------------------------------------------
  # Resolve raw files according to project route
  # ----------------------------------------------------------

  if (cfg$mode == "single_end") {

    manifest <- raw$manifest

    # PRJEB33360 historically used single-end files without requiring _1 suffix.
    # If an R1 marker is present, prefer it; otherwise use one raw file per run.
    if (any(manifest$Direction == "R1")) {
      manifest <- manifest[manifest$Direction == "R1", , drop = FALSE]
    }

    manifest <- manifest[order(manifest$Run_ID), , drop = FALSE]
    manifest <- manifest[!duplicated(manifest$Run_ID), , drop = FALSE]

    run_ids <- manifest$Run_ID
    fnFs <- manifest$Path

  } else if (cfg$mode == "single_end_R1") {

    manifest <- raw$manifest
    manifest <- manifest[manifest$Direction == "R1", , drop = FALSE]
    manifest <- manifest[order(manifest$Run_ID), , drop = FALSE]
    manifest <- manifest[!duplicated(manifest$Run_ID), , drop = FALSE]

    if (!nrow(manifest)) {
      stop(project, ": no R1 files found.")
    }

    run_ids <- manifest$Run_ID
    fnFs <- manifest$Path

  } else {

    manifest <- raw$manifest
    fwd <- manifest[manifest$Direction == "R1", , drop = FALSE]
    rev <- manifest[manifest$Direction == "R2", , drop = FALSE]

    paired_runs <- sort(intersect(fwd$Run_ID, rev$Run_ID))

    if (!length(paired_runs)) {
      stop(project, ": no complete R1/R2 pairs found.")
    }

    run_ids <- paired_runs
    fnFs <- fwd$Path[match(run_ids, fwd$Run_ID)]
    fnRs <- rev$Path[match(run_ids, rev$Run_ID)]
  }

  cat("Runs entering DADA2: ", length(run_ids), "\n", sep = "")

  # ----------------------------------------------------------
  # Reconstructed output paths
  # ----------------------------------------------------------

  filt_dir <- file.path(out_dir, "filtered_reconstructed")
  dir.create(filt_dir, recursive = TRUE, showWarnings = FALSE)

  if (cfg$mode %in% c("single_end", "single_end_R1")) {

    filtFs <- file.path(filt_dir, paste0(run_ids, "_F_filt.fastq.gz"))
    unlink(filtFs, force = TRUE)

    f <- cfg$filter

    args <- list(
      fwd = fnFs,
      filt = filtFs,
      truncLen = f$truncLen,
      trimLeft = f$trimLeft,
      maxN = f$maxN,
      maxEE = f$maxEE,
      truncQ = f$truncQ,
      rm.phix = f$rm.phix,
      compress = TRUE,
      multithread = DADA2_INTERNAL_MULTITHREAD,
      verbose = TRUE
    )

    if (!is.null(f$minLen)) args$minLen <- f$minLen

    cat("filterAndTrim...\n")
    filt_out <- do.call(dada2::filterAndTrim, args)
    rownames(filt_out) <- run_ids

    keep <- filt_out[, "reads.out"] > 0
    if (sum(keep) < 2) stop(project, ": fewer than 2 samples survived filtering.")

    run_ids2 <- run_ids[keep]
    filtFs2 <- filtFs[keep]

    cat("learnErrors...\n")
    learn_args <- list(
      fls = filtFs2,
      multithread = DADA2_INTERNAL_MULTITHREAD
    )
    if (!is.null(cfg$learn$nbases)) learn_args$nbases <- cfg$learn$nbases
    if (!is.null(cfg$learn$randomize)) learn_args$randomize <- cfg$learn$randomize

    errF <- do.call(dada2::learnErrors, learn_args)
    saveRDS(errF, file.path(out_dir, paste0(project, "_errF_reconstructed.rds")))

    cat("dada...\n")

    if (identical(cfg$dada_pool, "pseudo")) {
      derepFs <- dada2::derepFastq(filtFs2, verbose = TRUE)
      names(derepFs) <- run_ids2
      dadaFs <- dada2::dada(
        derepFs,
        err = errF,
        multithread = DADA2_INTERNAL_MULTITHREAD,
        pool = "pseudo"
      )
      seqtab <- dada2::makeSequenceTable(dadaFs)
    } else {
      names(filtFs2) <- run_ids2
      dadaFs <- dada2::dada(
        filtFs2,
        err = errF,
        multithread = DADA2_INTERNAL_MULTITHREAD
      )
      seqtab <- dada2::makeSequenceTable(dadaFs)
    }

    cat("removeBimeraDenovo...\n")
    seqtab_final <- dada2::removeBimeraDenovo(
      seqtab,
      method = "consensus",
      multithread = DADA2_INTERNAL_MULTITHREAD,
      verbose = TRUE
    )

    tracking <- data.frame(
      Run_ID = run_ids,
      Input = filt_out[run_ids, "reads.in"],
      Filtered = filt_out[run_ids, "reads.out"],
      stringsAsFactors = FALSE
    )

  } else {

    filtFs <- file.path(filt_dir, paste0(run_ids, "_F_filt.fastq.gz"))
    filtRs <- file.path(filt_dir, paste0(run_ids, "_R_filt.fastq.gz"))
    unlink(c(filtFs, filtRs), force = TRUE)

    f <- cfg$filter

    args <- list(
      fwd = fnFs,
      filt = filtFs,
      rev = fnRs,
      filt.rev = filtRs,
      truncLen = f$truncLen,
      trimLeft = f$trimLeft,
      maxN = f$maxN,
      maxEE = f$maxEE,
      truncQ = f$truncQ,
      rm.phix = f$rm.phix,
      compress = TRUE,
      multithread = DADA2_INTERNAL_MULTITHREAD,
      verbose = TRUE
    )
    if (!is.null(f$minLen)) args$minLen <- f$minLen

    cat("filterAndTrim...\n")
    filt_out <- do.call(dada2::filterAndTrim, args)
    rownames(filt_out) <- run_ids

    keep <- filt_out[, "reads.out"] > 0
    if (sum(keep) < 2) stop(project, ": fewer than 2 paired samples survived filtering.")

    run_ids2 <- run_ids[keep]
    filtFs2 <- filtFs[keep]
    filtRs2 <- filtRs[keep]

    cat("learnErrors F/R...\n")

    learnF <- list(
      fls = filtFs2,
      multithread = DADA2_INTERNAL_MULTITHREAD
    )
    learnR <- list(
      fls = filtRs2,
      multithread = DADA2_INTERNAL_MULTITHREAD
    )

    if (!is.null(cfg$learn$nbases)) {
      learnF$nbases <- cfg$learn$nbases
      learnR$nbases <- cfg$learn$nbases
    }
    if (!is.null(cfg$learn$randomize)) {
      learnF$randomize <- cfg$learn$randomize
      learnR$randomize <- cfg$learn$randomize
    }

    errF <- do.call(dada2::learnErrors, learnF)
    errR <- do.call(dada2::learnErrors, learnR)

    saveRDS(errF, file.path(out_dir, paste0(project, "_errF_reconstructed.rds")))
    saveRDS(errR, file.path(out_dir, paste0(project, "_errR_reconstructed.rds")))

    cat("dada F/R...\n")

    if (identical(cfg$dada_pool, "pseudo")) {

      derepFs <- dada2::derepFastq(filtFs2, verbose = TRUE)
      derepRs <- dada2::derepFastq(filtRs2, verbose = TRUE)
      names(derepFs) <- run_ids2
      names(derepRs) <- run_ids2

      dadaFs <- dada2::dada(
        derepFs,
        err = errF,
        multithread = DADA2_INTERNAL_MULTITHREAD,
        pool = "pseudo"
      )
      dadaRs <- dada2::dada(
        derepRs,
        err = errR,
        multithread = DADA2_INTERNAL_MULTITHREAD,
        pool = "pseudo"
      )

      merge_args <- c(
        list(
          dadaF = dadaFs,
          derepF = derepFs,
          dadaR = dadaRs,
          derepR = derepRs,
          verbose = TRUE
        ),
        cfg$merge
      )

    } else {

      names(filtFs2) <- run_ids2
      names(filtRs2) <- run_ids2

      dadaFs <- dada2::dada(
        filtFs2,
        err = errF,
        multithread = DADA2_INTERNAL_MULTITHREAD
      )
      dadaRs <- dada2::dada(
        filtRs2,
        err = errR,
        multithread = DADA2_INTERNAL_MULTITHREAD
      )

      merge_args <- c(
        list(
          dadaF = dadaFs,
          derepF = filtFs2,
          dadaR = dadaRs,
          derepR = filtRs2,
          verbose = FALSE
        ),
        cfg$merge
      )
    }

    cat("mergePairs...\n")
    mergers <- do.call(dada2::mergePairs, merge_args)

    cat("makeSequenceTable...\n")
    seqtab <- dada2::makeSequenceTable(mergers)

    if (ncol(seqtab) == 0) {
      stop(project, ": mergePairs produced an empty sequence table.")
    }

    cat("removeBimeraDenovo...\n")
    seqtab_final <- dada2::removeBimeraDenovo(
      seqtab,
      method = "consensus",
      multithread = DADA2_INTERNAL_MULTITHREAD,
      verbose = TRUE
    )

    tracking <- data.frame(
      Run_ID = run_ids,
      Input = filt_out[run_ids, "reads.in"],
      Filtered = filt_out[run_ids, "reads.out"],
      stringsAsFactors = FALSE
    )
  }

  # ----------------------------------------------------------
  # Save reconstructed objects
  # ----------------------------------------------------------

  saveRDS(
    seqtab,
    file.path(out_dir, paste0(project, "_seqtab_prechim_reconstructed.rds"))
  )
  recon_rds <- file.path(
    out_dir,
    paste0(project, "_seqtab_final_RECONSTRUCTED.rds")
  )
  saveRDS(seqtab_final, recon_rds)

  safe_write_csv(
    tracking,
    file.path(out_dir, paste0(project, "_filter_tracking.csv"))
  )

  # ----------------------------------------------------------
  # Exact fingerprint validation
  # ----------------------------------------------------------

  recon <- standardize_seqtab(seqtab_final)
  recon_hash <- canonical_hash(recon)

  reference_exists <- file.exists(cfg$reference)

  if (reference_exists) {
    orig <- standardize_seqtab(readRDS(cfg$reference))
    orig_hash <- canonical_hash(orig)

    sample_equal <- setequal(rownames(orig), rownames(recon))
    asv_equal <- setequal(colnames(orig), colnames(recon))

    common_samples <- intersect(rownames(orig), rownames(recon))
    common_asvs <- intersect(colnames(orig), colnames(recon))

    if (length(common_samples) && length(common_asvs)) {
      oo <- orig[common_samples, common_asvs, drop = FALSE]
      rr <- recon[common_samples, common_asvs, drop = FALSE]
      max_abs_diff <- max(abs(oo - rr))
      total_abs_diff <- sum(abs(oo - rr))
    } else {
      max_abs_diff <- NA_real_
      total_abs_diff <- NA_real_
    }

    comparison <- data.frame(
      Project_ID = project,
      Metric = c(
        "N_samples", "N_ASVs", "Total_reads", "Median_depth",
        "Sample_ID_set_equal", "ASV_sequence_set_equal",
        "Canonical_SHA256", "Canonical_hash_equal",
        "Common_samples", "Common_ASVs",
        "Max_abs_count_diff_on_overlap",
        "Total_abs_count_diff_on_overlap"
      ),
      Original = c(
        nrow(orig), ncol(orig), sum(orig), median(rowSums(orig)),
        TRUE, TRUE, orig_hash, TRUE,
        nrow(orig), ncol(orig), 0, 0
      ),
      Reconstructed = c(
        nrow(recon), ncol(recon), sum(recon), median(rowSums(recon)),
        sample_equal, asv_equal, recon_hash,
        identical(orig_hash, recon_hash),
        length(common_samples), length(common_asvs),
        max_abs_diff, total_abs_diff
      ),
      stringsAsFactors = FALSE
    )

    exact <- (
      identical(orig_hash, recon_hash) &&
      isTRUE(sample_equal) &&
      isTRUE(asv_equal) &&
      isTRUE(max_abs_diff == 0) &&
      isTRUE(total_abs_diff == 0)
    )

  } else {

    orig_hash <- cfg$expected$sha256
    sample_equal <- NA
    asv_equal <- NA
    exact <- (
      nrow(recon) == cfg$expected$samples &&
      ncol(recon) == cfg$expected$asvs &&
      sum(recon) == cfg$expected$reads &&
      identical(recon_hash, cfg$expected$sha256)
    )

    comparison <- data.frame(
      Project_ID = project,
      Metric = c(
        "N_samples", "N_ASVs", "Total_reads",
        "Canonical_SHA256", "Canonical_hash_equal"
      ),
      Original = c(
        cfg$expected$samples,
        cfg$expected$asvs,
        cfg$expected$reads,
        cfg$expected$sha256,
        TRUE
      ),
      Reconstructed = c(
        nrow(recon),
        ncol(recon),
        sum(recon),
        recon_hash,
        identical(recon_hash, cfg$expected$sha256)
      ),
      stringsAsFactors = FALSE
    )
  }

  safe_write_csv(
    comparison,
    file.path(out_dir, paste0(project, "_ASV_reconstruction_comparison.csv"))
  )

  status <- if (exact) {
    "EXACT_REPRODUCTION"
  } else {
    "NOT_EXACT__REVIEW_REQUIRED"
  }

  status_df <- data.frame(
    Project_ID = project,
    Route = cfg$route,
    Status = status,
    Raw_Root = raw$root,
    Reconstructed_Samples = nrow(recon),
    Expected_Samples = cfg$expected$samples,
    Reconstructed_ASVs = ncol(recon),
    Expected_ASVs = cfg$expected$asvs,
    Reconstructed_Total_Reads = sum(recon),
    Expected_Total_Reads = cfg$expected$reads,
    Reconstructed_SHA256 = recon_hash,
    Expected_SHA256 = cfg$expected$sha256,
    Reference_RDS_Exists = reference_exists,
    stringsAsFactors = FALSE
  )

  safe_write_csv(
    status_df,
    file.path(out_dir, paste0(project, "_STATUS.csv"))
  )

  writeLines(
    c(
      paste0("Project_ID: ", project),
      paste0("Recovered_route: ", cfg$route),
      paste0("Status: ", status),
      paste0("Raw_root: ", raw$root),
      paste0("Reconstructed_samples: ", nrow(recon)),
      paste0("Expected_samples: ", cfg$expected$samples),
      paste0("Reconstructed_ASVs: ", ncol(recon)),
      paste0("Expected_ASVs: ", cfg$expected$asvs),
      paste0("Reconstructed_total_reads: ", sum(recon)),
      paste0("Expected_total_reads: ", cfg$expected$reads),
      paste0("Reconstructed_SHA256: ", recon_hash),
      paste0("Expected_SHA256: ", cfg$expected$sha256),
      paste0("Reference_RDS_exists: ", reference_exists)
    ),
    file.path(out_dir, paste0(project, "_STATUS.txt"))
  )

  capture.output(
    sessionInfo(),
    file = file.path(out_dir, paste0(project, "_SessionInfo.txt"))
  )

  cat("\n============================================================\n")
  cat("Finished: ", project, "\n", sep = "")
  cat("Status: ", status, "\n", sep = "")
  cat("Reconstructed: ", nrow(recon), " samples / ",
      ncol(recon), " ASVs / ", sum(recon), " reads\n", sep = "")
  cat("SHA256 equal to archived fingerprint: ",
      identical(recon_hash, cfg$expected$sha256), "\n", sep = "")
  cat("End: ", format(Sys.time()), "\n", sep = "")
  cat("============================================================\n")

  status_df
}

# ------------------------------------------------------------
# Optional command-line mode
# Rscript 01_run_one_v1_dada2_project.R PRJNA978257
# ------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1L && args[1] %in% SUPPORTED_PROJECTS) {
  result <- run_v1_project(args[1])
  print(result)
}
