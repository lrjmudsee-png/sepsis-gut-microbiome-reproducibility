# PUBLIC RELEASE v1.0.1 — optional checkpoint reconstruction
# Scientific cleaning/metadata logic is preserved from the verified historical script14.
# Only input paths were mapped to the public repository layout.
# This script requires final ASV RDS and verified taxonomy CSV checkpoints that are
# not embedded in the code-only archive; see data/checkpoints/README.md.

options(stringsAsFactors = FALSE)

root <- Sys.getenv("SEPSIS_V1_PROJECT_ROOT", unset = getwd())
root <- normalizePath(root, winslash = "/", mustWork = FALSE)

# PUBLIC PATH CONFIGURATION. The compact public package does not bundle the
# larger historical ASV/taxonomy checkpoints required by this optional route.
# If those checkpoints are deposited separately, point CHECKPOINT_ROOT to the
# directory containing asv/ and taxonomy/ subfolders.
CHECKPOINT_ROOT <- Sys.getenv(
  "SEPSIS_V1_CHECKPOINT_ROOT",
  unset = file.path(root, "data", "checkpoints")
)
CHECKPOINT_ROOT <- normalizePath(CHECKPOINT_ROOT, winslash = "/", mustWork = FALSE)

meta_dir <- file.path(root, "data", "metadata")

ANALYSIS_READY_OUTPUT_ROOT <- Sys.getenv("SEPSIS_V1_ANALYSIS_READY_OUTPUT_ROOT", unset = "")
if (!nzchar(ANALYSIS_READY_OUTPUT_ROOT)) {
  ANALYSIS_READY_OUTPUT_ROOT <- file.path(root, "processed_data", "final_frozen_v1")
}
data_out <- normalizePath(ANALYSIS_READY_OUTPUT_ROOT, winslash = "/", mustWork = FALSE)

report_out <- file.path(root, "work", "checkpoint_reconstruction_audit")
dir.create(data_out, recursive = TRUE, showWarnings = FALSE)
dir.create(report_out, recursive = TRUE, showWarnings = FALSE)

sink(file.path(report_out, "14_Final_Freeze_Log.txt"), split = TRUE)
on.exit(sink(), add = TRUE)

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Package 'readxl' is required. Install it with install.packages('readxl').")
}

cfg <- data.frame(
  Project_ID = c(
    "PRJEB33360", "PRJNA691455", "PRJNA1010969",
    "PRJNA978257", "PRJNA430161", "PRJNA797231", "PRJNA912621"
  ),
  Seqtab = c(
    file.path(CHECKPOINT_ROOT, "asv/PRJEB33360_seqtab_final.rds"),
    file.path(CHECKPOINT_ROOT, "asv/PRJNA691455_seqtab_final.rds"),
    file.path(CHECKPOINT_ROOT, "asv/PRJNA1010969_seqtab_final.rds"),
    file.path(CHECKPOINT_ROOT, "asv/PRJNA978257_seqtab_final.rds"),
    file.path(CHECKPOINT_ROOT, "asv/PRJNA430161_seqtab_final.rds"),
    file.path(CHECKPOINT_ROOT, "asv/PRJNA797231_seqtab_final.rds"),
    file.path(CHECKPOINT_ROOT, "asv/PRJNA912621_seqtab_final.rds")
  ),
  Taxonomy = c(
    file.path(CHECKPOINT_ROOT, "taxonomy/PRJEB33360_taxonomy_silva1382.csv"),
    file.path(CHECKPOINT_ROOT, "taxonomy/PRJNA691455_taxonomy_silva1382.csv"),
    file.path(CHECKPOINT_ROOT, "taxonomy/PRJNA1010969_taxonomy_silva1382.csv"),
    file.path(CHECKPOINT_ROOT, "taxonomy/PRJNA978257_taxonomy_silva1382.csv"),
    file.path(CHECKPOINT_ROOT, "taxonomy/PRJNA430161_taxonomy_silva1382.csv"),
    file.path(CHECKPOINT_ROOT, "taxonomy/PRJNA797231_taxonomy_silva1382.csv"),
    file.path(CHECKPOINT_ROOT, "taxonomy/PRJNA912621_taxonomy_silva1382.csv")
  ),
  Version = c(
    "original_paired", "rerun_v2_R1_single_end", "rerun_v2_paired",
    "original", "original", "original", "original"
  )
)

write.csv(
  data.frame(
    Parameter = c(
      "Target_taxa", "Remove_chloroplast", "Remove_mitochondria",
      "Remove_eukaryota", "Retain_unclassified_genus",
      "PRJEB_core", "PRJEB_longitudinal_main",
      "PRJEB_longitudinal_sensitivity", "True_model_leakage"
    ),
    Value = c(
      "Bacteria or Archaea", "Yes", "Yes", "Yes",
      "Yes, when Bacteria/Archaea",
      "Primary core samples with cleaned depth > 0",
      "Longitudinal cleaned depth >= 1000",
      "Longitudinal cleaned depth >= 5000",
      "Same patient/specimen in Development and Locked_external_test"
    )
  ),
  file.path(report_out, "14_Freeze_Parameters.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

wcsv <- function(x, path) {
  write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
}

norm_run <- function(x) {
  x <- trimws(as.character(x))
  m <- regexpr("(SRR|ERR|DRR)[0-9]+", x, perl = TRUE, ignore.case = TRUE)
  out <- x
  ok <- m != -1
  out[ok] <- toupper(regmatches(x, m)[ok])
  out
}

rbind_fill <- function(xs) {
  xs <- xs[!vapply(xs, is.null, logical(1))]
  if (!length(xs)) return(data.frame())
  alln <- unique(unlist(lapply(xs, names)))
  xs <- lapply(xs, function(x) {
    for (nm in setdiff(alln, names(x))) x[[nm]] <- NA
    x[alln]
  })
  do.call(rbind, xs)
}

tx <- function(df, nm) {
  if (!nm %in% names(df)) return(rep(NA_character_, nrow(df)))
  z <- trimws(as.character(df[[nm]]))
  z[is.na(z) | !nzchar(z)] <- NA_character_
  z
}

feature_all <- function(z) {
  g <- if ("Genus" %in% names(z)) z[["Genus"]] else NA
  f <- if ("Family" %in% names(z)) z[["Family"]] else NA
  if (!is.na(g) && nzchar(g)) {
    if (is.na(f) || !nzchar(f)) f <- "unresolved"
    return(paste0("Genus__", g, "|Family__", f))
  }
  for (r in c("Family", "Order", "Class", "Phylum", "Kingdom", "Domain")) {
    v <- if (r %in% names(z)) z[[r]] else NA
    if (!is.na(v) && nzchar(v)) {
      return(paste0("Genus__unclassified|", r, "__", v))
    }
  }
  "Genus__unclassified|Taxonomy__unresolved"
}

feature_classified <- function(z) {
  g <- if ("Genus" %in% names(z)) z[["Genus"]] else NA
  f <- if ("Family" %in% names(z)) z[["Family"]] else NA
  if (is.na(g) || !nzchar(g)) return(NA_character_)
  if (is.na(f) || !nzchar(f)) f <- "unresolved"
  paste0("Genus__", g, "|Family__", f)
}

rel_by_depth <- function(counts, depth) {
  out <- counts
  good <- depth > 0
  if (any(good)) {
    out[good, ] <- sweep(counts[good, , drop = FALSE], 1, depth[good], "/")
  }
  if (any(!good)) out[!good, ] <- 0
  out
}

write_mat <- function(mat, path) {
  wcsv(data.frame(Run_ID = rownames(mat), mat, check.names = FALSE), path)
}

# Metadata
meta_files <- list.files(
  meta_dir, "^03_Metadata_Analysis_Ready.*\\.xlsx$",
  full.names = TRUE, ignore.case = TRUE
)
if (!length(meta_files)) stop("No 03_Metadata_Analysis_Ready*.xlsx file was found in data/metadata.")
meta_file <- rownames(file.info(meta_files))[order(file.info(meta_files)$mtime, decreasing = TRUE)][1]
sheets <- intersect(
  c("Core_Case_Control", "Longitudinal_Support", "Organ_Dysfunction"),
  readxl::excel_sheets(meta_file)
)
meta <- rbind_fill(lapply(sheets, function(s) {
  x <- as.data.frame(readxl::read_excel(meta_file, sheet = s))
  x$Source_Sheet <- s
  x
}))
for (nm in c("Project_ID", "Run_ID", "Include_Flag", "Review_Status")) {
  if (!nm %in% names(meta)) stop("Required metadata field is missing: ", nm)
}
meta$Project_ID <- trimws(as.character(meta$Project_ID))
meta$Run_ID <- norm_run(meta$Run_ID)
meta <- meta[
  meta$Include_Flag == "Include" &
  meta$Review_Status == "Confirmed" &
  !is.na(meta$Run_ID) & nzchar(meta$Run_ID), ,
  drop = FALSE
]
meta <- meta[!duplicated(paste(meta$Project_ID, meta$Run_ID, sep = "::")), ]

clean_summaries <- list()
sample_qc <- list()
exclusions <- list()
all_meta <- list()
all_long <- list()
file_index <- list()

for (i in seq_len(nrow(cfg))) {
  p <- cfg$Project_ID[i]
  cat("\nProcessing ", p, "\n", sep = "")

  if (!file.exists(cfg$Seqtab[i])) stop("Missing ASV sequence table: ", cfg$Seqtab[i])
  if (!file.exists(cfg$Taxonomy[i])) stop("Missing taxonomy table: ", cfg$Taxonomy[i])

  seqtab <- as.matrix(readRDS(cfg$Seqtab[i]))
  tax <- read.csv(cfg$Taxonomy[i], check.names = FALSE, stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8-BOM")
  asv_col <- intersect(c("ASV_Sequence", "Sequence", "ASV"), names(tax))
  if (!length(asv_col)) stop(p, " taxonomy没有ASV_Sequence列")
  tax$ASV_Sequence <- as.character(tax[[asv_col[1]]])

  miss <- setdiff(colnames(seqtab), tax$ASV_Sequence)
  if (length(miss)) stop(p, " 有ASVMissing taxonomy table: ", length(miss))

  tax <- tax[match(colnames(seqtab), tax$ASV_Sequence), , drop = FALSE]
  rownames(tax) <- colnames(seqtab)

  root_tax <- ifelse(!is.na(tx(tax, "Kingdom")), tx(tax, "Kingdom"), tx(tax, "Domain"))
  ranks <- intersect(
    c("Kingdom", "Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
    names(tax)
  )
  text <- apply(tax[, ranks, drop = FALSE], 1, function(z) {
    paste(tolower(as.character(z)), collapse = "|")
  })

  chlor <- grepl("chloroplast", text)
  mito <- grepl("mitochond", text)
  euk <- grepl("eukary", text)
  bac_arc <- grepl("bacteria|archaea", tolower(root_tax))
  keep <- bac_arc & !chlor & !mito & !euk

  reads_asv <- colSums(seqtab)
  total_before <- sum(reads_asv)
  total_after <- sum(reads_asv[keep])

  pdir <- file.path(data_out, p)
  dir.create(pdir, recursive = TRUE, showWarnings = FALSE)

  audit <- data.frame(
    Project_ID = p,
    ASV_Sequence = colnames(seqtab),
    Kingdom_or_Domain = root_tax,
    Phylum = tx(tax, "Phylum"),
    Family = tx(tax, "Family"),
    Genus = tx(tax, "Genus"),
    Total_Reads = as.numeric(reads_asv),
    Chloroplast = chlor,
    Mitochondria = mito,
    Eukaryota = euk,
    Keep_Target = keep,
    Cleaning_Decision = ifelse(
      keep, "Keep_Bacteria_Archaea",
      ifelse(chlor, "Remove_Chloroplast",
      ifelse(mito, "Remove_Mitochondria",
      ifelse(euk, "Remove_Eukaryota",
             "Remove_Non_Bacteria_Archaea_or_Unresolved_Root")))
    )
  )
  wcsv(audit, file.path(pdir, paste0(p, "_taxonomy_cleaning_audit.csv")))

  seq_clean <- seqtab[, keep, drop = FALSE]
  tax_clean <- tax[keep, , drop = FALSE]
  f_all <- apply(tax_clean, 1, feature_all)
  f_class <- apply(tax_clean, 1, feature_classified)

  counts_all <- t(rowsum(t(seq_clean), group = f_all, reorder = FALSE))
  depth <- rowSums(seq_clean)
  rel_all <- rel_by_depth(counts_all, depth)

  class_keep <- !is.na(f_class) & nzchar(f_class)
  if (any(class_keep)) {
    counts_class <- t(rowsum(
      t(seq_clean[, class_keep, drop = FALSE]),
      group = f_class[class_keep], reorder = FALSE
    ))
  } else {
    counts_class <- matrix(
      numeric(0), nrow = nrow(seq_clean), ncol = 0,
      dimnames = list(rownames(seq_clean), character(0))
    )
  }
  rel_class <- rel_by_depth(counts_class, depth)

  saveRDS(seq_clean, file.path(pdir, paste0(p, "_seqtab_target_cleaned.rds")))
  write_mat(counts_all, file.path(pdir, paste0(p, "_genus_all_target_counts.csv")))
  write_mat(rel_all, file.path(pdir, paste0(p, "_genus_all_target_relative.csv")))
  write_mat(counts_class, file.path(pdir, paste0(p, "_genus_classified_counts.csv")))
  write_mat(rel_class, file.path(pdir, paste0(p, "_genus_classified_relative.csv")))

  mp <- meta[meta$Project_ID == p, , drop = FALSE]
  matched <- mp$Run_ID[mp$Run_ID %in% rownames(seq_clean)]
  missing_runs <- setdiff(mp$Run_ID, rownames(seq_clean))
  extra_runs <- setdiff(rownames(seq_clean), mp$Run_ID)

  mf <- mp[match(matched, mp$Run_ID), , drop = FALSE]
  mf$Raw_Final_Depth <- as.numeric(rowSums(seqtab)[matched])
  mf$Cleaned_Target_Depth <- as.numeric(depth[matched])
  mf$Removed_Reads <- mf$Raw_Final_Depth - mf$Cleaned_Target_Depth
  mf$Removed_Read_Proportion <- ifelse(
    mf$Raw_Final_Depth > 0, mf$Removed_Reads / mf$Raw_Final_Depth, NA_real_
  )
  mf$Taxonomy_Cleaning_Version <- "uniform_target_Bacteria_Archaea_v1"
  mf$Genus_Data_Version <- cfg$Version[i]

  nonzero <- mf$Cleaned_Target_Depth > 0
  mf$Freeze_Set <- ifelse(nonzero, "Project_Final_Nonzero", "Excluded_Zero")

  if (p == "PRJEB33360") {
    core <- nonzero &
      mf$Analysis_Role == "Core_case_control" &
      mf$Primary_Sample_Flag == "Yes"
    long_base <- nonzero & mf$Analysis_Role == "Longitudinal"
    long_main <- long_base & mf$Cleaned_Target_Depth >= 1000
    long_sens <- long_base & mf$Cleaned_Target_Depth >= 5000

    mf$Freeze_Set[core] <- "PRJEB33360_Core"
    mf$Freeze_Set[long_main] <- "PRJEB33360_Longitudinal_Main_ge1000"
    mf$Freeze_Set[long_sens] <- paste0(
      mf$Freeze_Set[long_sens],
      ";PRJEB33360_Longitudinal_Sensitivity_ge5000"
    )

    make_ready <- function(sel, filename) {
      mm <- mf[sel, , drop = FALSE]
      rr <- rel_class[mm$Run_ID, , drop = FALSE]
      wcsv(data.frame(mm, rr, check.names = FALSE), file.path(pdir, filename))
    }
    make_ready(core, "PRJEB33360_Core_analysis_ready.csv")
    make_ready(long_main, "PRJEB33360_Longitudinal_Main_ge1000_analysis_ready.csv")
    make_ready(long_sens, "PRJEB33360_Longitudinal_Sensitivity_ge5000_analysis_ready.csv")

    if (any(long_base & !long_main)) {
      z <- mf[long_base & !long_main, ]
      exclusions[[length(exclusions) + 1]] <- data.frame(
        Project_ID = p, Run_ID = z$Run_ID,
        Analysis_Set = "PRJEB33360_Longitudinal_Main_ge1000",
        Exclusion_Reason = "Cleaned target depth below 1000",
        Cleaned_Target_Depth = z$Cleaned_Target_Depth
      )
    }
    if (any(long_base & !long_sens)) {
      z <- mf[long_base & !long_sens, ]
      exclusions[[length(exclusions) + 1]] <- data.frame(
        Project_ID = p, Run_ID = z$Run_ID,
        Analysis_Set = "PRJEB33360_Longitudinal_Sensitivity_ge5000",
        Exclusion_Reason = "Cleaned target depth below 5000",
        Cleaned_Target_Depth = z$Cleaned_Target_Depth
      )
    }
  }

  keep_meta <- mf[nonzero, , drop = FALSE]
  ready_class <- data.frame(
    keep_meta, rel_class[keep_meta$Run_ID, , drop = FALSE], check.names = FALSE
  )
  ready_all <- data.frame(
    keep_meta, rel_all[keep_meta$Run_ID, , drop = FALSE], check.names = FALSE
  )

  wcsv(mf, file.path(pdir, paste0(p, "_metadata_after_taxonomy_cleaning.csv")))
  wcsv(ready_class, file.path(pdir, paste0(p, "_analysis_ready_classified_genus.csv")))
  wcsv(ready_all, file.path(pdir, paste0(p, "_analysis_ready_all_target_features.csv")))

  if (ncol(rel_class) > 0) {
    long <- data.frame(
      Run_ID = rep(rownames(rel_class), each = ncol(rel_class)),
      Genus_Feature = rep(colnames(rel_class), times = nrow(rel_class)),
      Relative_Abundance = as.vector(t(rel_class))
    )
    long <- long[long$Relative_Abundance > 0, , drop = FALSE]
    keys <- intersect(
      c("Project_ID", "Run_ID", "Patient_ID", "Specimen_ID", "Phenotype",
        "Control_Type", "Primary_Sample_Flag", "Timepoint_Raw", "Timepoint_Day",
        "Analysis_Role", "Dataset_Split", "Organ_Dysfunction_Status",
        "Cleaned_Target_Depth", "Freeze_Set"),
      names(mf)
    )
    long <- merge(mf[keys], long, by = "Run_ID", all.y = TRUE, sort = FALSE)
    wcsv(long, file.path(pdir, paste0(p, "_classified_genus_relative_long.csv")))
    all_long[[length(all_long) + 1]] <- long
  }

  clean_summaries[[length(clean_summaries) + 1]] <- data.frame(
    Project_ID = p,
    Data_Version = cfg$Version[i],
    ASVs_Before = ncol(seqtab),
    ASVs_Kept_Target = sum(keep),
    ASVs_Removed = sum(!keep),
    Chloroplast_ASVs = sum(chlor),
    Mitochondria_ASVs = sum(mito),
    Eukaryota_ASVs = sum(euk),
    Other_or_Unresolved_Root_ASVs = sum(!bac_arc & !chlor & !mito & !euk),
    Reads_Before = total_before,
    Reads_Kept_Target = total_after,
    Reads_Removed = total_before - total_after,
    Reads_Removed_Proportion = (total_before - total_after) / total_before,
    Matched_Metadata_Runs = length(matched),
    Missing_Metadata_Runs = length(missing_runs),
    Extra_Abundance_Runs = length(extra_runs),
    Nonzero_After_Cleaning = sum(mf$Cleaned_Target_Depth > 0),
    Zero_After_Cleaning = sum(mf$Cleaned_Target_Depth == 0),
    All_Target_Features = ncol(counts_all),
    Classified_Genus_Features = ncol(counts_class),
    Relative_Sum_Min = min(rowSums(rel_all)),
    Relative_Sum_Max = max(rowSums(rel_all)),
    Status = ifelse(any(mf$Cleaned_Target_Depth == 0), "REVIEW_ZERO", "PASS")
  )

  sample_qc[[length(sample_qc) + 1]] <- mf[
    intersect(
      c("Project_ID", "Run_ID", "Patient_ID", "Phenotype", "Analysis_Role",
        "Primary_Sample_Flag", "Raw_Final_Depth", "Cleaned_Target_Depth",
        "Removed_Reads", "Removed_Read_Proportion", "Freeze_Set"),
      names(mf)
    )
  ]

  all_meta[[length(all_meta) + 1]] <- mf
  file_index[[length(file_index) + 1]] <- data.frame(
    Project_ID = p,
    Output_Directory = pdir
  )
}

clean_summary <- rbind_fill(clean_summaries)
sample_qc_all <- rbind_fill(sample_qc)
meta_all <- rbind_fill(all_meta)
exclusion_all <- rbind_fill(exclusions)
long_all <- rbind_fill(all_long)

wcsv(clean_summary, file.path(report_out, "14_Taxonomy_Cleaning_Summary.csv"))
wcsv(sample_qc_all, file.path(report_out, "14_Sample_Depth_After_Cleaning.csv"))
wcsv(exclusion_all, file.path(report_out, "14_Final_Run_Exclusions.csv"))
wcsv(rbind_fill(file_index), file.path(report_out, "14_Final_Freeze_File_Index.csv"))
wcsv(meta_all, file.path(data_out, "14_All_Final_Metadata.csv"))
wcsv(long_all, file.path(data_out, "14_All_Classified_Genus_Relative_Long.csv"))

# Freeze summary
freeze_rows <- list()
add_summary <- function(p, set_name, x) {
  data.frame(
    Project_ID = p,
    Analysis_Set = set_name,
    Final_N = nrow(x),
    Unique_Patients = if ("Patient_ID" %in% names(x)) {
      length(unique(x$Patient_ID[!is.na(x$Patient_ID) & nzchar(x$Patient_ID)]))
    } else NA_integer_,
    Minimum_Cleaned_Depth = if (nrow(x)) min(x$Cleaned_Target_Depth) else NA,
    Median_Cleaned_Depth = if (nrow(x)) median(x$Cleaned_Target_Depth) else NA,
    Maximum_Cleaned_Depth = if (nrow(x)) max(x$Cleaned_Target_Depth) else NA,
    Phenotype_Summary = if ("Phenotype" %in% names(x)) {
      paste(names(table(x$Phenotype)), as.integer(table(x$Phenotype)),
            sep = "=", collapse = "; ")
    } else ""
  )
}
for (p in cfg$Project_ID) {
  x <- meta_all[meta_all$Project_ID == p, , drop = FALSE]
  if (p == "PRJEB33360") {
    freeze_rows[[length(freeze_rows) + 1]] <- add_summary(
      p, "PRJEB33360_Core",
      x[x$Analysis_Role == "Core_case_control" &
        x$Primary_Sample_Flag == "Yes" &
        x$Cleaned_Target_Depth > 0, ]
    )
    freeze_rows[[length(freeze_rows) + 1]] <- add_summary(
      p, "PRJEB33360_Longitudinal_Main_ge1000",
      x[x$Analysis_Role == "Longitudinal" & x$Cleaned_Target_Depth >= 1000, ]
    )
    freeze_rows[[length(freeze_rows) + 1]] <- add_summary(
      p, "PRJEB33360_Longitudinal_Sensitivity_ge5000",
      x[x$Analysis_Role == "Longitudinal" & x$Cleaned_Target_Depth >= 5000, ]
    )
  } else {
    freeze_rows[[length(freeze_rows) + 1]] <- add_summary(
      p, "Project_Final_Nonzero",
      x[x$Cleaned_Target_Depth > 0, ]
    )
  }
}
freeze_summary <- rbind_fill(freeze_rows)
wcsv(freeze_summary, file.path(report_out, "14_Final_Sample_Freeze_Summary.csv"))

# Corrected leakage: only Development vs Locked_external_test is FAIL
leak_rows <- list()
for (id_col in c("Patient_ID", "Specimen_ID")) {
  if (!all(c(id_col, "Dataset_Split") %in% names(meta_all))) next
  valid <- !is.na(meta_all[[id_col]]) & nzchar(meta_all[[id_col]]) &
    !is.na(meta_all$Dataset_Split) & nzchar(meta_all$Dataset_Split)
  for (id in unique(meta_all[[id_col]][valid])) {
    z <- meta_all[valid & meta_all[[id_col]] == id, , drop = FALSE]
    splits <- unique(z$Dataset_Split)
    true_fail <- all(c("Development", "Locked_external_test") %in% splits)
    if (true_fail || "Not_for_model" %in% splits) {
      leak_rows[[length(leak_rows) + 1]] <- data.frame(
        Check_Name = paste0(id_col, "_Model_Leakage"),
        Project_ID = paste(unique(z$Project_ID), collapse = " | "),
        Entity_ID = id,
        Dataset_Splits = paste(splits, collapse = " | "),
        Run_IDs = paste(z$Run_ID, collapse = " | "),
        Status = ifelse(true_fail, "FAIL", "PASS_NOT_FOR_MODEL"),
        Details = ifelse(
          true_fail,
          "Entity appears in both Development and Locked_external_test.",
          "Not_for_model rows do not create model leakage."
        )
      )
    }
  }
}
leakage <- rbind_fill(leak_rows)
if (!nrow(leakage)) {
  leakage <- data.frame(
    Check_Name = "Model_Leakage", Project_ID = "ALL", Entity_ID = "",
    Dataset_Splits = "", Run_IDs = "", Status = "PASS",
    Details = "No patient/specimen crosses Development and Locked_external_test."
  )
}
wcsv(leakage, file.path(report_out, "14_Corrected_Leakage_QC.csv"))

# Final audit
audit_rows <- lapply(cfg$Project_ID, function(p) {
  c1 <- clean_summary[clean_summary$Project_ID == p, ]
  leak_fail <- sum(leakage$Status == "FAIL" & grepl(p, leakage$Project_ID, fixed = TRUE))
  review <- character()
  fail <- character()
  if (!nrow(c1) || c1$Status[1] != "PASS") fail <- c(fail, "Taxonomy cleaning did not pass")
  if (leak_fail > 0) fail <- c(fail, "True model leakage")
  if (p == "PRJEB33360" && any(
    sample_qc_all$Project_ID == p &
    sample_qc_all$Analysis_Role == "Longitudinal" &
    sample_qc_all$Cleaned_Target_Depth < 1000, na.rm = TRUE
  )) review <- c(review, "Prespecified low-depth longitudinal exclusions")
  if (c1$Reads_Removed_Proportion[1] > 0.005) {
    review <- c(review, "More than 0.5% reads removed by taxonomy cleaning")
  }
  data.frame(
    Project_ID = p,
    Cleaning_Status = c1$Status[1],
    Reads_Removed_Proportion = c1$Reads_Removed_Proportion[1],
    Nonzero_After_Cleaning = c1$Nonzero_After_Cleaning[1],
    Zero_After_Cleaning = c1$Zero_After_Cleaning[1],
    True_Leakage_Failures = leak_fail,
    Final_Audit_Status = if (length(fail)) "FAIL" else if (length(review)) {
      "PASS_WITH_DOCUMENTED_REVIEW"
    } else "PASS",
    Fail_Reasons = paste(fail, collapse = " | "),
    Review_Reasons = paste(review, collapse = " | "),
    Reviewer = "", Review_Date = "", Final_Decision = "", Decision_Notes = ""
  )
})
wcsv(rbind_fill(audit_rows), file.path(report_out, "14_Final_Freeze_Audit.csv"))

capture.output(sessionInfo(), file = file.path(report_out, "14_Final_Freeze_SessionInfo.txt"))

hash_files <- unique(c(
  meta_file, cfg$Seqtab, cfg$Taxonomy,
  list.files(data_out, recursive = TRUE, full.names = TRUE),
  list.files(report_out, full.names = TRUE)
))
writeLines(hash_files, file.path(report_out, "14_Files_To_Hash.txt"), useBytes = TRUE)

cat("\nCompleted.\nFinal data: ", data_out, "\nReports: ", report_out, "\n", sep = "")
