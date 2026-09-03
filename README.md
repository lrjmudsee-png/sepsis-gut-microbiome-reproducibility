# Reproducible multicohort analysis of the gut microbiome in sepsis

This repository contains the code and compact derived inputs required to reproduce the manuscript-level analyses, tables, figure-source data, and final result freeze for a selected multicohort reanalysis of seven public 16S rRNA gene sequencing projects.

## Recommended route: reproduce the manuscript results

The manuscript-result route starts from the validated compact processed checkpoint already bundled in this repository. It does **not** require the user to supply an external processed-data path.

### Step 1 — extract the complete repository

The repository can be stored anywhere on your computer. For example:

```text
D:/Sepsis_V1_public/
```

The path above is only an example. Use your own location.

### Step 2 — set the R working directory to the repository root

In R/RStudio:

```r
setwd("D:/Sepsis_V1_public")  # EXAMPLE ONLY — change to your own repository path
```

### Step 3 — install the analysis packages once on a new computer

```r
source("environment/install_core_packages.R")
```

The verified analysis environment used R 4.4.0. See `environment/verified_package_versions.csv` and `environment/verified_sessionInfo.txt`.

### Step 4 — run the one-command manuscript reproduction

```r
source("reproduce_manuscript_results.R")
```

or from a terminal opened in the repository root:

```bash
Rscript reproduce_manuscript_results.R
```

The master runner automatically executes:

1. `analysis/01_prepare_analysis_sets.R`
2. `analysis/02_discovery_statistics.R`
3. `analysis/03_harmonized_meta_analysis.R`
4. `analysis/04_sdi_external_validation.R`
5. `analysis/05_sdi_failure_diagnosis.R`
6. `analysis/06_longitudinal_support.R`
7. `analysis/07_generate_final_outputs.R`
8. `validation/validate_reproduction.R`

Regenerated outputs are written under:

`work/manuscript_reproduction/`

The final validation summary is:

`work/manuscript_reproduction/validation/reproduction_summary.csv`

A successful run should report:

`All_Scientific_Checks_PASS = TRUE`

If `work/manuscript_reproduction/` already exists, either delete it before rerunning or set `SEPSIS_V1_OVERWRITE=true` as described in the runner message.

## Path rules for public users

### Manuscript-result route

The following are **repository-relative paths** and should normally not be edited:

- `data/analysis_ready_checkpoint/`
- `data/metadata/`
- `analysis/`
- `expected_results/`
- `validation/`
- `work/manuscript_reproduction/`

The compact processed data required for manuscript reproduction are already included under `data/analysis_ready_checkpoint/`.

Do **not** replace these relative paths with an author's historical local path such as `E:/sepsis_project` or `C:/Users/<name>/...`.

For most users, the only personal path required for manuscript reproduction is the repository location supplied to `setwd(...)`.

See `docs/PATH_CONFIGURATION.md` for a complete path guide.

## Optional upstream route: raw FASTQ to reconstructed ASV

The optional raw DADA2 route requires Bioconductor package `dada2` and CRAN package `digest`. The optional checkpoint-reconstruction route additionally requires `readxl`. These upstream dependencies are separate from the core manuscript-analysis installer.

Raw FASTQ files are not redistributed in this repository. The optional upstream DADA2 route is provided for method transparency and is separate from the recommended exact manuscript-result route.

### What `run_raw_preprocessing.R` does

`run_raw_preprocessing.R` performs the project-specific **raw FASTQ → final reconstructed ASV table** step.

It does **not** automatically perform fresh SILVA taxonomy assignment or rebuild the exact historical analysis-ready checkpoint. Those are separate optional scripts described below.

### Raw FASTQ folder structure

A convenient layout is:

```text
F:/Sepsis_V1_raw_fastq/
├─ PRJEB33360/
├─ PRJNA691455/
├─ PRJNA978257/
├─ PRJNA1010969/
├─ PRJNA430161/
├─ PRJNA797231/
└─ PRJNA912621/
```

The drive letter and folder name are examples only.

### Run one BioProject

Windows example:

```bash
Rscript run_raw_preprocessing.R PRJEB33360 --raw-root="F:/Sepsis_V1_raw_fastq" --output-root="F:/Sepsis_V1_preprocessed"
```

`--raw-root` is required and must point either to the common parent containing BioProject subfolders or directly to the selected project's FASTQ folder.

`--output-root` is optional. If omitted, outputs are written under `work/raw_preprocessing/` inside the repository.

The project output is written under:

```text
<output-root>/<BioProject>/
```

### Upstream reproducibility boundary

Raw-data preprocessing is provided for method transparency. The historical DADA2 error-learning random state was not retained for every cohort, so fresh raw-data runs cannot be guaranteed to produce byte-identical ASV tables for all seven projects. Exact manuscript reproduction therefore uses the validated downstream checkpoint bundled in this repository.

See `docs/DADA2_REPRODUCIBILITY.md` and `docs/REPRODUCIBILITY_SCOPE.md`.

## Optional fresh SILVA 138.2 taxonomy reconstruction

`preprocessing/03_assign_silva1382_taxonomy.R` reproduces the stated taxonomy **method** from final ASV tables. Fresh taxonomy output is not a substitute for the historical taxonomy checkpoints used for exact manuscript reproduction.

If the reconstructed ASV tables were written to `F:/Sepsis_V1_preprocessed`, an R example is:

```r
Sys.setenv(
  SEPSIS_V1_PROJECT_ROOT = getwd(),
  SEPSIS_V1_ASV_ROOT = "F:/Sepsis_V1_preprocessed",
  SEPSIS_V1_SILVA_REF = "F:/references/SILVA_138_2/silva_nr99_v138.2_toGenus_trainset.fa.gz",
  SEPSIS_V1_TAXONOMY_OUTPUT_ROOT = "F:/Sepsis_V1_fresh_taxonomy"
)
source("preprocessing/03_assign_silva1382_taxonomy.R")
```

All `F:/...` paths above are examples and must be replaced with paths on the user's own computer.

## Optional rebuild from separately deposited historical checkpoints

`preprocessing/04_build_analysis_ready_from_checkpoints.R` rebuilds analysis-ready data from the larger final-ASV RDS and verified historical taxonomy CSV checkpoints. These larger checkpoints are **not bundled in this compact public package**.

If they are deposited separately, use a structure such as:

```text
F:/Sepsis_V1_checkpoints/
├─ asv/
│  ├─ PRJEB33360_seqtab_final.rds
│  └─ ...
└─ taxonomy/
   ├─ PRJEB33360_taxonomy_silva1382.csv
   └─ ...
```

Then:

```r
Sys.setenv(
  SEPSIS_V1_PROJECT_ROOT = getwd(),
  SEPSIS_V1_CHECKPOINT_ROOT = "F:/Sepsis_V1_checkpoints",
  SEPSIS_V1_ANALYSIS_READY_OUTPUT_ROOT = "F:/Sepsis_V1_analysis_ready_rebuilt"
)
source("preprocessing/04_build_analysis_ready_from_checkpoints.R")
```

Again, the paths above are examples only.

## Public datasets

The analysis uses seven public BioProjects:

- PRJEB33360
- PRJNA691455
- PRJNA978257
- PRJNA1010969
- PRJNA430161
- PRJNA797231
- PRJNA912621

Raw FASTQ files are not redistributed. See `docs/DATA_AVAILABILITY.md`.

## External validation

The SDI workflow preserves the original locked-validation sequence. Candidate genera, score definition, model, and threshold are fixed using the development data before A04/A05 are evaluated. The subsequent failure-diagnosis module is explicitly post-hoc and does not replace or relabel the failed locked external validation.

## Expected outputs and validation

The repository includes compact expected scientific outputs in `expected_results/`. The validator compares regenerated frozen inputs and scientific CSV outputs against these references. Run-time timestamps and PDF binary metadata are not used as scientific equality criteria.

## Final figure production and figure audit (v1.0.3)

The `figure_audit_20260902/` folder contains the final figure-production and figure-audit code and documentation for the 2026-09-02 figure corrections: the audited v1.2 redraw base, the v1.4 finalizer (Figure 1 dual-panel screening flow + Figure 4C cholestasis/SIC group-N labels), the figure-source consistency audit, the Figure 1 screening-flow CSV, and the official correction log / master workbook / corrected verification table. The figure-source consistency audit reports 29 PASS / 0 FAIL. See `figure_audit_20260902/README_V1_Figure_Audit_Corrections_20260902.md` for the evidence-adjudication order and run instructions.

## Public-release portability

The active public scripts are designed to avoid dependence on author-specific absolute paths. `validation/PUBLIC_RELEASE_STATIC_SCAN.csv` records the release scan.

Version 1.0.1 changes only public path configuration, documentation, and optional upstream path plumbing; the manuscript-analysis scripts `analysis/01` through `analysis/07` and the expected scientific results are unchanged from v1.0.0.

## License

Code is released under the MIT License. Derived inputs in this repository originate from publicly available sequencing studies and are provided solely to support reproducibility.

## Citation

Please cite the associated manuscript and the archived repository record. See `CITATION.md`.

## Public figure reproduction and publication-facing labels

The recommended manuscript-analysis route remains:

```bash
Rscript reproduce_manuscript_results.R
```

After validating the manuscript results, the public Figure layer can be checked and regenerated with:

```bash
Rscript figure_audit_20260902/09_audit_main_figure_sources_PUBLIC.R .
Rscript figure_audit_20260902/08_make_main_figures_PUBLIC.R .
```

Publication-facing table labels can be generated with:

```bash
Rscript figure_audit_20260902/10_normalize_publication_labels.R .
```

The Figure and label-normalization utilities read package-relative public files only; no author-specific working directory is required.

### Important terminology note

The verified analysis chain retains the historical internal machine identifier for the S06/PRJNA912621 analysis. Publication-facing output uses the clinically specific label **cholestasis/SIC**. This display normalization does not alter any numerical result or model.

### Public package scope

Internal manuscript drafts, figure-audit workbooks, correction logs, author-computer redraw scripts, and obsolete Figure/QC script versions are intentionally excluded from this public package.

### Supplementary Figure reproduction

Supplementary Figures S1–S3 are reproduced from frozen public inputs with:

```bash
Rscript figure_audit_20260902/11_make_supplementary_figures_PUBLIC.R .
```

The script performs strict source checks before completing and does not refit any statistical model.
