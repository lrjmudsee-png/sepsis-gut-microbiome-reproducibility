# Reproducible multicohort analysis of the gut microbiome in sepsis

This repository contains the analysis code, compact processed inputs, frozen scientific outputs, validation scripts, and figure-reproduction utilities associated with a multicohort reanalysis of seven public human gut 16S rRNA gene sequencing projects in sepsis.

The repository is designed around two reproducibility levels:

1. **Exact manuscript-result reproduction** from the validated compact processed checkpoint bundled in this repository.
2. **Optional upstream reconstruction** from public raw sequencing data for method transparency.

The exact manuscript-result route is the recommended route for readers and reviewers.

---

## 1. Public datasets

The study uses seven publicly available BioProjects:

- PRJEB33360
- PRJNA691455
- PRJNA978257
- PRJNA1010969
- PRJNA430161
- PRJNA797231
- PRJNA912621

Raw FASTQ files are not redistributed in this repository. They remain available from their original public repositories.

See:

```text
docs/DATA_AVAILABILITY.md
```

---

## 2. Repository structure

```text
.
├── README.md
├── reproduce_manuscript_results.R
├── run_raw_preprocessing.R
│
├── analysis/
│   ├── 01_prepare_analysis_sets.R
│   ├── 02_discovery_statistics.R
│   ├── 03_harmonized_meta_analysis.R
│   ├── 04_sdi_external_validation.R
│   ├── 05_sdi_failure_diagnosis.R
│   ├── 06_longitudinal_support.R
│   └── 07_generate_final_outputs.R
│
├── preprocessing/
├── data/
│   ├── analysis_ready_checkpoint/
│   └── metadata/
│
├── expected_results/
│   ├── frozen_result_inputs/
│   └── scientific_outputs/
│
├── validation/
├── environment/
├── docs/
│
├── figure_audit_20260902/
│   ├── 08_make_main_figures_PUBLIC.R
│   ├── 09_audit_main_figure_sources_PUBLIC.R
│   ├── 10_normalize_publication_labels.R
│   ├── 11_make_supplementary_figures_PUBLIC.R
│   ├── V1_Figure1_Screening_Flow_FINAL_20260901.csv
│   ├── README.md
│   └── README_SUPPLEMENTARY_FIGURES.md
│
├── figures_reproduced_public/
├── figure_source_audit_public/
├── publication_ready_tables/
├── PACKAGE_MANIFEST_SHA256.csv
└── PUBLIC_FINAL_INVENTORY.txt
```

---

## 3. Recommended route: reproduce the manuscript results

### Step 1 — obtain the complete repository

Clone the repository with Git, or download the repository archive from GitHub.

Store it anywhere on your computer.

Example only:

```text
D:/Sepsis_V1_public/
```

### Step 2 — open R or RStudio in the repository root

Example:

```r
setwd("D:/Sepsis_V1_public")
```

Replace the example path with your own repository location.

### Step 3 — install the required R packages

```r
source("environment/install_core_packages.R")
```

The verified analysis environment used R 4.4.0.

See:

```text
environment/verified_package_versions.csv
environment/verified_sessionInfo.txt
```

### Step 4 — run the complete manuscript-analysis pipeline

From R:

```r
source("reproduce_manuscript_results.R")
```

or from a terminal opened in the repository root:

```bash
Rscript reproduce_manuscript_results.R
```

The master runner executes:

1. `analysis/01_prepare_analysis_sets.R`
2. `analysis/02_discovery_statistics.R`
3. `analysis/03_harmonized_meta_analysis.R`
4. `analysis/04_sdi_external_validation.R`
5. `analysis/05_sdi_failure_diagnosis.R`
6. `analysis/06_longitudinal_support.R`
7. `analysis/07_generate_final_outputs.R`
8. `validation/validate_reproduction.R`

Regenerated outputs are written under:

```text
work/manuscript_reproduction/
```

The validation summary is written to:

```text
work/manuscript_reproduction/validation/reproduction_summary.csv
```

A successful run should report:

```text
All_Scientific_Checks_PASS = TRUE
```

If `work/manuscript_reproduction/` already exists, delete it before rerunning or follow the overwrite instructions printed by the runner.

---

## 4. Path configuration

The manuscript-result route uses repository-relative paths and should not require author-specific absolute paths.

The following locations should normally not be edited:

```text
data/analysis_ready_checkpoint/
data/metadata/
analysis/
expected_results/
validation/
work/manuscript_reproduction/
```

For most users, the only personal path required is the location of the repository itself.

See:

```text
docs/PATH_CONFIGURATION.md
```

---

## 5. Main Figure reproduction

Main Figures 1–4 are reproduced from frozen public figure-source files.

First audit the numerical sources:

```bash
Rscript figure_audit_20260902/09_audit_main_figure_sources_PUBLIC.R .
```

Then regenerate the main figures:

```bash
Rscript figure_audit_20260902/08_make_main_figures_PUBLIC.R .
```

These scripts do not refit the statistical models used in the manuscript. They reproduce the publication figures from frozen scientific outputs.

See:

```text
figure_audit_20260902/README.md
```

---

## 6. Supplementary Figure reproduction

Supplementary Figures S1–S3 are reproduced from frozen public inputs with:

```bash
Rscript figure_audit_20260902/11_make_supplementary_figures_PUBLIC.R .
```

The script performs strict source checks before completing and does not refit any statistical model.

Default output directory:

```text
supplementary_figures_reproduced_public/
```

See:

```text
figure_audit_20260902/README_SUPPLEMENTARY_FIGURES.md
```

---

## 7. Publication-facing table labels

The verified analysis chain retains a historical internal machine identifier for the S06/PRJNA912621 analysis.

For publication-facing output, the clinically specific **cholestasis/SIC** terminology is generated with:

```bash
Rscript figure_audit_20260902/10_normalize_publication_labels.R .
```

This normalization changes display labels only. It does not alter statistical models, frozen scientific values, or expected results.

---

## 8. External validation and post-hoc failure diagnosis

The SDI workflow preserves the locked-validation sequence used in the manuscript.

Candidate genera, score definition, model, and threshold are fixed using development data before the locked external comparisons are evaluated.

The subsequent failure-diagnosis analyses are explicitly post hoc and do not replace or relabel the failed locked external validation.

---

## 9. Optional upstream route: raw FASTQ preprocessing

The manuscript-result route above is the recommended exact-reproduction route.

For method transparency, the repository also includes optional upstream preprocessing code.

`run_raw_preprocessing.R` performs project-specific:

```text
raw FASTQ -> reconstructed ASV table
```

Example:

```bash
Rscript run_raw_preprocessing.R PRJEB33360 --raw-root="F:/Sepsis_V1_raw_fastq" --output-root="F:/Sepsis_V1_preprocessed"
```

The paths above are examples only.

Important reproducibility boundary:

- Raw FASTQ files are not redistributed.
- Historical DADA2 error-learning random states were not retained for every cohort.
- Therefore, fresh raw-data runs cannot be guaranteed to produce byte-identical ASV tables for all seven projects.
- Exact manuscript-result reproduction uses the validated downstream checkpoint bundled in this repository.

See:

```text
docs/DADA2_REPRODUCIBILITY.md
docs/REPRODUCIBILITY_SCOPE.md
```

---

## 10. Optional taxonomy reconstruction

Fresh SILVA 138.2 taxonomy reconstruction is provided for method transparency:

```text
preprocessing/03_assign_silva1382_taxonomy.R
```

Freshly reconstructed taxonomy is not a substitute for the historical taxonomy checkpoints used for exact manuscript-result reproduction.

See the preprocessing documentation for details.

---

## 11. Reproducibility boundary

This repository distinguishes between:

### Exact manuscript-result reproduction

Uses the compact validated checkpoint bundled in the repository and is expected to reproduce the manuscript-level scientific outputs and validation checks.

### Upstream methodological reconstruction

Uses public raw FASTQ files and optional taxonomy reconstruction scripts. This route reproduces the documented processing strategy but is not guaranteed to regenerate every historical intermediate file byte-for-byte.

This distinction is intentional and documented.

---

## 12. Data availability

All sequencing data analyzed in this study were obtained from publicly accessible repositories under the following accession numbers:

```text
PRJEB33360
PRJNA691455
PRJNA978257
PRJNA1010969
PRJNA430161
PRJNA797231
PRJNA912621
```

Processed analysis-ready metadata, frozen result inputs, scientific output tables, and figure-source data required for manuscript-result reproduction are included in this repository.

Raw FASTQ files are not redistributed.

---

## 13. Code availability

All analysis, validation, main-figure reproduction, supplementary-figure reproduction, and publication-label normalization code associated with the manuscript is publicly available in this repository.

A version-specific archival DOI will be added after the corresponding GitHub release is deposited in Zenodo.

---

## 14. Integrity and validation files

The repository includes package-level integrity and release-QC materials, including:

```text
PACKAGE_MANIFEST_SHA256.csv
PUBLIC_FINAL_INVENTORY.txt
validation/
```

These files are intended to make the public release auditable and to distinguish frozen scientific outputs from regenerated working files.

---

## 15. License

Code in this repository is released under the MIT License.

Derived inputs originate from publicly available sequencing studies and are provided solely to support scientific reproducibility.

See:

```text
LICENSE
```

---

## 16. Citation

Please cite the associated manuscript when available.

For software/repository citation, use the archived Zenodo record once the version-specific DOI has been created.

See:

```text
CITATION.md
```

---

## 17. Contact

Questions regarding the scientific analysis or reproducibility package should be directed to the corresponding author listed in the associated manuscript.
