# Public path configuration

This file explains which paths a public user should edit and which paths should remain repository-relative.

## 1. Manuscript-result reproduction

Recommended command sequence:

```r
setwd("D:/Sepsis_V1_public")
source("environment/install_core_packages.R")
source("reproduce_manuscript_results.R")
```

Only the `setwd(...)` example is a personal path. Replace it with the location where you extracted the repository.

Do not edit the internal relative paths such as:

- `data/analysis_ready_checkpoint/`
- `data/metadata/`
- `analysis/`
- `expected_results/`
- `validation/`
- `work/manuscript_reproduction/`

These paths move automatically with the repository.

## 2. Raw FASTQ preprocessing

Raw FASTQ is external and requires a user-supplied location:

```bash
Rscript run_raw_preprocessing.R PRJEB33360 --raw-root="F:/Sepsis_V1_raw_fastq" --output-root="F:/Sepsis_V1_preprocessed"
```

- `--raw-root`: required external FASTQ location.
- `--output-root`: optional external output location; defaults to `work/raw_preprocessing/`.

Both paths are user-specific examples.

## 3. Fresh taxonomy method reproduction

The SILVA 138.2 training file is external because of its size. Configure it using:

```r
Sys.setenv(SEPSIS_V1_SILVA_REF = "F:/references/SILVA_138_2/silva_nr99_v138.2_toGenus_trainset.fa.gz")
```

If taxonomy is being run on ASV outputs created by the public raw runner, also set:

```r
Sys.setenv(SEPSIS_V1_ASV_ROOT = "F:/Sepsis_V1_preprocessed")
Sys.setenv(SEPSIS_V1_TAXONOMY_OUTPUT_ROOT = "F:/Sepsis_V1_fresh_taxonomy")
```

## 4. Historical checkpoint reconstruction

The compact public repository does not include the larger historical final-ASV and taxonomy checkpoint files. If these are deposited separately, set:

```r
Sys.setenv(SEPSIS_V1_CHECKPOINT_ROOT = "F:/Sepsis_V1_checkpoints")
Sys.setenv(SEPSIS_V1_ANALYSIS_READY_OUTPUT_ROOT = "F:/Sepsis_V1_analysis_ready_rebuilt")
```

The checkpoint root must contain `asv/` and `taxonomy/` subfolders.

## 5. Paths that must not be copied

Do not copy the original authors' machine-specific paths, for example:

```text
E:/sepsis_project
C:/Users/<author>/Desktop/...
```

They are not required by the public pipeline.
