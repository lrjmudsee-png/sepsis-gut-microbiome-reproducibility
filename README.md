# Reproducible multicohort analysis of the gut microbiome in sepsis

Analysis code, compact processed checkpoints, reviewed results, and figure sources for a secondary analysis of seven public human gut 16S rRNA sequencing projects.

**Current release:** `v1.1.0`

This repository accompanies the manuscript *Context-dependent gut microbiota alterations in sepsis and failure of a locked genus-level dysbiosis index in external validation*.

## Overview

The analysis combines multicohort discovery, harmonized meta-analysis, external evaluation of a fixed six-genus sepsis dysbiosis index (SDI), and longitudinal/supportive analyses. The SDI showed apparent development discrimination (AUC 0.731) but failed to transfer in the external BioProject PRJNA1010969 (AUC 0.272 for sepsis versus trauma; AUC 0.201 for sepsis versus healthy controls). These two external comparisons share the same sepsis participants and are not independent external cohorts.

Version `v1.1.0` is the corrected publication release. It supersedes `v1.0.0` for reproduction of the current manuscript while retaining the original release as a historical baseline.

## Correction in v1.1.0

The longitudinal Benjamini–Hochberg adjustment now excludes intercept terms from inferential families while retaining the original grouping across modeled metrics. The corrected Shannon-diversity adjusted P values are:

- T01 overall time: q = 0.012633
- T02 depth sensitivity: q = 0.021361
- T03 overall time: q = 0.317783
- T06 time-by-cholestasis interaction: q = 0.266299

T06 is therefore exploratory and not FDR-significant. Raw P values, model estimates, confidence intervals, sample selection, SDI feature selection, score orientation, threshold, and external-validation AUCs are unchanged.

See [correction provenance and interpretation](docs/SUBMISSION_REVISION.md) and [v1.1.0 release notes](docs/RELEASE_NOTES_v1.1.0.md) for details.

## Reproduce the manuscript results

The recommended route starts from bundled processed checkpoints and does not require raw FASTQ downloads.

From the repository root:

```r
source("environment/install_core_packages.R")
source("reproduce_manuscript_results.R")
```

or:

```sh
Rscript reproduce_manuscript_results.R
```

Core packages are `vegan`, `metafor`, `nlme`, and `openxlsx`. R 4.4.0 was used for the recorded verification. The runner executes all seven analysis modules and validates generated scientific outputs against the reviewed expectations under `expected_results/`.

A successful run prints:

```text
All_Scientific_Checks_PASS = TRUE
```

## Independent verification of the FDR correction

Python 3 with the standard library only:

```sh
python validation/validate_submission_revision.py
```

The validator independently recalculates eligible BH-adjusted values, verifies multiplicity families, and checks that frozen non-FDR scientific fields match the historical baseline.

After a full R run, freshly generated frozen results can be checked with:

```sh
python validation/validate_submission_revision.py --results-root work/manuscript_reproduction/manuscript/04_final_freeze/21_results_freeze_v1/06_frozen_result_inputs
```

## Reproduce publication figures

Install `ggplot2` and `patchwork`, then run:

```sh
Rscript figure_audit_20260902/09_audit_main_figure_sources_PUBLIC.R .
Rscript figure_audit_20260902/08_make_main_figures_PUBLIC.R .
Rscript figure_audit_20260902/10_normalize_publication_labels.R .
Rscript figure_audit_20260902/11_make_supplementary_figures_PUBLIC.R .
```

These scripts use reviewed sources under `expected_results/` and do not refit statistical models.

## Data availability

The seven source BioProjects are:

- PRJEB33360
- PRJNA691455
- PRJNA978257
- PRJNA1010969
- PRJNA430161
- PRJNA797231
- PRJNA912621

Raw reads remain in their public repositories and are not redistributed here. Compact processed checkpoints are provided under `data/analysis_ready_checkpoint/` for manuscript-level reproduction. See [data availability](docs/DATA_AVAILABILITY.md).

## Repository structure

| Location | Contents |
| --- | --- |
| `analysis/` | Seven manuscript-analysis modules |
| `data/analysis_ready_checkpoint/` | Compact processed count/metadata checkpoints |
| `expected_results/` | Reviewed v1.1.0 result expectations |
| `submission_metadata/` | Public identifier linkage for checkpoint records |
| `submission_outputs/` | Current figure-source data and claim records |
| `figures_reproduced_public/` | Main figures |
| `supplementary_figures_reproduced_public/` | Supplementary figures and source audits |
| `publication_ready_tables/` | Publication-facing tables |
| `validation/` | Independent correction validation and historical baseline expectations |
| `submission_revision_audit/` | Correction provenance and recorded verification evidence |
| `preprocessing/` | Optional upstream reconstruction scripts |
| `environment/` | Package and session information |
| `docs/` | Workflow, provenance, data availability, and reproducibility boundaries |

The metadata crosswalk does not assert complete MIxS compliance and does not invent unavailable clinical or technical fields.

## Optional upstream reconstruction

See [path configuration](docs/PATH_CONFIGURATION.md), [DADA2 reproducibility](docs/DADA2_REPRODUCIBILITY.md), and [reproducibility scope](docs/REPRODUCIBILITY_SCOPE.md) before running `run_raw_preprocessing.R`. The bundled checkpoint route is the tested manuscript-result reproduction route; universal byte-identical reconstruction from historical raw-read processing states is not claimed.

## Version history

- `v1.1.0`: corrected publication release corresponding to the current manuscript.
- `v1.0.0`: historical baseline, archived at Zenodo DOI [10.5281/zenodo.22330040](https://doi.org/10.5281/zenodo.22330040).

The `v1.0.0` archive predates the corrected longitudinal multiple-testing adjustment and should not be used to reproduce the final manuscript results.

## Citation and license

See [CITATION.md](CITATION.md). The repository is distributed under the [MIT License](LICENSE).
