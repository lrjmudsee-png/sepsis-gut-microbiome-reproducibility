# Reproducible multicohort analysis of the gut microbiome in sepsis

Analysis code, compact processed checkpoints, results and figure sources for a
secondary analysis of seven public human gut 16S rRNA sequencing projects.

**Current version:** `1.0.0-submission-revision-20260906`.
The fixed six-genus score failed external evaluation. This repository documents
that result and the limits of its interpretation.

The published [v1.0.0 release](https://github.com/lrjmudsee-png/sepsis-gut-microbiome-reproducibility/releases/tag/v1.0.0)
and [Zenodo archive](https://doi.org/10.5281/zenodo.22330040) preserve the original
baseline. They do not contain the current correction. Cite the exact commit
when using this revision; no separate revision DOI has been assigned.

## What changed

The longitudinal BH adjustment now excludes intercepts while retaining the
original families across metrics. The T06 Shannon time-by-cholestasis
interaction has q = 0.266299 and is exploratory, not FDR-significant.
Raw P values, model estimates, intervals, sample selection and the SDI are
unchanged. Current tables, figures and claim records reflect the correction.

[Revision details](docs/SUBMISSION_REVISION.md) explain the correction,
development versus external performance, overlapping external comparisons,
retrospective screening, and limits of raw-data reconstruction.
The original README is preserved as a [baseline record](docs/README_v1.0.0_baseline.md).

## Reproduce the manuscript results

Download or clone the complete repository. The recommended route starts from
bundled processed checkpoints; it does not require raw FASTQ downloads.

In R/RStudio, open the repository root as your working directory and run:

```r
source("environment/install_core_packages.R")
source("reproduce_manuscript_results.R")
```

Or, after package installation, run from the repository root:

```sh
Rscript reproduce_manuscript_results.R
```

Core packages are `vegan`, `metafor`, `nlme` and `openxlsx`.
R 4.4.0 was used for the recorded verification. The runner selects a UTF-8
character locale on Windows when needed; other platforms should use a valid
UTF-8 locale. Baseline package versions are under `environment/`, and the
current tested session is in
`submission_revision_audit/github_validation_20260906/sessionInfo.txt`.

The runner executes all seven analysis modules and checks the generated
scientific outputs against reviewed expectations. Outputs and the 63-check
report are written under `work/manuscript_reproduction/`.
A successful run prints `All_Scientific_Checks_PASS = TRUE`; failed scientific
validation terminates with an error.

An existing work directory is protected from accidental reuse. Preserve or move
it before rerunning. `SEPSIS_V1_OVERWRITE=true` explicitly removes the previous
reproduction work directory; use it only when those outputs are no longer needed.

## Independently verify the correction

Python 3, using only its standard library:

```sh
python validation/validate_submission_revision.py --check-manifest
```

This independently recalculates every eligible BH value, checks multiplicity
families and verifies that frozen non-FDR scientific fields match the baseline.
The optional manifest check verifies the distributed file bytes; run it before
regenerating figure/report files, which can contain new timestamps.

After the full R run, check the newly computed results:

```sh
python validation/validate_submission_revision.py --results-root work/manuscript_reproduction/manuscript/04_final_freeze/21_results_freeze_v1/06_frozen_result_inputs
```

Neither command updates expected values. The expected-results comparison and
independent BH calculation provide different checks.

## Reproduce publication figures and display tables

Install `ggplot2` and `patchwork`, then run from the repository root:

```sh
Rscript figure_audit_20260902/09_audit_main_figure_sources_PUBLIC.R .
Rscript figure_audit_20260902/08_make_main_figures_PUBLIC.R .
Rscript figure_audit_20260902/10_normalize_publication_labels.R .
Rscript figure_audit_20260902/11_make_supplementary_figures_PUBLIC.R .
```

These utilities read the reviewed `expected_results/` sources. They do not
refit statistical models or automatically adopt unreviewed work-directory
outputs. Main figures, supplementary figures and publication-label copies
are written to their corresponding repository folders.

The recorded checks cover 24 main-figure source assertions and 17
supplementary-figure source assertions. PDF binary hashes are not a substitute
for numerical checks; regenerated PDFs can differ because of timestamps.

## Data and repository map

The seven BioProjects are PRJEB33360, PRJNA691455, PRJNA978257, PRJNA1010969,
PRJNA430161, PRJNA797231 and PRJNA912621. Raw reads remain in their public
source repositories. See [data availability](docs/DATA_AVAILABILITY.md).

| Location | Contents |
| --- | --- |
| `analysis/` | Seven manuscript-analysis modules |
| `data/analysis_ready_checkpoint/` | Bundled count/metadata checkpoints |
| `expected_results/` | Reviewed current result expectations |
| `submission_metadata/` | 471 checkpoint records with public identifier linkage; not 471 unique participants |
| `submission_outputs/` | Current figure sources and claim records |
| `figure_audit_20260902/` | Figure production, numerical audits and display-label normalization |
| `figures_reproduced_public/` | Main figures |
| `supplementary_figures_reproduced_public/` | Supplementary figures and source audit |
| `publication_ready_tables/` | Tables with publication-facing labels |
| `submission_revision_audit/` | Correction evidence and current verification reports |
| `validation/baseline_expected_results/` | Original v1.0.0 expectations |
| `validation/baseline_release_audit/` | Historical baseline packaging checks |
| `preprocessing/` | Optional upstream reconstruction scripts |
| `environment/` | Package installation and baseline environment records |
| `docs/` | Workflow, provenance, paths and reproducibility boundaries |

The metadata crosswalk does not assert complete MIxS compliance and does not
invent clinical or technical fields unavailable from the source checkpoints.

## Optional upstream reconstruction

See [path configuration](docs/PATH_CONFIGURATION.md),
[DADA2 reproducibility](docs/DADA2_REPRODUCIBILITY.md) and
[reproducibility scope](docs/REPRODUCIBILITY_SCOPE.md) before running
`run_raw_preprocessing.R`. Larger historical ASV/taxonomy checkpoints are not
bundled. The optional route documents methods; universal byte-identical
reconstruction from raw reads is not established.

## Citation and license

See [CITATION.md](CITATION.md) for the distinction between the baseline DOI
and this revision. This repository retains its [MIT license](LICENSE).
