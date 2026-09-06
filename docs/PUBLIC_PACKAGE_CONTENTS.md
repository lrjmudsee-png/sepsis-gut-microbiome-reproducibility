# Public release contents

This document describes the `v1.1.0` publication release. The root [README](../README.md) provides the main entry point, and [correction provenance](SUBMISSION_REVISION.md) documents the statistical correction from the historical v1.0.0 baseline.

## Core analysis

`analysis/` contains the seven manuscript-analysis modules.

## Processed data

`data/analysis_ready_checkpoint/` contains the compact processed checkpoints used for manuscript-level reproduction.

## Reviewed results

`expected_results/` contains the reviewed v1.1.0 scientific outputs and expected values used by the reproduction workflow.

## Figures and tables

- `figures_reproduced_public/` contains the main figures.
- `supplementary_figures_reproduced_public/` contains supplementary figures and source audits.
- `publication_ready_tables/` contains publication-facing tables.

## Validation and provenance

`validation/` contains independent correction validation together with the preserved v1.0.0 expectations used for field-level comparison.

`submission_revision_audit/` retains correction evidence and recorded verification reports. These files are preserved for transparency and provenance; they are not required for ordinary use of the analysis pipeline.

Historical packaging checks are kept separately under `validation/baseline_release_audit/` and describe the v1.0.0 baseline rather than the current release.

## Scope of the correction

The v1.1.0 correction changes longitudinal FDR-adjusted P values and propagates the resulting interpretation to affected tables, figures, and claim records. It does not alter the processed checkpoint, raw P values, model estimates, confidence intervals, sample selection, SDI feature selection, score orientation, threshold, or external-validation AUCs.
