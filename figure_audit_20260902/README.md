# Public figure reproduction

This directory contains only the public, package-relative Figure/QC entry points.

## Files

- `08_make_main_figures_PUBLIC.R`
  - Reproduces manuscript Figures 1–4 from frozen public CSV sources.
  - No statistical model is refit.
  - Figure 1 uses the final reconstructed screening flow.
  - Figure 3 preserves 4 direction reversals + 2 project-level absences.
  - Figure 4C uses cholestasis/SIC terminology and frozen group-specific N.

- `09_audit_main_figure_sources_PUBLIC.R`
  - Validates the numerical sources before plotting.
  - Includes exact genus-to-status mapping for Figure 3B.

- `10_normalize_publication_labels.R`
  - Creates publication-facing table CSV copies.
  - It does NOT modify `expected_results`.
  - Historical machine identifiers remain unchanged inside the verified analysis chain.

- `V1_Figure1_Screening_Flow_FINAL_20260901.csv`
  - Machine-readable Figure 1 screening source.

## Run from the repository root

```bash
Rscript figure_audit_20260902/09_audit_main_figure_sources_PUBLIC.R .
Rscript figure_audit_20260902/08_make_main_figures_PUBLIC.R .
Rscript figure_audit_20260902/10_normalize_publication_labels.R .
```

Or pass the repository root explicitly as argument 1.

Required plotting packages:

```r
install.packages(c("ggplot2", "patchwork"))
```

## Scientific boundary

These scripts are presentation/reproduction utilities. They do not change:

- DADA2/preprocessing parameters
- diversity calculations
- PERMANOVA
- genus meta-analysis
- six-genus SDI definition
- locked external validation
- longitudinal mixed models
- frozen expected scientific outputs

## Supplementary Figures S1–S3

Run:

```bash
Rscript figure_audit_20260902/11_make_supplementary_figures_PUBLIC.R .
```

This reproduces S1–S3 from `expected_results/frozen_result_inputs/` and writes its own source-audit/manifest files to `supplementary_figures_reproduced_public/`.
