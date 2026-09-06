# Changelog

## v1.1.0 — 2026-09-06

Corrected publication release corresponding to the current manuscript.

### Scientific correction

- Excluded intercept terms and non-finite P values from longitudinal Benjamini–Hochberg inferential families.
- Retained the original grouping across modeled metrics: omnibus tests within each analysis ID and coefficient tests within each analysis ID and model.
- Updated affected adjusted P values, tables, figures, and claim records.
- Corrected Shannon-diversity adjusted P values: T01 q = 0.012633; T02 q = 0.021361; T03 q = 0.317783; T06 q = 0.266299.
- T06 time-by-cholestasis interaction is exploratory and no longer FDR-significant.

### Unchanged scientific quantities

Raw P values, model estimates, confidence intervals, sample selection, input counts, genus selection, SDI feature selection, score direction, threshold, and external-validation AUCs are unchanged.

### Interpretation and reproducibility

- Development AUC is described as apparent performance.
- A04 and A05 are identified as two comparator-specific evaluations from one external BioProject that share the sepsis group, not as two independent external cohorts.
- Post-hoc feature-direction and feature-absence diagnostics are not interpreted causally.
- Added independent BH verification, updated reproducibility records, and clarified the limits of historical raw-read reconstruction.

## v1.0.0 — 2026-09-04

Initial archived public release.

- GitHub tag: `v1.0.0`
- Zenodo DOI: https://doi.org/10.5281/zenodo.22330040
- Commit: `c92a4055cd804e8942a07b8cd263f5b32ed6f4b3`

This release is retained as the historical baseline and predates the corrected longitudinal multiple-testing adjustment in v1.1.0.

## Historical preparation notes

The entries below describe internal package-preparation labels used before the first archived GitHub release. They were not published as GitHub Releases and are retained only for provenance.

### Internal package label v1.0.3 — 2026-09-02

Figure-audit code and documentation update. No manuscript-analysis logic or expected scientific result was changed.

### Internal package label v1.0.2 — 2026-09-01

Fixed an R syntax error in the public reproduction runner and regenerated packaging records. No manuscript-analysis logic or expected scientific result was changed.

### Internal package label v1.0.1 — 2026-08-31

Public path-usability and documentation updates. No manuscript-analysis logic or expected scientific result was changed.
