# Changelog

## Submission revision — 2026-09-06

Version: `1.0.0-submission-revision-20260906`. Based on published tag `v1.0.0`
(commit `c92a4055cd804e8942a07b8cd263f5b32ed6f4b3`); the tag and its DOI
archive remain unchanged. This is a branch update, not a new archival release.

- Exclude intercepts from longitudinal BH families; retain grouping across all
  metrics by analysis ID (omnibus) or analysis ID and model (coefficients).
- Update adjusted P values and affected tables/figures/claim records. T06
  Shannon time-by-cholestasis q changes from 0.040954 to 0.266299.
- Preserve all non-FDR frozen scientific fields and original expected results.
  Regenerated lock timestamps do not establish prospective registration.
- Describe development performance as apparent, identify the two overlapping
  external comparisons within one BioProject, and avoid causal interpretations
  of post-hoc failure diagnostics.
- Add the public sample-metadata crosswalk, current figure sources, original
  correction audit and a portable independent Python validator.
- Add Windows UTF-8 preflight and an error exit when scientific validation fails.
- Retain baseline packaging audits separately, update reproduction/citation
  guidance, and standardize text checkout endings to LF for package hashes.

## Baseline preparation history

The entries below describe preparation of the files ultimately published under
GitHub tag `v1.0.0`. Their internal package labels are historical; they are not
claims that GitHub releases v1.0.1, v1.0.2 or v1.0.3 were published.

## Public-final cleanup — 2026-09-03

Packaging/documentation cleanup only; no manuscript-analysis logic or frozen scientific result changed.

- Preserved `analysis/01–07` byte-for-byte from the uploaded verified package.
- Replaced the mixed internal/public Figure-audit directory with the final portable public Figure/QC scripts.
- Retained the latest public Figures 1–4 and the all-PASS public Figure-source audit.
- Removed obsolete author-environment redraw scripts, old Figure scripts, internal audit Word/Excel files and correction logs.
- Added a clean package-content guide and regenerated package-level SHA256 and static release-QC reports.

## v1.0.3 — 2026-09-02

Figure-audit code and documentation update. No manuscript-analysis logic or expected scientific result was changed.

- Added `figure_audit_20260902/` with the final figure-production and figure-audit code: the audited v1.2 redraw base, the v1.4 finalizer (with the `unname()` fix for the Figure 4C group-N check), the v1.1 figure-source consistency audit, the Figure 1 screening-flow CSV, and the official 2026-09-02 correction log / master workbook / corrected verification table.
- Figure 1 is now a dual-panel "Study selection and analysis architecture" (reconstructed screening flow + prespecified architecture); Figure 4C retains the cholestasis/SIC label with group-specific N.
- The 2026-09-02 figure-source consistency audit reports 29 PASS / 0 FAIL.
- Regenerated `PACKAGE_MANIFEST_SHA256.csv` and rebuilt the public zip.

## v1.0.2 — 2026-09-01

Critical bug fix for public release. No manuscript-analysis logic or expected scientific result was changed.

- Fixed an R syntax error in `reproduce_manuscript_results.R` (unescaped double quotes inside the missing-package error message) that prevented the master runner from parsing and executing at all.
- Regenerated `PACKAGE_MANIFEST_SHA256.csv` after the fix.
- Rebuilt the public zip from the fixed v1.0.2 tree.
- Recommended release-gate addition: run an R `parse()` syntax scan over all package scripts before archiving (the v1.0.1 gate covered absolute paths but not syntax).

## v1.0.1 — 2026-08-31

Public path-usability update. No manuscript-analysis logic or expected scientific result was changed.

- Expanded README instructions for manuscript-result and optional upstream routes.
- Added `docs/PATH_CONFIGURATION.md`.
- Added explicit public `--raw-root` and `--output-root` handling to `run_raw_preprocessing.R`.
- Added external raw/output path plumbing to project-specific DADA2 scripts without changing scientific DADA2 parameters.
- Allowed PRJNA797231 raw reconstruction to retain its reconstructed ASV output when the optional historical reference RDS is not bundled, instead of terminating after reconstruction.
- Added external SILVA, reconstructed-ASV, and taxonomy-output path options to the fresh taxonomy method-reproduction script.
- Added external checkpoint/output path options to the optional analysis-ready checkpoint reconstruction script.
- Clarified that manuscript reproduction uses the bundled compact processed checkpoint by repository-relative path.
- Regenerated the public absolute-path scan and package SHA256 manifest.

## v1.0.0 — 2026-08-31

- Public reproducibility release.
- Added portable analysis scripts for the verified manuscript-result pipeline.
- Added compact validated analysis-ready checkpoint.
- Added automated scientific-output validation.
- Added optional project-specific DADA2 preprocessing code.
- Documented cohort-specific raw-preprocessing reproducibility limitations.
- Removed machine-specific historical logs and absolute-path manifests from the public package.
