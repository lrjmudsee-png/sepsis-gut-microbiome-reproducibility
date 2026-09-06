# Version 1.1.0 release notes

Version `v1.1.0` is the corrected publication release of the sepsis gut microbiome reproducibility repository. It supersedes `v1.0.0` for reproduction of the current manuscript while preserving v1.0.0 as the historical baseline.

## Scientific correction

Longitudinal Benjamini–Hochberg adjustment now excludes intercept terms and non-finite P values while retaining the original inferential families across modeled metrics.

| Analysis | v1.0.0 q | v1.1.0 q |
| --- | ---: | ---: |
| T01 overall time | 0.005219 | 0.012633 |
| T02 depth sensitivity | 0.009132 | 0.021361 |
| T03 overall time | 0.127113 | 0.317783 |
| T06 time-by-cholestasis | 0.040954 | 0.266299 |

T06 is therefore exploratory and not FDR-significant.

The correction does not change raw P values, model estimates, confidence intervals, sample selection, input counts, SDI feature selection, score orientation, threshold, or external-validation AUCs.

## Interpretation clarifications

- The SDI development AUC of 0.731 is apparent development performance.
- A05 (sepsis versus trauma; AUC 0.272) and A04 (sepsis versus healthy; AUC 0.201) are comparator-specific evaluations from the same external BioProject and share the sepsis group; they are not independent external cohorts.
- Post-hoc genus-direction reversal and feature-absence analyses describe patterns associated with failed external transfer and do not establish a biological or technical cause.
- The reconstructed screening record and historical “lock” terminology do not establish prospective registration.

## Verification

The repository includes an independent standard-library Python implementation of the corrected BH calculation and preserves the original v1.0.0 expectations for field-level comparison. The manuscript reproduction workflow validates generated scientific outputs against the reviewed v1.1.0 expectations.

## Historical baseline

Version `v1.0.0` remains permanently available at Zenodo DOI https://doi.org/10.5281/zenodo.22330040.
