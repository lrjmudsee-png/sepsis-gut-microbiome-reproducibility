# Correction provenance and interpretation

Version `v1.1.0` is the corrected publication release derived from the historical `v1.0.0` baseline. The published v1.0.0 tag and Zenodo archive [10.5281/zenodo.22330040](https://doi.org/10.5281/zenodo.22330040) are retained unchanged for provenance.

## Multiplicity correction

The baseline mixed-model adjustment included intercept tests. Version v1.1.0 excludes intercepts and non-finite P values while retaining the original grouping across modeled metrics: omnibus terms within each analysis ID and coefficient tests within each analysis ID and model. Families were not narrowed according to which results were significant. Intercept adjusted P values are therefore missing in the corrected release.

| Shannon test | v1.0.0 q | v1.1.0 q |
| --- | ---: | ---: |
| T01 overall time | 0.005219 | 0.012633 |
| T02 overall time, depth sensitivity | 0.009132 | 0.021361 |
| T03 overall time | 0.127113 | 0.317783 |
| T06 time-by-cholestasis interaction | 0.040954 | 0.266299 |

T06 is exploratory and does not retain FDR significance. T01/T02 remain supported by their omnibus tests; these adjusted omnibus P values should not be described as tests of individual day-specific coefficients.

The correction changes neither model fits nor raw P values, estimates, confidence intervals, sample selection, input counts, genus selection, score direction, or threshold. Original expectations are preserved under `validation/baseline_expected_results/`. The independent Python audit verifies every eligible adjusted value and rejects changes to other frozen scientific fields; regenerated lock timestamps are the only other permitted difference.

## Boundaries of the evidence

- The six-genus SDI was selected in development data. Its development AUC of 0.731 is apparent performance, not an independently validated estimate.
- A05 (sepsis versus trauma; AUC 0.272) is the primary external comparison; A04 (sepsis versus healthy controls; AUC 0.201) is supportive. Both come from BioProject PRJNA1010969 and share the sepsis group, so they are not two independent external cohorts.
- The fixed score failed to transfer in this external setting. No score reversal, feature reselection, or threshold retuning is presented as successful validation. This failure does not establish that all microbiome prediction models fail.
- Four candidate genera reversed direction and two were absent at project level. These post-hoc observations describe patterns accompanying failed transfer and do not identify a biological or technical cause.
- The screening record was reconstructed retrospectively. Historical uses of “prespecified” and generated “lock” files do not establish prospective registration or a contemporaneous independently verified timestamp.
- Clinical setting, collection timing, treatment, and processing differences can limit transportability. Exact raw-read reconstruction is not established for all historical DADA2 states; the bundled checkpoint route is the tested manuscript-result reproduction route.

Current figures and generated claim records use wording consistent with these boundaries. Historical labels retained in upstream scripts are machine keys or baseline records and do not add evidence of prospective registration.

## Verification and provenance

`submission_metadata/` links checkpoint records to public source identifiers. `submission_outputs/` contains current figure sources and claim records. `submission_revision_audit/` preserves the correction audit and repository verification evidence. Historical baseline packaging checks are separately retained under `validation/baseline_release_audit/`.

The v1.1.0 release also includes Windows UTF-8 preflight handling and a nonzero failure exit when scientific validation fails. These changes do not alter the statistical estimates.
