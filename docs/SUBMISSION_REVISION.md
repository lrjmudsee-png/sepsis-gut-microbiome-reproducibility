# Submission revision and interpretation

The current branch contains the submission revision dated 2026-09-06. The
published `v1.0.0` tag and Zenodo archive [10.5281/zenodo.22330040](https://doi.org/10.5281/zenodo.22330040)
remain the baseline. This revision has no separately assigned archive DOI.

## Multiplicity correction

The baseline mixed-model adjustment included intercept tests. The revision
excludes intercepts and non-finite P values. It retains the original grouping
across all metrics: omnibus terms within each analysis ID; coefficient tests
within each analysis ID and model. Families were not narrowed according to
which results were significant. Intercept adjusted P values are now missing.

| Shannon test | Baseline q | Revised q |
| --- | ---: | ---: |
| T01 overall time | 0.005219 | 0.012633 |
| T02 overall time, depth sensitivity | 0.009132 | 0.021361 |
| T03 overall time | 0.127113 | 0.317783 |
| T06 time-by-cholestasis interaction | 0.040954 | 0.266299 |

T06 is exploratory and does not retain FDR significance. T01/T02 remain
supported by their omnibus tests; these adjusted omnibus P values must not be
described as the tests for individual day-specific coefficients.

The correction changes neither model fits nor raw P values, estimates,
confidence intervals, sample selection, input counts, genus selection, score
direction, or threshold. The original expectations are preserved under
`validation/baseline_expected_results/`. The independent Python audit verifies
every eligible adjusted value and rejects any other change to frozen scientific
fields. Regenerated lock timestamps are the only other permitted difference.

## Boundaries of the evidence

- The six-genus SDI was selected in development data. Its development AUC of
  0.731 is apparent performance, not an independently validated estimate.
- A05 (sepsis versus trauma; AUC 0.272) is the primary external comparison;
  A04 (sepsis versus healthy controls; AUC 0.201) is supportive. They come from
  the same BioProject, PRJNA1010969, and share the sepsis group. They are not
  two independent external cohorts.
- The fixed score failed to transfer in this external setting. No score reversal,
  feature reselection or threshold retuning is presented as successful validation.
  This failure does not establish that all microbiome prediction models fail.
- Four candidate genera reversed direction and two were absent at project
  level. These post-hoc observations describe patterns accompanying failure;
  they do not identify its biological or technical cause.
- The screening record was reconstructed retrospectively. Historical use of
  “prespecified” and generated “lock” files does not establish prospective
  registration or a contemporaneous, independently verified timestamp.
- Clinical setting, collection timing, treatment and processing differences
  can limit transportability. Exact raw-read reconstruction is not established
  for all historical DADA2 states; the bundled checkpoint route is the tested
  result-reproduction route.

Current figures and generated claim records use wording consistent with these
boundaries. Historical labels retained in upstream scripts are machine keys or
baseline records and do not add evidence of prospective registration.

## Repository additions

`submission_metadata/` links public checkpoint records to source identifiers.
`submission_outputs/` contains current figure sources and claim records.
`submission_revision_audit/` retains the 2026-09-05 correction audit and the
2026-09-06 repository verification. Baseline packaging checks are separately
identified under `validation/baseline_release_audit/`.

The 2026-09-06 repository update also corrects selection/validation wording and
adds a UTF-8 preflight on Windows and a nonzero failure exit for scientific
validation. These changes do not alter the statistical estimates.
