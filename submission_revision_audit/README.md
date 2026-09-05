# Revision evidence

The CSV files at this directory level record the initial 2026-09-05 scientific
correction against v1.0.0. `changed_fields.csv` has 368 changed cells: 358 adjusted
P values and 10 regenerated lock timestamps. `fdr_families.csv` records the
retained multiplicity families. `independent_checks.csv` records 36 frozen-table
comparisons and the two independent BH calculations. `reproduction_validation/`
contains the initial 63-check corrected-package run.

`github_validation_20260906/` contains the subsequent complete repository run,
after wording and runner improvements. It has 63 passing checks. The independent
Python validator also checked the newly computed frozen inputs against the
original baseline and recomputed all 295 eligible BH values across 10 families.

Run the portable audit with:

```sh
python validation/validate_submission_revision.py
python -m unittest discover -s validation -p test_submission_revision.py -v
```

The tests include hand-calculated BH values and deliberate corruptions of an
adjusted P value, an intercept adjustment and a raw P value; each corruption must
be rejected. The Python audit never updates expected values. The R runner's
Windows UTF-8 preflight was tested from a C character locale, and its validation
gate was tested with both failing and passing summaries.

All 19 distributed R scripts parsed successfully. All four main and three
supplementary figures were rendered and visually reviewed. Numerical figure
audits are in `figure_source_audit_public/` (24 checks) and
`supplementary_figures_reproduced_public/` (17 checks), from the repository root.

Package manifests describe file bytes at packaging time. Regenerating figures
or session reports can change bytes without changing scientific results.
The preserved baseline manifest describes the original archive only.
