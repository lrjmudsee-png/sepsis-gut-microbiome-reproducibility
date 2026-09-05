# Public package contents

The current submission revision is mapped in the root [README](../README.md).
See [SUBMISSION_REVISION.md](SUBMISSION_REVISION.md) for scientific changes.

`PUBLIC_FINAL_INVENTORY.txt` lists distributed files. `PACKAGE_MANIFEST_SHA256.csv`
contains their SHA256 hashes, excluding the manifest itself. Text files use LF
line endings in Git checkouts and archives.

`expected_results/` contains current reviewed expectations.
`validation/baseline_expected_results/` preserves the original expectations.
`validation/baseline_release_audit/` contains historical packaging checks;
those checks describe the baseline and do not certify the current revision.

Current numerical reproduction reports are under
`submission_revision_audit/github_validation_20260906/`. The original
2026-09-05 independent correction audit is also retained in
`submission_revision_audit/`. Figure-source audits are stored alongside the
public figure outputs. Work-directory outputs are not distributed.

The scientific correction changes longitudinal FDR values. It does not alter
the checkpoint, raw P values, model estimates, score, threshold or sample sets.
Claim wording and figure labels were also revised to state the evidence boundary.
