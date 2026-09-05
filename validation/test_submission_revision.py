"""Regression checks for independent correction validation (standard library)."""

import contextlib
import csv
import io
import math
import shutil
import tempfile
import unittest
from pathlib import Path

import validate_submission_revision as audit


class SubmissionAuditTests(unittest.TestCase):
    def test_bh_known_values_and_ties(self):
        # Hand-calculated BH values with an unsorted input and tied raw P values.
        actual = audit.bh_adjust([0.04, 0.01, 0.03, 0.01])
        self.assertEqual(actual, [0.04, 0.02, 0.04, 0.02])
        self.assertEqual(audit.bh_adjust([0.0, 1.0]), [0.0, 1.0])
        self.assertEqual(audit.bh_adjust([]), [])
        with self.assertRaises(ValueError):
            audit.bh_adjust([math.nan])

    def test_distributed_frozen_inputs(self):
        with contextlib.redirect_stdout(io.StringIO()):
            audit.audit_results(audit.ROOT / "expected_results/frozen_result_inputs")

    def check_rejected_mutation(self, field, value, intercept=False):
        with tempfile.TemporaryDirectory(prefix="sepsis_revision_test_") as scratch:
            root = Path(scratch) / "inputs"
            shutil.copytree(audit.ROOT / "expected_results/frozen_result_inputs", root)
            path = root / "28_20_Mixed_Model_Overall_Terms.csv"
            fields, rows = audit.read_csv(path)
            row = next(r for r in rows if
                       (r["Model_Term"] == "(Intercept)") == intercept
                       and math.isfinite(audit.number(r["P"])))
            row[field] = value
            with path.open("w", encoding="utf-8", newline="") as stream:
                writer = csv.DictWriter(stream, fieldnames=fields)
                writer.writeheader()
                writer.writerows(rows)
            with self.assertRaises(ValueError):
                audit.audit_results(root)

    def test_changed_adjustment_is_rejected(self):
        self.check_rejected_mutation("P_FDR", "0.999999999")

    def test_intercept_adjustment_is_rejected(self):
        self.check_rejected_mutation("P_FDR", "0.001", intercept=True)

    def test_changed_raw_p_is_rejected(self):
        self.check_rejected_mutation("P", "0.123456789")


if __name__ == "__main__":
    unittest.main()
