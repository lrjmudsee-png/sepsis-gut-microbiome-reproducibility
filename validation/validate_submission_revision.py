#!/usr/bin/env python3
"""Independently audit the submission FDR revision; Python standard library only.

Default: audit the distributed expected_results/frozen_result_inputs.
--results-root: audit freshly computed 06_frozen_result_inputs instead.
This program never edits results or updates expected values.
"""

import argparse
import csv
import hashlib
import math
import sys
from collections import defaultdict
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
MIXED = {
    "28_20_Mixed_Model_Overall_Terms.csv": ("Model_Term", ("Analysis_ID",)),
    "29_20_Mixed_Model_Coefficients.csv": ("Term", ("Analysis_ID", "Model")),
}
LOCKS = {
    "13_18_SDI_Candidate_Lock.csv",
    "14_18_SDI_Formula_Lock.csv",
    "15_18_SDI_Model_and_Threshold_Lock.csv",
}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def read_csv(path):
    with path.open(encoding="utf-8-sig", newline="") as stream:
        reader = csv.DictReader(stream)
        rows = list(reader)
        return reader.fieldnames, rows


def number(value):
    if value in ("", "NA", "NaN", "nan"):
        return math.nan
    return float(value)


def bh_adjust(pvalues):
    """BH step-up adjusted values, computed without R or third-party packages."""
    require(all(math.isfinite(p) and 0 <= p <= 1 for p in pvalues),
            "BH input must contain finite probabilities in [0, 1]")
    order = sorted(range(len(pvalues)), key=pvalues.__getitem__)
    result = [math.nan] * len(order)
    upper = 1.0
    for rank in range(len(order), 0, -1):
        index = order[rank - 1]
        upper = min(upper, pvalues[index] * len(order) / rank)
        result[index] = upper
    return result


def audit_results(results_root):
    baseline = ROOT / "validation/baseline_expected_results/frozen_result_inputs"
    expected_names = {p.name for p in baseline.glob("*.csv")}
    actual_names = {p.name for p in results_root.glob("*.csv")}
    require(bool(expected_names), "Baseline frozen inputs are missing")
    require(actual_names == expected_names,
            f"Frozen input inventory differs: {sorted(actual_names ^ expected_names)}")
    changes = 0
    family_counts = {}
    checked_rows = 0
    for name in sorted(expected_names):
        old_fields, old_rows = read_csv(baseline / name)
        fields, rows = read_csv(results_root / name)
        require(fields == old_fields and len(rows) == len(old_rows),
                f"{name}: columns or row count changed")
        allowed = {"P_FDR"} if name in MIXED else set()
        if name in LOCKS:
            allowed.add("Lock_Timestamp")
        for line, (old, new) in enumerate(zip(old_rows, rows), 2):
            for field in fields:
                if old[field] != new[field]:
                    require(field in allowed,
                            f"{name}:{line}: unexpected change to {field}")
                    changes += 1
        if name not in MIXED:
            continue
        term, grouping = MIXED[name]
        groups = defaultdict(list)
        for index, row in enumerate(rows):
            p = number(row["P"])
            if row[term] == "(Intercept)" or not math.isfinite(p):
                require(math.isnan(number(row["P_FDR"])),
                        f"{name}:{index + 2}: ineligible term has an adjusted P value")
            else:
                require(bool(row[term]) and all(row[key] for key in grouping),
                        f"{name}:{index + 2}: missing term or grouping key")
                groups[tuple(row[key] for key in grouping)].append((index, p))
        for key, entries in groups.items():
            expected = bh_adjust([p for _, p in entries])
            family_counts[(name, *key)] = len(entries)
            for (index, _), q in zip(entries, expected):
                actual = number(rows[index]["P_FDR"])
                require(math.isclose(actual, q, rel_tol=1e-11, abs_tol=1e-13),
                        f"{name}:{index + 2}: P_FDR {actual} differs from independent BH {q}")
                checked_rows += 1

    _, recorded = read_csv(ROOT / "submission_revision_audit/fdr_families.csv")
    recorded_counts = {}
    for row in recorded:
        key = (row["File"], row["Analysis_ID"])
        if row["Model"]:
            key += (row["Model"],)
        recorded_counts[key] = int(row["Tests_N"])
    require(family_counts == recorded_counts, "Multiplicity families differ from the revision record")
    print(f"PASS: {len(expected_names)} frozen tables; unchanged non-FDR scientific fields")
    print(f"PASS: {checked_rows} eligible BH rows across {len(family_counts)} families; intercepts excluded")
    print(f"Allowed changed cells versus v1.0.0: {changes} (FDR and lock timestamps only)")


def verify_manifest():
    _, rows = read_csv(ROOT / "PACKAGE_MANIFEST_SHA256.csv")
    require(bool(rows), "Package manifest is empty")
    seen = set()
    for row in rows:
        rel = Path(row["Path"])
        require(not rel.is_absolute() and ".." not in rel.parts,
                "Unsafe path in package manifest")
        require(row["Path"] not in seen, "Duplicate path in package manifest")
        seen.add(row["Path"])
        path = ROOT / rel
        require(path.is_file(), f"Missing manifest file: {rel}")
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        require(digest == row["SHA256"], f"SHA256 mismatch: {rel}")
    print(f"PASS: {len(rows)} package SHA256 entries (distributed bytes, LF text endings)")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--results-root", type=Path,
                        default=ROOT / "expected_results/frozen_result_inputs")
    parser.add_argument("--check-manifest", action="store_true",
                        help="Also verify distributed files, before regenerating figures or reports")
    args = parser.parse_args()
    try:
        audit_results(args.results_root)
        if args.check_manifest:
            verify_manifest()
    except (OSError, ValueError, KeyError, TypeError, csv.Error) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
