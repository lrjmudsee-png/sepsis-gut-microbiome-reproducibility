# Verification on 27 October 2026

- All seven R-generated PNG files match the reviewed revision byte-for-byte.
- All 39 source, text, alignment, PDF-audit, table-cell and strict raster checks passed.
- All 17 supplementary source assertions passed.
- All seven PDFs passed the 5-pt text and collision audits with zero failures and zero warnings. All seven panel-alignment checks passed at 1.5 pt.
- The main-table Word file rendered to two clean pages. Both pages retain the reviewed text; the recorded pixel comparison is in `document_rerun.json`.
- The optional manuscript assembler preserves all paragraph text and table cell matrices. All five supplementary PDF pages match the reviewed render.
- The workbook inventory reader retains the original sheet/cell/merged-range data. The Node styling script passes syntax checking; its presentation logic is unchanged from the already verified workbook production, with only CLI path handling adapted.
- The repository's existing independent correction validator passed: 36 frozen tables and 295 eligible BH rows across 10 families. No analysis module or expected result was changed.
- The static figure-source check reports 19 PASS, two advisory WARN, zero FAIL. Its R syntax hint is covered by a successful R run; its dynamic-width hint is covered by the actual 183-mm PDF dimensions. Existing startup locale/package-build warnings do not affect the exact PNG comparison.

Scientific interpretation and figure designs were retained from the reviewed revision. No statistical model was refit.
