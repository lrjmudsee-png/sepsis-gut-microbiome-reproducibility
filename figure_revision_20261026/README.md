# Manuscript figure and table revision, 26 October 2026

This module reproduces the revised Figures 1–4, Supplementary Figures S1–S3 and five main tables. It uses the reviewed v1.1.0 numerical sources. The update changes presentation, figure legends and table headers; it does not refit models or change numerical results.

## Reproduce the figures

From the repository root, with R and Python 3 available:

```sh
Rscript figure_revision_20261026/install_packages.R
python -m pip install -r figure_revision_20261026/requirements.txt
Rscript figure_revision_20261026/build_figures.R
python figure_revision_20261026/build_tables.py
python figure_revision_20261026/audit_figures.py
python figure_revision_20261026/verify_reproduction.py
```

The recorded environment is R 4.4.0 with Arial installed. Set `V1_QA_PYTHON` to the Python executable if `python` on PATH is not the desired interpreter. Arial must be installed separately; it is not redistributed. Other font versions or library versions may change raster bytes. Numerical-source, PDF-text and geometry checks remain available even when exact raster matching is unavailable.

By default, inputs come from this module's `frozen_inputs/` and exports go to `output/`. All paths are resolved from the script location, so execution does not require an author-specific working directory. To choose paths explicitly:

```sh
Rscript figure_revision_20261026/build_figures.R figure_revision_20261026/frozen_inputs work/revised_figures
python figure_revision_20261026/build_tables.py --output work/revised_figures/Main_Tables.docx
python figure_revision_20261026/audit_figures.py work/revised_figures
python figure_revision_20261026/verify_reproduction.py work/revised_figures
```

Exports comprise editable PDF and SVG, 300-dpi PNG and 600-dpi LZW TIFF. Figures are 183 mm wide. Render-time panel alignment is checked at a 1.5-pt tolerance before export; the final PDFs are checked for text below 5 pt and text collisions. Audit manifests, source assertions and session information are written under the output directory. `reference/` contains the reviewed PDF figures and the main-table Word/PDF documents for comparison.

## Design and scientific conventions

- Blue `#0072B2`, vermilion `#D55E00`, and neutral gray `#747B82` form the palette. Shape, line type and direct labels provide additional distinctions. The palette follows the accessibility principles discussed in [Wong, Nature Methods (2011)](https://doi.org/10.1038/nmeth.1618).
- Screening separates the six query-recovered projects from the independent accession cross-check that retained the seventh project. The 11 retrospective candidates remain outside the frozen analysis.
- External AUCs below 0.5 retain their original orientation. An absent genus is labelled absent rather than assigned a zero effect.
- Longitudinal plots retain means, standard errors, sample counts and the corrected v1.1.0 FDR values. Discharge estimates remain descriptive.
- Supplementary figures preserve all 17 source assertions. `input_provenance.json` records SHA256 hashes for the 15 inputs, all byte-identical to their corresponding reviewed repository sources when synchronized.

## Reproduce the main tables

```sh
python figure_revision_20261026/build_tables.py
```

This writes `output/Main_Tables.docx` from `table_sources/main_tables.json`. Tables use restrained horizontal rules, consistent typography, aligned numerical fields and footnotes. The five main tables are grouped across two pages. Open the Word file in Word or LibreOffice to export a PDF and check the final pagination. The reviewed reference is included under `reference/Main_Tables.pdf`.

## Optional local manuscript and workbook assembly

The manuscript assembler requires three author-provided files in a local source directory: `Original_Manuscript.docx`, `Original_Supplement.pdf`, and, for the workbook workflow, `Original_Supplementary_Data.xlsx`. These source documents are not required to reproduce the figures or main tables.

```sh
python figure_revision_20261026/build_documents.py --source-dir LOCAL_SOURCE_DIRECTORY --figures figure_revision_20261026/output --output work/revised_documents
```

`build_documents.py` inserts the four PNG figures into the local manuscript, applies the table styles, updates legends and composes the supplementary PDF using the vector figure pages. It expects the reviewed 53-reference manuscript and the reporting-page layout used in this revision; it is not a general manuscript converter.

The supplementary-workbook styling code uses Node.js and `@oai/artifact-tool`, the spreadsheet runtime used to produce the reviewed workbook. This optional dependency is not vendored and is not needed for figures or main tables. `openpyxl` is used only to read workbook content into the inventory, not to author the workbook.

```sh
python figure_revision_20261026/create_workbook_inventory.py INPUT.xlsx work/workbook_inventory.json
node figure_revision_20261026/style_workbook.mjs INPUT.xlsx work/workbook_inventory.json work/Supplementary_Data_1.xlsx
```

Inspect the exported workbook and compare cell values with the input. The reviewed styling changed only the two index descriptions; all 20,770 numeric cells were retained. The recorded integrity result is in `verification/original_package_integrity.json`.

## Provenance and version scope

`verification/` records this synchronization's independent rerun and the earlier document-integrity check. The PDF collision auditor includes a correction for Cairo text traces that batch several lines: overlap attribution uses glyph origins and line direction instead of one oversized trace rectangle. Geometry thresholds are unchanged. Audit scripts are bundled so no local skill installation is needed.

The repository's archived release remains v1.1.0. This presentation revision is a later Git commit; the existing Zenodo archive does not contain these new scripts or layouts. No Zenodo deposit, release tag or DOI was created by this synchronization.
