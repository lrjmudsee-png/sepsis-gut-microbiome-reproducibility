# Public Supplementary Figure reproduction

`11_make_supplementary_figures_PUBLIC.R` is the portable replacement for the author-environment script `23_make_supplementary_figures_v1_FINAL_REVISION.R`.

It reproduces Supplementary Figures S1–S3 directly from the frozen public inputs already included in the reproducibility package.

## Inputs

- `expected_results/frozen_result_inputs/01_15_Analysis_Set_Summary.csv`
- `expected_results/frozen_result_inputs/04_16_Alpha_Diversity_Tests.csv`
- `expected_results/frozen_result_inputs/23_19_Leave_One_Genus_Out_Sensitivity.csv`

## Run

From package root:

```bash
Rscript figure_audit_20260902/11_make_supplementary_figures_PUBLIC.R .
```

Or pass the package root explicitly:

```bash
Rscript 11_make_supplementary_figures_PUBLIC.R "/path/to/package-root"
```

## Outputs

Default output folder:

`./supplementary_figures_reproduced_public/`

The script produces:

- Supplementary Figure S1 PDF/TIFF
- Supplementary Figure S2 PDF/TIFF
- Supplementary Figure S3 PDF/TIFF
- source snapshots
- input MD5 manifest
- strict source-audit CSV
- reproduction manifest
- `sessionInfo()`

## Scientific boundary

No model is refit and no frozen file is overwritten.

S1 remains limited to A01–A06 development/locked-external analysis sets.
S2 displays descriptive mean-difference 95% CIs; formal inference remains Wilcoxon + BH-FDR.
S3 uses equal-weight SDI only, keeps a common y-axis, and explicitly marks A04/A05 Campylobacter and Atopobium as project-level absent.

## Windows compatibility

The public script uses regex-free package-relative path construction (`normalizePath` + `startsWith` + `substring`) to avoid Windows R/TRE regex parsing errors.
