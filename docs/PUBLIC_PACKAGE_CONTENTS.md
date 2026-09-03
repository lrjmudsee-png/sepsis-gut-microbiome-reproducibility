# Public package contents

This is the cleaned public reproducibility package.

## Retained scientific/reproduction components

- `analysis/01–07`: frozen manuscript-analysis pipeline
- `preprocessing/`: optional upstream preprocessing utilities
- `data/analysis_ready_checkpoint/`: compact validated processed checkpoint
- `data/metadata/`: analysis-ready metadata
- `expected_results/frozen_result_inputs/`: frozen validation inputs
- `expected_results/scientific_outputs/`: frozen manuscript-level outputs and figure sources
- `validation/validate_reproduction.R`: manuscript-result validation
- `validation/VERIFIED_REPRODUCTION_STATUS.csv`: verified scientific-layer status
- `figure_audit_20260902/`: portable public Figure/QC scripts only
- `figures_reproduced_public/`: latest publicly regenerated Figures 1–4
- `figure_source_audit_public/`: latest public Figure-source audit output
- `publication_ready_tables/`: publication-facing normalized CSV copies
- `environment/`: verified R/package information
- `docs/`: workflow, data, path and reproducibility documentation

## Intentionally excluded from the cleaned public package

The uploaded working package contained historical/internal Figure-development material. These are not needed by public users and have been removed:

- `05_图片核对数据表_核验修正版_20260902.docx`
- old `08_make_main_figures_PUBLIC.R` before the layout fix
- old `09_audit_main_figure_sources_PUBLIC.R` before the exact mapping check
- `22_redraw_main_figures_v1_AUDITED_v1_2.R`
- `22_redraw_main_figures_v1_AUDITED_v1_4_FINALIZER.R`
- `25_v1_figure_source_consistency_audit.R`
- `25_v1_figure_source_consistency_audit_v1_1.R`
- `DSH修复说明_20260902.md`
- internal Figure correction/readme files
- `Sepsis_V1_Figure_Data_Issues_and_Code_Change_Master_20260902.xlsx`
- `V1_Figure_Audit_Correction_Log_20260902.csv`
- stale Figure-specific `SHA256SUMS.txt`

The current package-level SHA256 manifest supersedes those historical manifests.

## Scientific-change statement

The public cleanup does not alter the scientific analysis scripts or frozen scientific results. It only:

1. removes internal/obsolete files;
2. installs the final portable public Figure scripts;
3. updates public documentation;
4. regenerates package-level QA manifests.

- `figure_audit_20260902/11_make_supplementary_figures_PUBLIC.R`: portable Supplementary Figure S1–S3 reproduction with embedded source audit.
