# Optional historical ASV/taxonomy checkpoints

The compact public package does not redistribute the larger historical final-ASV RDS files or verified historical taxonomy checkpoint CSVs.

They are only required for the optional `preprocessing/04_build_analysis_ready_from_checkpoints.R` route. The recommended manuscript-result reproduction route does **not** require them because the compact validated analysis-ready checkpoint is already bundled under `data/analysis_ready_checkpoint/`.

If the larger historical checkpoints are deposited separately (for example on Zenodo/OSF), use:

```text
<checkpoint-root>/
├─ asv/
│  ├─ PRJEB33360_seqtab_final.rds
│  ├─ PRJNA691455_seqtab_final.rds
│  └─ ...
└─ taxonomy/
   ├─ PRJEB33360_taxonomy_silva1382.csv
   ├─ PRJNA691455_taxonomy_silva1382.csv
   └─ ...
```

Then point the public script to the user's own location:

```r
Sys.setenv(SEPSIS_V1_CHECKPOINT_ROOT = "F:/Sepsis_V1_checkpoints")
source("preprocessing/04_build_analysis_ready_from_checkpoints.R")
```

The `F:/...` path is an example only.
