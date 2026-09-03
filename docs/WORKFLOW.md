# Workflow overview

```text
Public 16S projects
        │
        ├─ optional method-transparency route
        │       raw FASTQ
        │          │
        │          ▼
        │   run_raw_preprocessing.R
        │          │
        │          ▼
        │   project-specific DADA2
        │          │
        │          ▼
        │   reconstructed final ASV
        │          │
        │          ├─ optional fresh SILVA 138.2 taxonomy method reproduction
        │          │     preprocessing/03_assign_silva1382_taxonomy.R
        │          │
        │          └─ exact historical checkpoint reconstruction requires the
        │                separately deposited historical ASV/taxonomy checkpoints
        │
        └─ RECOMMENDED EXACT MANUSCRIPT ENTRY
                bundled validated analysis-ready checkpoint
                                      │
                                      ▼
                           prepare analysis sets
                                      │
                                      ▼
                           discovery statistics
                                      │
                                      ▼
                         harmonized meta-analysis
                                      │
                                      ▼
                      SDI development + locked test
                                      │
                                      ▼
                         post-hoc failure diagnosis
                                      │
                                      ▼
                       longitudinal/supportive models
                                      │
                                      ▼
                    final tables + figure sources + PDFs
                                      │
                                      ▼
                         automated equality validation
```

See `PATH_CONFIGURATION.md` for the distinction between user-specific external paths and repository-relative manuscript paths.
