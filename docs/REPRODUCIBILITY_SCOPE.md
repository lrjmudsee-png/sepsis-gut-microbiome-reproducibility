# Reproducibility scope

This repository distinguishes three reproducibility layers.

## Layer 1 — manuscript-result reproduction (recommended)

Bundled validated analysis-ready checkpoint → analysis scripts 01–07 → final tables,
figure-source data, PDFs, claim registry, and result freeze.

This is the public default and the route used for scientific output equality checks.

## Layer 2 — checkpoint reconstruction

Final ASV checkpoint + verified historical taxonomy checkpoint + curated public-study
metadata → analysis-ready checkpoint.

The code is provided, but the larger ASV/taxonomy checkpoint files are not embedded in
this code archive.

## Layer 3 — raw-data method reproduction

Public FASTQ → project-specific DADA2 → ASV table → fresh SILVA 138.2 taxonomy.

This documents the complete preprocessing method. However, historical stochastic state
for DADA2 error learning was not retained for every cohort, and fresh SILVA classification
can also show small cell-level differences from the historical taxonomy checkpoint.
Therefore Layer 3 is not presented as a universal byte-identical reconstruction route.
