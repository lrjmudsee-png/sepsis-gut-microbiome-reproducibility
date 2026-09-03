# DADA2 upstream reproducibility

| BioProject | Verified upstream status |
|---|---|
| PRJEB33360 | raw FASTQ → final ASV exact |
| PRJNA797231 | raw FASTQ → final ASV exact after preserving/restoring sample labels by run order |
| PRJNA691455 | raw/filtering exact; archived historical error model → final ASV exact |
| PRJNA1010969 | raw/filtering exact; archived historical error models → final ASV exact |
| PRJNA978257 | raw → filtered FASTQ sequence/quality/order exact; fresh historical error-learning step not byte-identical |
| PRJNA430161 | same limitation as above |
| PRJNA912621 | same limitation as above |

For the cohorts with unrecovered historical DADA2 random/error-learning state, this
repository does not attempt to brute-force random seeds. Exact manuscript reproduction
uses the validated downstream checkpoint.
