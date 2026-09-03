# Install direct packages used by the manuscript-result reproduction route.
cran <- c("vegan", "metafor", "openxlsx")
missing <- cran[!vapply(cran, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

if (!requireNamespace("nlme", quietly = TRUE)) install.packages("nlme")

cat("Core analysis packages are available.\n")
cat("For raw preprocessing, install Bioconductor 'dada2' separately.\n")
