# Install the R packages Flame needs (keep in sync with DESCRIPTION).
# Usage: Rscript install_packages.R

options(repos = c(CRAN = "https://cloud.r-project.org"))

flame_all_packages <- c(
  # science core
  "R6", "httr", "curl", "jsonlite", "gprofiler2", "WebGestaltR", "enrichR",
  "tidyr", "dplyr", "data.table", "stringr", "plyr", "poolr", "igraph", "htmltools",
  # API server
  "plumber", "mirai", "later", "DBI", "RSQLite", "sodium", "mime",
  # development and tests
  "pkgload", "testthat", "httptest2", "callr", "httr2"
)

# Bioconductor packages (clusterProfiler ORA/GSEA; msigdbr itself is on CRAN)
flame_bioc_packages <- c("clusterProfiler", "fgsea")
flame_all_packages <- c(flame_all_packages, "msigdbr")

ncpus <- max(1L, parallel::detectCores() - 1L)
flame_new_packages <- setdiff(flame_all_packages, rownames(installed.packages()))
if (length(flame_new_packages)) install.packages(flame_new_packages, Ncpus = ncpus)
flame_new_bioc <- setdiff(flame_bioc_packages, rownames(installed.packages()))
if (length(flame_new_bioc)) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
  BiocManager::install(flame_new_bioc, update = FALSE, ask = FALSE, Ncpus = ncpus)
}
missing <- setdiff(c(flame_all_packages, flame_bioc_packages), rownames(installed.packages()))
if (length(missing)) {
  message("Could not install: ", paste(missing, collapse = ", "),
          "\nWebGestaltR needs a Rust toolchain; plumber needs libsodium (see README).")
} else {
  message("All Flame packages are installed.")
}
