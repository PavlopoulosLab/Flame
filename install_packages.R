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

flame_new_packages <- setdiff(flame_all_packages, rownames(installed.packages()))
if (length(flame_new_packages)) install.packages(flame_new_packages, Ncpus = max(1L, parallel::detectCores() - 1L))
missing <- setdiff(flame_all_packages, rownames(installed.packages()))
if (length(missing)) {
  message("Could not install: ", paste(missing, collapse = ", "),
          "\nWebGestaltR needs a Rust toolchain; plumber needs libsodium (see README).")
} else {
  message("All Flame packages are installed.")
}
