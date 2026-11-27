# Install the R packages Flame needs (keep in sync with DESCRIPTION).
# Usage: Rscript install_packages.R          (everything, including test tools)
#        FLAME_RUNTIME_ONLY=1 Rscript install_packages.R   (server only; used by the Docker image)

# Keep a configured CRAN mirror (e.g. Posit Package Manager binaries in rocker images).
if (is.null(getOption("repos")) || getOption("repos")[["CRAN"]] %in% c("@CRAN@", ""))
  options(repos = c(CRAN = "https://cloud.r-project.org"))

flame_all_packages <- c(
  # science core
  "R6", "httr", "curl", "jsonlite", "gprofiler2", "WebGestaltR", "enrichR",
  "tidyr", "dplyr", "data.table", "stringr", "plyr", "poolr", "igraph", "htmltools", "Matrix",
  # bench statistics (Dunnett) and web clients (REVIGO)
  "mvtnorm", "httr2",
  # API server
  "plumber", "httpuv", "mirai", "later", "DBI", "RSQLite", "sodium", "mime",
  # loading the package from source (api/run.R)
  "pkgload"
)
if (!identical(Sys.getenv("FLAME_RUNTIME_ONLY"), "1"))
  flame_all_packages <- c(flame_all_packages, "testthat", "httptest2", "callr")

# Bioconductor packages (clusterProfiler ORA/GSEA, PROGENy pathway activity; msigdbr itself is on CRAN)
# org.*.db: current GO annotations for clusterProfiler's enrichGO / gseGO (one per organism, see
# CP_ORGDB in R/enrich-annotations-clusterprofiler.R); org.Hs.eg.db also names the TCGA genes.
flame_orgdb_packages <- c("org.Hs.eg.db", "org.Mm.eg.db", "org.Rn.eg.db", "org.Dr.eg.db", "org.Dm.eg.db",
                          "org.Ce.eg.db", "org.Sc.sgd.db", "org.Bt.eg.db", "org.Cf.eg.db", "org.Gg.eg.db",
                          "org.Ss.eg.db", "org.Mmu.eg.db", "org.Pt.eg.db")
flame_bioc_packages <- c("clusterProfiler", "fgsea", "progeny", "GO.db", flame_orgdb_packages)
flame_all_packages <- c(flame_all_packages, "msigdbr")

ncpus <- max(1L, parallel::detectCores() - 1L)
flame_new_packages <- setdiff(flame_all_packages, rownames(installed.packages()))
if (length(flame_new_packages)) install.packages(flame_new_packages, Ncpus = ncpus)
flame_new_bioc <- setdiff(flame_bioc_packages, rownames(installed.packages()))
if (length(flame_new_bioc)) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
  BiocManager::install(flame_new_bioc, update = FALSE, ask = FALSE, Ncpus = ncpus)
  # progeny can be missing from the current Bioconductor release's package index for a platform
  # (e.g. a Linux build failure, often cascading from one of its own dependencies) even though
  # the rest of flame_bioc_packages install fine; fall back to the previous release for just
  # that package rather than forcing everything onto an older, fully-downgraded Bioc version.
  flame_still_missing <- setdiff(flame_new_bioc, rownames(installed.packages()))
  if (length(flame_still_missing)) {
    flame_prior_bioc <- sprintf("https://bioconductor.org/packages/%.2f/bioc",
                                 as.numeric(as.character(BiocManager::version())) - 0.01)
    install.packages(flame_still_missing, repos = c(flame_prior_bioc, BiocManager::repositories()), Ncpus = ncpus)
  }
}
missing <- setdiff(c(flame_all_packages, flame_bioc_packages), rownames(installed.packages()))
if (length(missing)) {
  if (identical(Sys.getenv("FLAME_RUNTIME_ONLY"), "1")) stop("Could not install: ", paste(missing, collapse = ", "))
  message("Could not install: ", paste(missing, collapse = ", "),
          "\nWebGestaltR needs a Rust toolchain; plumber needs libsodium (see README).")
} else {
  message("All Flame packages are installed.")
}
