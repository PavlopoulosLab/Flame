setwd(this.path::this.dir())

cat("==============================\n")
cat("Starting tests for Flame \n\n")
cat("==============================\n\n")

source("./unit_tests_init.R")
source("./unit_tests_enrichment.R")
source("./unit_tests_links.R")
source("./unit_tests_interoperability.R")
