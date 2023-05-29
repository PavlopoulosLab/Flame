#step 0 set Default repository for automated download ####
# this is optional, uncomment only if needed

# local({r <- getOption("repos")
# r["CRAN"] <- "https://cran.r-project.org" 
# options(repos=r)
# })

#step1 install all required packages from CRAN ####
flame_all_packages <- c("devtools",
               "shiny",
               "shinyjs",
               "shinydashboard",
               "shinyWidgets",
               "shinyalert",
               "bsplus",
               "stringr",
               "httr",
               "httpuv",
               "curl",
               "jsonlite",
               "gprofiler2",
               "WebGestaltR",
               "DT",
               "tidyr",
               "enrichR",
               "plotly",
               "igraph",
               "visNetwork",
               "upsetjs",
               "plyr",
               "poolr"
)
flame_new_packages <- subset(flame_all_packages, !(flame_all_packages %in% rownames(installed.packages())))
install.packages(flame_new_packages)
  