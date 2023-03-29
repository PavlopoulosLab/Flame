loadVariantExample <- function() {
  tryCatch({
    snp_example <- paste(unlist(readRDS("examples/variants.RDS")), collapse = "\n")
    updateTextAreaInput(session, "snp_textAreaList", value = snp_example)
  }, error = function(e) {
    cat(paste("SNP example error: ", e))
    renderError("SNP example error.")
  })
}

resetVariantFields <- function() {
  tryCatch({
    updateTextAreaInput(session, "snp_textAreaList", value = "")
  }, error = function(e) {
    cat(paste("SNP clear error: ", e))
    renderError("SNP clear error.")
  })
}

handleVariantSubmit <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")
    convertVariantsToGenes(input$snp_textAreaList)
    shinyjs::show("snp_results")
  }, error = function(e) {
    cat(paste("Text submit error: ", e))
    renderError("Text submit error.")
  }, finally = {
    removeModal()
  })
}

handleVariantUpload <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")
    convertVariantsToGenes(readVariantUpload())
    shinyjs::show("snp_results")
  }, error = function(e) {
    cat(paste("Text submit error: ", e))
    renderError("Text submit error.")
  }, finally = {
    removeModal()
  })
}

readVariantUpload <- function() {
  variantFile <- input$snp_fileUpload
  fileType <- tolower(substr(variantFile$name, nchar(variantFile$name) - 2,
                             nchar(variantFile$name)))
  if (fileType == "tsv" || fileType == "txt")
    variantInput <- read.delim(variantFile$datapath, header = F)
  else if (fileType == "csv")
    variantInput <- read.csv(variantFile$datapath, header = F)
  return(variantInput)
}

convertVariantsToGenes <- function(variantList) {
  if (isInputNotEmpty(variantList)) {
    variants <- unlist(variantList, "\n")
    result <- gprofiler2::gsnpense(variants, filter_na = T)
    result$index <- 1:nrow(result)
    variant_cols <- colnames(result)[grep("variants.", colnames(result))]
    names <- result %>%
      gather(key = "colname", value = "value",
             variant_cols[1]:variant_cols[length(variant_cols)]) %>%
      filter(value != 0) %>%
      mutate(value = paste(colname, sep = ", ")) %>%
      group_by(index) %>%
      summarise(value = paste(value, collapse = ", "))
    
    names$effect <- lapply(names$value, function (i) {
      tp <- gsub("_", " ", 
                 gsub("variants.", "",
                      gsub("_variant", "", i)
                 )
      )
      tp_str <- paste(tp, collapse = ", ")
      return(tp_str)
    })
    
    result <- result %>%
      dplyr::left_join(names, by = "index")
    
    result_formatted <- subset(result,
                               select = c(rs_id, gene_names, ensgs, chromosome,
                                          start, end, strand, effect))
    result_formatted <- attachVariantTableLinks(result_formatted)
    names(result_formatted) <- c("SNP", "Gene", "ENSEMBL ID", "Chromosome",
                                 "Start", "End", "Strand", "Effect Type")
    renderShinyDataTable("snpViewer", result_formatted,
                         scrollY = "200px", fileName = "snp")
    currentVariantResults <<- result
  }
}

isInputNotEmpty <- function(input) {
  isNotEMpty <- T
  if (input == "") {
    isNotEMpty <- F
    renderWarning("Please, paste your input list in the text area first.")
  }
  return(isNotEMpty)
}

addVariantsToFiles <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")
    choice <- input$snp_choose_column
    if (choice == "ensgs") {
      list_of_ids <- unlist(currentVariantResults$ensgs)
      nametype <- "variants_ensg"
    } else {
      list_of_ids <- unlist(currentVariantResults$gene_names) 
      nametype <- "variants_geneName"
    }
    buildUserListFromText(paste(list_of_ids, collapse = "\n"), nametype)
  }, error = function(e) {
    cat(paste("Extracting list from SNPs error: ", e))
    renderError("Extracting list from SNPs error.")
  }, finally = {
    removeModal()
  })
}

deleteVariants <- function() {
  tryCatch({
    output$snpViewer <- renderDataTable(c())
    currentVariantResults <<- data.frame()
    shinyjs::hide("snp_results")
  }, error = function(e) {
    cat(paste("Text-mining reset error: ", e))
    renderError("Text-mining reset error.")
  })
}
