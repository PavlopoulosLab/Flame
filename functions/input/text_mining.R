loadTextMiningExample <- function() {
  tryCatch({
    txt_example <- readRDS("examples/textmining_example.RDS")
    updateTextAreaInput(session, "textmining_textinput", value = txt_example)
  }, error = function(e) {
    cat(paste("Text-mining example error:  ", e))
    renderError("Text-mining example error.")
  })
}

resetTextMiningFields <- function() {
  tryCatch({
    updateTextAreaInput(session, "textmining_textinput", value = "")
    updateSelectizeInput(session, "textmining_organism",
                         selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]")
  }, error = function(e) {
    cat(paste("Text clear error:  ", e))
    renderError("Text clear error.")
  })
}

handleTextMining <- function() {
  tryCatch({
    text <- input$textmining_textinput
    species <- ORGANISMS[ORGANISMS$print_name == input$textmining_organism, ]$taxid
    # js call to change the default TAGGER_SPECIES VALUE to species
    shinyjs::runjs(sprintf("updateSpecies(%s)", species))
    if (areEnoughTextCharacters(text)) {
      renderModal("<h2>Please wait.</h2><p>Contacting the EXTRACT web server...</p>")
      parsedText <- parseTextForPOSTRequests(text, species)
      text_enc <- parsedText$text_enc
      entity_types_txt <- parsedText$entity_types_txt
      
      extracted_terms <- getEntities(text_enc, entity_types_txt)
      if (existTaggedProteins(extracted_terms)) {
        enriched_text <- getHTML(text_enc, entity_types_txt)
        currentTextminingResult <<- unlist(extracted_terms$ID)
        extracted_terms <- prepareExtractedTermsForPrint(extracted_terms)
        printExtractResults(enriched_text, extracted_terms)
      }
    }
  }, error = function(e) {
    cat(paste("Functional enrichment analysis error:  ", e))
    renderError("Error during enrichment. Try again in a while.")
  }, finally = {
    removeModal()
  })
}

areEnoughTextCharacters <- function(text) {
  areEnough <- T
  if(nchar(text) < 3) {
    areEnough <- F
    renderWarning("Fill the text-search input with at least 3 characters.")
  }
  return(areEnough)
}

parseTextForPOSTRequests <- function(text, species) {
  entity_types <- c(-3)
  text <- gsub("\n", " ", text)
  text <- gsub("\r", "", text)
  text_enc <- httpuv::encodeURIComponent(text)
  entity_types_txt <- paste(entity_types, sep = "+", collapse = "+")
  entity_types_txt <- sprintf("%s+%s", entity_types_txt, species)
  return(list(text_enc = text_enc, entity_types_txt = entity_types_txt))
}

getEntities <- function(text_enc, entity_types_txt) {
  url <- "https://tagger.jensenlab.org/GetEntities"
  params <- sprintf("document=%s&entity_types=%s&auto_detect=0&format=tsv",
                    text_enc, entity_types_txt)
  request <- httr::POST(url, body = params)
  if (isPOSTResponseValid(request)) {
    request_res <- data.table::fread(
      sprintf("%s\n", rawToChar(httr::content(request,"raw"))), header = F
    )
    names(request_res) <- c("Name", "Type", "ID")
    request_res <- request_res[request_res$Type != -3, ]
  }
  else
    request_res <- NULL
  return(request_res)
}

existTaggedProteins <- function(extracted_terms) {
  exist <- T
  if (is.null(extracted_terms)) {
    exist <- F
    renderWarning("No genes/proteins found in the submitted text for the selected organism.")
  }
  return(exist)
}

getHTML <- function(text_enc, entity_types_txt) {
  url <- "https://tagger.jensenlab.org/GetHTML"
  params <- sprintf("document=%s&entity_types=%s&auto_detect=0",
                    text_enc, entity_types_txt)
  request <- httr::POST(url, body = params)
  if (isPOSTResponseValid(request))
    request_res <- rawToChar(httr::content(request, "raw"))
  else
    request_res <- NULL
  return(request_res)
}

prepareExtractedTermsForPrint <- function(extracted_terms) {
  extracted_terms <- attachTextMiningDBLinks(extracted_terms)
  extracted_terms$check <- unlist(lapply(extracted_terms$ID_noLINKS, function(i) {
    return(HTML(sprintf("<input type='checkbox' name='text_mining_result_table[]' value='%s' />", i)))
  }))
  extracted_terms <- subset(extracted_terms, select = c(check, Name, Type, ID))
  names(extracted_terms) <- c("#", "Gene Name", "Species (TaxID)", "ID")
  return(extracted_terms)
}

printExtractResults <- function(enriched_text, extracted_terms) {
  output$extracted_text <- renderUI({ HTML(enriched_text) })
  renderShinyDataTable("extracted_terms", extracted_terms,
                       scrollY = "200px", fileName = "text-mining")
  shinyjs::show("textmining_tagger_results")
}

addTextMiningToFiles <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Parsing your input.</p>")
    selected <- unlist(strsplit(as.character(input$textmining_selected), ","))
    textMiningInputList <- currentTextminingResult[currentTextminingResult %in% selected]
    buildUserListFromText(paste(
      textMiningInputList, sep = "\n", collapse = "\n"
      ), "textmining")
  }, error = function(e) {
    cat(paste("Extracting list from text-mining error:  ", e))
    renderError("Extracting list from text-mining error.")
  }, finally = {
    removeModal()
  })
}

deleteTextmining <- function() {
  tryCatch({
    resetTextMiningResults()
  }, error = function(e) {
    cat(paste("Text-mining reset error:  ", e))
    renderError("Text-mining reset error.")
  })
}
