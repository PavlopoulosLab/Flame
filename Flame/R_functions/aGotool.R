handleAGotool <- function(aGOtoolSelect, aGoCorrectionMethod, aGOtoolOrganism, aGOtoolDatasources, aGOtoolPvalue,
                          gconvertTargetGotool, aGoBarSelect2,aGoScatterSelect, aGoNetworkSelect, aGoNetworkSelect2,aGoNetworkSelect3,
                          aGoHeatmapSelect, aGoHeatmapSelect2, output, session){
  
  if (identical(file_data, list())) session$sendCustomMessage("handler_alert", "Please, first upload your data files.")
  else {
    genesForaGotool <- file_data[file_names==aGOtoolSelect][[1]]
    if (is.null(aGOtoolDatasources)) session$sendCustomMessage("handler_alert", "Please, select one or more datasources.")
    else {
      session$sendCustomMessage("handler_disableAllButtons", T) # disable all buttons until execution is over
      session$sendCustomMessage("handler_disableSourcesTabsaGoTool", T) # disable all datasources tab panels
      session$sendCustomMessage("handler_startLoader", c(12,10))
      
      ###
      input_genes <- genesForaGotool
      gProfOrganism <- organismsFromFile[organismsFromFile$print_name == aGOtoolOrganism,]$gprofiler_ID #organism as gprofiler input
      genesForaGotool <- gconvert(unlist(genesForaGotool), organism = gProfOrganism, target = "ENSP") #gene convert to ENS ID using gProfOrganism format
      genesForaGotool <- genesForaGotool[genesForaGotool$name!="nan",]
      genesForaGotool <- genesForaGotool[c("input", "name", "target")]
      taxid <- organismsFromFile[organismsFromFile$print_name == aGOtoolOrganism, ]$Taxonomy_ID
      aGOtoolDatasources <- unlist(aGOtoolDatasources) 
      limit_2_entity_type <- paste(unlist(aGOtoolDatasources), sep = ";", collapse = ";")
      
      uniprotIdGenes<-c()
      for (i in 1:length(genesForaGotool$name))
      {
        uniprotIdGenes[i] <- sprintf("%s.%s", taxid, genesForaGotool$target[i])
      }
      foreground <- paste(unlist(uniprotIdGenes), sep = "%0d", collapse = "%0d")
      o_or_u_or_both <- "both"  #representation level : over-, underrepresented or both
      if (aGoCorrectionMethod == "P-value"){
        FDR_cutoff <-"1"
        p_value_cutoff <- aGOtoolPvalue
      } 
      else
      { 
        p_value_cutoff <- "1"
        FDR_cutoff<-aGOtoolPvalue
      }
      
      #the api POST request
      post_args <- list(output_format = "tsv",
                        enrichment_method = "genome",
                        taxid = sprintf("%s",taxid), #string
                        limit_2_entity_type = limit_2_entity_type,#sprintf("%s",limit_2_entity_type), #string
                        foreground = foreground,
                        o_or_u_or_both = o_or_u_or_both,
                        p_value_cutoff = sprintf("%s",p_value_cutoff),#string
                        FDR_cutoff = sprintf("%s",FDR_cutoff)#string
      )
      # API request
      request <- POST("https://agotool.org/api_orig", body = post_args, encode = "json")
      response <- rawToChar(content(request,"raw"))
      response <- gsub("PFAM \\(Protein FAMilies\\)", "PFAM", response)
      response <- gsub("UniProt keywords", "UniProt", response)
      result_df <- read.csv(text = response, sep="\t", stringsAsFactors = FALSE)
      session$sendCustomMessage("handler_startLoader", c(12,30))
      if(!is.null(result_df) & nrow(result_df)>0)
      {
        aGotoolResults <<- data.frame()
        aGotoolResults <<- result_df[,c(19, 1, 3, 6, 7, 13,12,11,15)]
        names(aGotoolResults)[1] <<- "Source"
        names(aGotoolResults)[2] <<- "Term_ID"
        names(aGotoolResults)[3] <<- "Function"
        names(aGotoolResults)[4] <<- "P-value"
        names(aGotoolResults)[5] <<- "FDR"
        names(aGotoolResults)[6] <<- "Term Size"
        names(aGotoolResults)[7] <<- "Query size"
        names(aGotoolResults)[8] <<- "Intersection Size"
        names(aGotoolResults)[9] <<- "Positive Hits"
        
        LogPvalue <- format((-log10(as.numeric(as.character(format(aGotoolResults$`P-value`, scientific = F))))),format="e", digit=3)
        LogFDR<- format((-log10(as.numeric(as.character(format(aGotoolResults$FDR , scientific = F))))),format="e", digit=3)
        enrScore <- enrich_score(aGotoolResults$`Intersection Size`,aGotoolResults$`Term Size`)
        aGotoolResults <<- as.data.frame(cbind("Source"=aGotoolResults$Source, "Term_ID"=aGotoolResults$Term_ID, "Function"=aGotoolResults$Function, 
                                               "P-value"= formatC( aGotoolResults$`P-value`, format = "e", digits = 2), "-log10Pvalue"= LogPvalue, 
                                               "FDR"=  formatC( aGotoolResults$FDR, format = "e", digits = 2), "-log10FDR"=LogFDR,"Term Size"=aGotoolResults$`Term Size`,
                                               "Query size"=aGotoolResults$`Query size`,"Intersection Size"=aGotoolResults$`Intersection Size`,
                                               "Enrichment Score %"=enrScore,"Positive Hits"= aGotoolResults$`Positive Hits` ))
        
        aGotoolResults$`Enrichment Score %` <<- as.numeric(as.character(aGotoolResults$`Enrichment Score %`))
        aGotoolResults$`-log10Pvalue` <<- as.numeric(as.character(aGotoolResults$`-log10Pvalue`))
        aGotoolResults$`-log10FDR` <<- as.numeric(as.character(aGotoolResults$`-log10FDR`))
        aGotoolResults$`Positive Hits` <<- as.character(aGotoolResults$`Positive Hits`)
        ###
        positiveGenes <- strsplit(gsub(sprintf("%s.",taxid),"", aGotoolResults[["Positive Hits"]]), ";")
        positiveGenes <- paste(unlist(positiveGenes), collapse=",")
        positiveGenes <- strsplit(positiveGenes, ",")
        positiveGenes <- unique(unlist(positiveGenes))
        
        truefalse <- genesForaGotool$target %in% positiveGenes 
        mergedGenes <- cbind(genesForaGotool, truefalse)
        true <- mergedGenes [grepl("TRUE", mergedGenes$truefalse),]
        trueGenes <- true$input
        false <- mergedGenes [grepl("FALSE", mergedGenes$truefalse),]
        falseGenes<- false$input
        list.a <- as.list(trueGenes)
        list.b <- as.list(falseGenes)
        list.c <- as.list(as.character(input_genes[[1]]))
        list.d <- as.list(genesForaGotool$input)
        nonCommonGenes <- c(setdiff(list.c, list.d), setdiff(list.b, list.a)) # genes in input and not in output 
        clearaGoTables(output) # resetting any previous tables before updating
        sourceParameters<-c()
        for (i in 1:length(aGOtoolDatasources))
        {
          if (aGOtoolDatasources[i]==-55) {sourceParameters[i]<-"PFAM"}
          else if (aGOtoolDatasources[i]==-54) {sourceParameters[i]<-"INTERPRO"}
          else if (aGOtoolDatasources[i]== -51) {sourceParameters[i]<-"UniProt"}
          else {sourceParameters[i] <- "Disease Ontology"}
        }
        param_aGotool <- "" # String variable for execution parameters to be printed
        param_aGotool <- paste("File: ", aGOtoolSelect, "\nOrganism: ", aGOtoolOrganism, "\nSignificance threshold: ", aGoCorrectionMethod, "\nP-Value cut-off: ", aGOtoolPvalue,"\nTerm_ID output: ", gconvertTargetGotool, "\nDatabases: " , sep ="")
        param_aGotool <- paste(param_aGotool, paste(sourceParameters, collapse=', ' ), "\n", sep="")
        
        
        output$aGo_Parameters <- renderUI(
          box(
            title = "Parameters ", 
            width = NULL,
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("aGoParameters")
          )
        )
        output$aGoParameters <- renderText(param_aGotool)
        nonCommonGenes <- unlist(nonCommonGenes)
        geneNumber1 <- length(nonCommonGenes)
        output$nothitaGo <- renderUI(
          box(
            title = paste ("Unidentified Elements ", "(", as.character(geneNumber1), ")", sep=""),  
            width = NULL,
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            verbatimTextOutput("nonCommonaGo")
          )
        )
        output$nonCommonaGo <- renderText(nonCommonGenes)
        sources_list <- c("-51", "-55", "-54", "-26")
        all_aGotool <<- data.frame()
        session$sendCustomMessage("handler_startLoader", c(12,60))
        # START name conversion ####
        # convert the names of the output genes in accordance with the user's preference  output type
        output$notconvertaGo <- renderUI("") # resetting UI box for unconverted genes (for after switching to USERINPUT)
        if (gconvertTargetGotool != "USERINPUT"){
          convertedGenesOutput <- gconvert(unlist(genesForaGotool$target), organism = gProfOrganism, target = gconvertTargetGotool)
          notConverted <- convertedGenesOutput[grepl("nan", convertedGenesOutput$target),]
          notConverted <- notConverted$input
          notConverted <- as.character(unlist(notConverted))
          geneNumber2 <- length(notConverted)
          output$notconvertaGo <- renderUI(
            box(
              title = paste("Unconverted Proteins " , "(", geneNumber2, ")", sep=""),  
              width = NULL,
              status = "primary", 
              solidHeader = TRUE,
              collapsible = TRUE,
              verbatimTextOutput("notConvertedaGo")
            )
          )
          output$notConvertedaGo <- renderText(notConverted)
          
          for (i in 1:nrow(aGotoolResults)){
            genesOutput<-c()
            initialSplitGenes <- strsplit(gsub(sprintf("%s.",taxid),"", aGotoolResults[["Positive Hits"]][i]), ";")[[1]]
            for (j in 1:length(initialSplitGenes)){
              inputGenes <- convertedGenesOutput[grepl(initialSplitGenes[j], convertedGenesOutput$input),]
              genesOutput[j] <- inputGenes$target[1] # in case of more than one matches Ens--> target namespace
              if (genesOutput[j] == "nan") genesOutput[j] <- inputGenes$input[1]
            }
            aGotoolResults[["Positive Hits"]][i] <<- paste(unique(genesOutput), sep=",", collapse = ",")
          }
        } else  {
          initialSplitGenes <- strsplit(gsub(sprintf("%s.",taxid),"", aGotoolResults[["Positive Hits"]]), ";")
          for (i in 1:length(initialSplitGenes)){
            aGotoolResults[["Positive Hits"]][i] <<- paste(initialSplitGenes[[i]], collapse = ", ")
          }
        }
        # END name conversion ####
        aGotoolResults <<- aGotoolResults[with(aGotoolResults,order(-`-log10Pvalue`)),]
        
        session$sendCustomMessage("handler_startLoader", c(12,70))
        if (aGoCorrectionMethod == "P-value"){
          aGotoolResults <<- subset(aGotoolResults, select=-c(`-log10FDR`, FDR)) #remove the FDR and logFDR columns from the table
        }
        else {
          aGotoolResults <<- subset(aGotoolResults, select=-c(`-log10Pvalue`, `P-value`)) #remove the pvalue and logPvalue columns from the table
          colnames(aGotoolResults)[4] <<-"P-value"
          colnames(aGotoolResults)[5] <<- "-log10Pvalue"
          aGotoolResults <<- aGotoolResults[with(aGotoolResults,order(-`-log10Pvalue`)),]
        }
        for (i in 1:length(aGOtoolDatasources)){
          session$sendCustomMessage("handler_enableSourceTabaGoTool", match(aGOtoolDatasources[i], sources_list)) # enable current datasource tab panel
          if (aGOtoolDatasources[i] == "-51"){
            Uniprot <- aGotoolResults[grepl("^UniProt$", aGotoolResults$Source),]
            if (nrow(Uniprot) > 0){
              Uniprot$Term_ID <- paste("<a href='https://www.uniprot.org/keywords/", Uniprot$Term_ID, "' target='_blank'>", Uniprot$Term_ID, "</a>", sep="")
              Uniprot$`Positive Hits`<-  gsub(",", ", ", Uniprot$`Positive Hits`)
              all_aGotool <<- rbind(all_aGotool, Uniprot)
              output$uniprotTable <- renderTableFunc(Uniprot, aGOtoolSelect, 11, "aGotool_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
            }
            else output$uniprotTable <- DT::renderDataTable(Uniprot)
          } else if (aGOtoolDatasources[i] == "-26"){
            DiseaseOntology <- aGotoolResults[grepl("^Disease Ontology$", aGotoolResults$Source),]
            if (nrow(DiseaseOntology) > 0){
              DiseaseOntology$Term_ID <- paste("<a href='https://diseases.jensenlab.org/Entity?order=textmining,knowledge,experiments&textmining=10&knowledge=10&experiments=10&type1=-26&type2=9606&id1=", DiseaseOntology$Term_ID, "' target='_blank'>", DiseaseOntology$Term_ID, "</a>", sep="")
              DiseaseOntology$`Positive Hits`<-  gsub(",", ", ", DiseaseOntology$`Positive Hits`)
              all_aGotool <<- rbind(all_aGotool, DiseaseOntology)
              output$diseaseTable <- renderTableFunc(DiseaseOntology, aGOtoolSelect, 11, "aGotool_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
            }
            else output$diseaseTable <- DT::renderDataTable(DiseaseOntology)
          }else if (aGOtoolDatasources[i] == "-55"){
            PFAM <- aGotoolResults[grepl("^PFAM$", aGotoolResults$Source),]
            if (nrow(PFAM) > 0){
              PFAM$Term_ID <- paste("<a href='http://pfam.xfam.org/family/", PFAM$Term_ID, "' target='_blank'>", PFAM$Term_ID, "</a>", sep="")
              PFAM$`Positive Hits`<-  gsub(",", ", ", PFAM$`Positive Hits`)
              all_aGotool <<- rbind(all_aGotool, PFAM)
              output$pfamTable <- renderTableFunc(PFAM, aGOtoolSelect, 11, "aGotool_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
            }
            else output$pfamTable <- DT::renderDataTable(PFAM)
          }else if (aGOtoolDatasources[i] == "-54"){
            INTERPRO <- aGotoolResults[grepl("^INTERPRO$", aGotoolResults$Source),]
            if (nrow(INTERPRO) > 0){
              INTERPRO$Term_ID <- paste("<a href='http://www.ebi.ac.uk/interpro/entry/InterPro/",  INTERPRO$Term_ID, "' target='_blank'>",  INTERPRO$Term_ID, "</a>", sep="")
              INTERPRO$`Positive Hits`<-  gsub(",", ", ", INTERPRO$`Positive Hits`)
              all_aGotool <<- rbind(all_aGotool, INTERPRO)
              output$interproTable <- renderTableFunc(INTERPRO, aGOtoolSelect, 11, "aGotool_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
            }else output$interproTable <- DT::renderDataTable(INTERPRO)
          }
          if (nrow(all_aGotool) > 0){
            session$sendCustomMessage("handler_enableSourceTabaGoTool", 0)
            all_aGotool <<- all_aGotool[with(all_aGotool,order(-`-log10Pvalue`)),]
            output$aGo_all <- renderTableFunc(all_aGotool, "ALL", 11, "aGotool_Results_", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
          }
        }
        
        updatePlotaGoSources(as.character(unique(aGotoolResults$Source)), session) # Updating hit datasource selection boxes in Plots
        
        all_aGotool_Barselect <- all_aGotool[grepl(paste(aGoBarSelect2, collapse="|"), all_aGotool$Source),]
        updateSliderInput(session, "aGoSliderBarplot","Choose a number of results to view:", min = 1, max = length(all_aGotool_Barselect$Term_ID), value = 10, step = 1)
        
        all_aGotool_Scatterselect <- all_aGotool[grepl(aGoScatterSelect,  all_aGotool$Source),]
        updateSliderInput(session, "aGoSliderScatter","Choose a number of results to view:", min = 1, max = length(all_aGotool_Scatterselect$Term_ID), value = 10, step = 1)
        
        all_aGotool_Networkselect <- all_aGotool[grepl(aGoNetworkSelect,  all_aGotool$Source),]
        updateSliderInput(session, "aGoSliderNetwork","Choose a number of results to view:", min = 1, max = length(all_aGotool_Networkselect$Term_ID), value = 10, step = 1)
        
        all_aGotool_Networkselect2 <- all_aGotool[grepl(aGoNetworkSelect2,  all_aGotool$Source),]
        updateSliderInput(session, "aGoSliderNetwork2","Choose a number of results to view:", min = 1, max = length(all_aGotool_Networkselect2$Term_ID), value = length(all_aGotool_Networkselect$Term_ID), step = 1)
        
        all_aGotool_Networkselect3 <- all_aGotool[grepl(aGoNetworkSelect3,  all_aGotool$Source),]
        updateSliderInput(session, "aGoSliderNetwork3","Choose a number of results to view:", min = 1, max = length(all_aGotool_Networkselect3$Term_ID), value = length(all_aGotool_Networkselect$Term_ID), step = 1)
        
        all_aGotool_Heatmapselect <- all_aGotool[grepl(aGoHeatmapSelect,  all_aGotool$Source),]
        updateSliderInput(session, "aGoSliderHeatmap","Choose a number of results to view:", min = 1, max = length(all_aGotool_Heatmapselect$Term_ID), value = 10, step = 1)
        
        all_aGotool_Heatmapselect2 <- all_aGotool[grepl(aGoHeatmapSelect2,  all_aGotool$Source),]
        updateSliderInput(session, "aGoSliderHeatmap2","Choose a number of results to view:", min = 1, max = length(all_aGotool_Heatmapselect2$Term_ID), value = 10, step = 1)
        
      }else session$sendCustomMessage("handler_alert", paste("No valid results found.", sep=""))
    }
    session$sendCustomMessage("handler_finishLoader", c(12,100))
    session$sendCustomMessage("handler_enableAllButtons", T) # now enable buttons again 
  }
}
##FUNCTIONS##
# void function that clears all current data from the gost datasources tables

clearaGoTables <- function(output){
  output$aGo_all <- renderDataTable(c())
  output$uniprotTable <- renderDataTable(c())
  output$diseaseTable <- renderDataTable(c())
  output$pfamTable <- renderDataTable(c())
  output$interproTable <- renderDataTable(c())
  
}
# This function calculates the enrichment score
# @param intersection_size: hit genes from query
# @param term_size: number of genes in database
# @return: score, calculated enrichment score
enrich_score <- function(intersection_size, term_size){
  intersection_size <- as.numeric(as.character(intersection_size))
  term_size <- as.numeric(as.character(term_size))
  score <- round((intersection_size/term_size) * 100, 2)
  return(score)
}

# This function updates the datasource selection boxes of some plots at the Plots tab
updatePlotaGoSources <- function(aGoSources, session){
  
  if (!is.na(match("UniProt", aGoSources))) selected <- "UniProt"
  else if (!is.na(match("Disease Ontology", aGoSources))) selected <- "Disease Ontology"
  else if (!is.na(match("INTERPRO", aGoSources))) selected <- "INTERPRO"
  else if (!is.na(match("PFAM", aGoSources))) selected <- "PFAM"
  
  
  updatePickerInput(session, "aGoBarSelect2", label="Select datasources", choices = aGoSources, selected = selected)
  updateSelectInput(session,"aGoScatterSelect", label="Select datasource", choices = aGoSources, selected = selected)
  updateSelectInput(session,"aGoHeatmapSelect", label="Select datasource", choices = aGoSources, selected = selected)
  updateSelectInput(session,"aGoHeatmapSelect2", label="Select datasource", choices = aGoSources, selected = selected)
  updateSelectInput(session,"aGoNetworkSelect", label="Select datasource", choices = aGoSources, selected = selected)
  updateSelectInput(session,"aGoNetworkSelect2", label="Select datasource", choices = aGoSources, selected = selected)
  updateSelectInput(session,"aGoNetworkSelect3", label="Select datasource", choices = aGoSources, selected = selected)
  
}