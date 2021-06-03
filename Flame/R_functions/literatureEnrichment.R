handleLiteratureEnrich <- function(literatureSelect,literatureCorrectionMethod, literatureOrganism,literaturePvalue, 
                                   gconvertTargetLiterature,literatureSliderScatter,literatureSliderBarplot,literatureSliderHeatmap,
                                   literatureSliderHeatmap2,literatureSliderNetwork, literatureSliderNetwork2, output, session){
  
  if (identical(file_data, list())) session$sendCustomMessage("handler_alert", "Please, first upload your data files.")
  else {
    
    if (literatureOrganism =="") session$sendCustomMessage("handler_alert", "Please, select an organism.")
    else {
      session$sendCustomMessage("handler_disableAllButtons", T) # disable all buttons until execution is over
      session$sendCustomMessage("handler_startLoader", c(13,10))
      genesForLiterature <- file_data[file_names==literatureSelect][[1]]
      gProfOrganism <- organismsFromFile[organismsFromFile$print_name == literatureOrganism,]$gprofiler_ID #organism as gprofiler input
      genesForLiterature <- gconvert(unlist(genesForLiterature), organism = gProfOrganism, target = "ENSP") #gene convert to ENS ID using gProfOrganism format
      taxid <- organismsFromFile[organismsFromFile$print_name == literatureOrganism, ]$Taxonomy_ID
      uniprotIdGenes <- paste(taxid, genesForLiterature$target, sep = "." )
      session$sendCustomMessage("handler_startLoader", c(13,30))
      if (literatureCorrectionMethod == "P-value"){
        FDR_cutoff <-"1"
        p_value_cutoff <- literaturePvalue
      } 
      else
      { 
        p_value_cutoff <- "1"
        FDR_cutoff<-literaturePvalue
      }
      
      foreground <- paste(unlist(uniprotIdGenes), sep = "%0d", collapse = "%0d")
      o_or_u_or_both <- "both"  #representation level : over-, underrepresented or both
      
      #the api POST request
      post_args <- list(output_format = "tsv",
                        enrichment_method = "genome",
                        taxid = sprintf("%s",taxid), #string
                        limit_2_entity_type = "-56", #string
                        foreground = foreground,
                        o_or_u_or_both = o_or_u_or_both,
                        p_value_cutoff = sprintf("%s",p_value_cutoff),#string
                        FDR_cutoff =  sprintf("%s",FDR_cutoff)#string
      )
      # API request
      session$sendCustomMessage("handler_startLoader", c(13,40))
      request <- POST("https://agotool.org/api_orig", body = post_args, encode = "json")
      response<-rawToChar(content(request,"raw"))
      response<-gsub("PMID \\(PubMed IDentifier\\)", "PubMed", response)
      result_df <- read.csv(text = response, sep="\t", stringsAsFactors = FALSE)
      
      if(!is.null(result_df) & nrow(result_df)>0)
      {
        LiteratureResults <<- data.frame()
        LiteratureResults <<- result_df[,c(19, 1, 3, 6, 7, 13, 12, 11, 15)]
        names(LiteratureResults)[1] <<- "Source"
        names(LiteratureResults)[2] <<- "Term_ID"
        names(LiteratureResults)[3] <<- "Publication"
        names(LiteratureResults)[4] <<- "P-value"
        names(LiteratureResults)[5] <<- "FDR"
        names(LiteratureResults)[6] <<- "Term Size"
        names(LiteratureResults)[7] <<- "Query size"
        names(LiteratureResults)[8] <<- "Intersection Size"
        names(LiteratureResults)[9] <<- "Positive Hits"
        
        LogPvalue <<- format((-log10(as.numeric(as.character(format(LiteratureResults$`P-value`, scientific = F))))),format="e", digit=3)
        LogFDR <<- format((-log10(as.numeric(as.character(format(LiteratureResults$FDR , scientific = F))))),format="e", digit=3)
        enrScore <- enrich_score(LiteratureResults$`Intersection Size`, LiteratureResults$`Term Size`)
        
        LiteratureResults <<- as.data.frame(cbind("Source"= LiteratureResults$Source, "Term_ID"= LiteratureResults$Term_ID, "Publication"= LiteratureResults$Publication,
                                                  "P-value"= formatC( LiteratureResults$`P-value`, format = "e", digits = 2), "-log10Pvalue"= LogPvalue,
                                                  "FDR"= formatC( LiteratureResults$FDR, format = "e", digits = 2), "-log10FDR"= LogFDR,"Term Size"= LiteratureResults$`Term Size`,
                                                  "Query size"= LiteratureResults$`Query size`,"Intersection Size"= LiteratureResults$`Intersection Size`,
                                                  "Enrichment Score %"= enrScore,"Positive Hits"= LiteratureResults$`Positive Hits` ))
        
        LiteratureResults$`Enrichment Score %` <<- as.numeric(as.character(LiteratureResults$`Enrichment Score %`))
        LiteratureResults$`-log10Pvalue` <<- as.numeric(as.character(LiteratureResults$`-log10Pvalue`))
        LiteratureResults$`-log10FDR` <<- as.numeric(as.character(LiteratureResults$`-log10FDR`))
        LiteratureResults$`Positive Hits` <<- as.character(LiteratureResults$`Positive Hits`)
        LiteratureResults$Term_ID <<- as.character(LiteratureResults$Term_ID)
        
        LiteratureResults <<- LiteratureResults[with(LiteratureResults,order(-`-log10Pvalue`)),]
        session$sendCustomMessage("handler_startLoader", c(13,50))
        convertedGenesOutput <- gconvert(unlist(genesForLiterature$target), organism = gProfOrganism, target = gconvertTargetLiterature)
        session$sendCustomMessage("handler_startLoader", c(13,70))
        for (i in 1:nrow(LiteratureResults))
        {
          genesOutput<-c()
          initialSplitGenes <- strsplit(gsub(sprintf("%s.",taxid),"", LiteratureResults[["Positive Hits"]][i]), ";")[[1]]
          for (j in 1:length(initialSplitGenes))
          {
            inputGenes <- convertedGenesOutput[grepl(initialSplitGenes[j], convertedGenesOutput$input),]
            genesOutput[j] <- inputGenes$target[1] # in case of more than one matches Ens--> target namespace
          }
          LiteratureResults[["Positive Hits"]][i] <<- paste(unique(genesOutput), sep=",", collapse = ",")
        }
        param_literature <- "" # String variable for execution parameters to be printed
        param_literature <- paste("File: ", literatureSelect, "\nOrganism: ", literatureOrganism, "\nSignificance threshold :", literatureCorrectionMethod, "\nP-Value cut-off: ", literaturePvalue,"\nTerm_ID output: ", gconvertTargetLiterature, "\nDatabases: " , "PubMed", sep ="")
        output$literatureParameters <- renderText(param_literature)
        
        if (literatureCorrectionMethod == "P-value"){
          LiteratureResults <<- subset(LiteratureResults, select=-c( FDR,`-log10FDR`)) #remove the FDR and logFDR columns from the table
        }
        else {
          LiteratureResults <<- subset(LiteratureResults, select=-c(`-log10Pvalue`, `P-value`)) #remove the pvalue and logPvalue columns from the table
          colnames(LiteratureResults)[4] <<-"P-value"
          colnames(LiteratureResults)[5] <<- "-log10Pvalue"
          LiteratureResults <<- LiteratureResults[with(LiteratureResults, order(-`-log10Pvalue`)),]
        }
        
        all_literature <<- data.frame()
        all_literature <<- LiteratureResults ## table with links and space separated genes
        pubmedIds <- sapply(strsplit(all_literature$Term_ID, ":"), `[`, 2) # create the numeric part of the pubMedid (PMID:25764759--> 25764759 for the link)
        all_literature$Term_ID <<- paste("<a href='https://pubmed.ncbi.nlm.nih.gov/", pubmedIds, "' target='_blank'>", all_literature$Term_ID, "</a>", sep="")
        all_literature$`Positive Hits` <-  gsub(",", ", ", all_literature$`Positive Hits`)
        output$literatureTable <- renderTableFunc(all_literature, literatureSelect, 11, "HPA_gProfiler_Results'PUBMED Enrichment", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
        
        ##update sliders for the plots
        updateSliderInput(session, "literatureSliderScatter", "Choose a number of results to view:", min = 1, 
                          max = 1, value = 0, step = 1)
        updateSliderInput(session, "literatureSliderScatter", "Choose a number of results to view:", min = 1,
                          max = length(all_literature$Term_ID), value = 10, step = 1)
        
        updateSliderInput(session, "literatureSliderBarplot", "Choose a number of results to view:", min = 1, 
                          max = 1, value = 0, step = 1)
        updateSliderInput(session, "literatureSliderBarplot", "Choose a number of results to view:", min = 1,
                          max = length(all_literature$Term_ID), value = 10, step = 1)
        
        updateSliderInput(session, "literatureSliderHeatmap", "Choose a number of results to view:", min = 1, 
                          max = 1, value = 0, step = 1)
        updateSliderInput(session, "literatureSliderHeatmap", "Choose a number of results to view:", min = 2,
                          max = length(all_literature$Term_ID), value = 10, step = 1)
        
        updateSliderInput(session, "literatureSliderHeatmap2", "Choose a number of results to view:", min = 1, 
                          max = 1, value = 0, step = 1)
        updateSliderInput(session, "literatureSliderHeatmap2", "Choose a number of results to view:", min = 2,
                          max = length(all_literature$Term_ID), value = 10, step = 1)
        
        updateSliderInput(session, "literatureSliderNetwork", "Choose a number of results to view:", min = 1, 
                          max = 1, value = 0, step = 1)
        updateSliderInput(session, "literatureSliderNetwork", "Choose a number of results to view:", min = 2,
                          max = length(all_literature$Term_ID), value = 10, step = 1)
        
        updateSliderInput(session, "literatureSliderNetwork2", "Choose a number of results to view:", min = 1, 
                          max = 1, value = 0, step = 1)
        
        updateSliderInput(session, "literatureSliderNetwork2", "Choose a number of results to view:", min = 2,
                          max = length(all_literature$Term_ID), value = 10, step = 1)
        
        updateSliderInput(session, "literatureSliderNetwork3", "Choose a number of results to view:", min = 1, 
                          max = 1, value = 0, step = 1)
        
        updateSliderInput(session, "literatureSliderNetwork3", "Choose a number of results to view:", min = 2,
                          max = length(all_literature$Term_ID), value = 10, step = 1)
        
      }else session$sendCustomMessage("handler_alert", paste("No valid results found.", sep=""))
      session$sendCustomMessage("handler_startLoader", c(13,100))
      session$sendCustomMessage("handler_enableAllButtons", T) # now enable buttons again 
    }
  }
}
# This function calculates the enrichment score
# @param intersection_size: hit genes from query
# @param term_size: number of genes in database
# @return: score, calculated enrichment score
enrich_score <- function(intersection_size, term_size){
  intersection_size <- as.numeric(as.character(intersection_size))
  term_size <- as.numeric(as.character(term_size))
  score <- round((intersection_size/term_size) * 100, 2)
  return(score)}