handleEnrichment <- function(selectEnrichFile, significance_threshold, organism, datasources, pvalue, output,barSelect2, 
                             scatterSelect, heatmapSelect, heatmapSelect2, networkSelect, networkSelect2, networkSelect3, gconvertTargetGprofiler, session){
  
  if (identical(file_data, list())) session$sendCustomMessage("handler_alert", "Please, first upload your data files.")
  else {
    genesForgprofiler <- file_data[file_names==selectEnrichFile][[1]]
    if (is.null(datasources)) session$sendCustomMessage("handler_alert", "Please, select one or more datasources.")
    else {
      session$sendCustomMessage("handler_disableAllButtons", T) # disable all buttons until execution is over
      session$sendCustomMessage("handler_startLoader", c(2,10)) # gprofiler
      session$sendCustomMessage("handler_disableSourcesTabs", T) # disable all datasources tab panels
      
      organism <- organismsFromFile[organismsFromFile$print_name == organism,]$gprofiler_ID
      linkOrganism <- organismsFromFile[organismsFromFile$gprofiler_ID == organism,]$KEGG
      ###
      input_genes <- genesForgprofiler
      genesForgprofiler <- gconvert(unlist(genesForgprofiler), organism = organism, target = "ENSG")
      session$sendCustomMessage("handler_startLoader", c(2,30))
      if (identical(genesForgprofiler, NULL)) session$sendCustomMessage("handler_alert", paste("Valid genes for analysis not found.", sep=""))
      else {
        genesForgprofiler <- genesForgprofiler[genesForgprofiler$target!="nan",] 
        genesForgprofiler <- genesForgprofiler[c("input","target", "name")]
        gostres_m <- gost(genesForgprofiler$target, source = datasources, evcodes = T, organism = organism,  correction_method = significance_threshold, user_threshold = pvalue )
        session$sendCustomMessage("handler_startLoader", c(2,50))
        if (identical(gostres_m, NULL)) session$sendCustomMessage("handler_alert", "Functional enrichment could not return any valid resutls.")
        else {
          ENRICHMENT_SCORE <- enrich_score(gostres_m$result$intersection_size, gostres_m$result$term_size)
          LOG_PVALUE <- format((-log10(as.numeric(as.character(gostres_m$result$p_value)))), format="e", digits = 2)
          gostres <<- data.frame()
          gostres <<- as.data.frame(cbind("Source"= gostres_m$result$source,"Term_ID"=gostres_m$result$term_id, "Function" =gostres_m$result$term_name,
                                          "P-value"= formatC(gostres_m$result$p_value, format = "e", digits = 2),"-log10Pvalue"=LOG_PVALUE, "Term Size"= gostres_m$result$term_size, 
                                          "Query size"= gostres_m$result$query_size, "Intersection Size"= gostres_m$result$intersection_size, "Enrichment Score %"= ENRICHMENT_SCORE,
                                          "Positive Hits"= gostres_m$result$intersection))
          gostres$`Enrichment Score %` <<- as.numeric(as.character(gostres$`Enrichment Score %`))
          gostres$`-log10Pvalue` <<- as.numeric(as.character(gostres$`-log10Pvalue`))
          gostres$`Positive Hits` <<- as.character(gostres$`Positive Hits`)
          for (i in 1:nrow(gostres)){ # converting gostres$`Positive Hits back to user $input here
            genesOutput <- c()
            initialSplitGenes <- strsplit(gostres$`Positive Hits`[i], ",")[[1]]
            
            for (j in 1:length(initialSplitGenes)) genesOutput[j] <- genesForgprofiler[grepl(initialSplitGenes[j], genesForgprofiler$target),]$input[1]
            gostres$`Positive Hits`[i] <<- paste(unique(genesOutput), sep=",", collapse = ",")
          }
          ### unidentified genes 
          positiveGenes <- paste(gostres$`Positive Hits`, collapse=",")
          positiveGenes <- strsplit(positiveGenes, ",")
          positiveGenes <- unique(unlist(positiveGenes))
          truefalse <- genesForgprofiler$input %in% positiveGenes 
          mergedGenes <- cbind(genesForgprofiler, truefalse)
          true <- mergedGenes [grepl("TRUE", mergedGenes$truefalse),]
          trueGenes <- true$input
          false <- mergedGenes [grepl("FALSE", mergedGenes$truefalse),]
          falseGenes<- false$input
          list.a <- as.list(trueGenes)
          list.b <- as.list(falseGenes)
          list.c <- as.list(as.character(input_genes[[1]]))
          list.d <- as.list(genesForgprofiler$input)
          nonCommonGenes <- c(setdiff(list.c, list.d), setdiff(list.b, list.a)) # genes in input and not in output 
          param_funcEnr <- "" # String variable for execution parameters to be printed
          param_funcEnr <- paste("File: ", selectEnrichFile, "\nOrganism: ", organism, "\nCorrection Method: ", significance_threshold, 
                                 "\nUser threshold: ", pvalue, "\nID type Output: ", gconvertTargetGprofiler, "\nDatabases: " , sep ="")
          param_funcEnr <- paste(param_funcEnr, paste(datasources, collapse=', ' ), "\n", sep="")
          output$gprofParameters <- renderUI(
            box(
              title = "Parameters", 
              width = NULL,
              status = "primary", 
              solidHeader = TRUE,
              collapsible = TRUE,
              verbatimTextOutput("gprof_parameters")
            )
          )
          output$gprof_parameters <- renderText(param_funcEnr)
          ###
          nonCommonGenes <- paste(nonCommonGenes, collapse=",")
          nonCommonGenes <- strsplit(nonCommonGenes, ",")
          nonCommonGenes <- as.character(unlist(nonCommonGenes))
          geneNumber1 <- length(nonCommonGenes)
          output$nothit <- renderUI(
            box(
              title = paste ("Unidentified Elements ", "(", geneNumber1, ")", sep=""), 
              width = NULL,
              status = "primary", 
              solidHeader = TRUE,
              collapsible = TRUE,
              verbatimTextOutput("nonCommon")
            )
          )
          output$nonCommon <- renderText(nonCommonGenes)
          clearTables(output) # resetting any previous tables before updating
          sources_list <- c("GO:MF", "GO:CC", "GO:BP", "KEGG", "REAC", "WP", "TF", "MIRNA", "CORUM", "HPA", "HP")
          all_gost <<- data.frame()
          # START name conversion ####
          # convert the names of the output genes in accordance with the user's preference  output type
          output$notconvert <- renderUI("") # resetting UI box for unconverted genes (for after switching to USERINPUT)
          if (gconvertTargetGprofiler != "USERINPUT"){
            convertedGenesOutput <- gconvert(unique(unlist(genesForgprofiler$input)), organism = organism, target = gconvertTargetGprofiler) #genesForgprofiler$name
            ### Unconverted genes
            notConverted <- convertedGenesOutput[grepl("nan", convertedGenesOutput$target),]
            notConverted <- notConverted$input
            notConverted <- as.character(unlist(notConverted))
            geneNumber2 <- length(notConverted)
            
            output$notconvert <- renderUI(
              box(
                title = paste("Unconverted Genes " , "(", geneNumber2, ")", sep=""),  
                width = NULL,
                status = "primary", 
                solidHeader = TRUE,
                collapsible = TRUE,
                verbatimTextOutput("notConverted")
              )
            )
            output$notConverted <- renderText(notConverted)
            for (i in 1:nrow(gostres))
            {
              genesOutput <- c()
              initialSplitGenes <- strsplit(gostres$`Positive Hits`[i], ",")[[1]]
              for (j in 1:length(initialSplitGenes))
              {
                inputGenes <- convertedGenesOutput[grepl(initialSplitGenes[j], convertedGenesOutput$input),]
                
                genesOutput[j] <- inputGenes$target[1] # in case of more than one matches Ens--> target namespace
                if (genesOutput[j] == "nan") genesOutput[j] <- inputGenes$input[1]
              }
              
              gostres$`Positive Hits`[i] <<- paste(unique(genesOutput), sep=",", collapse = ",")
            }
          }
          # END name conversion ####
          session$sendCustomMessage("handler_startLoader", c(2,70))
          for (i in 1:length(datasources)){
            session$sendCustomMessage("handler_enableSourceTab", match(datasources[i], sources_list)) # enable current datasource tab panel
            if (datasources[i] == "GO:MF"){
              GOMF_gost <- gostres[grepl("^GO:MF$", gostres$Source),]
              if (nrow(GOMF_gost) > 0){
                GOMF_gost$`Positive Hits` <-  gsub(",", ", ", GOMF_gost$`Positive Hits`)
                GOMF_gost$Term_ID <- paste("<a href='https://www.ebi.ac.uk/QuickGO/term/", GOMF_gost$Term_ID, "' target='_blank'>", GOMF_gost$Term_ID, "</a>", sep="")
                all_gost <<- rbind(all_gost, GOMF_gost)
                output$table_gomf <- renderTableFunc(GOMF_gost, selectEnrichFile, 11, "GO:MF_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_gomf <- DT::renderDataTable(GOMF_gost)
            } else if(datasources[i] == "GO:CC"){
              GOCC_gost <- gostres[grepl("^GO:CC$", gostres$Source),]
              if (nrow(GOCC_gost) > 0){
                GOCC_gost$`Positive Hits` <-  gsub(",", ", ", GOCC_gost$`Positive Hits`)
                GOCC_gost$Term_ID <- paste("<a href='https://www.ebi.ac.uk/QuickGO/term/", GOCC_gost$Term_ID, "' target='_blank'>", GOCC_gost$Term_ID, "</a>", sep="")
                all_gost <<- rbind(all_gost, GOCC_gost)
                output$table_gocc <- renderTableFunc(GOCC_gost, selectEnrichFile, 11, "GO:CC_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_gocc <- DT::renderDataTable(GOCC_gost)
            } else if(datasources[i] == "GO:BP"){
              GOBP_gost <- gostres[grepl("^GO:BP$", gostres$Source),]
              
              if (nrow(GOBP_gost) > 0){
                GOBP_gost$`Positive Hits` <-  gsub(",", ", ", GOBP_gost$`Positive Hits`)
                GOBP_gost$Term_ID <- paste("<a href='https://www.ebi.ac.uk/QuickGO/term/", GOBP_gost$Term_ID, "' target='_blank'>", GOBP_gost$Term_ID, "</a>", sep="")
                all_gost <<- rbind(all_gost, GOBP_gost)
                output$table_gobp <- renderTableFunc(GOBP_gost, selectEnrichFile, 11, "GO:BP_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_gobp <- DT::renderDataTable(GOBP_gost)
            } else if(datasources[i] == "KEGG"){
              KEGG_gost <- gostres[grepl("^KEGG$", gostres$Source),]
              if (nrow(KEGG_gost) > 0){
                #creates links with @param KEGG_gost and return@updated KEGG_gost with links and gene colors
                
                for(j in 1:length(KEGG_gost$`Positive Hits`)){
                  kegg_genes <- gconvert(strsplit(KEGG_gost$`Positive Hits`[j], ",")[[1]], organism = organism, target = "ENTREZGENE_ACC")$target
                  kegg_genes <- paste(kegg_genes, collapse =",")
                  kegg_genes <- gsub(",", "%20orange+", kegg_genes)
                  kegg_genes <- (paste("+",kegg_genes, sep=""))
                  kegg_genes <- (paste(kegg_genes, "%20orange", sep=""))
                }
                KEGG_gost$Term_ID <- paste("<a href='https://www.genome.jp/kegg-bin/show_pathway?", gsub("KEGG:", linkOrganism, KEGG_gost$Term_ID), 
                                           kegg_genes, "' target='_blank'>", KEGG_gost$Term_ID, "</a>", sep="")
                KEGG_gost$`Positive Hits` <-  gsub(",", ", ", KEGG_gost$`Positive Hits`)
                all_gost <<- rbind(all_gost, KEGG_gost)
                output$table_kegg <- renderTableFunc(KEGG_gost, selectEnrichFile, 11, "KEGG_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_kegg <- DT::renderDataTable(KEGG_gost)
            } else if(datasources[i] == "REAC"){
              REAC_gost <- gostres[grepl("^REAC$", gostres$Source),]
              if (nrow(REAC_gost) > 0){
                REAC_gost$Term_ID <- paste("<a href='https://reactome.org/content/detail/", gsub("REAC:", "", REAC_gost$Term_ID), "' target='_blank'>", REAC_gost$Term_ID, "</a>", sep="")
                REAC_gost$`Positive Hits` <-  gsub(",", ", ", REAC_gost$`Positive Hits`)
                all_gost <<- rbind(all_gost, REAC_gost)
                output$table_reac <- renderTableFunc(REAC_gost, selectEnrichFile, 11, "REAC_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_reac <- DT::renderDataTable(REAC_gost)
            } else if(datasources[i] == "WP"){
              WP_gost <- gostres[grepl("^WP$", gostres$Source),]
              if (nrow(WP_gost) > 0){
                WP_gost$Term_ID <- paste("<a href='https://www.wikipathways.org/index.php/Pathway:", gsub("WP:", "", WP_gost$Term_ID), "' target='_blank'>", WP_gost$Term_ID, "</a>", sep="")
                WP_gost$`Positive Hits` <-  gsub(",", ", ", WP_gost$`Positive Hits`)
                all_gost <<- rbind(all_gost,WP_gost)
                output$table_wp <- renderTableFunc(WP_gost, selectEnrichFile, 11, "WP_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_wp <- DT::renderDataTable(WP_gost)
            } else if(datasources[i] == "TF"){
              TF_gost <- gostres[grepl("^TF$", gostres$Source),]
              if (nrow(TF_gost) > 0){
                TF_gost$`Positive Hits` <-  gsub(",", ", ", TF_gost$`Positive Hits`)
                all_gost <<- rbind(all_gost,TF_gost)
                output$table_tf <- renderTableFunc(TF_gost, selectEnrichFile, 11, "TF_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_tf <- DT::renderDataTable(TF_gost)
            } else if(datasources[i] == "MIRNA"){
              MIRNA_gost <- gostres[grepl("^MIRNA$", gostres$Source),]
              if (nrow(MIRNA_gost) > 0){
                MIRNA_gost$Term_ID <- paste("<a href='http://mirtarbase.cuhk.edu.cn/php/search.php?org=", linkOrganism,"&opt=mirna_id&kw=",
                                            gsub("MIRNA:", "", MIRNA_gost$Term_ID), "' target='_blank'>", MIRNA_gost$Term_ID, "</a>", sep="")
                MIRNA_gost$`Positive Hits` <-  gsub(",", ", ", MIRNA_gost$`Positive Hits`)
                all_gost <<- rbind(all_gost,MIRNA_gost)
                output$table_mirna <- renderTableFunc(MIRNA_gost, selectEnrichFile, 11, "MIRNA_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_mirna <- DT::renderDataTable(MIRNA_gost)
            } else if(datasources[i] == "CORUM"){
              CORUM_gost <- gostres[grepl("^CORUM$", gostres$Source),]
              if (nrow(CORUM_gost) > 0){
                CORUM_gost$`Positive Hits` <-  gsub(",", ", ", CORUM_gost$`Positive Hits`)
                all_gost <<- rbind(all_gost,CORUM_gost)
                output$table_corum <- renderTableFunc(CORUM_gost, selectEnrichFile, 11, "CORUM_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_corum <- DT::renderDataTable(CORUM_gost)
            } else if(datasources[i] == "HPA"){
              HPA_gost <- gostres[grepl("^HPA$", gostres$Source),]
              if (nrow(HPA_gost) > 0){
                HPA_gost$`Positive Hits` <-  gsub(",", ", ", HPA_gost$`Positive Hits`)
                all_gost <<- rbind(all_gost,HPA_gost)
                output$table_hpa <- renderTableFunc(HPA_gost, selectEnrichFile, 11, "HPA_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_hpa <- DT::renderDataTable(HPA_gost)
            } else if(datasources[i] == "HP"){
              HP_gost <- gostres[grepl("^HP$", gostres$Source),]
              if (nrow(HP_gost) > 0){
                HP_gost$`Positive Hits` <-  gsub(",", ", ", HP_gost$`Positive Hits`)
                all_gost <<- rbind(all_gost, HP_gost)
                output$table_hp <- renderTableFunc(HP_gost, selectEnrichFile, 11, "HP_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
              }
              else output$table_hp <- DT::renderDataTable(HP_gost)
            }
          }
          if (nrow(all_gost) > 0){
            session$sendCustomMessage("handler_enableSourceTab", 0)
            all_gost <<- all_gost[with(all_gost,order(-`-log10Pvalue`)),]
            
            output$table_all <- renderTableFunc(all_gost, selectEnrichFile, 11, "ALL_gProfiler_Results", "Positive Hits",c(2,3,4,5,6,7,8,9,10,11))
          }
          session$sendCustomMessage("handler_startLoader", c(2,90))
          drawManhattan(gostres_m, output) # call function to draw manhattan and the table with the clicked events
          
          updatePlotDataSources(unique(gostres_m$result$source), session) # Updating hit datasource selection boxes in Plots
          
          # Updating SliderInput in Plots
          all_gost_barselect <- all_gost[grepl(paste(barSelect2, collapse="|"), all_gost$Source),]
          updateSliderInput(session, "sliderBarplot", "Choose a number:", min = 1, max = length(all_gost_barselect$Term_ID), value = 10, step = 1)
          
          all_gost_scatterSelect <- all_gost[grepl(scatterSelect, all_gost$Source),]
          updateSliderInput(session, "sliderScatter", "Choose a number:", min = 1, max = length(all_gost_scatterSelect$Term_ID), value = 10, step = 1)
          
          gostres_heatmapSelect <- gostres[grepl(heatmapSelect, gostres$Source),]
          updateSliderInput(session, "sliderHeatmap", "Choose a number:", min = 2, max = length(gostres_heatmapSelect$Term_ID), value = 10, step = 1)
          
          gostres_heatmapSelect2 <- gostres[grepl(heatmapSelect2, gostres$Source),]
          updateSliderInput(session, "sliderHeatmap2", "Choose a number of results to view:", min = 2, max = length(gostres_heatmapSelect$Term_ID), value = 10, step = 1)
          
          gostres_networkSelect <- gostres[grepl(networkSelect, gostres$Source),]
          updateSliderInput(session, "sliderNetwork", "Choose a number:", min = 2, max = length(gostres_networkSelect$Term_ID), value = 10, step = 1)
          
          gostres_networkSelect2 <- gostres[grepl(networkSelect2, gostres$Source),]
          updateSliderInput(session, "sliderNetwork2", "Choose a number:", min = 2, max = length(gostres_networkSelect2$Term_ID), value = 10, step = 1)
          
          gostres_networkSelect3 <- gostres[grepl(networkSelect3, gostres$Source),]
          updateSliderInput(session, "sliderNetwork3", "Choose a number:", min = 2, max = length(gostres_networkSelect3$Term_ID), value = 10, step = 1)
        }
      }
      session$sendCustomMessage("handler_startLoader", c(2,100))
      
      session$sendCustomMessage("handler_enableAllButtons", T) # now enable buttons again
    }
  }
}

# Sub Routines ####

# void function that clears all current data from the gost datasources tables
# alternatively do with javascript
clearTables <- function(output){
  output$table_gomf <- renderDataTable(c())
  output$table_gocc <- renderDataTable(c())
  output$table_gobp <- renderDataTable(c())
  output$table_kegg <- renderDataTable(c())
  output$table_reac <- renderDataTable(c())
  output$table_wp <- renderDataTable(c())
  output$table_tf <- renderDataTable(c())
  output$table_mirna <- renderDataTable(c())
  output$table_corum <- renderDataTable(c())
  output$table_hpa <- renderDataTable(c())
  output$table_hp <- renderDataTable(c())
}

# draws Manhattan plot in another tab "Plots"
# @param gostres1, a copy of gostres at the time this function is called (during the FE analysis)
# @return void
drawManhattan <- function(gostres1, output){ 
  session$sendCustomMessage("handler_startLoader", c(3,100))
  # click_event <<- list() # TODO probably remove this global var
  output$manhattan_table <- DT::renderDataTable(as.data.frame(list())) # clears previous results
  output$manhattan <- renderPlotly({
    
    gostplot(
      gostres1,
      capped = TRUE,
      interactive = T,
      pal = c(`GO:MF` = "#dc3912", `GO:BP` = "#ff9900", `GO:CC` = "#109618", KEGG =
                "#dd4477", REAC = "#3366cc", WP = "#0099c6", TF = "#5574a6", MIRNA = "#22aa99", HPA =
                "#6633cc", CORUM = "#66aa00", HP = "#990099") #, event_register(p, 'plotly_click')
    )})
  session$sendCustomMessage("handler_finishLoader", 3)
}

# This function updates the datasource selection boxes of some plots at the Plots tab
updatePlotDataSources <- function(gostresSources, session){
  
  if (!is.na(match("GO:BP", gostresSources))) selected <- "GO:BP"
  else if (!is.na(match("GO:MF", gostresSources))) selected <- "GO:MF"
  else if (!is.na(match("GO:CC", gostresSources))) selected <- "GO:CC"
  else if (!is.na(match("KEGG", gostresSources))) selected <- "KEGG"
  else if (!is.na(match("REAC", gostresSources))) selected <- "REAC"
  else if (!is.na(match("WP", gostresSources))) selected <- "WP"
  else if (!is.na(match("TF", gostresSources))) selected <- "TF"
  else if (!is.na(match("HP", gostresSources))) selected <- "HP"
  else if (!is.na(match("HPA", gostresSources))) selected <- "HPA"
  else if (!is.na(match("MIRNA", gostresSources))) selected <- "MIRNA"
  else if (!is.na(match("CORUM", gostresSources))) selected <- "CORUM"
  
  # updateSelectInput(session, "barSelect", label="Select datasource", choices = gostresSources, selected = selected)
  updatePickerInput(session, "barSelect2", label="Select datasources", choices = gostresSources, selected = selected)
  updateSelectInput(session,"scatterSelect", label="Select datasource", choices = gostresSources, selected = selected)
  updateSelectInput(session,"heatmapSelect", label="Select datasource", choices = gostresSources, selected = selected)
  updateSelectInput(session,"heatmapSelect2", label="Select datasource", choices = gostresSources, selected = selected)
  updateSelectInput(session,"networkSelect", label="Select datasource", choices = gostresSources, selected = selected)
  updateSelectInput(session,"networkSelect2", label="Select datasource", choices = gostresSources, selected = selected)
  updateSelectInput(session,"networkSelect3", label="Select datasource", choices = gostresSources, selected = selected)
  
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
