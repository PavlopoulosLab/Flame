# When the user uploads files, this event appends the file names and 
# data into two respective static global lists: file_names, inputGeneLists
# @param inputFiles: multiple uploaded files (list)
handleInputFiles <- function(inputFiles) {
  if (!(length(inputFiles$name)+length(file_names)) > FILE_LIMIT) {
    for (i in 1:nrow(inputFiles)){
      if(is.na(match(inputFiles$name[i], file_names))){
        tempData <- readChar(inputFiles$datapath[i], file.info(inputFiles$datapath[i])$size)
        tempData <- trimFunction(tempData)
        if (nrow(tempData) > 0){
          file_names[[length(file_names)+1]] <<- inputFiles$name[i]
          inputGeneLists[[length(inputGeneLists)+1]] <<- tempData # read.csv(inputFiles$datapath[i], header=F) # old
          colnames(inputGeneLists[[length(inputGeneLists)]]) <<- file_names[[length(file_names)]]
        }
      } else
        renderWarning(paste0("A file named ", inputFiles$name[i] ," already exists."))
    }
    updateFileBoxes()
  }
  else
    renderWarning(paste0("You have exceed the maximum numbers of files (", FILE_LIMIT, ")."))
}

# Event that parses the data from the text area where the user can paste a list of genes
# The gene list is added to checkboxes/ upset/ etc with a random name suffix (gene_list_random_name) 
# The user can then rename/remove the list and select it for enrichment in the same way with uploaded files
# Also checks the size of the text using the object.size() function
handleTextSubmit <- function(inputText, prefix) {
  flag <- T
  if (inputText == "")
    renderWarning("Please, paste your gene list.")
  else {
    if (object.size(inputText) < OBJECT_SIZE_LIMIT){ # estimate object.size(inputText)
      if (!(length(inputText)+length(file_names)) > FILE_LIMIT) {
        # create random name and DF
        random_name <- getRandomName(prefix)
        text_data_frame <- trimFunction(inputText)
        # update globals: file_names and inputGeneLists 
        if (nrow(text_data_frame) > 0){
          file_names[[length(file_names)+1]] <<- random_name
          inputGeneLists[[length(inputGeneLists)+1]] <<- text_data_frame
          colnames(inputGeneLists[[length(inputGeneLists)]]) <<- file_names[[length(file_names)]]
          # update controls
          updateTextAreaInput(session, "text", value = "")
          updateFileBoxes()
        } else
          renderWarning("Please, paste your gene list.")
      } else {
        renderWarning(paste0("You have exceed the maximum numbers of files (", FILE_LIMIT, ")."))
        flag <- F
      }
    } else 
    {
      updateTextAreaInput(session, "text", value = "")
      renderWarning("Make sure your list is < 1MB.")
    }
  }
  output$url_checked <- renderText({""})
  return(flag)
}

# Function that checks json file data created form POST request
# The user imported gene lists are added to checkboxes/ upset/ etc with a random name suffix (gene_list_random_name)
# @param inFile(string): path to saved json from from the API POST request
# @param session(Shiny session)
# @return error_flag(boolean): check if !correct data format and passed limit checks
parse_import_data <- function(inFile, session){
  error_flag <- F
  raw_json <- fromJSON(inFile)
  
  if (!(length(raw_json)) > FILE_LIMIT) { # each json raw is a gene list
    for (i in 1:length(raw_json)){
      if (object.size(raw_json[[i]]) < OBJECT_SIZE_LIMIT){
        raw_json[[i]] <- trimList(raw_json[[i]]) # removing spaces and commas
        raw_json[[i]] <- raw_json[[i]][ raw_json[[i]] != '' ]
        if (length(raw_json[[i]]) > 0){ # if list not empty
          
          # random_name <- getRandomName(names(raw_json)[i]) # json list names are unique already
          file_names[[length(file_names)+1]] <<- names(raw_json)[i]
          inputGeneLists[[length(inputGeneLists)+1]] <<- as.data.frame(raw_json[[i]])
          colnames(inputGeneLists[[length(inputGeneLists)]]) <<- file_names[[length(file_names)]]
          updateFileBoxes()
          
        }
      } else{
        error_flag <- T
        renderWarning("Make sure every gene list is < 1MB.")
        break
      }
    }
  } else{
    error_flag <- T
    renderWarning(paste0("Make sure you import <= (", FILE_LIMIT, ") gene lists."))
  }
  return(error_flag)
}
# This event creates an example data input in the text area
handleRandomExample <- function(){
  exampleGeneList <- readRDS("random_genes.rds")
  exampleGeneList <- sample(c(exampleGeneList), RANDOM_GENES_NUMBER, replace = FALSE)
  exampleGeneList <- paste(exampleGeneList, collapse = ',')
  exampleGeneList <- gsub(",","\n", exampleGeneList)
  updateTextAreaInput(session, "text", value = exampleGeneList)
}

# This event clears the text area
handleClearExample <- function(){
  updateTextAreaInput(session, "text", value = "")
}

# This event selects or deselects all the uploaded files 
# by calling the updateFileCheckboxes function
# @param "Select/Deselect Button" click
handleSelectAllFiles <- function(selectAll){
  if (selectAll == 0) updateCheckboxGroupInput(session,"checkboxFiles",choices=file_names)
  else updateCheckboxGroupInput(session,"checkboxFiles",choices=file_names,selected=file_names)
}

# This event keeps the positions of the files to be renamed in the global variable global_positions
# and passes the execution to javascript, which calls modal windows for the user
# to rename each selected file
# @param "Rename Button" click
handlePrepareRename <- function(checkBoxFiles){
  global_positions <<- which(file_names %in% checkBoxFiles)
  session$sendCustomMessage("handler_rename", checkBoxFiles)
}

# This function is responsible for renaming files and data columns based
# on javascript pop-up text windows, assigned by the user
# @param js_fileNames: assigned by the updateFileNames() function
# of update_rshiny_value.js
handleRename <- function(js_fileNames){
  for (i in 1:length(global_positions)){
    if(js_fileNames[i] != ""){
      if (file_names[global_positions[[i]][1]] != js_fileNames[[i]][1]){
        if(is.na(match(js_fileNames[i], file_names))){
          file_names[global_positions[i]] <<- js_fileNames[i]
          colnames(inputGeneLists[[global_positions[i]]]) <<- js_fileNames[i]
        } else
          renderWarning(paste0("Duplicate name: ", js_fileNames[i], " . Name didn't change.")) 
      }
    } else
      renderWarning("Empty name found. Name didn't change.")
  }
  updateFileBoxes()
}

# This event removes selected checkbox entries from file names and data lists
# and then updates checkboxgroup, by calling the updateFileCheckboxes function
# Also considers empty selections
# @param "Remove Button" click
handleRemove <- function(checkboxFiles){
  positions <- which(file_names %in% checkboxFiles)
  if (!identical(positions, integer(0))){
    file_names <<- file_names[-positions]
    inputGeneLists <<- inputGeneLists[-positions]
    updateFileBoxes()
  }
}

# This event creates an upset plot for selected files
# @param "Create Upset plot" Click
handleSubmitUpset <- function() {
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Handling UpSet Plot.</p>")
    checkboxFiles <- input$checkboxFiles
    mode <- input$mode
    
    positions <- which(file_names %in% checkboxFiles)
    if (!identical(positions, integer(0)) && (length(positions) >= 2)){ # only execute if files have been selected
      positions <- which(file_names %in% checkboxFiles)
      upset_list <<- ""
      for (i in 1:length(positions)) upset_list <<- c(upset_list, (inputGeneLists[positions[i]][[1]]))
      upset_list <<- upset_list[upset_list != ""]
      create_upset(mode)
      output$Hovered_Set <- renderText(("Hovered Set:")) # displays the labels of hovered sets
    }
    else
      renderWarning("Please, select 2 or more files.")
  }, error = function(e) {
    print(paste("Error :  ", e))
    renderError("Problem with UpSet Plot.")
  }, finally = {
    removeModal()
  })
}

# This event uses the create_upset function to recreate the upset plot depending on the mode
handleModeUpset <- function(mode){
  tryCatch({
    renderModal("<h2>Please wait.</h2><br /><p>Handling UpSet Plot.</p>")
    if (length(upset_list) > 1)
      create_upset(mode)
  }, error = function(e) {
    print(paste("Error :  ", e))
    renderError("Problem with UpSet Plot.")
  }, finally = {
    removeModal()
  })
}

# This event displays the selected file
# @param input$Select_view
handleSelectView <- function(selectView){
  gene_tables<- as.data.frame(inputGeneLists[file_names == selectView])
  output$contents <- DT::renderDataTable(gene_tables, server = FALSE, 
                                         extensions = 'Buttons',
                                         options = list(
                                           pageLength = 10,
                                           "dom" = 'T<"clear">lBfrtip',
                                           buttons = list(list(extend='copy', filename='Gene_Data_Table'),
                                                          list(extend= 'csv', filename='Gene_Data_Table'), 
                                                          list(extend='excel', filename='Gene_Data_Table'), 
                                                          list(extend='pdf', filename='Gene_Data_Table'),
                                                          list(extend='print', filename='Gene_Data_Table')
                                           )
                                         ), rownames= FALSE)
}

# Displays the title of the hovered upon column (combination of files)
# Also displays the names of the elements(genes) of each column when the user hovers on the respective column
handleUpsetHover <- function(upsetjs_hover){
  hoveredGenes <- paste(as.character(upsetjs_hover$elems), collapse = '\n')
  hoveredGenes <- gsub("\n",", ", hoveredGenes)
  output$hovered <- renderText({upsetjs_hover$name})
  output$hoveredElements <- renderText({hoveredGenes})
}

# This event (upsetjs_click) depending on the mode creates a modal window 
# requesting confirmation for appending the results  to the files or
handleUpsetClick <- function(mode, upsetjs_click){
  if (!identical(as.character(upsetjs_click$elems), character(0))){
    if (mode == "Intersection"){
      showModal(modalDialog(
        title="Intersection Results",
        paste("Would you like to add :", upsetjs_click$name, " to the input file list? ",  sep=" "),
        footer = tagList(actionButton("intersection_ok", "OK"),
                         modalButton("Cancel"))
      ))
    }
    else if (mode == "Distinct Intersections"){
      showModal(modalDialog(
        title=" Distinct Intersections Results",
        paste("Would you like to add :", upsetjs_click$name, " to the input file list? ",  sep=" "),
        footer = tagList(actionButton("distinct_ok", "OK"),
                         modalButton("Cancel"))
      ))
    } else if (mode == "Union"){ # Union Mode
      showModal(modalDialog(
        title=" Union Results",
        paste("Would you like to add :", upsetjs_click$name, " to the input file list? ",  sep=" "),
        footer = tagList(actionButton("union_ok", "OK"),
                         modalButton("Cancel"))
      ))
    } else if (mode == "Distinct per File"){
      showModal(modalDialog(
        title=" Distinct Results per File",
        paste("Would you like to add :", upsetjs_click$name, " to the input file list? ",  sep=" "),
        footer = tagList(actionButton("distinct_per_file_ok", "OK"),
                         modalButton("Cancel"))
      ))
    }
  }
}

# Intersection modal window control
# If the user confirms (OK), a file with the results is created
# and appended to the lists for further analysis
# Also appoints a random name (random generator) as a suffix to the initial name
handleIntersection <- function(upsetjs_click){
  mode_append_list(upsetjs_click, "Intersection_list")
  removeModal()
  updateFileBoxes()
}

# Same for Distinct, as above
handleDistinct <- function(upsetjs_click){
  mode_append_list(upsetjs_click, "Distinct_Intersections_list")
  removeModal()
  updateFileBoxes()
}

# Same for Union, as above
handleUnion <- function(upsetjs_click){
  mode_append_list(upsetjs_click, "Union_list")
  removeModal()
  updateFileBoxes()
}

# Same for Distinct per File, as above
handleDistinctPerFile <- function(upsetjs_click){
  mode_append_list(upsetjs_click, "Distinct_list")
  removeModal()
  updateFileBoxes()
}

# Sub Routines ####

# This function updates all File-related boxes across the app
# i.e. checkbox groups and select inputs
# @param session: R shiny session for environment control
updateFileBoxes <- function() {
  updateCheckboxGroupInput(session, "checkboxFiles", choices = file_names)
  updateSelectInput(session, "functional_enrichment_file", choices = file_names)
  updateSelectInput(session, "literature_enrichment_file", choices = file_names)
  updateSelectInput(session, "selectView", choices = file_names)
  updateSelectInput(session, "selectUpset", choices = file_names)
  updateSelectInput(session, "gconvert_select", choices = file_names)
  updateSelectInput(session, "gorth_select", choices = file_names)
  updateSelectInput(session, "aGOtoolSelect", choices = file_names)
  updateSelectInput(session, "literatureSelect", choices = file_names)
  updateSelectInput(session, "STRINGnetworkSelect", choices = file_names)
  
}

# Function that parses the user text input gene, removing commas, spaces, carriage returns and new lines
# @param textDF: the text data frame to be parsed
# return: the parsed data frame (trimmed, empty values removed)
trimFunction <- function(textDF){
  textDF <- gsub(",", "\n", textDF)
  textDF <- gsub(" ", "\n", textDF)
  textDF <- gsub("\t", "\n", textDF)
  textDF <- gsub("\r", "\n", textDF)
  textDF <- str_split(textDF, "\n")
  textDF <- lapply(textDF, function(z){ z[!is.na(z) & z != ""]})
  textDF <- as.data.frame(textDF)
  return(textDF)
}

trimList <- function(inList){
  inList <- gsub(",", "", inList)
  inList <- gsub(" ", "", inList)
  inList <- gsub("\t", "", inList)
  inList <- gsub("\r", "", inList)
  return(inList)
}

# This function draws the Upset plot
# Can be called either from button either from radioGroup
create_upset <- function(mode){
  if(mode =="Distinct Intersections"){
    # upset_listDistinct <- createListForDistinct()
    output$upsetjs <- renderUpsetjs({
      
      upsetjs()%>% fromList( upset_list, order.by = "cardinality",
                             limit = NULL,
                             shared = NULL,
                             shared.mode = "click",
                             colors = NULL)%>%
        interactiveChart()%>%
        generateDistinctIntersections()%>%
        chartLabels(combination.name = "Distinct Intersection Size")%>% 
        chartFontSizes(font.family="Segoe UI", chart.label = "18px", set.label="10px")%>%
        chartTheme(color="#383f4f")%>%
        chartLayout(width.ratios=c(0.2, 0.1, 0.7), bar.padding=0.3)
    })
  }
  else if (mode =="Intersection"){
    output$upsetjs <- renderUpsetjs({
      
      upsetjs()%>% fromList( upset_list, order.by = "cardinality",
                             limit = NULL,
                             shared = NULL,
                             shared.mode = "click",
                             colors = NULL)%>%
        interactiveChart()%>%
        generateIntersections()%>%
        chartLabels(combination.name = "Intersection Size")%>% 
        chartFontSizes(font.family="Segoe UI", chart.label = "18px", set.label="10px")%>%
        chartTheme(color="#383f4f")%>%
        chartLayout(width.ratios=c(0.2, 0.1, 0.7), bar.padding=0.3)
    })
  }
  else if (mode =="Union"){
    output$upsetjs <- renderUpsetjs({
      
      upsetjs()%>% fromList( upset_list, order.by = "cardinality",
                             limit = NULL,
                             shared = NULL,
                             shared.mode = "click",
                             colors = NULL)%>%
        interactiveChart()%>%
        generateUnions()%>%
        chartLabels(combination.name = "Union Size")%>% 
        chartFontSizes(font.family="Segoe UI", chart.label = "18px", set.label="10px")%>%
        chartTheme(color="#383f4f")%>%
        chartLayout(width.ratios=c(0.2, 0.1, 0.7), bar.padding=0.3)
    })
  } else if (mode == "Distinct per File"){
    upset_listDistinct <- createListForDistinct()
    if (!identical(upset_listDistinct, NULL)){
      output$upsetjs <- renderUpsetjs({
        
        upsetjs()%>% fromList( upset_listDistinct, order.by = "cardinality",
                               limit = NULL,
                               shared = NULL,
                               shared.mode = "click",
                               colors = NULL)%>%
          interactiveChart()%>%
          # generateDistinctIntersections()%>%
          chartLabels(combination.name = "Distinct Size")%>% 
          chartFontSizes(font.family="Segoe UI", chart.label = "18px", set.label="10px")%>%
          chartTheme(color="#383f4f")%>%
          chartLayout(width.ratios=c(0.2, 0.1, 0.7), bar.padding=0.3)
      })
    }else
    {
      updateRadioButtons(session, "mode", selected = "Union")
      renderWarning("No distinct genes found.")
    }
  }
}

# This function appends the two global file variables: file_names and inputGeneLists
# @param mode_list: one of "Intersection_list", "Distinct_Intersections_list" and "Union_list"
mode_append_list<- function(upsetjs_click, mode_list){
  random_name <- getRandomName(paste(mode_list, upsetjs_click$name, sep="_"))
  file_names[[length(file_names)+1]] <<- random_name
  inputGeneLists[[length(inputGeneLists)+1]] <<- as.data.frame(as.character(upsetjs_click$elems))
  colnames(inputGeneLists[[length(inputGeneLists)]]) <<-file_names[[length(file_names)]]
}

# This function creates the upset list for distinct combo values out of the global upset_list variable
# @return distinct combos list
createListForDistinct <- function(){
  distList <- c()
  uniqueGenes <- unique(as.character(unlist(upset_list))) # getting unique gene names from all "checked" lists that were sent for upset plot
  for (i in 1:length(uniqueGenes)){
    # listName <- paste(names(list.search(upset_list, uniqueGenes[i] %in% .)), collapse="&") # list.search from rlist package
    listName <- names(list.search(upset_list, uniqueGenes[i] %in% .))
    if (length(listName) == 1){ # if gene is included in more than one list, ignore
      if (identical(distList[[listName]], NULL)){ # if combo list does not exist, create and name it
        distList[[length(distList)+1]] <- list() # create
        names(distList)[[length(distList)]] <- listName # and name
      }
      distList[[listName]] <- c(unlist(distList[[listName]]), uniqueGenes[i]) # append gene at the corresponding commbo list
    }
  }
  return(distList)
}

# generate random sufix for file name, where the final name does not already exist
# @param prefix (String): depending on source, either from input text list, or from upset plot click
# @return name (String), prefix_suffix
getRandomName <- function(prefix){
  suffix <- paste(sample(c(0:9, letters, LETTERS), 6), collapse="")
  while (!is.na(match(paste(prefix, suffix, sep="_"), file_names))) suffix <- paste(sample(c(0:9, letters, LETTERS), 6), collapse="")
  name <- paste(prefix, suffix, sep="_")
  return(name)
}

