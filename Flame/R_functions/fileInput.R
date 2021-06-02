# When the user uploads files, this event appends the file names and 
# data into two respective static global lists: file_names, file_data
# @param inputFiles: multiple uploaded files (list)
# @param session: R shiny session for environment control
handleInputFiles <- function(inputFiles, session){
  if (!(length(inputFiles$name)+length(file_names)) > FILE_LIMIT) {
    for (i in 1:nrow(inputFiles)){
      if(is.na(match(inputFiles$name[i], file_names))){
        tempData <- readChar(inputFiles$datapath[i], file.info(inputFiles$datapath[i])$size)
        tempData <- trimFunction(tempData)
        if (nrow(tempData) > 0){
          file_names[[length(file_names)+1]] <<- inputFiles$name[i]
          file_data[[length(file_data)+1]] <<- tempData # read.csv(inputFiles$datapath[i], header=F) # old
          colnames(file_data[[length(file_data)]]) <<- file_names[[length(file_names)]]
        }
      } else session$sendCustomMessage("handler_alert", paste("A file named ", inputFiles$name[i] ," already exists.", sep = ""))
    }
    updateFileBoxes(session)
  }
  else session$sendCustomMessage("handler_alert", paste("You have exceed the maximum numbers of files (", FILE_LIMIT, ").", sep = ""))
}

# Event that parses the data from the text area where the user can paste a list of genes
# The gene list is added to checkboxes/ upset/ etc with a random name suffix (gene_list_random_name) 
# The user can then rename/remove the list and select it for enrichment in the same way with uploaded files
# Also checks the size of the text using the object.size() function
handleTextSubmit <- function(inputText, prefix, session){
  flag <- T
  if (inputText == "") session$sendCustomMessage("handler_alert", "Please, paste your gene list.")
  else {
    if (object.size(inputText) < 1048576){ # estimate object.size(inputText)
      if (!(length(inputText)+length(file_names)) > FILE_LIMIT) {
        # create random name and DF
        random_name <- getRandomName(prefix)
        text_data_frame <- trimFunction(inputText)
        # update globals: file_names and file_data 
        if (nrow(text_data_frame) > 0){
          file_names[[length(file_names)+1]] <<-  random_name
          file_data[[length(file_data)+1]] <<- text_data_frame
          colnames(file_data[[length(file_data)]]) <<- file_names[[length(file_names)]]
          # update controls
          updateTextAreaInput(session, "text", value = "")
          updateFileBoxes(session)
        } else session$sendCustomMessage("handler_alert", "Please, paste your gene list.")
      }else {
        session$sendCustomMessage("handler_alert", paste("You have exceed the maximum numbers of files (", FILE_LIMIT, ").", sep = ""))
        flag <- F
      }
    } else 
    {
      updateTextAreaInput(session, "text", value = "")
      session$sendCustomMessage("handler_alert", paste("Make sure your list is < 1MB.", sep = ""))
    }
  }
  output$url_checked <- renderText({""})
  return(flag)
}

# This event creates an example data input in the text area
handleRandomExample <- function(session){
  exampleGeneList <- readRDS("random_genes.rds")
  exampleGeneList <- sample(c(exampleGeneList),100,replace = FALSE)
  exampleGeneList <- paste(exampleGeneList, collapse = ',')
  exampleGeneList <- gsub(",","\n", exampleGeneList)
  updateTextAreaInput(session, "text", value = exampleGeneList)
}

# This event clears the text area
handleClearExample <- function(session){
  updateTextAreaInput(session, "text", value = "")
}

# This event selects or deselects all the uploaded files 
# by calling the updateFileCheckboxes function
# @param "Select/Deselect Button" click
handleSelectAllFiles <- function(selectAll, session){
  if (selectAll == 0) updateCheckboxGroupInput(session,"checkboxFiles",choices=file_names)
  else updateCheckboxGroupInput(session,"checkboxFiles",choices=file_names,selected=file_names)
}

# This event keeps the positions of the files to be renamed in the global variable global_positions
# and passes the execution to javascript, which calls modal windows for the user
# to rename each selected file
# @param "Rename Button" click
handlePrepareRename <- function(checkBoxFiles, session){
  global_positions <<- which(file_names %in% checkBoxFiles)
  session$sendCustomMessage("handler_rename", checkBoxFiles)
}

# This function is responsible for renaming files and data columns based
# on javascript pop-up text windows, assigned by the user
# @param js_fileNames: assigned by the updateFileNames() function
# of update_rshiny_value.js
handleRename <- function(js_fileNames, session){
  for (i in 1:length(global_positions)){
    if(js_fileNames[i] != ""){
      if (file_names[global_positions[[i]][1]] != js_fileNames[[i]][1]){
        if(is.na(match(js_fileNames[i], file_names))){
          file_names[global_positions[i]] <<- js_fileNames[i]
          colnames(file_data[[global_positions[i]]]) <<- js_fileNames[i]
        } else session$sendCustomMessage("handler_alert", paste("Duplicate name: ", js_fileNames[i], " . Name didn't change.", sep="")) 
      }
    } else session$sendCustomMessage("handler_alert", paste("Empty name found. Name didn't change.", sep=""))
  }
  updateFileBoxes(session)
}

# This event removes selected checkbox entries from file names and data lists
# and then updates checkboxgroup, by calling the updateFileCheckboxes function
# Also considers empty selections
# @param "Remove Button" click
handleRemove <- function(checkboxFiles, session){
  positions <- which(file_names %in% checkboxFiles)
  if (!identical(positions, integer(0))){
    file_names <<- file_names[-positions]
    file_data <<- file_data[-positions]
    updateFileBoxes(session)
  }
}

# This event creates an upset plot for selected files
# @param "Create Upset plot" Click
handleSubmitUpset <- function(checkboxFiles, mode, output, session){
  
  positions <- which(file_names %in% checkboxFiles)
  if (!identical(positions, integer(0)) && (length(positions) >= 2)){ # only execute if files have been selected
    session$sendCustomMessage("handler_disableAllButtons", T) # disable all buttons until execution is over
    session$sendCustomMessage("handler_startLoader", c(0,30))
    positions <- which(file_names %in% checkboxFiles)
    upset_list <<- ""
    for (i in 1:length(positions)) upset_list <<- c(upset_list, (file_data[positions[i]][[1]]))
    upset_list <<- upset_list[upset_list != ""]
    session$sendCustomMessage("handler_startLoader", c(0,70))
    create_upset(mode, output)
    output$Hovered_Set <- renderText(("Hovered Set:")) # displays the labels of hovered sets
    session$sendCustomMessage("handler_enableAllButtons", T) # disable all buttons until execution is over
    session$sendCustomMessage("handler_startLoader", c(0,100))
    session$sendCustomMessage("handler_finishLoader", 0)
  }
  else session$sendCustomMessage("handler_alert", "Please, select 2 or more files.")
}

# This event uses the create_upset function to recreate the upset plot depending on the mode
handleModeUpset <- function(mode, output, session){
  if (length(upset_list) > 1){
    session$sendCustomMessage("handler_disableAllButtons", T) # disable all buttons until execution is over
    session$sendCustomMessage("handler_startLoader", c(0,30))
    create_upset(mode, output)
    session$sendCustomMessage("handler_startLoader", c(0,100))
    session$sendCustomMessage("handler_finishLoader", 0)
    session$sendCustomMessage("handler_enableAllButtons", T) # disable all buttons until execution is over
  }
}

# This event displays the selected file
# @param input$Select_view
handleSelectView <- function(selectView, output, session){
  session$sendCustomMessage("handler_startLoader", c(1,30))
  gene_tables<- as.data.frame(file_data[file_names == selectView])
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
  session$sendCustomMessage("handler_startLoader", c(1,100))
  session$sendCustomMessage("handler_finishLoader", 1)
}

# Displays the title of the hovered upon column (combination of files)
# Also displays the names of the elements(genes) of each column when the user hovers on the respective column
handleUpsetHover <- function(upsetjs_hover, output){
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
handleIntersection <- function(upsetjs_click, session){
  mode_append_list(upsetjs_click, "Intersection_list")
  removeModal()
  updateFileBoxes(session)
}

# Same for Distinct, as above
handleDistinct <- function(upsetjs_click, session){
  mode_append_list(upsetjs_click, "Distinct_Intersections_list")
  removeModal()
  updateFileBoxes(session)
}

# Same for Union, as above
handleUnion <- function(upsetjs_click, session){
  mode_append_list(upsetjs_click, "Union_list")
  removeModal()
  updateFileBoxes(session)
}

# Same for Distinct per File, as above
handleDistinctPerFile <- function(upsetjs_click, session){
  mode_append_list(upsetjs_click, "Distinct_list")
  removeModal()
  updateFileBoxes(session)
}

# Sub Routines ####

# This function updates all File-related boxes across the app
# i.e. checkbox groups and select inputs
# @param session: R shiny session for environment control
updateFileBoxes <- function(session) {
  updateCheckboxGroupInput(session, "checkboxFiles", choices = file_names)
  updateSelectInput(session, "selectEnrichFile", choices = file_names)
  updateSelectInput(session, "selectView", choices = file_names)
  updateSelectInput(session, "selectUpset", choices = file_names)
  updateSelectInput(session, "gconvert_select", choices = file_names)
  updateSelectInput(session, "gorth_select", choices = file_names)
  updateSelectInput(session, "aGOtoolSelect", choices = file_names)
  updateSelectInput(session, "literatureSelect", choices = file_names)
  updateSelectInput(session, "STRINGnetworkSelect", choices = file_names)
  
}

# Function that parses the user text input gene , removing commas, spaces, carriage returns and new lines
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

# This function draws the Upset plot
# Can be called either from button either from radioGroup
create_upset <- function(mode, output){
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
      session$sendCustomMessage("handler_alert", paste("No distinct genes found.", sep = ""))
      
      
    }
  }
}

# This function appends the two global file variables: file_names and file_data
# @param mode_list: one of "Intersection_list", "Distinct_Intersections_list" and "Union_list"
mode_append_list<- function(upsetjs_click, mode_list){
  random_name <- getRandomName(paste(mode_list, upsetjs_click$name, sep="_"))
  file_names[[length(file_names)+1]] <<- random_name
  file_data[[length(file_data)+1]] <<- as.data.frame(as.character(upsetjs_click$elems))
  colnames(file_data[[length(file_data)]]) <<-file_names[[length(file_names)]]
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

