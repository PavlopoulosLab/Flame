generateConvertDiv <- function(prefix) {
  switch(
    prefix,
    "gconvert" = {
      title <- "Gene ID Conversion"
      listLabel <- "1. Select list to convert:"
      buttonLabel <- "Convert IDs"
      selectComponent <- selectInput(
        paste0(prefix, "_target"),
        label = "3. Select target namespace:",
        selected = "ENTREZGENE",
        width = "80%",
        choices = c(NAMESPACES[["CORE"]], unlist(NAMESPACES[["SPECIAL"]]))
      )
    },
    "gorth" = {
      title <- "Orthology Search"
      listLabel <- "1. Select list for orthology search:"
      buttonLabel <- "Orthology search"
      selectComponent <- selectizeInput(
        paste0(prefix, "_target"),
        label = "3. Select target organism:",
        choices = NULL,
        multiple = F,
        selected = NULL,
        width = "80%",
        options = list(placeholder = 'Select an option or start typing...')
      )
    }
  )
  
  return(
    tags$div(
      tags$h3(title),
      tags$br(),
      fluidRow(
        column(
          4,
          selectInput(paste0(prefix, "_select"), listLabel, choices = NULL),
          actionButton(paste0(prefix, "_button"), buttonLabel,
                       icon = icon("paper-plane"), class = "submit_button")
        ),
        column(
          8,
          selectizeInput(
            inputId = paste0(prefix, "_organism"),
            label = "2. Select input organism:",
            choices = NULL,
            multiple = F,
            selected = NULL,
            width = "80%",
            options = list(placeholder = 'Select an option or start typing...')
          ),
          selectComponent,
        )
      ),
      tags$hr(),
      DT::dataTableOutput(paste0(prefix, "_table"))
    )
  )
}
