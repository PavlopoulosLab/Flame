generateStringNetworkPage <- function() {
  tags$div(
    tags$h3("Protein-Protein Interaction Network"),
    tags$br(),
    fluidRow(
      width = 12,
      column(
        4,
        selectInput("STRINGnetworkSelect", "Select file for analysis", choices = NULL),
      ),
      column(
        4,
        selectizeInput(
          "STRINGnetworkOrganism", 
          label = "Select organism:",
          choices = ORGANISMS_FROM_FILE$print_name,
          multiple = F,
          selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
          width = "65%",
          options = list(placeholder = 'Select an option or start typing...')
        ),
        tags$br(),
        radioButtons(
          inputId = 'STRINGnetworkType', 
          label = 'Select interaction type:', 
          choiceNames = list ("Full network","Physical subnetwork"),
          choiceValues = list('functional', 'physical')
        ) %>%
          shinyInput_label_embed(
            shiny_iconlink("circle-info") %>%
              bs_embed_popover(
                title = "Full network: edges indicate both functional and physical associations.\n
              Physical subnetwork: edges indicate existence of a physical complex.",
                placement = "left"
              )
          )
      ),
      column(
        4,
        radioButtons(
          inputId = 'STRINGnetworkEdges', 
          label = 'Select meaning of network edges:', 
          choiceNames = list ("Evidence","Confidence"),
          choiceValues = list('evidence', 'confidence')
        ) %>%
          shinyInput_label_embed(
            shiny_iconlink("circle-info") %>%
              bs_embed_popover(
                title = "Evidence: edge colors indicate the type of interaction evidence.\nConfidence: edge thickness indicates the strength of data support based on the interaction score", placement = "left"
              )
          ),
        tags$br(),
        pickerInput(
          inputId = 'STRINGnetworkScore', 
          label = 'Select interaction score cut-off:', 
          choices = list (`highest confidence (0.900)` = 900,
                          `high confidence (0.700)` = 700,
                          `medium confidence (0.400)` = 400,
                          `low confidence (0.150)` = 150),
          selected = list(`medium confidence (0.400)` = 400)
        )
      )
    ),
    actionButton("runStringNetwork", "Run analysis", icon("paper-plane"), class = "submit_button"),
    tags$br(),
    tags$hr(),
    tags$br(),
    verbatimTextOutput("networkParameters"),
    tags$br(),
    htmlOutput("string_legend"),
    tags$br(),
    tags$br(),
    htmlOutput("string_export_buttons"),
    tags$br(),
    htmlOutput("string_viewer")
  )
}
