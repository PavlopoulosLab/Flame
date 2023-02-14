generateStringNetworkPage <- function() {
  tags$div(
    tags$h3("Protein-Protein Interaction Network"),
    tags$br(),
    fluidRow(
      width = 12,
      column(
        4,
        selectInput("STRINGnetworkSelect", "1. Select list:",
                    choices = NULL, width = "80%",),
        selectizeInput(
          inputId = "string_network_organism", 
          label = "2. Select organism:",
          choices = NULL,
          multiple = F,
          selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
          width = "80%",
          options = list(placeholder = "Select an option or start typing...")
        )
      ),
      column(
        4,
        radioButtons(
          inputId = "STRINGnetworkType", 
          label = "3. Select interaction type:", 
          choiceNames = list("Full network", "Physical subnetwork"),
          choiceValues = list("functional", "physical")
        ) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Full network: edges indicate both functional and physical associations.\nPhysical subnetwork: edges indicate existence of a physical complex."
              )
          ),
        radioButtons(
          inputId = "STRINGnetworkEdges",
          label = "4. Select meaning of network edges:", 
          choiceNames = list ("Evidence", "Confidence"),
          choiceValues = list("evidence", "confidence")
        ) %>%
          bsplus::shinyInput_label_embed(
            bsplus::shiny_iconlink("circle-info") %>%
              bsplus::bs_embed_popover(
                title = "Evidence: edge colors indicate the type of interaction evidence.\nConfidence: edge thickness indicates the strength of data support based on the interaction score"
              )
          )
      ),
      column(
        4,
        pickerInput(
          inputId = "STRINGnetworkScore", 
          label = "5. Select interaction score cut-off:", 
          choices = list (`highest confidence (0.900)` = 900,
                          `high confidence (0.700)` = 700,
                          `medium confidence (0.400)` = 400,
                          `low confidence (0.150)` = 150),
          selected = 400
        )
      )
    ),
    actionButton("runStringNetwork", "Run analysis", icon("paper-plane"), class = "submit_button"),
    tags$br(),
    tags$hr(),
    verbatimTextOutput("networkParameters"),
    htmlOutput("string_legend"),
    tags$br(),
    htmlOutput("string_export_buttons"),
    tags$br(),
    htmlOutput("string_viewer")
  )
}
