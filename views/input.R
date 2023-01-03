generateInputPage <- function() {
  tags$div(
    tags$h3("Input Lists"),
    tags$br(),
    fluidRow(
      class = "inputControlRow",
      generateCurrentListsColumn(),
      column(
        8,
        tabsetPanel(
          generateUploadPanel(),
          generateTextMiningPanel(),
          generateVolcanoPanel(),
          generateViewPanel()
        )
      )
    ),
    tags$hr(),
    tabsetPanel(
      id = "inputPlots",
      generateUpsetPlotPanel(),
      generateVolcanoPlotPanel()
    )
  )
}

generateCurrentListsColumn <- function() {
  column(
    4,
    tags$div(
      class = "checkListDiv",
      verbatimTextOutput("url_checked"),
      checkboxGroupInput(
        inputId = "checkboxLists",
        label = "Current lists:"
      ),
      checkboxInput("selectAll", "Select/Deselect All"),
      actionButton("rename", "Rename", icon("pencil")),
      actionButton("remove", "Remove", icon("trash"))
    )
  )
}

generateUploadPanel <- function() {
  tabPanel(
    title = "Upload", 
    icon = icon("upload"),
    tags$br(),
    tags$div(
      class = "floatLeft",
      textAreaInput(
        inputId = "textAreaList",
        label = "Paste input list:",
        placeholder = "Write or paste your list here.\nClick the Example Button to load an example list.",
        height = "270px",
        width = "290px") %>%
        shinyInput_label_embed(
          shiny_iconlink("circle-info") %>%
            bs_embed_popover(
              title = "Input can consist of mixed typed of IDs separated by comma, space, new line or tab."
            )
        ),
      actionButton("textSubmit", "Add to lists", icon("paper-plane")),
      actionButton("example", "Example", icon("bookmark")),
      actionButton("clear", "Clear", icon("broom"))
    ),
    tags$div(
      class = "floatLeft",
      fileInput("fileUpload", "or Upload from file(s):", multiple = T,
                accept = c(".tsv", ".csv", ".txt")) %>%
        shinyInput_label_embed(
          shiny_iconlink("circle-info") %>%
            bs_embed_popover(
              title = "Upload up to 10 files (up to 1MB each)."
            )
        )
    )
  )
}

generateTextMiningPanel <- function() {
  tabPanel(
    title = "Text-mining",
    icon = icon("envelope-open-text"),
    tags$br(),
    tags$div(
      fluidRow(
        column(
          5,
          tags$div(
            textAreaInput(
              inputId = "textmining_textinput",
              label = "1. Input text:",
              placeholder = "Write or paste a text here.\n\nClick the 'Load Example' button to load an example text.",
              resize = "vertical", height = "200px", width = "90%"),
            selectizeInput(
              inputId = "textmining_organism",
              label = "2. Select organism:",
              choices = ORGANISMS_FROM_FILE$print_name,
              selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
              multiple = F,
              width = "90%",
              options = list(placeholder = "Select an option or start typing...")
            ),
            actionButton("textmining_submit", "Submit", icon("paper-plane")),
            actionButton("textmining_addExample", "Example", icon("bookmark")),
            actionButton("textmining_clearText", "Clear", icon("broom"))
          )
        ),
        column(
          7,
          tags$div(
            id = "textmining_tagger_results",
            style = "display:none",
            tabsetPanel(
              tabPanel(
                "Identified Genes",
                dataTableOutput(outputId = "extracted_terms")
              ),
              tabPanel(
                "Annotated Text",
                wellPanel(htmlOutput(outputId = "extracted_text"))
              )
            ),
            tags$br(),
            actionButton("textmining_addList", "Add to lists", icon("paper-plane")),
            actionButton("textmining_delete", "Delete", icon("broom"))
          )
        )
      )
    )
  )
}

generateVolcanoPanel <- function() {
  tabPanel(
    title = "Volcano",
    icon = icon("volcano"),
    tags$br(),
    fluidRow(
      column(
        4,
        fileInput("volcanoUpload", "Upload gene fold change file:", multiple = F,
                  accept = c(".tsv", ".csv", ".txt")) %>%
          shinyInput_label_embed(
            shiny_iconlink("circle-info") %>%
              bs_embed_popover(
                title = "Upload a 3-column file (symbol, logFC, pvalue)."
              )
          ),
        actionButton("volcano_addExample", "Example", icon("bookmark")),
      ),
      column(
        8,
        dataTableOutput(outputId = "volcanoViewer")
      )
    )
  )
}

generateViewPanel <- function() {
  tabPanel(
    title = "View",
    icon = icon("eye"),
    tags$br(),
    selectInput(
      inputId = "selectView",
      label = "Select list to view:",
      choices = NULL,
      width = "100%"
    ),
    DT::dataTableOutput("selectedListView")
  )
}

generateUpsetPlotPanel <- function() {
  tabPanel(
    title = "Upset Plot",
    icon = icon("chart-column"),
    tags$div(
      tags$br(),
      radioButtons(
        inputId = "upsetMode",
        label = "Select mode:",
        choices = c("Intersection", "Distinct Combinations", "Union"),
        inline = TRUE
      ) %>%
        shinyInput_label_embed(
          shiny_iconlink("circle-info") %>%
            bs_embed_popover(
              title = "Select at least 2 files to create the Upset plot from the checkbox file list.
                  \nThe UpSet plot Intersection option visualizes the total number of common elements amongthe selected sets, even though they may also participate in other sets.
                  \nThe Distinct Combinations option visualizes the common number of genes, among chosen sets, that do not exist in any other set. This option is the closest to a Venn diagram.
                  \nThe Union option appends the unique elements among chosen sets and creates all possible combinations."
            )
        ),
      actionButton(inputId = "submitUpset", label = "Generate",
                   icon = icon("palette"), class = "submit_button"),
      upsetjsOutput("upsetjsView"),
      tags$br(),
      tags$br(),
      fluidRow(
        tags$div(
          class = "upsetMargin",
          column(2, textOutput("hoveredInfoLabel")),
          column(2, textOutput("hoveredListName")),
          column(8, textOutput("hoveredElements"))
        )
      )
    )
  )
}

generateVolcanoPlotPanel <- function() {
  tabPanel(
    title = "Volcano Plot",
    icon = icon("volcano"),
    tags$br(),
    fluidRow(
      column(
        9,
        plotlyOutput("volcanoPlot", height = "750px")
      ),
      column(
        3,
        tags$div(
          id = "volcanoPanel",
          sliderInput(
            inputId = "volcano_pvalue_slider",
            label = "Choose -log10Pvalue threshold:",
            min = 0, max = 5,
            value = DEFAULT_VOLCANO_LOG10PVALUE_THRESHOLD,
            step = DEFAULT_VOLCANO_LOG10PVALUE_STEP,
            width = "100%"
          ) %>%
            shinyInput_label_embed(
              shiny_iconlink("circle-info") %>%
                bs_embed_popover(
                  title = "0.05 pvalue == 1.30103 -log10pvalue\n0.01 pvalue == 2 -log10pvalue"
                )
            ),
          verbatimTextOutput("volcanoMetricConversions"),
          sliderInput(
            inputId = "volcano_fc_slider",
            label = "Choose |log2FC| threshold:",
            min = 0, max = 5,
            value = DEFAULT_VOLCANO_LOG2FC_THRESHOLD,
            step = DEFAULT_VOLCANO_LOG2FC_STEP,
            width = "100%"
          ),
          actionButton("volcanoRedraw", "Redraw",
                       icon("palette"), class = "submit_button"),
          actionButton("volcanoSubmit", "Add to lists", icon("paper-plane")),
          tags$br(),
          tags$br(),
          verbatimTextOutput("volcanoSelected")
        )
      )
    )
  )
}
