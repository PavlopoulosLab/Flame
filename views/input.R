inputPage <- tags$div(
  tags$h3("File Input"),
  tags$br(),
  tabsetPanel(
    id = "panels",
    tabPanel(
      "Upload Files", 
      fluidRow(
        column(
          4,
          tags$br(),
          fileInput("files", "1. Choose a File", multiple = TRUE,
                    accept = c(".tsv", ".csv", ".txt"))%>%
            shinyInput_label_embed(
              shiny_iconlink("circle-info") %>%
                bs_embed_popover(
                  title = "Upload up to 10 files (up to 1MB each).", content = "Choose a favorite", placement = "left"
                )
            )
        ),
        column(
          4, 
          tags$br(),
          textAreaInput(
            "text",
            "or 2. Paste your gene list",
            placeholder = "Write or paste your list here.\nClick the Example Button to load an example list.",
            height="80px") %>%
            shinyInput_label_embed(
              shiny_iconlink("circle-info") %>%
                bs_embed_popover(
                  title = "Input can consist of mixed typed of IDs separated by comma, space, new line or tab.", placement = "left"
                )
            ),
          actionButton("textSubmit", "Add to files ", icon("paper-plane")),
          actionButton("example", "Example", icon("bookmark")),
          actionButton("clear", "Clear", icon("broom"))
        ),
        column(
          4,
          tags$br(),
          verbatimTextOutput("url_checked"),
          checkboxGroupInput("checkboxFiles", label = ("3.Select files for Upset plot or Rename/Remove your uploaded Files")),
          checkboxInput("selectAll", "Select/Deselect All"),
          actionButton("rename", "Rename", icon("pencil")),
          actionButton("remove", "Remove", icon("trash"))
        ),
      ), 
      tags$br(),
      tags$hr(),
      actionButton("submitUpset", "Create Upset plot", class = "btn-submit"),
      tags$br(),
      tags$br(),
      column(
        4,
        radioButtons(
          "mode",
          "Select Mode",
          c("Intersection","Distinct Intersections", "Distinct per File", "Union"),
          inline = TRUE) %>%
          shinyInput_label_embed(
            shiny_iconlink("circle-info") %>%
              bs_embed_popover(
                title = "Select at least 2 files to create the Upset plot from the checkbox file list.
                                                    \nThe UpSet plot Intersection option visualizes the total number of common elements amongthe selected sets, even though they may also participate in other sets.
                                                    \nThe Distinct Intersections option visualizes the common number of genes, among chosen sets, that do not exist in any other set. This option is the closest to a Venn diagram.
                                                    \nThe Distinct elements per file shows the distinct elements of each input list.
                                                    \nThe Union option appends the unique elements among chosen sets and creates all possible combinations.
                                                    ", placement = "left"
              )
          )
      ),
      upsetjsOutput("upsetjs"),
      tags$br(),
      tags$br(),
      tags$br(),
      fluidRow(
        column(2, textOutput("Hovered_Set")),
        column(2, textOutput("hovered")),
        column(8, textOutput("hoveredElements"))
      ),
    ),
    tabPanel(
      "View Data",
      tags$br(),
      selectInput("selectView", "Select file to view", choices = NULL, width = "25%"),
      DT::dataTableOutput("contents")
    )
  )
)
