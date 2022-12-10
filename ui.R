source("global_variables.R", local = TRUE)
source("functions/init.R", local = TRUE)
source("views/welcome.R", local = TRUE) 
source("views/input.R", local = TRUE)
source("views/enrichment.R", local = TRUE)
source("views/network.R", local = TRUE) 
source("views/conversion.R", local = TRUE) 
source("views/help.R", local = TRUE)
source("views/about.R", local = TRUE) 
source("views/footer.R", local = TRUE)

initializeUIApp()

dashboardPage(
  title = "Flame",
  skin = "yellow",
  dashboardHeader(
    titleWidth = "356px",
    title = tags$a(
      href = 'http://bib.fleming.gr:8084/app/flame/',
      tags$img(src = 'logo.png')
    )
  ),
  dashboardSidebar(
    width = "356px",
    sidebarMenu(
      id = "sideBarId",
      menuItem("Welcome", tabName = "welcome", icon = icon("house")),
      tags$hr(),
      menuItem("File Input", tabName = "file_handler", icon = icon("file-contract")),
      tags$hr(),
      menuItem("Functional Enrichment", tabName = "functional_enrichment", icon = icon("gears")),
      tags$hr(),
      menuItem("Literature Enrichment", tabName = "literature_enrichment", icon = icon("book")),
      tags$hr(),
      menuItem("Network Analysis", tabName = "string_network", icon=icon("diagram-project")),
      tags$hr(),
      menuItem("Conversion", tabName = "Conversion", icon = icon("right-left"),
               menuSubItem("Gene ID Conversion", tabName = "gconvert", icon = icon("angles-right")),
               menuSubItem("Orthology Search", tabName = "gorth", icon = icon("angles-right"))),
      tags$hr(),
      menuItem("Help", tabName = "help", icon = icon("question")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "Flame.css")),
    tags$head(tags$script(src = "js/rshiny_handlers.js")),
    tags$head(tags$script(src = "js/update_rshiny_values.js")),
    tags$head(tags$script(src = 'https://string-db.org/javascript/combined_embedded_network_v2.0.2.js')),
    useShinyjs(),
    div(class="ldBar", "data-preset" = "bubble"), # TODO remove after replaced with modals
    tabItems(
      tabItem(tabName = "welcome", welcomePage),
      tabItem("file_handler", inputPage),
      tabItem("functional_enrichment", generateEnrichmentPage("functional")),
      tabItem("literature_enrichment", generateEnrichmentPage("literature")),
      tabItem("string_network", generateStringNetworkPage()),
      tabItem("gconvert", generateConvertPage()),
      tabItem("gorth", generateOrthologyPage()),
      tabItem("help", helpPage),
      tabItem("about", aboutPage)
    ),
    footer
  )
)
