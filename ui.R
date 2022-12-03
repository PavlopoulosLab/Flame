source("global_variables.R", local = TRUE)
source("functions/init.R", local = TRUE)
source("views/welcome.R", local = TRUE) 
source("views/input.R", local = TRUE)
source("views/gprofiler.R", local = TRUE)
source("views/agotool.R", local = TRUE)
source("views/literature.R", local = TRUE)
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
      menuItem("Functional Enrichment Analysis", tabName = "fEnrichment", icon = icon("gears"),
               menuSubItem("Ontologies & Pathways (gProfiler)", tabName = "gProfiler", icon = icon("dna")),
               menuSubItem("Domains & Diseases (aGOtool)", tabName = "aGOtool", icon = icon("heart-pulse"))),
      tags$hr(),
      menuItem("Literature Enrichment", tabName = "literature_search", icon = icon("book")),
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
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "loading-bar.css")),
    tags$head(tags$script(src = "Flame.js")),
    tags$head(tags$script(src = "rshiny_handlers.js")),
    tags$head(tags$script(src = "update_rshiny_values.js")),
    tags$head(tags$script(src = "slider_update_overide.js")),
    tags$head(tags$script(src = "loading-bar.js")),
    tags$head(tags$script(src = 'https://string-db.org/javascript/combined_embedded_network_v2.0.2.js')),
    useShinyjs(),
    
    div(class="ldBar", "data-preset"="bubble"),
    tabItems(
      tabItem(tabName = "welcome", welcomePage),
      tabItem("file_handler", inputPage),
      tabItem("gProfiler", generateGprofilerPage()),
      tabItem("aGOtool", generateAGOToolPage()),
      tabItem("literature_search", generateLiteraturePage()),
      tabItem("string_network", generateStringNetworkPage()),
      tabItem("gconvert", convertPage),
      tabItem("gorth", orthologyPage),
      tabItem("help", helpPage),
      tabItem("about", aboutPage)
    ),
    footer
  )
)
