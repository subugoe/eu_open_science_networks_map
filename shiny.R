#' simple dashboard showing contact points for EC open science policy implementation
#'
#' re-uses a lot of code from SuperZip example from Joe Cheng <joe@rstudio.com>
#'
#' <https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example>
library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(writexl)
#' load data
my_all <- readr::read_csv("data/data_table.csv")
#' define user interface
ui <- navbarPage("European Open Science Networks (Beta)",
                 id = "nav",
                 tabPanel(
                   "Interactive map",
                   div(
                     class = "outer",
                     
                     tags$head(# Include our custom CSS
                       includeCSS("styles.css")),
                     leafletOutput("map", width = "100%", height = "100%"),
                     absolutePanel(
                       id = "controls",
                       class = "panel panel-default",
                       fixed = TRUE,
                       draggable = TRUE,
                       top = 60,
                       left = "auto",
                       right = 20,
                       bottom = "auto",
                       width = 330,
                       height = "auto",
                       
                       h4("European Open Science Networks"),
                       p(
                         "European Open Science Networks provides an overview of national contact points associated with Open Science initiatives in Europe."
                       ),
                       p(
                         "Filter by network:
                         "
                       ),
                       
                       checkboxInput("openaire", tags$a(href = "https://www.openaire.eu/contact-noads" ,"OpenAIRE (NOADs)"), TRUE),
                       checkboxInput("rda", tags$a(href = "https://www.rd-alliance.org/groups/national-groups", "Research Data Alliance (RDA) Europe Nodes"), TRUE),
                       checkboxInput("egi", tags$a(href = "https://wiki.egi.eu/wiki/NGI_International_Liaison" ,"EGI NLIs (International Liaisons)"), TRUE),
                       checkboxInput("gofair", tags$a(href = "https://www.go-fair.org/countries/map/", "GO FAIR"), TRUE)
                       ,
                       checkboxInput("geant", tags$a(href = "https://www.geant.org/About/NRENs", "Géant NRENs"), TRUE)
                       ,
                       checkboxInput("prace", tags$a(href = "http://www.prace-ri.eu/members/", "PRACE members"), TRUE)
                       ,
                       p('Contact us:'),
                       p(
                         tags$a(href = "mailto:fava@sub.uni-goettingen.de", "Ilaria Favia"),
                         " | ",
                         tags$a(href = "mailto:bangert@sub.uni-goettingen.de", "Daniel Bangert")
                       )
                     ),
                     tags$div(
                       id = "cite",
                       'This application is built by the Göttingen State and University Library using open source tools. Source code hosted on ',
                       a(href = "https://github.com/subugoe/eu_open_science_networks_map", "GitHub.")
                     )
                   )
                 ),
                 tabPanel("Table View", 
                          dataTableOutput("table"),
                          downloadButton("download_csv", "Download (csv)"),
                          downloadButton("download_xlsx", "Download (xlsx)")
                 ),
                 tabPanel("About",
                          includeMarkdown("about.md"))
                 )
#' data
server <- function(input, output, session) {
  strands_oaire <- eventReactive(input$openaire, {
    ifelse(input$openaire, "OpenAIRE (NOADs)", NA)
  })
  strands_rda <- eventReactive(input$rda, {
    ifelse(input$rda, "RDA Europe (Nodes)", NA)
  })
  strands_egi <- eventReactive(input$egi, {
    ifelse(input$egi, "EGI NLIs (International Liaison)", NA)
  })
  strands_gofair <- eventReactive(input$gofair, {
    ifelse(input$gofair, "GO FAIR", NA)
  })
  strands_geant <- eventReactive(input$geant, {
    ifelse(input$geant, "Geant NREN", NA)
  })
  strands_prace <- eventReactive(input$prace, {
    ifelse(input$prace, "PRACE Member", NA)
  })
  
  
  output$map <- renderLeaflet({
    my_all %>%
      filter(role %in% c(
        strands_oaire(),
        strands_rda(),
        strands_egi(),
        strands_gofair(),
        strands_geant(),
        strands_prace()
      )) %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(~ lng,
                 ~ lat,
                 popup = ~ content,
                 clusterOptions = markerClusterOptions()) %>%
      addMiniMap()
  })
  data_input <- reactive({
    my_all %>%
      filter(role %in% c(
        strands_oaire(),
        strands_rda(),
        strands_egi(),
        strands_gofair(),
        strands_geant(),
        strands_prace()
      )) %>%
      unite(contact_email, contact, email) %>% 
      unite(contact_email_2, contact_secondary, email_secondary) %>%
      unite(contact_email_3, contact_tertiary, email_tertiary) %>%
      gather(4:6, key = type, value = contact) %>%
      separate(contact, c("Contact", "Email"), sep = "_") %>% 
      select(Initiative = role, 
             Institution = Name, 
             Country, City, Contact, Email) %>%
      filter(!Email == "NA")
  })
  output$table  <- renderDataTable({
    data_input()
  }, rownames = FALSE, filter = 'bottom')
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("eu_os_networks", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(data_input(), file)
    }
  )
  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste("eu_os_networks", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(data_input(), file)
    }
  )
}
#run app
shinyApp(ui, server)
