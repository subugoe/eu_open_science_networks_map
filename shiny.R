#' simple dashboard showing contact points for EC open science policy implemntation
#'
#' re-uses a lot of code from SuperZip example from Joe Cheng <joe@rstudio.com>
#'
#' <https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example>
library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
#' load data
my_all <- readr::read_csv("data/data_table.csv")
#' define user interface
ui <- navbarPage("European Research Policy Table",
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
                       
                       h4("European Research Policy Table"),
                       p(
                         "The European Research Policy Table provides an overview of national contact points associated with European research infrastructure projects. By mapping the landscape, the European Research Policy Table aims to help connect researchers and other stakeholders with expert support on research policy and Open Science issues."
                       ),
                       p(
                         "The European Research Policy Table currently covers the following initiatives:
                         "
                       ),
                       
                       checkboxInput("openaire", "OpenAIRE (NOADs)", TRUE),
                       checkboxInput("rda", "Research Data Alliance (RDA) Europe Nodes", TRUE),
                       checkboxInput("egi", "EGI NLIs (International Liaisons)", TRUE),
                       checkboxInput("gofair", "GO FAIR", TRUE)
                       ,
                       p("Contacts:"),
                       p(
                         tags$a(href = "mailto:fava@sub.uni-goettingen.de", "Ilaria Favia"),
                         " | ",
                         tags$a(href = "mailto:bangert@sub.uni-goettingen.de", "Daniel Bangert")
                       )
                     ),
                     tags$div(
                       id = "cite",
                       'This applications is build by State and University Library Göttingen using open source tools. Source code hosted on ',
                       a(href = "https://github.com/njahn82/eu_science_policy_table", "GitHub.")
                     )
                   )
                 ),
                 tabPanel("Table View", 
                          dataTableOutput("table"))
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
    ifelse(input$gofair, "GoFAIR (TBC)", NA)
  })
  
  
  output$map <- renderLeaflet({
    my_all %>%
      filter(role %in% c(
        strands_oaire(),
        strands_rda(),
        strands_egi(),
        strands_gofair()
      )) %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(~ lng,
                 ~ lat,
                 popup = ~ content,
                 clusterOptions = markerClusterOptions())
  })
  output$table  <- renderDataTable({
    my_all %>%
      filter(role %in% c(
        strands_oaire(),
        strands_rda(),
        strands_egi(),
        strands_gofair()
      )) %>%
      select(2:5, Country)
  }, rownames = FALSE, filter = 'bottom')
}
#run app
shinyApp(ui, server)
