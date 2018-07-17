#' simple dashboard showing contact points for EC open science policy implemntation
#'
#' re-uses a lot of code from SuperZip example from Joe Cheng <joe@rstudio.com>
#'
#' <https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example>
library(shiny)
library(leaflet)
library(tidyverse)
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
                       
                       h2("Explore"),
                       p("Display contact points by EC open science policy strand"),
                       
                       checkboxInput("openaire", "OpenAIRE (NOADs)", TRUE),
                       checkboxInput("rda", "Research Data Alliance (RDA) Europe Nodes", TRUE),
                       checkboxInput("egi", "EGI NLIs (International Liaisons)", TRUE),
                       checkboxInput("gofair", "GO FAIR", TRUE)
                       ,
                       p("Contact:"),
                       a(href = "mailto:fava@sub.uni-goettingen.de", "Ilaria Favia")
                     ),
                     tags$div(id = "cite",
                              'Build by State and University Library Göttingen')
                   )
                 ))
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
      addMarkers( ~ lng,
                  ~ lat,
                  popup = ~ content,
                  clusterOptions = markerClusterOptions())
  })
}
#run app
shinyApp(ui, server)
