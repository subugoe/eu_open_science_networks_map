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
                       
                       shiny::selectInput(
                         "x",
                         label = NULL,
                         choices = c(
                           "OpenAIRE (NOADs)",
                           "RDA National Groups (chairs, from RDA website)",
                           "GoFAIR (TBC)"
                         ),
                         selected = "OpenAIRE (NOADs)",
                         multiple = FALSE
                       ),
                       p("Contact:"),
                       a(href="mailto:fava@sub.uni-goettingen.de", "Ilaria Fabia")
                     ),
                     tags$div(id="cite",
                              'Build by State and University Library Göttingen')
                   )
                 ))
#' data
server <- function(input, output, session) {
  points <- eventReactive(input$x, {
    my_all %>%
      filter(role == input$x)
  })
  
  output$map <- renderLeaflet({
    leaflet(points()) %>%
      addTiles() %>%
      addMarkers(~ lng, ~ lat, popup = ~ content, 
                 clusterOptions = markerClusterOptions())
  })
}
#run app
shinyApp(ui, server)
