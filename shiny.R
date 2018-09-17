#' simple dashboard showing contact points for EC open science policy implementation
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
                         "This dashboard provides an overview of national contact points associated with European Open Science networks. By mapping the landscape, this application aims to help connect researchers and other stakeholders with expert support on research policy and Open Science issues."
                       ),
                       p(
                         "The following initiatives are currently covered:
                         "
                       ),
                       
                       checkboxInput("openaire", tags$a(href = "https://www.openaire.eu/contact-noads" ,"OpenAIRE (NOADs)"), TRUE),
                       checkboxInput("rda", tags$a(href = "https://www.rd-alliance.org/groups/national-groups", "Research Data Alliance (RDA) Europe Nodes"), TRUE),
                       checkboxInput("egi", tags$a(href = "https://wiki.egi.eu/wiki/NGI_International_Liaison" ,"EGI NLIs (International Liaisons)"), TRUE),
                       checkboxInput("gofair", tags$a(href = "https://www.go-fair.org/countries/map/", "GO FAIR"), TRUE)
                       ,
                       p('We welcome contributions to this application. 
                         Please contact us or create an issue on', tags$a(href = "https://github.com/subugoe/eu_open_science_networks_map", "GitHub")),
                       p(
                         tags$a(href = "mailto:fava@sub.uni-goettingen.de", "Ilaria Favia"),
                         " | ",
                         tags$a(href = "mailto:bangert@sub.uni-goettingen.de", "Daniel Bangert")
                       )
                     ),
                     tags$div(
                       id = "cite",
                       'This application is built by the State and University Library Göttingen using open source tools. Source code hosted on ',
                       a(href = "https://github.com/subugoe/eu_open_science_networks_map", "GitHub.")
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
    ifelse(input$gofair, "GO FAIR", NA)
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
                 clusterOptions = markerClusterOptions()) %>%
      addMiniMap()
  })
  output$table  <- renderDataTable({
    my_all %>%
      filter(role %in% c(
        strands_oaire(),
        strands_rda(),
        strands_egi(),
        strands_gofair()
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
    
  }, rownames = FALSE, filter = 'bottom')
}
#run app
shinyApp(ui, server)
