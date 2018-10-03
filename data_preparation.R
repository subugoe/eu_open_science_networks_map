#' tyding spreadsheet
library(tidyverse)
library(googledrive)
googledrive::drive_download(as_id("1_CQUEDJsrXaCFcPvC_ypGqorhvprbeCGvdw8erAkTAQ"), "data/refine_all.xlsx", overwrite = TRUE)
my_df <- readxl::read_xlsx("data/refine_all.xlsx") %>%
  mutate(wikidata_link = paste0("https://www.wikidata.org/wiki/", `Institution Wikidata ID`)) %>%
  mutate(contact_1 = ifelse(
    is.na(contact),
    NA,
    paste0("<a href='mailto:", email, "'>", contact, "</a><br/>")
  )) %>%
  mutate(contact_2 = ifelse(
    is.na(contact_secondary),
    NA,
    paste0(
      "<a href='mailto:",
      email_secondary,
      "'>",
      contact_secondary,
      "</a><br/>"
    )
  )) %>%
  mutate(contact_3 = ifelse(
    is.na(contact_tertiary),
    NA,
    paste0(
      "<a href='mailto:",
      email_tertiary,
      "'>",
      contact_tertiary,
      "</a>"
    )
  )) %>%
  mutate(
    content = paste0(
      "<b><a href='",
      wikidata_link,
      "'>",
      Name,
      "</a></b><p>",
      role,
      ":</br>",
      ifelse(is.na(contact_1), "", contact_1),
      ifelse(is.na(contact_2), "", contact_2),
      ifelse(is.na(contact_3), "", contact_3),
      "</p>"
    )
  )
#' test leaflet
library(leaflet)
my_df %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers( ~ lng,
              ~ lat,
              popup = ~ content,
              clusterOptions = markerClusterOptions())
#' export it
write_csv(my_df, "data/data_table.csv")
