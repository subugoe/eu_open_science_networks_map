#' get urls from wikidata
library(tidyverse)
library(WikidataR)
my_df <- readr::read_csv("data/data_table.csv")
#' helper function to fetch homepages (P856) from Wikidata
url_fetch <- function(wikidata_id = NULL) {
  WikidataR::get_property(wikidata_id) %>%
    purrr::map("claims") %>%
    purrr::map("P856") %>%
    purrr::map("mainsnak") %>%
    purrr::map_df("datavalue") %>%
    mutate(wikidata_id = wikidata_id)
    
}
wd_urls <- purrr::map_df(my_df$`Institution Wikidata ID`, url_fetch)
# do some url disambiguation
wd_urls %>%
  mutate(scheme = map(value, function(x)
    httr::parse_url(x)) %>%
      map_chr("scheme")) %>%
  mutate(hostname = map(value, function(x)
    httr::parse_url(x)) %>%
      map_chr("hostname")) %>%
  mutate(homepage = paste0(scheme, "://", hostname)) %>%
  select(homepage, wikidata_id) %>%
  distinct() -> wd_links
# join datasets and export
my_df %>%
  left_join(wd_links, by = c("Institution Wikidata ID" = "wikidata_id")) %>%
  # update content
  mutate(
    content = paste0(
      "<b><a href='",
      ifelse(!is.na(homepage), homepage, wikidata_link),
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
  ) %>%
  readr::write_csv("data/data_table.csv")
