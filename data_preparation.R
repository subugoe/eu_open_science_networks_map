#' tyding spreadsheet
library(tidyverse)
sheets <- length(readxl::excel_sheets("data/einfra_contacts.xlsx"))
purrr::map_df(2:sheets, function(x) {
             readxl::read_xlsx("data/einfra_contacts.xlsx", sheet = x) %>% 
    gather(2:17, key = "role", value = "contact") %>%
    filter(!is.na(contact)) %>%
    select(grid_id = 1, 2:3)
  }) -> tt

tt %>%
  inner_join(grid, by = c("grid_id" = "ID")) %>%
  select(-4, -9, -10, -11) %>%
  mutate(content = paste0(
                 "<b><a href='", link,"'>", Name, "</a></b></br>",
                 role, "</br>",
                contact)) -> my_df
readr::write_csv(my_df, "data/data_table.csv")
