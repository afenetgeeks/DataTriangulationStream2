## code to prepare `lga` dataset goes here

library(DataTriangulationStream2)

lga_from_dhis2 <- dplyr::tbl(stream2_pool, "measles_alt_denominator")  %>%
  collect() %>%
  filter(State %in%  unique(State)) %>%
  group_by(State, LGA) %>%
  summarise(n = dplyr::n()) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  dplyr::select(-n)

lga <- tibble::tibble(State = unique(lga_from_dhis2$State), LGA ="State level data")  %>%
  rbind(lga_from_dhis2)%>%
  mutate(State = dplyr::case_when(
    State == "Fct" ~ "Federal Capital Territory",
    TRUE ~ State
  ))

usethis::use_data(lga, overwrite = TRUE)
