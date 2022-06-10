## code to prepare `lga` dataset goes here

library(DataTriangulationStream2)

lga_from_sormas <- dplyr::tbl(stream2_pool, "sormas_measles_geocodes")  %>%
  collect() %>%
  filter(State %in%  unique(State)) %>%
  group_by(State, LGA) %>%
  summarise(n = dplyr::n()) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  dplyr::select(-n)

lga <- tibble::tibble(State = unique(lga_from_sormas$State), LGA ="State Level data")  %>%
  rbind(lga_from_sormas)%>%
  mutate(State = dplyr::case_when(
    State == "Fct" ~ "Federal Capital Territory",
    TRUE ~ State
  ))




# sum(lga %>% group_by(State) %>%
#   summarise(n = dplyr::n()) %>%
#   arrange(desc(n)) %>%
#   ungroup() %>% dplyr::pull(n))

usethis::use_data(lga, overwrite = TRUE)
