## code to prepare `states_gadm_sp_data` dataset goes here


library(GADMTools)
states_gadm_sp_data <- GADMTools::gadm_sp_loadCountries(c("NGA"), level=1, basefile = "./data-raw/")

states_gadm_sp_data$spdf@data <- states_gadm_sp_data$spdf@data |>
  mutate(NAME_1 = str_replace(NAME_1,pattern = "Nassarawa",replacement = "Nasarawa"),
         NAME_1 = str_replace(NAME_1,pattern = "Akwa Ibom",replacement = "Akwa-Ibom"))

usethis::use_data(states_gadm_sp_data, overwrite = TRUE)

