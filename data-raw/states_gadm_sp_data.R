## code to prepare `states_gadm_sp_data` dataset goes here


library(GADMTools)
states_gadm_sp_data <- GADMTools::gadm_sp_loadCountries(c("NGA"), level=1, basefile = "./data-raw/")

usethis::use_data(states_gadm_sp_data, overwrite = TRUE)

