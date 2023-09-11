#' map_confirmed_diphtheria_cases_penta1_coverage_annual_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_confirmed_diphtheria_cases_penta1_coverage_annual_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' map_confirmed_diphtheria_cases_penta1_coverage_annual_data Server Functions
#'
#' @noRd 
mod_map_confirmed_diphtheria_cases_penta1_coverage_annual_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_map_confirmed_diphtheria_cases_penta1_coverage_annual_data_ui("map_confirmed_diphtheria_cases_penta1_coverage_annual_data_1")
    
## To be copied in the server
# mod_map_confirmed_diphtheria_cases_penta1_coverage_annual_data_server("map_confirmed_diphtheria_cases_penta1_coverage_annual_data_1")
