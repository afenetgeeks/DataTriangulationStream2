#' penta_coverage_diphtheria_confirmed_cases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_penta_coverage_diphtheria_confirmed_cases_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' penta_coverage_diphtheria_confirmed_cases Server Functions
#'
#' @noRd 
mod_penta_coverage_diphtheria_confirmed_cases_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_penta_coverage_diphtheria_confirmed_cases_ui("penta_coverage_diphtheria_confirmed_cases_1")
    
## To be copied in the server
# mod_penta_coverage_diphtheria_confirmed_cases_server("penta_coverage_diphtheria_confirmed_cases_1")
