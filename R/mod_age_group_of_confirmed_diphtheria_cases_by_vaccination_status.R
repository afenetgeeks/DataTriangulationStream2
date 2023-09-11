#' age_group_of_confirmed_diphtheria_cases_by_vaccination_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' age_group_of_confirmed_diphtheria_cases_by_vaccination_status Server Functions
#'
#' @noRd 
mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_ui("age_group_of_confirmed_diphtheria_cases_by_vaccination_status_1")
    
## To be copied in the server
# mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_server("age_group_of_confirmed_diphtheria_cases_by_vaccination_status_1")
