#' penta_vaccine_stock_analysis_penta_coverage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_penta_vaccine_stock_analysis_penta_coverage_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' penta_vaccine_stock_analysis_penta_coverage Server Functions
#'
#' @noRd 
mod_penta_vaccine_stock_analysis_penta_coverage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_penta_vaccine_stock_analysis_penta_coverage_ui("penta_vaccine_stock_analysis_penta_coverage_1")
    
## To be copied in the server
# mod_penta_vaccine_stock_analysis_penta_coverage_server("penta_vaccine_stock_analysis_penta_coverage_1")
