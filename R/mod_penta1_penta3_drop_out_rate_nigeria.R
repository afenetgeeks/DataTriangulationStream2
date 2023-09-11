#' penta1_penta3_drop_out_rate_nigeria UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_penta1_penta3_drop_out_rate_nigeria_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' penta1_penta3_drop_out_rate_nigeria Server Functions
#'
#' @noRd 
mod_penta1_penta3_drop_out_rate_nigeria_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_penta1_penta3_drop_out_rate_nigeria_ui("penta1_penta3_drop_out_rate_nigeria_1")
    
## To be copied in the server
# mod_penta1_penta3_drop_out_rate_nigeria_server("penta1_penta3_drop_out_rate_nigeria_1")
