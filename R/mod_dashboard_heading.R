#' dashboard_heading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashboard_heading_ui <- function(id){
  ns <- NS(id)
  tagList(

    f7Row(splitLayout(img(src = "www/flag.jpeg", height = 60, width = 110,align = "left"),
                      h1("RI/VPDs triangulation dashboard", align = "center"),
                      img(src = "www/nigeria-coat-of-arms.svg", height = 60, width = 110, align = "right"),
                      cellWidths = c("20%","58%", "20%"),
                      cellArgs = list(style = "padding: 20px"))
          )

  )
}

#' dashboard_heading Server Functions
#'
#' @noRd
mod_dashboard_heading_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_dashboard_heading_ui("dashboard_heading_1")

## To be copied in the server
# mod_dashboard_heading_server("dashboard_heading_1")
