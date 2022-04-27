#' dashboard_heading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_dashboard_heading_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "mainHeader",
        img(class = "title-left-img", src = "www/nigeria-coat-of-arms.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img"),
        div(class = "title-text-div",
            h6(class= "title-text", "Nigeria RI/VPDs Data"),
            h5(class= "title-text", "Triangulation Dashboard")),
        img(class = "title-right-img", src = "www/flag.jpeg",  height = 40, width = 80, alt="nigeria coat of arms", role="img", align="center")
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
