#' footer_information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom htmltools HTML
mod_footer_information_ui <- function(id){
  ns <- NS(id)
  tagList(
    f7Row(tags$i(style="color: #0e7290; font-size: 10px",
                 "*PMCCs - Post Measles Campaign Coverage Survey,
        *WUENIC - WHO and UNICEF Estimates of National Immunization Coverage,
        *MICS/ NICS - Multiple Indicator Cluster Survey/Nigeria Immunization Coverage Survey,
        *NDHS - National Demographic and Health Survey,
      *SMART survey - National Nutrition and Health Survey")),
    f7Row(#textOutput("keepAlive"),
      HTML(paste0(
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'><small>&copy; <script>document.write(yyyy);</script></small> <a href='#' target='_blank'>Nigeria Data Triangulation Dashboard</a> </p>")
      )
    )

  )
}

#' footer_information Server Functions
#'
#' @noRd
mod_footer_information_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_footer_information_ui("footer_information_1")

## To be copied in the server
# mod_footer_information_server("footer_information_1")
