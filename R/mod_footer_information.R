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
    div(class="row-page abbreviations_last_updated_div",
        div(class = "abbreviations_div",
            p("*WUENIC - WHO and UNICEF Estimates of National Immunization Coverage"),
            p("*MICS/ NICS - Multiple Indicator Cluster Survey/Nigeria Immunization Coverage Survey"),
            p("*NDHS - National Demographic and Health Survey"),
            p("*SMART survey - National Nutrition and Health Survey")),

        div(class = "last_updated_div",
            p("Last Updated:"),
            p("-27/06/2024")
        )
    ),

    div(class="footer",

        HTML(paste0(
          "<script>",
          "var today = new Date();",
          "var yyyy = today.getFullYear();",
          "</script>",
          "<p style = 'text-align: center;'>&copy; <script>document.write(yyyy);</script> All Rights Reserved </p>")
        ),
        img(class = "npchcda-img", src = "www/nphcda-logo.svg", height = 35, width = 100, alt="nphcda logo", role="img"),

        img(class = "npchcda-img", src = "www/ncdc-logo.jpg", height = 35, width = 100, alt="ncdc logo", role="img")

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
