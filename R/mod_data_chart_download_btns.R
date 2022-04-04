#' data_chart_download_btns UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_chart_download_btns_ui <- function(id){
  ns <- NS(id)
  tagList(

    HTML('<a id="downloadData" class="btn btn-default shiny-download-link download-data-btn" href="" target="_blank" download>
                      <i class="fa fa-download" aria-hidden="true"></i>
                      <div class = tooltipdiv> <p class="tooltiptext">Download the data for this Chart</p> </div>
                     </a>'),

    HTML('<a id="downloadChart" class="btn btn-default shiny-download-link download-data-btn download-chart-btn" href="" target="_blank" download>
                     <i class="fa fa-chart-bar"></i>
                      <div class = tooltipdiv>
                          <p class="tooltiptext">
                              Download this Chart
                          </p>
                      </div>
                     </a>')

  )
}

#' data_chart_download_btns Server Functions
#'
#' @noRd
mod_data_chart_download_btns_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$download_chart_data <- downloadHandler(
      filename = "Chart 1- National Measles Coverage (%) by different sources, Nigeria (National Wide).csv",
      content = function(file) {
        readr::write_csv(measles_coverage(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_data_chart_download_btns_ui("data_chart_download_btns_1")

## To be copied in the server
# mod_data_chart_download_btns_server("data_chart_download_btns_1")
