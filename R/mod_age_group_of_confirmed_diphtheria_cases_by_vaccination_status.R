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


    div(class = "col-6 col-6-t measles-col",
        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/age-group-vaccination-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        HTML("<h6 class = 'column-title'> Chart 2: Age group of confirmed diphtheria cases by vaccination status</h6>"),

        HTML(paste0('<a id="', ns("downloadData"), '" class="btn btn-default shiny-download-link download-data-btn" href="" target="_blank" download>
                      <i class="fa fa-download" aria-hidden="true"></i>
                      <div class = tooltipdiv> <p class="tooltiptext">Download the data for this Chart</p> </div>
                     </a>')),

        HTML(paste0('<a id="', ns("downloadChart"), '" class="btn btn-default shiny-download-link download-data-btn download-chart-btn" href="" target="_blank" download>
                     <i class="fa fa-chart-bar"></i>
                      <div class = tooltipdiv>
                          <p class="tooltiptext">
                              Download this Chart
                          </p>
                      </div>
                     </a>')),

        withSpinner(
          plotlyOutput(ns("plot")),
          type = 6, size = 0.3,hide.ui = F)
    )
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
