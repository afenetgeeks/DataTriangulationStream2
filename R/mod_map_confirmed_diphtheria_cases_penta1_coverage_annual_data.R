#' map_confirmed_diphtheria_cases_penta1_coverage_annual_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_confirmed_diphtheria_cases_penta1_coverage_annual_data_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-12 col-12-t measles-col",

        span(class = "info-icon-container",
             tags$a(class = "info-icon-link", href="#",
                    img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                    span(class="info-tooltiptext",
                         p(style="color:#ffffff;font-size:10px;algin:left;", "The blue bubbles represent clusters of measles cases in a State. The numbers in each bubble are cases in that cluster"),
                         p(style="color:#ffffff;font-size:10px;algin:left;","Click on a cluster bubble to zoom in a cluster")))),

        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/partially-vaccinated-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        HTML("<h6 class = 'column-title column-title-map'>Chart 5: Confirmed diphtheria cases, Penta 1 coverage </h6>"),


        div(class = "map_charts_inputs",

            pickerInput(inputId = ns("picker_year"), label =  NULL,
                        choices = years_vector_util(), multiple = F, selected = "2023",
                        options = list(title = "Years",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

            pickerInput(inputId = ns("picker_month"), label =  NULL,
                        choices = c("Year Data", months_vector_util()), multiple = F, selected = "Year Data",
                        options = list(title = "Months",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

            pickerInput(ns("picker_state"),label = NULL,
                        choices = c(national_util(),sort(states_vector_util())), multiple = T,selected = national_util(),
                        options = list(title = "State",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2"))

        ),


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

        withSpinner(leafletOutput(ns("mvcMap"), height=440),type = 6, size = 0.4,hide.ui = F)
    )


  )
}

#' map_confirmed_diphtheria_cases_penta1_coverage_annual_data Server Functions
#'
#' @noRd
mod_map_confirmed_diphtheria_cases_penta1_coverage_annual_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_map_confirmed_diphtheria_cases_penta1_coverage_annual_data_ui("map_confirmed_diphtheria_cases_penta1_coverage_annual_data_1")

## To be copied in the server
# mod_map_confirmed_diphtheria_cases_penta1_coverage_annual_data_server("map_confirmed_diphtheria_cases_penta1_coverage_annual_data_1")
