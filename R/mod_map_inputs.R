#' map_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "map_charts_inputs",

        pickerInput(inputId = ns("picker_year"), label =  NULL,
                    choices = years_vector_util(), multiple = F, selected = "2021",
                    options = list(title = "Years",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

        pickerInput(ns("picker_state"),label = NULL,
                    choices = c(national_util(),sort(states_vector_util())), multiple = T,selected = national_util(),
                    options = list(title = "State",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

    )

  )
}

#' map_inputs Server Functions
#'
#' @noRd
mod_map_inputs_server <- function(id){

  moduleServer(id, function(input, output, session){

    ns <- session$ns

    dropdown_inputs <-  reactiveValues()

    observe({

      dropdown_inputs$picker_year_var   <- input$picker_year
      dropdown_inputs$picker_state_var  <- input$picker_state

    })

    return(dropdown_inputs)

  })
}

## To be copied in the UI
# mod_map_inputs_ui("map_inputs_1")

## To be copied in the server
# mod_map_inputs_server("map_inputs_1")
