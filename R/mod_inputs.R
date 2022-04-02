#' inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'@importFrom shinyMobile f7Col f7SmartSelect f7Button
#'
mod_inputs_ui <- function(id, multiple_year = FALSE,  multiple_state = FALSE){
  ns <- NS(id)

  tagList(

    f7Row( f7Col(),f7Col(f7SmartSelect(ns("picker_month"),label = "Months", choices = months_vector_util(),
                                       selected = months_vector_util(), searchbar =F,multiple = T,openIn = "popover")),
           f7Col(f7SmartSelect(ns("picker_year"),label = "Year",choices = years_vector_util(), selected = "2021",searchbar = F,multiple =  multiple_year,openIn = "popover")),
           f7Col(f7SmartSelect(ns("picker_state"),label = "State", choices = c(national_util(),sort(states_vector_util())), selected = national_util(), searchbar =F,multiple = multiple_state,openIn = "popover")),
           f7Col(tags$div(style = "margin: 34px 3px 5px 6px;",f7Button(inputId = ns("apply"), label = "Update",size = "large", fill = T,shadow = T)))
    )


  )
}

#' inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){

  moduleServer(id, function(input, output, session){

    ns <- session$ns

   dropdown_inputs <-  reactiveValues()

    observeEvent(input$apply >=0,{

      dropdown_inputs$picker_year_var   <- input$picker_year
      dropdown_inputs$picker_month_var  <- input$picker_month
      dropdown_inputs$picker_state_var  <- input$picker_state

    })

    return(dropdown_inputs)



  })
}

## To be copied in the UI
# mod_inputs_ui("inputs_1")

## To be copied in the server
# mod_inputs_server("inputs_1")
