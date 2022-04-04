#' inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput
#'
mod_inputs_ui <- function(id){
  ns <- NS(id)

  tagList(

    div(class = "row-page inputs-div",
        div(class = "input-3 input-2-t",
        pickerInput(inputId = ns("picker_month"), label = span(class = "info-icon-container", "Months",
                                                             tags$a(class = "info-icon-link", href="#",
                                                                 img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                                                                 span(class="info-tooltiptext", "Month information"))),
                        choices =  months_vector_util(), multiple = T, selected = months_vector_util(),
                        options = list(title = "Months",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2"))),


        div(class = "input-3 input-2-t", pickerInput(ns("picker_year"), label =  span(class = "info-icon-container", "Years",
                                                                           tags$a(class = "info-icon-link", href="#",
                                                                                  img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                                                                                  span(class="info-tooltiptext", "Years information"))),
                                                     choices = years_vector_util(), multiple = F, selected = "2021",
                                                     options = list(title = "Years",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2"))),


        div(class = "input-3 input-2-t",pickerInput( ns("picker_state"),label =  span(class = "info-icon-container", "States",
                                                                          tags$a(class = "info-icon-link", href="#",
                                                                                 img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                                                                                 span(class="info-tooltiptext", "State information"))),
                                                    choices = c(national_util(),sort(states_vector_util())), multiple = F,selected = national_util(),
                                                    options = list(title = "State",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2"))),
        div(class = "input-2 input-2-t", actionButton(inputId =  ns("update"),class = "update-btn", label = "Update"))
    )


    # f7Row( f7Col(),f7Col(f7SmartSelect(ns("picker_month"),label = "Months", choices = months_vector_util(),
    #                                    selected = months_vector_util(), searchbar = F, multiple = T,openIn = "popover")),
    #        f7Col(f7SmartSelect(ns("picker_year"),label = "Year",choices = years_vector_util(), selected = "2021",searchbar = F,multiple =  multiple_year,openIn = "popover")),
    #        f7Col(f7SmartSelect(ns("picker_state"),label = "State", choices = c(national_util(),sort(states_vector_util())), selected = national_util(), searchbar =F,multiple = multiple_state,openIn = "popover")),
    #        f7Col(tags$div(style = "margin: 34px 3px 5px 6px;",f7Button(inputId = ns("update"), label = "Update",size = "large", fill = T,shadow = T)))
    # )


  )
}

#' inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){

  moduleServer(id, function(input, output, session){

    ns <- session$ns

   dropdown_inputs <-  reactiveValues()

    observeEvent(input$update >=0,{

      dropdown_inputs$picker_year_var   <- input$picker_year
      dropdown_inputs$picker_month_var  <- input$picker_month
      dropdown_inputs$picker_state_var  <- input$picker_state

    })

    observe({
      print(input$update)
    })


    return(dropdown_inputs)

  })
}

## To be copied in the UI
# mod_inputs_ui("inputs_1")

## To be copied in the server
# mod_inputs_server("inputs_1")
