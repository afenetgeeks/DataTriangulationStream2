#' inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput updatePickerInput
#'
mod_inputs_ui <- function(id, disease){
  ns <- NS(id)

  tagList(

    div(class = "row-page inputs-div",

        div(class = "input-3 input-2-t",

            span(class = "info-icon-container", "Disease" ,tags$a(class = "info-icon-link", href="#",
                                                                                                img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon")
                                                                                              #  span(class="info-tooltiptext disease-info", "Disease information")
                                                                  )),
            nav_links(disease)),

        div(class = "input-3 input-2-t",pickerInput( ns("picker_state"),label =  span(class = "info-icon-container", "States",
                                                                                      tags$a(class = "info-icon-link", href="#",
                                                                                             img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                                                                                             span(class="info-tooltiptext", "You can select only one State at a time. Select 'Federal Government' for National level data."))),
                                                     choices = c(national_util(), sort(states_vector_util())), multiple = F, selected = national_util(),
                                                     options = list(title = "State",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2"))),

        div(class = "input-3 input-2-t",   pickerInput(inputId = ns("picker_lga"), label = span(class = "info-icon-container", "LGA",
                                                                                                tags$a(class = "info-icon-link", href="#",
                                                                                                       img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                                                                                                       span(class="info-tooltiptext", "You can select only one LGA at a time. Select 'State level data' for State level data."))),
                                                       choices = "State level data" , multiple = F,selected = "State level data",
                                                       options = list(title = "LGA",`actions-box` = TRUE,size = 10, `selected-text-format` = "count > 2"))),


        div(class = "input-3 input-2-t", pickerInput(ns("picker_year"), label =  span(class = "info-icon-container", "Years",
                                                                                      tags$a(class = "info-icon-link", href="#",
                                                                                             img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                                                                                             span(class="info-tooltiptext", "You can select one or more Years"))),
                                                     choices = years_vector_util(), multiple = T, selected = "2022",
                                                     options = list(title = "Years",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2"))),


        div(class = "input-3 input-2-t",pickerInput(inputId = ns("picker_month"), label = span(class = "info-icon-container", "Months",
                                                                                             tags$a(class = "info-icon-link", href="#",
                                                                                                 img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                                                                                                 span(class="info-tooltiptext", "You can select one or more Months"))),
                                                        choices =  months_vector_util(), multiple = T, selected = months_vector_util(),
                                                        options = list(title = "Months",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2"))),

        div(class = "input-2 input-2-t", actionButton(inputId =  ns("update"),class = "update-btn", label = "Update"))
    )

  )
}

#' inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){

  moduleServer(id, function(input, output, session){

    ns <- session$ns

    state_selected <- reactive({input$picker_state})

    lga_list <- reactive({

      if(sum(state_selected() == national_util()) == 1 ){

        "State level data"

      }else{
       c("State level data", sort( lga %>%filter(State %in% state_selected()) %>%
          dplyr::pull(LGA)))
      }

    })



    observeEvent(input$picker_state, {

      # Method 1
      updatePickerInput(session = session,
                        inputId = "picker_lga",
                        choices = unique(lga_list()),
                        selected = "State level data")

    })


   dropdown_inputs <-  reactiveValues()


    observeEvent(input$update >=0,{

      dropdown_inputs$picker_lga_var    <- input$picker_lga
      dropdown_inputs$picker_year_var   <- input$picker_year
      dropdown_inputs$picker_month_var  <- input$picker_month
      dropdown_inputs$picker_state_var  <- state_selected()

    })


    return(dropdown_inputs)

  })
}

## To be copied in the UI
# mod_inputs_ui("inputs_1")

## To be copied in the server
# mod_inputs_server("inputs_1")
