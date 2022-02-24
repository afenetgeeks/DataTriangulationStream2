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
#'@import shinyMobile
#'
mod_inputs_ui <- function(id){
  ns <- NS(id)

  tagList(

    f7Row(splitLayout(img(src = "flag.jpeg", height = 60, width = 110,align = "left"),
                      h1("RI/VPDs triangulation dashboard", align = "center"),
                      img(src = "nigeria-coat-of-arms.svg", height = 60, width = 110, align = "right"),
                      cellWidths = c("20%","58%", "20%"),
                      cellArgs = list(style = "padding: 20px"))),


    f7Row( f7Col(),f7Col(f7SmartSelect(ns("picker_month"),label = "Months", choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                       selected = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),searchbar =F,multiple = T,openIn = "popover")),
           f7Col(f7SmartSelect(ns("picker_year"),label = "Year",choices = c("2017", "2018", "2019", "2020", "2021"),selected = "2021",searchbar =F,multiple = F,openIn = "popover")),
           f7Col(f7SmartSelect(ns("picker_state"),label = "State", choices = c(national,sort(states_vec)), selected = "Federal Government",searchbar =F,multiple = F,openIn = "popover")),
           f7Col(tags$div(style = "margin: 34px 3px 5px 6px;",f7Button(inputId = ns("apply"), label = "Update",size = "large", fill = T,shadow = T)))
    ),
    f7Row( tags$i(style="color:#0e7290;font-size:10px","Hover over a chart to display the chart download button on the top right corner of that chart")),

  )
}

#' inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    stream2_data <- reactiveValues()


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
